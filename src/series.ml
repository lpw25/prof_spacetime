
type t = {
  snapshots: Snapshot.t list;
  frames: (Address.t * Location.t) list;
  locations: Location.t Address.Map.t Lazy.t;
  reduced: Location.t Address.Map.t Lazy.t;
  depth: int;
}

let snapshots t = t.snapshots

let locations_of_snapshots snapshots =
  List.fold_left
    Snapshot.locations'
    Address.Map.empty snapshots

let reduced_locations snapshots locations =
  let maximums =
    Address.Map.fold
      (fun addr _ acc ->
         let max =
           List.fold_left
             (fun acc snapshot ->
                let words = Snapshot.words snapshot addr in
                if words > acc then words
                else acc)
             0 snapshots
         in
         Address.Map.add addr max acc)
      locations Address.Map.empty
  in
  let num_important = 50 in
  let addr_array = Array.make num_important None in
  let max_array = Array.make num_important 0 in
  Address.Map.iter
    (fun addr max ->
       if max > max_array.(0) then begin
         try
           for i = 1 to num_important - 1 do
             if max > max_array.(i) then begin
               max_array.(i-1) <- max_array.(i);
               addr_array.(i-1) <- addr_array.(i)
             end else begin
               max_array.(i-1) <- max;
               addr_array.(i-1) <- Some addr;
               raise Exit
             end
           done;
           max_array.(num_important - 1) <- max;
           addr_array.(num_important - 1) <- Some addr;
         with Exit -> ()
       end)
    maximums;
  Array.fold_left
    (fun acc addr ->
       match addr with
       | None -> acc
       | Some addr ->
         match Address.Map.find addr locations with
         | exception Not_found -> acc
         | loc -> Address.Map.add addr loc acc)
    Address.Map.empty addr_array

let initial series =
  let snapshots = List.map Snapshot.initial series in
  let frames = [] in
  let locations = lazy (locations_of_snapshots snapshots) in
  let reduced = lazy (reduced_locations snapshots (Lazy.force locations)) in
  let depth = 0 in
  { snapshots; frames; locations; reduced; depth }

let project { snapshots; frames; locations; depth } addr =
  let parent_locations = Lazy.force locations in
  let frames =
    match Address.Map.find addr parent_locations with
    | frame -> (addr, frame) :: frames
    | exception Not_found -> frames
  in
  let snapshots =
    List.map (fun snapshot -> Snapshot.project snapshot addr) snapshots
  in
  let locations = lazy (locations_of_snapshots snapshots) in
  let reduced = lazy (reduced_locations snapshots (Lazy.force locations)) in
  let depth = depth + 1 in
  { snapshots; frames; locations; reduced; depth }

let locations { locations } = Lazy.force locations

let addresses { snapshots } =
  List.fold_left
    Snapshot.addresses'
    Address.Set.empty snapshots

let projections t =
  Address.Set.fold
    (fun addr acc ->
       let proj = project t addr in
       Address.Map.add addr proj acc)
    (addresses t) Address.Map.empty

let locations_json { locations } =
  let locations = Lazy.force locations in
  let locations_json_assoc =
    Address.Map.to_json_assoc Location.to_json locations
  in
  let unknown = ("0", Location.to_json Location.unknown) in
  `Assoc (locations_json_assoc @ [unknown])

let snapshots_json { snapshots; locations } =
  let locations = Lazy.force locations in
  let jsons = List.map (Snapshot.to_json locations) snapshots in
  `List jsons

let reduced_locations_json path { reduced } =
  let reduced = Lazy.force reduced in
  let reduced_json_assoc =
    Address.Map.to_json_assoc Location.to_json reduced
  in
  let unknown = ("0", Location.to_json Location.unknown) in
  let other = ("1", Location.to_json (Location.other path)) in
  `Assoc (reduced_json_assoc @ [other; unknown])

let reduced_snapshots_json { snapshots; reduced } =
  let reduced = Lazy.force reduced in
  let jsons = List.map (Snapshot.to_json reduced) snapshots in
  `List jsons

let frames_json { frames } =
  let star_json =
    `Assoc [ "display", `String "*";
             "path", `String "/red" ]
  in
  let _, jsons =
    List.fold_right
      (fun (addr, loc) (path, jsons) ->
         let path = path ^ "/" ^ (Address.to_string addr) in
         let json =
           `Assoc [ "display", `String (Location.display loc);
                    "path", `String path ]
         in
         (path, json :: jsons))
      frames ("/red", [star_json])
  in
  `List jsons

let to_json t =
  `Assoc [ "locations", locations_json t;
           "snapshots", snapshots_json t;
           "frames", frames_json t;
           "depth", `Int t.depth;
           "mode", `String "bytes"; ]

let to_reduced_json path t =
  `Assoc [ "locations", reduced_locations_json path t;
           "snapshots", reduced_snapshots_json t;
           "frames", frames_json t;
           "depth", `Int t.depth;
           "mode", `String "bytes"; ]
