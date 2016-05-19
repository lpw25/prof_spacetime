
type t = {
  snapshots: Snapshot.t list;
  frames: (Address.t * Location.t) list;
  locations: Location.t Address.Map.t Lazy.t;
  reduced_bytes: Location.t Address.Map.t Lazy.t;
  reduced_blocks: Location.t Address.Map.t Lazy.t;
  reduced_allocations: Location.t Address.Map.t Lazy.t;
  depth: int;
}

let snapshots t = t.snapshots

let locations_of_snapshots snapshots =
  List.fold_left
    Snapshot.locations'
    Address.Map.empty snapshots

let reduced_locations mode snapshots locations =
  let maximums =
    Address.Map.fold
      (fun addr _ acc ->
         let max =
           List.fold_left
             (fun acc snapshot ->
                let count =
                  match mode with
                  | Path.Bytes -> Snapshot.words snapshot addr
                  | Path.Blocks -> Snapshot.blocks snapshot addr
                  | Path.Allocations -> failwith "Not implemented"
                in
                if count > acc then count
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
  let reduced_bytes =
    lazy (reduced_locations Path.Bytes snapshots (Lazy.force locations))
  in
  let reduced_blocks =
    lazy (reduced_locations Path.Blocks snapshots (Lazy.force locations))
  in
  let reduced_allocations =
    lazy (reduced_locations Path.Allocations snapshots (Lazy.force locations))
  in
  let depth = 0 in
  { snapshots; frames; locations; reduced_bytes;
    reduced_blocks; reduced_allocations; depth }

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
  let reduced_bytes =
    lazy (reduced_locations Path.Bytes snapshots (Lazy.force locations))
  in
  let reduced_blocks =
    lazy (reduced_locations Path.Blocks snapshots (Lazy.force locations))
  in
  let reduced_allocations =
    lazy (reduced_locations Path.Allocations snapshots (Lazy.force locations))
  in
  let depth = depth + 1 in
  { snapshots; frames; locations; reduced_bytes;
    reduced_blocks; reduced_allocations; depth }

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

let all_locations_json path t =
  let locations = Lazy.force t.locations in
  let locations_json_assoc =
    Address.Map.to_json_assoc
      (fun addr loc ->
         let path = Path.project path addr in
         Location.to_json path loc)
      locations
  in
  let unknown = ("0", Location.to_json path Location.unknown) in
  `Assoc (locations_json_assoc @ [unknown])

let reduced_locations_json path t =
  let mode = Path.mode path in
  let reduced =
    match mode with
    | Path.Bytes -> Lazy.force t.reduced_bytes
    | Path.Blocks -> Lazy.force t.reduced_blocks
    | Path.Allocations -> Lazy.force t.reduced_allocations
  in
  let reduced_json_assoc =
    Address.Map.to_json_assoc
      (fun addr loc ->
         let path = Path.project path addr in
         Location.to_json path loc)
      reduced
  in
  let unknown = ("0", Location.to_json path Location.unknown) in
  let all_path = Path.with_kind path Path.All in
  let other = ("1", Location.to_json all_path Location.other) in
  `Assoc (reduced_json_assoc @ [other; unknown])

let locations_json path t =
  match Path.kind path with
  | Path.Reduced -> reduced_locations_json path t
  | Path.All -> all_locations_json path t

let all_snapshots_json mode t =
  let locations = Lazy.force t.locations in
  let jsons = List.map (Snapshot.to_json mode locations) t.snapshots in
  `List jsons

let reduced_snapshots_json mode t =
  let reduced =
    match mode with
    | Path.Bytes -> Lazy.force t.reduced_bytes
    | Path.Blocks -> Lazy.force t.reduced_blocks
    | Path.Allocations -> Lazy.force t.reduced_allocations
  in
  let jsons = List.map (Snapshot.to_json mode reduced) t.snapshots in
  `List jsons

let snapshots_json path t =
  let mode = Path.mode path in
  match Path.kind path with
  | Path.Reduced -> reduced_snapshots_json mode t
  | Path.All -> all_snapshots_json mode t

let frames_json path { frames } =
  let initial = Path.with_mode Path.initial (Path.mode path) in
  let star_json =
    `Assoc [ "display", `String "*";
             "path", `String (Path.to_string initial) ]
  in
  let _, jsons =
    List.fold_right
      (fun (addr, loc) (path, jsons) ->
         let path = Path.project path addr in
         let json =
           `Assoc [ "display", `String (Location.display loc);
                    "path", `String (Path.to_string path) ]
         in
         (path, json :: jsons))
      frames (initial, [star_json])
  in
  `List jsons

let bytes_json path =
  match Path.mode path with
  | Path.Bytes -> `Bool true
  | Path.Blocks | Path.Allocations -> `Bool false

let modes_json path =
  let modes = [Path.Bytes; Path.Blocks; Path.Allocations] in
  let modes_json =
    List.map
      (fun mode ->
         let display_json = `String (Path.mode_to_display_string mode) in
         let selected = (mode = (Path.mode path)) in
         let selected_json = `Bool selected in
         let path_json = `String (Path.to_string (Path.with_mode path mode)) in
         `Assoc [ "display", display_json;
                  "selected", selected_json;
                  "path", path_json; ])
      modes
  in
  `List modes_json

let to_json path t =
  `Assoc [ "locations", locations_json path t;
           "snapshots", snapshots_json path t;
           "frames", frames_json path t;
           "depth", `Int t.depth;
           "bytes", bytes_json path;
           "modes", modes_json path; ]
