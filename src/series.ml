
type t = {
  max_depth: int;
  snapshots: Snapshot.t list;
  frames: (Address.t * Location.t) list;
  locations: Location.t Address.Map.t Lazy.t;
}

let locations_of_snapshots snapshots =
  List.fold_left
    Snapshot.locations'
    Address.Map.empty snapshots

let initial series =
  let max_depth = 0 in
  let snapshots = List.map Snapshot.initial series in
  let frames = [] in
  let locations = lazy (locations_of_snapshots snapshots) in
  { max_depth; snapshots; frames; locations }

let project { max_depth; snapshots; frames; locations } addr =
  let max_depth = max_depth + 1 in
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
  { max_depth; snapshots; frames; locations }

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

let locations_json { max_depth; locations } =
  let locations = Lazy.force locations in
  let locations_json_assoc =
    Address.Map.to_json_assoc Location.to_json locations
  in
  let unknown_json =
    `Assoc [ "depth", `Int 0;
             "display", `String "UNKNOWN";
             "foreign", `Bool false ]
  in
    if max_depth = 0 then
      `Assoc (("UNKNOWN", unknown_json) :: locations_json_assoc)
    else `Assoc locations_json_assoc

let snapshots_json { snapshots; locations } =
  let locations = Lazy.force locations in
  let jsons = List.map (Snapshot.to_json locations) snapshots in
  `List jsons

let frames_json { frames } =
  let star_json =
    `Assoc [ "display", `String "*";
             "path", `String "" ]
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
      frames ("", [star_json])
  in
  `List jsons

let to_json t =
  `Assoc [ "locations", locations_json t;
           "snapshots", snapshots_json t;
           "frames", frames_json t; ]
