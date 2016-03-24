
type t = Snapshot.t list

let initial series =
  let snapshots = List.map Snapshot.initial series in
  snapshots

let project snapshots addr =
  let snapshots =
    List.map (fun snapshot -> Snapshot.project snapshot addr) snapshots
  in
  snapshots

let locations snapshots =
    List.fold_left
      (fun acc snapshot -> Snapshot.locations' acc snapshot)
      Address.Map.empty snapshots

let to_json snapshots =
  let locations = locations snapshots in
  let locations_json = Address.Map.to_json Location.to_json locations in
  let snapshots_json = List.map (Snapshot.to_json locations) snapshots in
    `Assoc ["locations", locations_json;
            "snapshots", `List snapshots_json]
