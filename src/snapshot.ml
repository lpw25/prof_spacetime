
type t = {
  max_depth: int;
  time : float;
  stats : Spacetime_lib.Stats.t;
  index : projection Address.Map.t;
}

and projection = {
  entries : Spacetime_lib.Entries.t;
  snapshot : t Lazy.t;
  blocks : int Lazy.t;
  words : int Lazy.t;
}

let time t = t.time

let stats t = t.stats

let nth_or_last max l =
  let rec loop n = function
    | [] -> None
    | [hd] -> Some(hd, n)
    | hd :: tl ->
      if n = max then Some(hd, n)
      else loop (n + 1) tl
  in
  loop 0 l

let rec create max_depth time stats entries =
  let preindex =
    Spacetime_lib.Entries.fold
      (fun entry acc ->
         let backtrace = Spacetime_lib.Entry.backtrace entry in
         match nth_or_last max_depth backtrace with
         | None -> acc
         | Some (loc, _) ->
           let addr = Address.of_int64 (Spacetime_lib.Location.address loc) in
           let entries =
             try
               Address.Map.find addr acc
             with Not_found -> Spacetime_lib.Entries.empty
           in
           let entries = Spacetime_lib.Entries.add entry entries in
           Address.Map.add addr entries acc)
      entries Address.Map.empty
  in
  let index =
    Address.Map.map
      (fun entries ->
         let snapshot =
           lazy (create (max_depth + 1) time stats entries)
         in
         let words =
           lazy
             (Spacetime_lib.Entries.fold
                (fun entry acc -> acc + (Spacetime_lib.Entry.words entry))
                entries 0)
         in
         let blocks =
           lazy
             (Spacetime_lib.Entries.fold
                (fun entry acc -> acc + (Spacetime_lib.Entry.blocks entry))
                entries 0)
         in
         {entries; snapshot; words; blocks})
      preindex
  in
  { max_depth; time; stats; index }

let initial snapshot =
  let time = Spacetime_lib.Snapshot.time snapshot in
  let stats = Spacetime_lib.Snapshot.stats snapshot in
  let entries = Spacetime_lib.Snapshot.entries snapshot in
  create 0 time stats entries

let project t addr =
  match Address.Map.find addr t.index with
  | exception Not_found ->
    { t with max_depth = t.max_depth + 1; index = Address.Map.empty; }
  | { snapshot = lazy snapshot } -> snapshot

let locations' acc snapshot =
  Address.Map.fold
    (fun addr proj acc ->
       match Spacetime_lib.Entries.choose proj.entries with
       | exception Not_found -> acc
       | entry ->
         let backtrace = Spacetime_lib.Entry.backtrace entry in
         match nth_or_last snapshot.max_depth backtrace with
         | None -> acc
         | Some(location, depth) ->
           let location = Location.create ~depth ~location in
           Address.Map.add addr location acc)
    snapshot.index acc

let locations snapshot = locations' Address.Map.empty snapshot

let addresses' acc snapshot =
  Address.Map.fold
    (fun addr _ acc -> Address.Set.add addr acc)
    snapshot.index acc

let addresses snapshot =
  addresses' Address.Set.empty snapshot

let get_values' locations t =
  let words =
    Address.Map.fold
      (fun addr _ acc ->
         match Address.Map.find addr t.index with
         | exception Not_found -> Address.Map.add addr 0 acc
         | { words = lazy words } -> Address.Map.add addr words acc)
      locations Address.Map.empty
  in
  let values = Address.Map.to_assoc_list words in
  let values = Address.Assoc_list.sort_val (fun x y -> compare y x) values in
  values

let to_summary_list locations t =
  let values = get_values' locations t in
  List.map (fun (address, value) ->
    let key =
      match Address.Map.find address locations with
      | loc -> Location.display loc
      | exception Not_found -> Address.to_string address
    in
    address, key, value)
    values

let to_json locations t =
  let values = get_values' locations t in
  let values = Address.Assoc_list.to_json_assoc (fun words -> `Int words) values in
  let values =
    let scanned = Spacetime_lib.Stats.words_scanned t.stats in
    let scanned_profinfo =
      Spacetime_lib.Stats.words_scanned_with_profinfo t.stats
    in
    if t.max_depth = 0 then
      `Assoc (("UNKNOWN", `Int (scanned - scanned_profinfo)) :: values)
    else
      `Assoc values
  in
  `Assoc ["time", `Float t.time;
          "values", values]
