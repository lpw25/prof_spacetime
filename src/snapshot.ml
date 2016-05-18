
type t = {
  time : float;
  stats : Spacetime_lib.Stats.t;
  index : projection Address.Map.t;
  initial : bool;
}

and projection = {
  entries : Spacetime_lib.Entries.t;
  location : Spacetime_lib.Location.t;
  path : string;
  snapshot : t Lazy.t;
  blocks : int;
  words : int;
}

let time t = t.time

let stats t = t.stats

let nth depth l =
  let rec loop n = function
    | [] -> None
    | [hd] -> Some(hd, true)
    | hd :: tl ->
      if n = depth then Some(hd, false)
      else loop (n + 1) tl
  in
  loop 0 l

let rec create initial depth path time stats entries =
  let preindex =
    Spacetime_lib.Entries.fold
      (fun entry acc ->
         let words = Spacetime_lib.Entry.words entry in
         let blocks = Spacetime_lib.Entry.words entry in
         let backtrace = Spacetime_lib.Entry.backtrace entry in
         match nth depth backtrace with
         | None -> acc
         | Some (loc, bottom) ->
           let addr = Address.of_int64 (Spacetime_lib.Location.address loc) in
           let entries_acc, loc_acc, words_acc, blocks_acc =
             try
               Address.Map.find addr acc
             with Not_found -> Spacetime_lib.Entries.empty, loc, 0, 0
           in
           let entries_acc =
             if bottom then entries_acc
             else Spacetime_lib.Entries.add entry entries_acc
           in
           let words_acc = words_acc + words in
           let blocks_acc = blocks_acc + blocks in
           Address.Map.add addr
             (entries_acc, loc_acc, words_acc, blocks_acc) acc)
      entries Address.Map.empty
  in
  let index =
    Address.Map.fold
      (fun addr (entries, location, words, blocks) acc ->
         let depth = depth + 1 in
         let path = path ^ "/" ^ Address.to_string addr in
         let snapshot =
           lazy (create false depth path time stats entries)
         in
         let proj = {entries; location; snapshot; words; blocks; path} in
         Address.Map.add addr proj acc)
      preindex Address.Map.empty
  in
  { time; stats; index; initial }

let initial snapshot =
  let time = Spacetime_lib.Snapshot.time snapshot in
  let stats = Spacetime_lib.Snapshot.stats snapshot in
  let entries = Spacetime_lib.Snapshot.entries snapshot in
  create true 0 "/red" time stats entries

let project t addr =
  match Address.Map.find addr t.index with
  | exception Not_found ->
    { t with index = Address.Map.empty; }
  | { snapshot = lazy snapshot } -> snapshot

let locations' acc snapshot =
  Address.Map.fold
    (fun addr proj acc ->
       let location = proj.location in
       let select =
         if Spacetime_lib.Entries.is_empty proj.entries then None
         else Some proj.path
       in
       let location = Location.create ~select ~location in
       let location =
         match Address.Map.find addr acc with
         | exception Not_found -> location
         | location_acc -> Location.merge location_acc location
       in
       Address.Map.add addr location acc)
    snapshot.index acc

let locations snapshot = locations' Address.Map.empty snapshot

let addresses' acc snapshot =
  Address.Map.fold
    (fun addr _ acc -> Address.Set.add addr acc)
    snapshot.index acc

let addresses snapshot =
  addresses' Address.Set.empty snapshot

let words t addr =
  match Address.Map.find addr t.index with
  | exception Not_found -> 0
  | { words } -> words

let blocks t addr =
  match Address.Map.find addr t.index with
  | exception Not_found -> 0
  | { blocks } -> blocks

let get_values' locations t =
  let words = Address.Map.map (fun _ -> 0) locations in
  let words, other =
    Address.Map.fold
      (fun addr proj (map, other) ->
         if Address.Map.mem addr locations then
           Address.Map.add addr proj.words map, other
         else
           map, other + proj.words)
      t.index (words, 0)
  in
  let values = Address.Map.to_assoc_list words in
  let values = Address.Assoc_list.sort_val (fun x y -> compare y x) values in
  values, other

let to_summary_list locations t =
  let values, _ = get_values' locations t in
  List.map (fun (address, value) ->
    let key =
      match Address.Map.find address locations with
      | loc -> Location.display loc
      | exception Not_found -> Address.to_string address
    in
    address, key, value)
    values

let to_json locations t =
  let scanned = Spacetime_lib.Stats.words_scanned t.stats in
  let scanned_profinfo =
    Spacetime_lib.Stats.words_scanned_with_profinfo t.stats
  in
  let values, other = get_values' locations t in
  let values = Address.Assoc_list.to_json_assoc (fun words -> `Int words) values in
  let other =
    if other <> 0 then ("1", `Int other)
    else ("1", `Int 0)
  in
  let unknown =
    if t.initial then
      ("0", `Int (scanned - scanned_profinfo))
    else
      ("0", `Int 0)
  in
  `Assoc ["time", `Float t.time;
          "values", `Assoc (values @ [other; unknown])]
