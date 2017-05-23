
type t = {
  time : float;
  stats : Spacetime_lib.Stats.t option;
  index : projection Address.Map.t;
  initial : bool;
}

and projection = {
  entries : Spacetime_lib.Entry.t list;
  location : Spacetime_lib.Location.t;
  snapshot : t Lazy.t;
  blocks : int;
  words : int;
  allocations : int;
  indirect_calls : int;
  direct_calls : int;
}

let time t = t.time

let stats t = t.stats

let proj_words p = p.words

let word_size = 8

let proj_bytes p = p.words * word_size

let proj_blocks p = p.blocks

let proj_allocations p = p.allocations

let proj_indirect_calls p = p.indirect_calls

let proj_direct_calls p = p.direct_calls

let nth depth l =
  let rec loop n = function
    | [] -> None
    | [hd] -> Some(hd, true)
    | hd :: tl ->
      if n = depth then Some(hd, false)
      else loop (n + 1) tl
  in
  loop 0 l

let rec create initial depth time stats entries ~inverted =
  let preindex =
    List.fold_left
      (fun acc entry ->
         let words = Spacetime_lib.Entry.words entry in
         let blocks = Spacetime_lib.Entry.blocks entry in
         let allocations = Spacetime_lib.Entry.allocations entry in
         let indirect_calls = Spacetime_lib.Entry.indirect_calls entry in
         let direct_calls = Spacetime_lib.Entry.direct_calls entry in
         let backtrace = Spacetime_lib.Entry.backtrace entry in
         let backtrace = if inverted then List.rev backtrace else backtrace in
         match nth depth backtrace with
         | None -> acc
         | Some (loc, bottom) ->
           let addr = Address.of_int64 (Spacetime_lib.Location.address loc) in
           let entries_acc, loc_acc, words_acc, blocks_acc, allocations_acc,
               indirect_calls_acc, direct_calls_acc =
             try
               Address.Map.find addr acc
             with Not_found -> [], loc, 0, 0, 0, 0, 0
           in
           let entries_acc =
             if bottom then entries_acc
             else entry :: entries_acc
           in
           let words_acc = words_acc + words in
           let blocks_acc = blocks_acc + blocks in
           let allocations_acc = allocations_acc + allocations in
           let indirect_calls_acc = indirect_calls_acc + indirect_calls in
           let direct_calls_acc = direct_calls_acc + direct_calls in
           Address.Map.add addr
             (entries_acc, loc_acc, words_acc, blocks_acc, allocations_acc,
               indirect_calls_acc, direct_calls_acc)
             acc)
      Address.Map.empty entries
  in
  let index =
    Address.Map.fold
      (fun addr (entries, location, words, blocks, allocations, indirect_calls,
             direct_calls) acc ->
         let depth = depth + 1 in
         let snapshot =
           lazy (create false depth time stats entries ~inverted)
         in
         let proj =
           { entries; location; snapshot; words; blocks; allocations;
             indirect_calls; direct_calls;
           }
         in
         Address.Map.add addr proj acc)
      preindex Address.Map.empty
  in
  { time; stats; index; initial }

let initial snapshot ~inverted =
  let time = Spacetime_lib.Snapshot.time snapshot in
  let stats = Spacetime_lib.Snapshot.stats snapshot in
  let entries = Spacetime_lib.Snapshot.entries snapshot in
  create true 0 time stats entries ~inverted

let project t addr =
  match Address.Map.find addr t.index with
  | exception Not_found ->
    { t with index = Address.Map.empty; }
  | { snapshot = lazy snapshot } -> snapshot

let locations' acc snapshot =
  Address.Map.fold
    (fun addr proj acc ->
       let location = proj.location in
       let selectable =
         match proj.entries with
         | [] -> false
         | _ -> true
       in
       let location = Location.create ~selectable ~location in
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

let allocations t addr =
  match Address.Map.find addr t.index with
  | exception Not_found -> 0
  | { allocations } -> allocations

let indirect_calls t addr =
  match Address.Map.find addr t.index with
  | exception Not_found -> 0
  | { indirect_calls } -> indirect_calls

let direct_calls t addr =
  match Address.Map.find addr t.index with
  | exception Not_found -> 0
  | { direct_calls } -> direct_calls

let get_values ~get_value locations t =
  let values = Address.Map.map (fun _ -> 0) locations in
  let values, other =
    Address.Map.fold
      (fun addr proj (map, other) ->
         let value = get_value proj in
         if Address.Map.mem addr locations then
           Address.Map.add addr value map, other
         else
           map, other + value)
      t.index (values, 0)
  in
  let values = Address.Map.to_assoc_list values in
  let values = Address.Assoc_list.sort_val (fun x y -> compare y x) values in
  values, other

let to_summary_list ?(mode = `Words) locations t =
  let values, _ =
    let get_value =
      match mode with
      | `Words -> proj_words
      | `Blocks -> proj_blocks
      | `Allocations -> proj_allocations
      | `Indirect_calls -> proj_indirect_calls
      | `Direct_calls -> proj_direct_calls
    in
    get_values ~get_value locations t
  in
  List.map (fun (address, value) ->
    let key =
      match Address.Map.find address locations with
      | loc -> Location.display loc
      | exception Not_found -> Address.to_string address
    in
    address, key, value)
    values

let to_json mode locations t =
  let unknown =
    if t.initial then
      match mode with
      | Path.Bytes ->
        begin match t.stats with
        | None -> ("0", `Int 0)
        | Some stats ->
          let scanned = Spacetime_lib.Stats.words_scanned stats in
          let scanned_profinfo =
            Spacetime_lib.Stats.words_scanned_with_profinfo stats
          in
          ("0", `Int ((scanned - scanned_profinfo) * word_size))
        end
      | Path.Blocks | Path.Allocations | Path.Indirect_calls
      | Path.Direct_calls ->
        ("0", `Int 0)
    else
      ("0", `Int 0)
  in
  let get_value =
    match mode with
    | Path.Bytes -> proj_bytes
    | Path.Blocks -> proj_blocks
    | Path.Allocations -> proj_allocations
    | Path.Direct_calls -> proj_direct_calls
    | Path.Indirect_calls -> proj_indirect_calls
  in
  let values, other = get_values ~get_value locations t in
  let values =
    Address.Assoc_list.to_json_assoc (fun value -> `Int value) values
  in
  let other = ("1", `Int other) in
  `Assoc ["time", `Float t.time;
          "values", `Assoc (values @ [other; unknown])]
