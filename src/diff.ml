
type diff =
  { words: int
  ; allocations: int
  ; blocks: int
  }

let empty_diff = { allocations = 0; words = 0; blocks = 0 }

module AllocMap = Map.Make(String)
module NameSet = Set.Make(String)


let get_alloc_map snapshot =
  let rec loop snapshot acc =
    Section.fold Section.(fun addr item acc ->
      let value = Item.value item in
      let words = Allocation.words value in
      let allocations = Allocation.allocations value in
      let blocks = Allocation.blocks value in
      try
        let display = Item.display item in
        let name = String.sub display 0 (String.index display ':') in
        AllocMap.update name (fun v -> Some (match v with
          | None ->
            { words; allocations; blocks }
          | Some v ->
            { words = words + v.words
            ; allocations = allocations + v.allocations
            ; blocks = blocks + v.blocks
            }
        )) acc
      with Not_found -> loop (Item.select item) acc
    ) snapshot acc
  in
  loop snapshot AllocMap.empty

let get_keys map =
  AllocMap.fold (fun k _ acc -> NameSet.add k acc) map NameSet.empty

let diff_maps map0 map1 =
  let keys = NameSet.union (get_keys map0) (get_keys map1) in
  let diffs =
    NameSet.elements keys
    |> List.map (fun key ->
      let diff0 = try AllocMap.find key map0 with Not_found -> empty_diff in
      let diff1 = try AllocMap.find key map1 with Not_found -> empty_diff in
      let diff =
        { words = diff0.words - diff1.words
        ; allocations = diff0.allocations - diff1.allocations
        ; blocks = diff0.blocks - diff1.blocks
        }
      in
      (key, diff))
    |> List.filter (fun (k, { allocations }) -> allocations != 0)
  in
  let max_length = NameSet.fold (fun n l -> max l (String.length n)) keys 0 in
  let padded_string i =
    let str = (if i < 0 then "" else "+") ^ string_of_int i in
    let len = String.length str in
    if len < 14 then (String.make (14 - len) ' ') ^ str else str
  in
  let pad = String.make (max_length - 4) ' ' in
  Printf.printf "File%s          Words    Allocations         Blocks\n" pad;
  diffs
    |> List.filter (fun (_, d) -> d.words != 0)
    |> List.sort (fun (_, a) (_, b) -> a.words - b.words)
    |> List.iter (fun (k, { allocations; words; blocks }) ->
      Printf.printf "%s%s %s %s %s\n"
          k
          (String.make (max_length - String.length k) ' ')
          (padded_string words)
          (padded_string allocations)
          (padded_string blocks))

let compare_snapshots snapshot0 snapshot1 =
  let a0 = Snapshot.allocation_entries ~inverted:false snapshot0 in
  let a1 = Snapshot.allocation_entries ~inverted:false snapshot1 in
  diff_maps (get_alloc_map a0) (get_alloc_map a1)

let diff ref data =
  match List.rev (Series.snapshots ref), List.rev (Series.snapshots data) with
  | snapshot0 :: _, snapshot1 :: _ ->
    compare_snapshots snapshot0 snapshot1
  | _, _ ->
    Printf.eprintf "Empty snaphot!\n"
