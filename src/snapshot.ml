
type t = {
  time : float;
  stats : Spacetime_lib.Stats.t option;
  allocation_entries: Section.Allocation.t Section.t Lazy.t;
  inverted_allocation_entries: Section.Allocation.t Section.t Lazy.t;
}

let create snapshot =
  let time = Spacetime_lib.Snapshot.time snapshot in
  let stats = Spacetime_lib.Snapshot.stats snapshot in
  let raw_allocation_entries =
    Spacetime_lib.Snapshot.allocation_entries snapshot
  in
  let allocation_entries =
    lazy (
      Section.of_allocation_entries ~inverted:false raw_allocation_entries
    )
  in
  let inverted_allocation_entries =
    lazy (
      Section.of_allocation_entries ~inverted:true raw_allocation_entries
    )
  in
  { time; stats; allocation_entries; inverted_allocation_entries }

let time t = t.time

let stats t = t.stats

let allocation_entries t ~inverted =
  if inverted then Lazy.force t.inverted_allocation_entries
  else Lazy.force t.allocation_entries
