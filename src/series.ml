
type t =
  { snapshots : Snapshot.t list;
    has_call_counts : bool;
    final_time : float;
    call_entries : Section.Call.t Section.t Lazy.t;
    inverted_call_entries : Section.Call.t Section.t Lazy.t; }

let create series =
  let snapshots =
    List.map Snapshot.create (Spacetime_lib.Series.snapshots series)
  in
  let has_call_counts = Spacetime_lib.Series.has_call_counts series in
  let final_time = Spacetime_lib.Series.final_time series in
  let raw_call_entries = Spacetime_lib.Series.call_entries series in
  let call_entries =
    lazy (Section.of_call_entries ~inverted:false raw_call_entries)
  in
  let inverted_call_entries =
    lazy (Section.of_call_entries ~inverted:true raw_call_entries)
  in
  { snapshots; has_call_counts; final_time;
    call_entries; inverted_call_entries }

let snapshots { snapshots } = snapshots

let has_call_counts { has_call_counts } = has_call_counts

let final_time { final_time } = final_time

let call_entries t ~inverted =
  if inverted then Lazy.force t.inverted_call_entries
  else Lazy.force t.call_entries
