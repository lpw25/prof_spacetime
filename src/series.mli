
type t

val create : Spacetime_lib.Series.t -> t

val snapshots : t -> Snapshot.t list

val has_call_counts : t -> bool

val final_time : t -> float

val call_entries : t -> inverted:bool -> Section.Call.t Section.t
