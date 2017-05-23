
type t

val create : Spacetime_lib.Snapshot.t -> t

val time : t -> float

val stats : t -> Spacetime_lib.Stats.t option

val allocation_entries : t -> inverted:bool -> Section.Allocation.t Section.t
