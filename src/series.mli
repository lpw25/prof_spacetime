
type t

val initial : Spacetime_lib.Series.t -> t

val project : t -> Address.t -> t

val projections : t -> t Address.Map.t

val locations : t -> Location.t Address.Map.t

val addresses : t -> Address.Set.t

val to_json : t -> Yojson.Basic.json

val snapshots : t -> Snapshot.t list
