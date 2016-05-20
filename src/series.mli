
type t

val initial
   : Spacetime_lib.Series.t
  -> resolve_foreign:(Int64.t -> (string * int) option)
  -> t

val project : t -> Address.t -> t

val projections : t -> t Address.Map.t

val locations : t -> Location.t Address.Map.t

val addresses : t -> Address.Set.t

val snapshots : t -> Snapshot.t list

val to_json : Path.t -> t -> Yojson.Basic.json
