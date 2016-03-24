

type t

val time : t -> float

val stats : t -> AProf.Stats.t

val initial : AProf.Snapshot.t -> t

val project : t -> Address.t -> t

val locations : t -> Location.t Address.Map.t

val locations' : Location.t Address.Map.t -> t -> Location.t Address.Map.t

val to_json : Location.t Address.Map.t -> t -> Yojson.Basic.json
