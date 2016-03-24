
type t

val initial : AProf.Series.t -> t

val project : t -> Address.t -> t

val locations : t -> Location.t Address.Map.t

val to_json : t -> Yojson.Basic.json
