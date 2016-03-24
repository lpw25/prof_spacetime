
type t

val create : depth:int -> location:AProf.Location.t -> t

val to_json : t -> Yojson.Basic.json
