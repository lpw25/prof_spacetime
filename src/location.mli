
type t

val create : depth:int -> location:Spacetime_lib.Location.t -> t

val to_json : t -> Yojson.Basic.json
