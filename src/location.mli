
type t

val create : depth:int -> location:Spacetime_lib.Location.t -> t

val display : t -> string

val to_json : t -> Yojson.Basic.json
