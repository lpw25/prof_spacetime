
type t

val unknown : t

val other : t

val create : location:Spacetime_lib.Location.t -> selectable:bool -> t

val merge : t -> t -> t

val display : t -> string

val to_json : Path.t -> t -> Yojson.Basic.json
