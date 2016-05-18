
type t

val unknown : t

val other : string -> t

val create : location:Spacetime_lib.Location.t -> select:string option -> t

val merge : t -> t -> t

val display : t -> string

val to_json : t -> Yojson.Basic.json
