
type t

val unknown : t

val other : t

val create
   : location:Spacetime_lib.Location.t
  -> selectable:bool
  -> resolve_foreign:(Int64.t -> (string * int) option)
  -> t

val merge : t -> t -> t

val display : t -> string

val to_json : Path.t -> t -> Yojson.Basic.json
