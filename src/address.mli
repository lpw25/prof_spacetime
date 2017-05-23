
type t

val of_int64 : Int64.t -> t

val to_string : t -> string

val equal : t -> t -> bool

val compare : t -> t -> int

val equal_list : t list -> t list -> bool

module Set : Set.S with type elt = t

module Map : Map.S with type key = t

val pp : Format.formatter -> t -> unit
