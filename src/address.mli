
type t

val of_int64 : Int64.t -> t

val to_string : t -> string

val equal : t -> t -> bool

val compare : t -> t -> int

module Set : Set.S with type elt = t

module Map : sig
  include Map.S with type key = t
  val to_json_assoc :
    ('a -> Yojson.Basic.json) -> 'a t -> (string * Yojson.Basic.json) list
end
