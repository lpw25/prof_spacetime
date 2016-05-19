
type t

val of_int64 : Int64.t -> t

val to_string : t -> string

val equal : t -> t -> bool

val compare : t -> t -> int

module Set : Set.S with type elt = t

module Assoc_list : sig
  type nonrec 'a t = (t * 'a) list

  val sort_val : ('a -> 'a -> int) -> 'a t -> 'a t
  val to_json_assoc :
    ('a -> Yojson.Basic.json) -> 'a t -> (string * Yojson.Basic.json) list
end

module Map : sig
  include Map.S with type key = t
  val to_json_assoc :
    (key -> 'a -> Yojson.Basic.json) -> 'a t -> (string * Yojson.Basic.json) list
  val to_assoc_list : 'a t -> 'a Assoc_list.t
end
