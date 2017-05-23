
module Mode : sig

  module Call : sig

    type t =
      | Calls
      | Indirect_calls

    val equal : t -> t -> bool

    val pp : Format.formatter -> t -> unit

  end

  module Allocation : sig

    type t =
      | Bytes
      | Blocks
      | Allocations

    val equal : t -> t -> bool

    val pp : Format.formatter -> t -> unit

  end

  type t =
    | Allocation of int * Allocation.t
    | Call of Call.t

  val equal : t -> t -> bool

  val pp : Format.formatter -> t -> unit

end

module Direction : sig

  type t =
    | Normal
    | Inverted

  val equal : t -> t -> bool

  val pp : Format.formatter -> t -> unit

end

module Path : sig

  type t

  val create : Mode.t -> Direction.t -> Address.t list -> t

  val mode : t -> Mode.t

  val direction : t -> Direction.t

  val addresses : t -> Address.t list

  val with_mode : t -> Mode.t -> t

  val with_direction : t -> Direction.t -> t

  val pp : Format.formatter -> t -> unit

end

module Frame : sig

  type t

  val path : t -> Path.t

  val selected : t -> Address.t option

  val display : t -> string option

end

module Row : sig

  type t

  val address : t -> Address.t

  val value : t -> int

  val percentage : t -> float

  val display : t -> string

  val selection : t -> Path.t option

end

type t

val table : series:Series.t -> path:Path.t -> t

val row : t -> int -> Row.t option

val size : t -> int

val time : t -> float

val total : t -> int

val frames : t -> Frame.t list

val find : (Row.t -> bool) -> t -> int

module Memo : sig

  type table = t

  type t

  val create : series:Series.t -> t

  val table : t -> path:Path.t -> table

end

