module Mode : sig

  type t =
    | Bytes
    | Blocks
    | Allocations

  val equal : t -> t -> bool

  val all : t list

  val pp : Format.formatter -> t -> unit

end

module Kind : sig

  type t =
    | All
    | Reduced

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

  val create : Mode.t -> Kind.t -> Direction.t -> Address.t list -> t

  val mode : t -> Mode.t

  val kind : t -> Kind.t

  val direction : t -> Direction.t

  val addresses : t -> Address.t list

  val with_mode : t -> Mode.t -> t

  val with_kind : t -> Kind.t -> t

  val with_direction : t -> Direction.t -> t

  val equal : t -> t -> bool

  val pp : Format.formatter -> t -> unit

end

module Frame : sig

  type t

  val path : t -> Path.t

  val selected : t -> Address.t option

  val display : t -> string option

end

module Layer : sig

  module Point : sig

    type t

    val time : t -> float

    val value : t -> int

  end

  type t

  val points : t -> Point.t list

  val display : t -> string

  val foreign : t -> bool

  val selection : t -> Path.t option

end

type t

val chart : series:Series.t -> path:Path.t -> t

val frames : t -> Frame.t list

val layers : t -> Layer.t list

val path : t -> Path.t

val max_value : t -> int

val max_time : t -> float

module Memo : sig

  type chart = t

  type t

  val create : series:Series.t -> t

  val chart : t -> path:Path.t -> chart

end
