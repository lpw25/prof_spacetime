
type mode =
  | Bytes
  | Blocks
  | Allocations

type kind =
  | Reduced
  | All

type t

val initial : t

val create : mode -> kind -> Address.t list -> t

val mode : t -> mode

val kind : t -> kind

val addresses : t -> Address.t list

val with_mode : t -> mode -> t

val with_kind : t -> kind -> t

val project : t -> Address.t -> t

val of_string : string -> t option

val to_string : t -> string

val mode_to_string : mode -> string

val kind_to_string : kind -> string

val mode_to_display_string : mode -> string
