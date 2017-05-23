
type 'a section

module Allocation : sig

  type t

  val create : words:int -> blocks:int -> allocations:int -> t

  val words : t -> int

  val bytes : t -> int

  val blocks : t -> int

  val allocations : t -> int

  val zero : t

  val sum : t -> t -> t

  val word_size : int

end

module Call : sig

  type t

  val create : calls:int -> direct:bool -> t

  val calls : t -> int

  val direct : t -> bool

  val zero : t

  val sum : t -> t -> t

end

module Item : sig

  type 'a t

  val display : 'a t -> string

  val foreign : 'a t -> bool

  val path : 'a t -> Address.t list

  val value : 'a t -> 'a

  val empty : 'a t -> bool

  val select : 'a t -> 'a section

end

type 'a t = 'a section

val empty : 'a t

val of_allocation_entries :
  inverted:bool -> Spacetime_lib.Allocation_entry.t list -> Allocation.t t

val of_call_entries :
  inverted:bool -> Spacetime_lib.Call_entry.t list -> Call.t t

val project : 'a t -> Address.t -> 'a Item.t option

val select : 'a t -> Address.t -> 'a t

val fold : (Address.t -> 'a Item.t -> 'b -> 'b) -> 'a t -> 'b -> 'b
