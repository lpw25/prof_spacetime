
module Mode : sig

  type t =
    | Words of { index : int }
    | Blocks of { index : int }
    | Allocations of { index : int }
    | Calls
    | Indirect_calls

end

val print
  :  Spacetime_lib.Series.t
  -> mode:Mode.t
  -> inverted:bool
  -> print_filename:bool
  -> print_symbol:bool
  -> print_line_number:bool
  -> unit
