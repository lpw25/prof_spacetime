open Spacetime_lib

module Mode = struct

  module Allocation = struct

    type t =
      | Words
      | Blocks
      | Allocations

    let to_string = function
      | Words -> "words"
      | Blocks -> "blocks"
      | Allocations -> "allocations"

  end

  module Call = struct

    type t =
      | Calls
      | Indirect_calls

    let to_string = function
      | Calls -> "calls"
      | Indirect_calls -> "indirect-calls"

  end

  type t =
    | Words of { index : int }
    | Blocks of { index: int }
    | Allocations of { index : int }
    | Calls
    | Indirect_calls

  type allocation_or_call =
    | Allocation of int * Allocation.t
    | Call of Call.t

  let allocation_or_call = function
    | Words { index } -> Allocation(index, Allocation.Words)
    | Blocks { index } -> Allocation(index, Allocation.Blocks)
    | Allocations { index } -> Allocation(index, Allocation.Allocations)
    | Calls -> Call Call.Calls
    | Indirect_calls -> Call Call.Indirect_calls

end

let filter_opt l =
  let rec loop acc = function
    | [] -> List.rev acc
    | x :: xs ->
      let acc =
        match x with
        | Some x -> x :: acc
        | None -> acc
      in
      loop acc xs
  in
  loop [] l

let position_to_string pos
      ~address ~symbol ~print_filename ~print_symbol ~print_line_number =
  let filename =
    if print_filename then Some (Position.filename pos) else None
  in
  let position =
    if print_line_number then begin
      let line_number = Position.line_number pos in
      let start_char = Position.start_char pos in
      let end_char = Position.end_char pos in
      let position =
        Printf.sprintf "%d,%d-%d" line_number start_char end_char
      in
      Some position
    end else begin
      None
    end
  in
  let symbol =
    if print_symbol then begin
      match symbol with
      | None -> Some (Int64.to_string address)
      | Some symbol -> Some symbol
    end else begin
      None
    end
  in
  String.concat ","
    (filter_opt [filename; position; symbol])

let backtrace_to_string backtrace ~inverted
      ~print_filename ~print_symbol ~print_line_number =
  let backtrace =
    List.concat
      (List.map
         (fun loc ->
           let address = Location.address loc in
           let symbol = Location.symbol loc in
           let positions = Location.position loc in
           List.map
             (position_to_string ~address ~symbol
                ~print_filename ~print_symbol ~print_line_number)
             positions)
         backtrace)
  in
  let backtrace = if inverted then List.rev backtrace else backtrace in
  String.concat ";" backtrace

let print_allocation_entry entry ~mode ~inverted
      ~print_filename ~print_symbol ~print_line_number =
  let number =
    match mode with
    | Mode.Allocation.Words -> Allocation_entry.words entry
    | Mode.Allocation.Blocks -> Allocation_entry.blocks entry
    | Mode.Allocation.Allocations -> Allocation_entry.allocations entry
  in
  if number > 0 then begin
    let backtrace = Allocation_entry.backtrace entry in
    let backtrace =
      backtrace_to_string backtrace
        ~inverted ~print_filename ~print_symbol ~print_line_number
    in
    try
      Printf.printf "%s %d\n" backtrace number
    with Sys_error _ ->
      close_out_noerr stdout;
      exit 2
  end

let print_call_entry entry ~mode ~inverted
      ~print_filename ~print_symbol ~print_line_number =
  let number =
    match mode with
    | Mode.Call.Calls -> Call_entry.calls entry
    | Mode.Call.Indirect_calls ->
      if Call_entry.direct entry then 0
      else Call_entry.calls entry
  in
  if number > 0 then begin
    let backtrace = Call_entry.backtrace entry in
    let backtrace =
      backtrace_to_string backtrace
        ~inverted ~print_filename ~print_symbol ~print_line_number
    in
    try
      Printf.printf "%s %d\n" backtrace number
    with Sys_error _ ->
      close_out_noerr stdout;
      exit 2
  end

let print series ~mode ~inverted
      ~print_filename ~print_symbol ~print_line_number =
  let print_symbol =
    if print_filename || print_symbol || print_line_number then
      print_symbol
    else
      true
  in
  match Mode.allocation_or_call mode with
  | Allocation(index, mode) ->
      let snapshots = Series.snapshots series in
      let num_snapshots = List.length snapshots in
      let () =
        if index > num_snapshots - 1 then begin
          Printf.eprintf
            "Snapshot index out of bound, there are only %d in total\n%!"
            num_snapshots;
          exit 1
        end
      in
      let snapshot = List.nth snapshots index in
      let entries = Snapshot.allocation_entries snapshot in
      List.iter
        (print_allocation_entry ~mode ~inverted
           ~print_filename ~print_symbol ~print_line_number)
        entries
  | Call mode ->
      let () =
        if not (Series.has_call_counts series) then begin
          Printf.eprintf
            "Profile does not contain call counts\n%!";
          exit 1
        end
      in
      let entries = Series.call_entries series in
      List.iter
        (print_call_entry ~mode ~inverted
           ~print_filename ~print_symbol ~print_line_number)
        entries

