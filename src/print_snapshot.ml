open Spacetime_lib

let id x = x

let filter_map l ~f =
  let rec loop acc = function
    | [ ]     -> List.rev acc
    | x :: xs ->
      loop
        (match f x with
         | Some x -> x :: acc
         | None   ->      acc)
        xs
  in
  loop [] l
;;

let position_to_string
      ~address ~symbol pos ~print_filename ~print_symbol ~print_line_number =
  let filename =
    if print_filename then Some (Position.filename pos) else None
  in
  let line_number =
    if print_line_number then
      Some (Printf.sprintf "%d,%d-%d"
              (Position.line_number pos)
              (Position.start_char pos)
              (Position.end_char pos))
    else None
  in
  let symbol =
    if print_symbol then begin
      match symbol with
      | None     -> Some (Int64.to_string address)
      | Some sym -> Some sym
    end else
      None
  in
  String.concat "," (filter_map [filename ; line_number ; symbol] ~f:id)
;;

let backtrace_to_string backtrace ~print_filename ~print_symbol ~print_line_number =
  let location_to_string loc =
    List.map
      (position_to_string
         ~address:(Location.address loc) ~symbol:(Location.symbol loc)
         ~print_filename ~print_symbol ~print_line_number)
      (Location.position loc)
  in

  List.map location_to_string backtrace
  |> List.concat
  |> String.concat ";"
;;

let print_entry entry ~mode ~print_filename ~print_symbol ~print_line_number =
  let number =
    match mode with
    | `Words       -> Entry.words       entry
    | `Blocks      -> Entry.blocks      entry
    | `Allocations -> Entry.allocations entry
  in
  if number > 0 then begin
    backtrace_to_string (Entry.backtrace entry)
      ~print_filename ~print_symbol ~print_line_number
    |> print_string;
    print_char ' ';
    print_int number;
    print_newline ()
  end

let print snapshot ~mode ~print_filename ~print_symbol ~print_line_number =
  let print_filename =
    if not print_filename && not print_symbol && not print_line_number then
      true
    else print_filename
  in
  List.iter
    (print_entry ~mode ~print_filename ~print_symbol ~print_line_number)
    (Snapshot.entries snapshot)
