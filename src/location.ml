
type t = {
  display: string;
  foreign: bool;
  select: string option;
}

let unknown =
  let display = "Unknown" in
  let foreign = false in
  let select = None in
  { display; foreign; select }

let other path =
  let display = "Other" in
  let foreign = false in
  let select = Some path in
  { display; foreign; select }

let create ~location ~select =
  let display =
    match Spacetime_lib.Location.position location with
    | Some pos ->
      let name = Spacetime_lib.Position.filename pos in
      let line = string_of_int (Spacetime_lib.Position.line_number pos) in
      let first = string_of_int (Spacetime_lib.Position.start_char pos) in
      let last = string_of_int (Spacetime_lib.Position.end_char pos) in
      String.concat ""
        [name; "{"; line; ":"; first; "-"; last; "}"]
    | None ->
      match Spacetime_lib.Location.symbol location with
      | Some s -> s
      | None -> Int64.to_string (Spacetime_lib.Location.address location)
  in
  let foreign = Spacetime_lib.Location.foreign location in
  { display; foreign; select }

let merge l1 l2 =
  match l1.select, l2.select with
  | _, None -> l1
  | None, _ -> l2
  | Some _, Some _ -> l1

let display { display } = display

let select_json = function
  | None -> `Null
  | Some select -> `String select

let to_json { display; foreign; select } =
  `Assoc [ "display", `String display;
           "foreign", `Bool foreign;
           "select", select_json select; ]
