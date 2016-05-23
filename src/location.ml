
type t = {
  display: string;
  foreign: bool;
  selectable: bool;
}

let unknown =
  let display = "Unknown" in
  let foreign = false in
  let selectable = false in
  { display; foreign; selectable }

let other =
  let display = "Other" in
  let foreign = false in
  let selectable = true in
  { display; foreign; selectable }

let create ~location ~selectable =
  let display =
    match Spacetime_lib.Location.position location with
    | Some pos ->
      let name = Spacetime_lib.Position.filename pos in
      let line = string_of_int (Spacetime_lib.Position.line_number pos) in
      let first = string_of_int (Spacetime_lib.Position.start_char pos) in
      let last = string_of_int (Spacetime_lib.Position.end_char pos) in
      if Spacetime_lib.Position.start_char pos < 0 then
        String.concat ""
          [name; ":"; line; ]
      else
        String.concat ""
          [name; ":"; line; ","; first; "--"; last; ]
    | None ->
      match Spacetime_lib.Location.symbol location with
      | Some s -> s
      | None -> Int64.to_string (Spacetime_lib.Location.address location)
  in
  let foreign = Spacetime_lib.Location.foreign location in
  { display; foreign; selectable }

let merge l1 l2 =
  { l1 with selectable = l1.selectable || l2.selectable }

let display { display } = display

let path_json path selectable =
  if selectable then `String (Path.to_string path)
  else `Null

let to_json path { display; foreign; selectable } =
  `Assoc [ "display", `String display;
           "foreign", `Bool foreign;
           "path", path_json path selectable; ]
