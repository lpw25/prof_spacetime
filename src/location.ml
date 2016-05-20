
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

let create ~location ~selectable ~resolve_foreign =
  let foreign = Spacetime_lib.Location.foreign location in
  let display =
    if foreign then
      (* CR mshinwell: this should be reworked, maybe [Spacetime_lib]
         should receive [resolve_foreign] (or pull it out of the compiler
         distribution and make it depend on owee). *)
      let address = Spacetime_lib.Location.address location in
      match resolve_foreign address with
      | None ->
        begin match Spacetime_lib.Location.symbol location with
        | Some s -> s
        | None -> Int64.to_string address
        end
      | Some (filename, line) ->
        Printf.sprintf "%s:%d" filename line
    else
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
