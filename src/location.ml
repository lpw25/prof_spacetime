
type t = {
  depth: int;
  display: string;
  foreign: bool;
}

let create ~depth ~location =
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
  { depth; display; foreign }

let to_json { depth; display; foreign } =
  `Assoc [ "depth", `Int depth;
           "display", `String display;
           "foreign", `Bool foreign ]
