
type t = {
  depth: int;
  display: string;
  foreign: bool;
}

let create ~depth ~location =
  let display =
    match AProf.Location.position location with
    | Some pos ->
      let name = AProf.Position.filename pos in
      let line = string_of_int (AProf.Position.line_number pos) in
      let first = string_of_int (AProf.Position.start_char pos) in
      let last = string_of_int (AProf.Position.end_char pos) in
      String.concat ""
        [name; "{"; line; ":"; first; "-"; last; "}"]
    | None ->
      match AProf.Location.symbol location with
      | Some s -> s
      | None -> Int64.to_string (AProf.Location.address location)
  in
  let foreign = AProf.Location.foreign location in
  { depth; display; foreign }

let to_json { depth; display; foreign } =
  `Assoc [ "depth", `Int depth;
           "display", `String display;
           "foreign", `Bool foreign ]
