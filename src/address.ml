
type t = Int64.t

let of_int64 t = t

let to_string = Int64.to_string

let equal = Int64.equal

let compare = Int64.compare

module Ord = struct

  type t = Int64.t

  let compare = compare

end

module Set = Set.Make(Ord)

module Map = struct

  include Map.Make(Ord)

  let to_json value_to_json t =
    let assoc =
      fold
        (fun key value acc ->
           (to_string key, value_to_json value) :: acc)
        t []
    in
    `Assoc assoc

end
