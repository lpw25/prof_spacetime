
type t = Int64.t

let of_int64 t = t

let to_string = Int64.to_string

let equal = Int64.equal

let compare = Int64.compare

let equal_list l1 l2 =
  let rec loop l1 l2 =
    match l1, l2 with
    | [], [] -> true
    | t1 :: rest1, t2 :: rest2 ->
        equal t1 t2 && loop rest1 rest2
    | _ :: _, [] -> false
    | [], _ :: _ -> false
  in
  loop l1 l2

module Ord = struct

  type t = Int64.t

  let compare = Int64.compare

end

module Set = Set.Make(Ord)

module Map = Map.Make(Ord)

let pp ppf t =
  Format.fprintf ppf "%Li" t
