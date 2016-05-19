
type t = Int64.t

let of_int64 t = t

let to_string = Int64.to_string

let equal = Int64.equal

let compare = Int64.compare

module Ord = struct

  type t = Int64.t

  let compare = Int64.compare

end

module Set = Set.Make(Ord)

module Assoc_list = struct
  type nonrec 'a t = (t * 'a) list

  let sort_val cmp t =
    List.sort (fun a b -> cmp (snd a) (snd b)) t

  let to_json_assoc value_to_json t =
    List.fold_left
      (fun acc (key,value) ->
         (to_string key, value_to_json value) :: acc)
      [] t
end

module Map = struct

  include Map.Make(Ord)

  let to_json_assoc value_to_json t =
    fold
      (fun key value acc ->
         (to_string key, value_to_json key value) :: acc)
      t []

  let to_assoc_list t =
    fold (fun key value acc -> (key, value) :: acc) t []
end

