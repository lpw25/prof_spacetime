
module Allocation = struct

  type t =
    { words : int;
      blocks : int;
      allocations : int; }

  let create ~words ~blocks ~allocations =
    { words; blocks; allocations }

  let words { words } = words

  let byte_size = 8

  let word_size =
    Sys.word_size / byte_size

  let bytes { words } = words * word_size

  let blocks { blocks } = blocks

  let allocations { allocations } = allocations

  let zero =
    { words = 0;
      blocks = 0;
      allocations = 0; }

  let sum t1 t2 =
    { words = t1.words + t2.words;
      blocks = t1.blocks + t2.blocks;
      allocations = t1.allocations + t2.allocations; }

end

module Call = struct

  type t =
    { calls : int;
      direct : bool; }

  let create ~calls ~direct =
    { calls; direct }

  let calls { calls } = calls

  let direct { direct } = direct

  let zero =
    { calls = 0;
      direct = true; }

  let sum t1 t2 =
    { calls = t1.calls + t2.calls;
      direct = t1.direct && t2.direct; }

end


type 'a t = 'a item Address.Map.t

and 'a item =
  { section : 'a t Lazy.t;
    empty : bool;
    display : string;
    foreign : bool;
    path : Address.t list;
    value : 'a; }

module Item = struct

  type 'a t = 'a item

  let display { display } = display

  let foreign { foreign } = foreign

  let path { path } = path

  let value { value } = value

  let empty { empty } = empty

  let select t = Lazy.force t.section

end

type 'a section = 'a t

let empty = Address.Map.empty

let nth depth l =
  let rec loop n = function
    | [] -> None
    | [hd] ->
      if n = 0 then Some(hd, true)
      else None
    | hd :: tl ->
      if n = 0 then Some(hd, false)
      else loop (n - 1) tl
  in
  loop depth l

let display_location loc =
  match Spacetime_lib.Location.position loc with
  | [] -> begin
      match Spacetime_lib.Location.symbol loc with
      | Some s -> s
      | None ->
          Printf.sprintf "0x%Lx"
            (Spacetime_lib.Location.address loc)
    end
  | positions -> begin
      let one_pos pos =
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
      in
      let pos =
        String.concat "; " (List.map one_pos positions)
      in
      match Spacetime_lib.Location.symbol loc with
      | Some symbol -> Printf.sprintf "%s (%s)" pos symbol
      | None -> pos
    end

let rec create ~backtrace ~nil ~cons ~inverted ~depth ~path ~entries =
  let preindex =
    List.fold_left
      (fun acc entry ->
         let backtrace = backtrace entry in
         let backtrace = if inverted then List.rev backtrace else backtrace in
         match nth depth backtrace with
         | None -> acc
         | Some (loc, last) ->
           let addr = Address.of_int64 (Spacetime_lib.Location.address loc) in
           let entries_acc, loc_acc, value_acc =
             try
               Address.Map.find addr acc
             with Not_found -> [], loc, nil
           in
           let entries_acc =
             if last then entries_acc else entry :: entries_acc
           in
           let value_acc = cons entry value_acc in
           Address.Map.add addr (entries_acc, loc_acc, value_acc) acc)
      Address.Map.empty entries
  in
  Address.Map.fold
    (fun addr (entries, loc, value) acc ->
       let depth = depth + 1 in
       let path = addr :: path in
       let empty =
         match entries with
         | [] -> true
         | _ :: _ -> false
       in
       let section =
         lazy (create ~backtrace ~nil ~cons ~inverted ~depth ~path ~entries)
       in
       let display = display_location loc in
       let foreign = Spacetime_lib.Location.foreign loc in
       let item =
         { section; empty; display; foreign; path; value; }
       in
       Address.Map.add addr item acc)
    preindex Address.Map.empty

let of_allocation_entries =
  let nil = Allocation.zero in
  let cons entry t =
    let words = Spacetime_lib.Allocation_entry.words entry in
    let blocks = Spacetime_lib.Allocation_entry.blocks entry in
    let allocations = Spacetime_lib.Allocation_entry.allocations entry in
    Allocation.sum t (Allocation.create ~words ~blocks ~allocations)
  in
  let backtrace = Spacetime_lib.Allocation_entry.backtrace in
  let depth = 0 in
  let path = [] in
  fun ~inverted entries ->
    create ~backtrace ~nil ~cons ~inverted ~depth ~path ~entries

let of_call_entries =
  let nil = Call.zero in
  let cons entry t =
    let calls = Spacetime_lib.Call_entry.calls entry in
    let direct = Spacetime_lib.Call_entry.direct entry in
    Call.sum t (Call.create ~calls ~direct)
  in
  let backtrace = Spacetime_lib.Call_entry.backtrace in
  let depth = 0 in
  let path = [] in
  fun ~inverted entries ->
    create ~backtrace ~nil ~cons ~inverted ~depth ~path ~entries

let project t addr =
  match Address.Map.find addr t with
  | item -> Some item
  | exception Not_found -> None

let select t addr =
  match Address.Map.find addr t with
  | item -> Item.select item
  | exception Not_found -> empty

let fold f t init =
  Address.Map.fold f t init
