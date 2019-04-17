
module Mode = struct

  module Call = struct

    type t =
      | Calls
      | Indirect_calls

    let pp ppf = function
      | Calls -> Format.fprintf ppf "Calls"
      | Indirect_calls -> Format.fprintf ppf "Indirect_calls"

    let equal t1 t2 =
      match t1, t2 with
      | Calls, Calls -> true
      | Indirect_calls, Indirect_calls -> true
      | Calls, Indirect_calls -> false
      | Indirect_calls, Calls -> false

    let proj _ = Section.Call.calls

    let filter = function
      | Calls -> fun _ -> true
      | Indirect_calls -> fun call -> not (Section.Call.direct call)

  end

  module Allocation = struct

    type t =
      | Bytes
      | Blocks
      | Allocations

    let pp ppf = function
      | Bytes -> Format.fprintf ppf "Bytes"
      | Blocks -> Format.fprintf ppf "Blocks"
      | Allocations -> Format.fprintf ppf "Allocations"

    let equal t1 t2 =
      match t1, t2 with
      | Bytes, Bytes -> true
      | Blocks, Blocks -> true
      | Allocations, Allocations -> true
      | Bytes, (Blocks | Allocations) -> false
      | Blocks, (Bytes | Allocations) -> false
      | Allocations, (Blocks | Bytes) -> false

    let proj = function
      | Bytes -> Section.Allocation.bytes
      | Blocks -> Section.Allocation.blocks
      | Allocations -> Section.Allocation.allocations

    let filter _ =
      fun _ -> true

  end

  type t =
    | Allocation of int * Allocation.t
    | Call of Call.t

  let pp ppf = function
    | Allocation(index, amode) ->
        Format.fprintf ppf "Allocation(%i, %a)"
          index Allocation.pp amode
    | Call cmode ->
        Format.fprintf ppf "Call(%a)"
          Call.pp cmode

  let equal t1 t2 =
    match t1, t2 with
    | Allocation(snap1, alloc1), Allocation(snap2, alloc2) ->
        snap1 = snap2 && Allocation.equal alloc1 alloc2
    | Call call1, Call call2 ->
        Call.equal call1 call2
    | Allocation _, Call _ -> false
    | Call _, Allocation _ -> false

end

module Direction = struct

  type t =
    | Normal
    | Inverted

  let pp ppf = function
    | Normal -> Format.fprintf ppf "Normal"
    | Inverted -> Format.fprintf ppf "Inverted"

  let inverted = function
    | Normal -> false
    | Inverted -> true

  let equal t1 t2 =
    match t1, t2 with
    | Normal, Normal -> true
    | Inverted, Inverted -> true
    | Normal, Inverted -> false
    | Inverted, Normal -> false

end

module Path = struct

  type t =
    { mode : Mode.t;
      direction : Direction.t;
      addresses : Address.t list; }

  let create mode direction addresses =
    { mode; direction; addresses }

  let mode { mode; _ } = mode

  let direction { direction; _ } = direction

  let addresses { addresses; _ } = addresses

  let with_mode t mode = { t with mode }

  let with_direction t direction = { t with direction }

  let pp ppf t =
    let pp_addresses ppf addresses =
      let pp_sep ppf () =
        Format.fprintf ppf ";@ "
      in
      Format.fprintf ppf "[@[<2>%a@]]"
        (Format.pp_print_list ~pp_sep Address.pp) addresses
    in
    Format.fprintf ppf
      "@[<2>{@ mode = %a;@ \
               direction = %a;@  \
               addresses = %a;@ }@]"
      Mode.pp t.mode
      Direction.pp t.direction
      pp_addresses t.addresses

  let equal t1 t2 =
    Mode.equal t1.mode t2.mode
    && Direction.equal t1.direction t2.direction
    && Address.equal_list t1.addresses t2.addresses

  let hash = Hashtbl.hash

  module Hash = struct

    type nonrec t = t

    let equal = equal
    let hash = hash

  end

  module Tbl = Hashtbl.Make(Hash)

end

module Row = struct

  type t =
    { address : Address.t;
      value : int;
      percentage : float;
      display : string;
      selection : Path.t option; }

  let create ~address ~value ~percentage ~display ~selection =
    { address; value; percentage; display; selection }

  let address { address; _ } = address

  let value { value; _ } = value

  let percentage { percentage; _ } = percentage

  let display { display; _ } = display

  let selection { selection; _ } = selection

  let dummy =
    { address = Address.of_int64 0L;
      value = 0;
      percentage = 0.0;
      display = "";
      selection = None; }

end

module Frame = struct

  type t =
    { path : Path.t;
      selected : Address.t option;
      display : string option; }

  let path { path; _ } = path

  let selected { selected; _ } = selected

  let display { display; _ } = display

  let initial mode direction rest =
    let path = Path.create mode direction [] in
    let selected =
      match rest with
      | [] -> None
      | address :: _ -> Some address
    in
    let display = None in
    { path; selected; display }

  let of_item path item rest =
    let selected =
      match rest with
      | [] -> None
      | address :: _ -> Some address
    in
    let display =
      match item with
      | None -> begin
          match Path.addresses path with
          | [] -> None
          | addr :: _ -> Some (Address.to_string addr)
        end
      | Some item -> Some (Section.Item.display item)
    in
    { path; selected; display }

end

type t =
  { rows : Row.t array;
    frames : Frame.t list;
    time : float;
    total : int; }

let of_section frames section time mode direction filter proj =
  let size, total =
    Section.fold
      (fun _ item acc ->
         let value = Section.Item.value item in
         if not (filter value) then acc
         else begin
           let (size, total) = acc in
           let size = size + 1 in
           let value = proj value in
           let total = total + value in
           (size, total)
         end)
      section (0, 0)
  in
  let rows = Array.make size Row.dummy in
  let _ =
    Section.fold
      (fun address item index ->
         let value = Section.Item.value item in
         if not (filter value) then index
         else begin
           let value = proj value in
           let percentage =
             ((float_of_int value) /. (float_of_int total)) *. 100.
           in
           let display = Section.Item.display item in
           let selection =
             if Section.Item.empty item then None
             else begin
               let addresses = Section.Item.path item in
               let path = Path.create mode direction addresses in
               Some path
             end
           in
           let row =
             Row.create ~address ~value ~percentage ~display ~selection
           in
           Array.set rows index row;
           index + 1
         end)
      section 0
  in
  Array.sort
    (fun row1 row2 -> compare (Row.value row2) (Row.value row1))
    rows;
  { rows; frames; time; total }

let table ~series ~path =
  let mode = Path.mode path in
  let direction = Path.direction path in
  let rec loop frames section addresses = function
    | [] -> frames, section
    | addr :: rest ->
        let item = Section.project section addr in
        let addresses = addr :: addresses in
        let path = Path.create mode direction addresses in
        let frame = Frame.of_item path item rest in
        let frames = frame :: frames in
        let section =
          match item with
          | None -> Section.empty
          | Some item -> Section.Item.select item
        in
        loop frames section addresses rest
  in
  let inverted = Direction.inverted direction in
  match mode with
  | Allocation(index, amode) ->
      let raddresses = List.rev (Path.addresses path) in
      let initial_frame = Frame.initial mode direction raddresses in
      let snapshots = Series.snapshots series in
      let initial_section, time =
        if index < List.length snapshots then begin
          let snapshot = List.nth snapshots index in
          let initial_section =
            Snapshot.allocation_entries snapshot ~inverted
          in
          let time = Snapshot.time snapshot in
          initial_section, time
        end else begin
          Section.empty, 0.0
        end
      in
      let frames, section =
        loop [initial_frame] initial_section [] raddresses
      in
      let filter = Mode.Allocation.filter amode in
      let proj = Mode.Allocation.proj amode in
      of_section frames section time mode direction filter proj
  | Call cmode ->
      let raddresses = List.rev (Path.addresses path) in
      let initial_frame = Frame.initial mode direction raddresses in
      let initial_section = Series.call_entries series ~inverted in
      let frames, section =
        loop [initial_frame] initial_section [] raddresses
      in
      let time = Series.final_time series in
      let filter = Mode.Call.filter cmode in
      let proj = Mode.Call.proj cmode in
      of_section frames section time mode direction filter proj

let row t i =
  if i < Array.length t.rows then Some (Array.get t.rows i)
  else None

let size t = Array.length t.rows

let frames { frames; _ } = frames

let time { time; _ } = time

let total { total; _ } = total

let find p t =
  let rec loop rows p i =
    if i >= Array.length rows then raise Not_found
    else if i < 0 then begin
      Printf.eprintf "WTF!\n%!";
      raise Exit
    end else begin
      if p (Array.get rows i) then i
      else loop rows p (i + 1)
    end
  in
  loop t.rows p 0

module Memo = struct

  type table = t

  type t =
    { series : Series.t;
      cache : table Path.Tbl.t; }

  let create ~series =
    let cache = Path.Tbl.create 5 in
    { series; cache }

  let table t ~path =
    match Path.Tbl.find t.cache path with
    | table -> table
    | exception Not_found ->
        let series = t.series in
        let table = table ~series ~path in
        Path.Tbl.add t.cache path table;
        table

end

