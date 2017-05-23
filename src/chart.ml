
module Mode = struct

  type t =
    | Bytes
    | Blocks
    | Allocations

  let pp ppf = function
    | Bytes -> Format.fprintf ppf "Bytes"
    | Blocks -> Format.fprintf ppf "Blocks"
    | Allocations -> Format.fprintf ppf "Allocations"

  let proj = function
    | Bytes -> Section.Allocation.bytes
    | Blocks -> Section.Allocation.blocks
    | Allocations -> Section.Allocation.allocations

  let equal t1 t2 =
    match t1, t2 with
    | Bytes, Bytes -> true
    | Blocks, Blocks -> true
    | Allocations, Allocations -> true
    | Bytes, (Blocks | Allocations) -> false
    | Blocks, (Bytes | Allocations) -> false
    | Allocations, (Bytes | Blocks) -> false

  let all = [Bytes; Blocks; Allocations]

end

module Kind = struct

  type t =
    | All
    | Reduced

  let pp ppf = function
    | All -> Format.fprintf ppf "All"
    | Reduced -> Format.fprintf ppf "Reduced"

  let equal t1 t2 =
    match t1, t2 with
    | All, All -> true
    | Reduced, Reduced -> true
    | All, Reduced -> false
    | Reduced, All -> false

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
      kind : Kind.t;
      direction : Direction.t;
      addresses: Address.t list; }

  let create mode kind direction addresses =
    { mode; kind; direction; addresses }

  let mode { mode; _ } = mode

  let kind { kind; _ } = kind

  let direction { direction; _ } = direction

  let addresses { addresses; _ } = addresses

  let with_mode t mode = { t with mode }

  let with_kind t kind = { t with kind }

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
               kind = %a;@ \
               direction = %a;@  \
               addresses = %a;@ }@]"
      Mode.pp t.mode
      Kind.pp t.kind
      Direction.pp t.direction
      pp_addresses t.addresses

  let equal t1 t2 =
    Mode.equal t1.mode t2.mode
    && Kind.equal t1.kind t2.kind
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

module Layer = struct

  module Point = struct

    type t =
      { time : float;
        value : int; }

    let create ~time ~value =
      { time; value }

    let time { time } = time

    let value { value } = value

    let add t ts =
      let rec loop t = function
        | [] -> [t]
        | s :: rest ->
          if Pervasives.compare t.time s.time = 0 then
            { time = s.time; value = t.value + s.value; } :: rest
          else
            s :: loop t rest
      in
      loop t ts

  end

  type t =
    { points: Point.t list;
      display : string;
      foreign : bool;
      selection : Path.t option; }

  let create ~points ~display ~foreign ~selection =
    { points; display; foreign; selection }

  let points { points } = points

  let display { display } = display

  let foreign { foreign } = foreign

  let selection { selection } = selection

  let unknown path snapshots =
    match Path.addresses path, Path.mode path with
    | [], Mode.Bytes ->
        let points =
          List.map
            (fun snapshot ->
               let time = Snapshot.time snapshot in
               let value =
                 match Snapshot.stats snapshot with
                 | None -> 0
                 | Some stats ->
                     let scanned = Spacetime_lib.Stats.words_scanned stats in
                     let scanned_profinfo =
                       Spacetime_lib.Stats.words_scanned_with_profinfo stats
                     in
                     let unknown_words = scanned - scanned_profinfo in
                     let unknown_bytes =
                       unknown_words * Section.Allocation.word_size
                     in
                     unknown_bytes
               in
               Point.create ~time ~value)
            snapshots
        in
        let display = "Unknown" in
        let foreign = false in
        let selection = None in
        Some { points; display; foreign; selection }
    | _, _ -> None

  let reduction_target = 50

  let reduce path layers =
    let other_points = ref [] in
    let layer_array = Array.make reduction_target None in
    let max_array = Array.make reduction_target 0 in
    List.iter
      (fun layer ->
         let max =
           List.fold_left
             (fun acc point ->
                let value = Point.value point in
                if value > acc then value
                else acc)
             0 layer.points
         in
         let other =
           if max > max_array.(0) then begin
             let other = layer_array.(0) in
             try
               for i = 1 to reduction_target - 1 do
                 if max > max_array.(i) then begin
                   max_array.(i-1) <- max_array.(i);
                   layer_array.(i-1) <- layer_array.(i)
                 end else begin
                   max_array.(i-1) <- max;
                   layer_array.(i-1) <- Some layer;
                   raise Exit
                 end
               done;
               max_array.(reduction_target - 1) <- max;
               layer_array.(reduction_target - 1) <- Some layer;
               other
             with Exit -> other
           end else begin
             Some layer
           end
         in
         match other with
         | None -> ()
         | Some other ->
           List.iter
             (fun point -> other_points := Point.add point !other_points)
             other.points)
      layers;
    let layers =
      Array.fold_left
        (fun acc layer ->
           match layer with
           | None -> acc
           | Some layer -> layer :: acc)
        [] layer_array
    in
    let layers =
      match !other_points with
      | [] -> layers
      | points ->
        let others =
          let display = "Other" in
          let foreign = false in
          let path = Path.with_kind path All in
          let selection = Some path in
          { points; display; foreign; selection }
        in
        others :: layers
    in
    layers

end

module Frame = struct

  type t =
    { path : Path.t;
      selected : Address.t option;
      display : string option; }

  let path { path } = path

  let selected { selected } = selected

  let display { display } = display

  let initial mode kind direction rest =
    let path = Path.create mode kind direction [] in
    let selected =
      match rest with
      | [] -> None
      | address :: _ -> Some address
    in
    let display = None in
    { path; selected; display }

  let of_items path items rest =
    let selected =
      match rest with
      | [] -> None
      | address :: _ -> Some address
    in
    let rec loop items =
      match items with
      | [] -> begin
          match Path.addresses path with
          | [] -> None
          | addr :: _ -> Some (Address.to_string addr)
        end
      | (_, None) :: rest -> loop rest
      | (_, Some item) :: _ -> Some (Section.Item.display item)
    in
    let display = loop items in
    { path; selected; display }

end

type t =
  { path : Path.t;
    layers : Layer.t list;
    frames : Frame.t list;
    max_value : int;
    max_time : float; }

let path { path } = path

let layers { layers } = layers

let frames { frames } = frames

let max_value { max_value } = max_value

let max_time { max_time } = max_time

let chart ~series ~path =
  let snapshots = Series.snapshots series in
  let mode = Path.mode path in
  let direction = Path.direction path in
  let kind = Path.kind path in
  let rec loop frames sections addresses = function
    | [] -> frames, sections
    | addr :: rest -> begin
        let items =
          List.map
            (fun (time, section) ->
               match Section.project section addr with
               | None -> time, None
               | Some item -> time, Some item)
            sections
        in
        let addresses = addr :: addresses in
        let path = Path.create mode kind direction addresses in
        let frame = Frame.of_items path items rest in
        let frames = frame :: frames in
        let sections =
          List.map
            (fun (time, item) ->
               match item with
               | None -> time, Section.empty
               | Some item -> time, Section.Item.select item)
            items
        in
        loop frames sections addresses rest
      end
  in
  let inverted = Direction.inverted direction in
  let initial_sections =
    List.map
      (fun snapshot ->
         let time = Snapshot.time snapshot in
         let section = Snapshot.allocation_entries ~inverted snapshot in
         time, section)
      snapshots
  in
  let raddresses = List.rev (Path.addresses path) in
  let initial_frame = Frame.initial mode kind direction raddresses in
  let frames, sections =
    loop [initial_frame] initial_sections [] raddresses
  in
  let proj = Mode.proj mode in
  let addresses, max_value, max_time =
    List.fold_left
      (fun (addresses, max_value, max_time) (time, section) ->
         let max_time = if time > max_time then time else max_time in
         let addresses, total =
           Section.fold
             (fun addr item (addresses, total) ->
                let addresses = Address.Set.add addr addresses in
                let value = proj (Section.Item.value item) in
                let total = total + value in
                (addresses, total))
             section (addresses, 0)
         in
         let max_value = if total > max_value then total else max_value in
         (addresses, max_value, max_time))
      (Address.Set.empty, 0, 0.0) sections
  in
  let layers =
    Address.Set.fold
      (fun addr acc ->
         let init =
           let points = [] in
           let display = Address.to_string addr in
           let foreign = false in
           let selection = None in
           Layer.create ~points ~display ~foreign ~selection
         in
         let layer =
           List.fold_right
             (fun (time, section) layer ->
                let points = Layer.points layer in
                let display = Layer.display layer in
                let foreign = Layer.foreign layer in
                let selection = Layer.selection layer in
                match Section.project section addr with
                | None ->
                    let value = 0 in
                    let point = Layer.Point.create ~time ~value in
                    let points = point :: points in
                    Layer.create ~points ~display ~foreign ~selection
                | Some item ->
                    let value = proj (Section.Item.value item) in
                    let point = Layer.Point.create ~time ~value in
                    let points = point :: points in
                    let display = Section.Item.display item in
                    let foreign = Section.Item.foreign item in
                    let selection =
                      if Section.Item.empty item then selection
                      else begin
                        let addresses = Section.Item.path item in
                        Some (Path.create mode kind direction addresses)
                      end
                    in
                    Layer.create ~points ~display ~foreign ~selection)
             sections init
         in
         layer :: acc)
      addresses []
  in
  let layers =
    match kind with
    | Kind.All -> layers
    | Kind.Reduced -> Layer.reduce path layers
  in
  let layers =
    match Layer.unknown path snapshots with
    | None -> layers
    | Some unknown -> unknown :: layers
  in
  { path; layers; frames; max_value; max_time; }

module Memo = struct

  type chart = t

  type t =
    { series : Series.t;
      cache : chart Path.Tbl.t; }

  let create ~series =
    let cache = Path.Tbl.create 5 in
    { series; cache }

  let chart t ~path =
    match Path.Tbl.find t.cache path with
    | chart -> chart
    | exception Not_found ->
        let series = t.series in
        let chart = chart ~series ~path in
        Path.Tbl.add t.cache path chart;
        chart

end
