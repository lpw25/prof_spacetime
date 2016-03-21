
module StringMap = Map.Make(String)

module Graph = struct

  let map_object (map : 'a StringMap.t) =
    let bindings = Array.of_list (StringMap.bindings map) in
    let values = Array.map (fun (k, v) -> (k, Js.Unsafe.inject v)) bindings in
    Js.Unsafe.obj values

  type location =
    < display : Js.js_string Js.t Js.readonly_prop;
      foreign : bool Js.readonly_prop;
      depth: int Js.readonly_prop; > Js.t

  let location ~(depth : int) ~(display : string) ~(foreign : bool) =
    object%js
      val display = Js.string display
      val foreign = foreign
      val depth = depth
    end

  type names

  let names ~(names : location StringMap.t) : names =
    map_object names

  type values

  type snapshot =
    < time : float Js.readonly_prop;
      values : values Js.readonly_prop > Js.t

  let snapshot ~(time : float) ~(values: int StringMap.t) : snapshot =
    object%js
      val time = time
      val values = map_object values
    end

  type state =
    < names : names Js.readonly_prop;
      snapshots : snapshot Js.js_array Js.t Js.readonly_prop > Js.t

  let state ~(names : names) ~(snapshots : snapshot list) : state =
    object%js
      val names = names
      val snapshots = Js.array (Array.of_list snapshots)
    end

  let run (init : state) (change : string -> state) : unit =
    Js.Unsafe.global##run init (fun id -> change (Js.to_string id))

end

let nth_or_last max l =
  let rec loop n = function
    | [] -> None
    | [hd] -> Some(hd, n)
    | hd :: tl ->
      if n = max then Some(hd, n)
      else loop (n + 1) tl
  in
  loop 0 l

type index = AProf.Entries.t StringMap.t

let index depth entries =
  let open AProf in
    Entries.fold
      (fun entry acc ->
         let backtrace = Entry.backtrace entry in
         match nth_or_last depth backtrace with
         | None -> acc
         | Some (loc, _) ->
           let id = Int64.to_string (Location.address loc) in
           let entries =
             try
               StringMap.find id acc
             with Not_found -> Entries.empty
           in
           let entries = Entries.add entry entries in
           StringMap.add id entries acc)
      entries StringMap.empty

type state = {
  max_depth: int;
  snapshots: (AProf.Snapshot.t * index) list;
}

let initial_state series =
  let open AProf in
  let max_depth = 0 in
  let snapshots =
    List.map
      (fun snapshot ->
         snapshot, index max_depth (AProf.Snapshot.entries snapshot))
      series
  in
  { max_depth; snapshots }

let project_state { max_depth; snapshots } id =
  let open AProf in
  let max_depth = max_depth + 1 in
  let snapshots =
    List.map
      (fun (snapshot, idx) ->
         let entries =
           try
             StringMap.find id idx
           with Not_found -> Entries.empty
         in
         let idx = index max_depth entries in
         (snapshot, idx))
      snapshots
  in
  { max_depth; snapshots }

let names { max_depth; snapshots } =
  let open AProf in
  List.fold_left
    (fun acc (_, index) ->
       StringMap.fold
         (fun id entries acc ->
            match Entries.choose entries with
            | exception Not_found -> acc
            | entry ->
              let backtrace = Entry.backtrace entry in
              match nth_or_last max_depth backtrace with
              | None -> acc
              | Some(loc, depth) ->
                let display =
                  match Location.position loc with
                  | Some pos ->
                    let name = Position.filename pos in
                    let line = string_of_int (Position.line_number pos) in
                    let first = string_of_int (Position.start_char pos) in
                    let last = string_of_int (Position.end_char pos) in
                    String.concat ""
                      [name; "{"; line; ":"; first; "-"; last; "}"]
                  | None ->
                    match Location.symbol loc with
                    | Some s -> s
                    | None -> id
                in
                let foreign = Location.foreign loc in
                let location = Graph.location ~depth ~display ~foreign in
                StringMap.add id location acc)
         index acc)
    StringMap.empty snapshots

let snapshot init snapshot index =
  let open AProf in
  let time = Snapshot.time snapshot in
  let values =
    StringMap.fold
      (fun id entries acc ->
         let words =
           try
             StringMap.find id acc
           with Not_found -> assert false
         in
         let words =
           Entries.fold
             (fun entry acc -> acc + (Entry.words entry))
             entries words
         in
         StringMap.add id words acc)
      index init
  in
  Graph.snapshot ~time ~values

let view_state state =
  let names = names state in
  let init =
    StringMap.fold
      (fun id _ acc ->
         StringMap.add id 0 acc)
      names StringMap.empty
  in
  let snapshots =
    List.map
      (fun (snap, idx) -> snapshot init snap idx)
      state.snapshots
  in
  let names = Graph.names names in
  Graph.state ~names ~snapshots


let main series =
  let state = ref (initial_state series) in
  Graph.run (view_state !state)
    (fun id ->
       state := project_state !state id;
       view_state !state)

let string_of_response response =
  let buffer = File.CoerceTo.arrayBuffer response in
  match Js.Opt.to_option buffer with
  | None -> failwith "Could not load profiling data"
  | Some buffer ->
    let array = new%js Typed_array.uint8Array_fromBuffer buffer in
    let len = array##.length in
    let s =
      String.init len (fun i -> Char.chr (Typed_array.unsafe_get array i))
    in
    s

let () =
  try
    let xml = XmlHttpRequest.create () in
    xml##.responseType := Js.string "arraybuffer";
    xml##_open (Js.string "GET") (Js.string "series") Js._true;
    xml##.onload :=
      Dom.full_handler
        (fun this evt ->
           if this##.status = 200 then begin
             let response = string_of_response this##.response in
             let profile =
               (Marshal.from_string response 0 : AProf.Series.t)
             in
             main profile
           end else begin
             failwith "Could not load profiling data"
           end;
           Js._true);
    xml##send Js.null;
  with _ -> failwith "Could not load profiling data"

