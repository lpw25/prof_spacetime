open Lwt
open Cohttp
module Server = Cohttp_lwt_unix.Server

module Url : sig

  type t

  val of_path : Chart.Path.t -> t

  val to_path : t -> Chart.Path.t

  val of_string : string -> t option

  val to_string : t -> string

end = struct

  type t = Chart.Path.t

  let bytes_mode = "/bytes"
  let blocks_mode = "/block"
  let allocations_mode = "/alloc"

  let mode_len =
    let bytes_len = String.length bytes_mode in
    let blocks_len = String.length blocks_mode in
    let allocations_len = String.length allocations_mode in
    assert (bytes_len = blocks_len);
    assert (bytes_len = allocations_len);
    bytes_len

  let reduced_kind = "/red"
  let all_kind = "/all"

  let kind_len =
    let reduced_len = String.length reduced_kind in
    let add_len = String.length all_kind in
    assert (reduced_len = add_len);
    reduced_len

  let default_suffix = "series.json"

  let suffix_len = String.length default_suffix

  let initial_path = "/initial.json"

  let initial =
    let mode = Chart.Mode.Bytes in
    let kind = Chart.Kind.Reduced in
    let direction = Chart.Direction.Normal in
    let addresses = [] in
    Chart.Path.create mode kind direction addresses

  let () =
    let initial_len = String.length initial_path in
    assert (initial_len < mode_len + kind_len + suffix_len)

  let split_string s from until =
    let rec loop s segments prev n until =
      if prev >= until then begin
        segments
      end else if n >= until then begin
        let final = String.sub s prev (until - prev) in
        (final :: segments)
      end else if s.[n] = '/' then begin
        let segments =
          if prev >= n then segments
          else (String.sub s prev (n - prev)) :: segments
        in
        loop s segments (n + 1) (n + 1) until
      end else begin
        loop s segments prev (n + 1) until
      end
    in
    loop s [] from from until

  let of_path p = p
  let to_path t = t

  let of_string s =
    let len = String.length s in
    if len >= (mode_len + kind_len + suffix_len) then begin
      let mode = String.sub s 0 mode_len in
      let kind = String.sub s mode_len kind_len in
      let suffix = String.sub s (len - suffix_len) suffix_len in
      try
        if suffix = default_suffix then begin
          let mode =
            if mode = bytes_mode then Some Chart.Mode.Bytes
            else if mode = blocks_mode then Some Chart.Mode.Blocks
            else if mode = allocations_mode then Some Chart.Mode.Allocations
            else None
          in
          let kind =
            if kind = reduced_kind then Some Chart.Kind.Reduced
            else if kind = all_kind then Some Chart.Kind.All
            else None
          in
          let direction = Chart.Direction.Normal in
          let segments =
            split_string s (mode_len + kind_len) (len - suffix_len)
          in
          let addresses =
            List.map
              (fun str -> Address.of_int64 (Int64.of_string str))
              segments
          in
          match mode, kind with
          | None, _ -> None
          | _, None -> None
          | Some mode, Some kind ->
              let path = Chart.Path.create mode kind direction addresses in
              Some path
        end else None
      with Failure _ -> None
    end else begin
      if s = initial_path then Some initial
      else None
    end

  let to_string t =
    let mode =
      match Chart.Path.mode t with
      | Chart.Mode.Bytes -> bytes_mode
      | Chart.Mode.Blocks -> blocks_mode
      | Chart.Mode.Allocations -> allocations_mode
    in
    let kind =
      match Chart.Path.kind t with
      | Chart.Kind.Reduced -> reduced_kind
      | Chart.Kind.All -> all_kind
    in
    let rec loop = function
      | [] -> mode ^ kind
      | addr :: rest -> loop rest ^ "/" ^ (Address.to_string addr)
    in
    let body = loop (Chart.Path.addresses t) in
    body ^ "/" ^ default_suffix

end

module Json = struct

  let of_modes path =
    let modes_json =
      List.map
        (fun mode ->
           let display =
             match mode with
             | Chart.Mode.Bytes -> "Live bytes"
             | Chart.Mode.Blocks -> "Live blocks"
             | Chart.Mode.Allocations -> "All allocated words"
           in
           let selected = Chart.Mode.equal mode (Chart.Path.mode path) in
           let url = Url.of_path (Chart.Path.with_mode path mode) in
           `Assoc [ "display", `String display;
                    "selected", `Bool selected;
                    "path", `String (Url.to_string url); ])
        Chart.Mode.all
    in
    `List modes_json

  let of_layer index layer =
    let display = Chart.Layer.display layer in
    let foreign = Chart.Layer.foreign layer in
    let path_json =
      match Chart.Layer.selection layer with
      | None -> `Null
      | Some path ->
          let url = Url.of_path path in
          `String (Url.to_string url)
    in
    let value_jsons =
      List.map
        (fun point ->
           let time = Chart.Layer.Point.time point in
           let y = Chart.Layer.Point.value point in
           `Assoc ["time", `Float time;
                   "y", `Int y])
        (Chart.Layer.points layer)
    in
    `Assoc [ "index", `Int index;
             "display", `String display;
             "foreign", `Bool foreign;
             "path", path_json;
             "values", `List value_jsons; ]

  let of_frames frames =
    let jsons =
      List.fold_left
        (fun acc frame ->
           let display =
             match Chart.Frame.display frame with
             | None -> "(Top of stack)"
             | Some display -> display
           in
           let url = Url.of_path (Chart.Frame.path frame) in
           let json =
             `Assoc [ "path", `String (Url.to_string url);
                      "display", `String display; ]
           in
           json :: acc)
        [] frames
    in
    `List jsons

  let of_chart chart =
    let layers = Chart.layers chart in
    let frames = Chart.frames chart in
    let path = Chart.path chart in
    let max_time = Chart.max_time chart in
    let max_y = Chart.max_value chart in
    let layer_jsons = List.mapi of_layer layers in
    let frames_json = of_frames frames in
    let depth = List.length frames in
    let bytes =
      match Chart.Path.mode path with
      | Chart.Mode.Bytes -> true
      | Chart.Mode.Blocks | Chart.Mode.Allocations -> false
    in
    let modes_json = of_modes path in
    `Assoc [ "layers", `List layer_jsons;
             "frames", frames_json;
             "depth", `Int depth;
             "bytes", `Bool bytes;
             "modes", modes_json;
             "max_time", `Float max_time;
             "max_y", `Int max_y; ]

end


let serve ~address ~port ~title series =
  let memo = Chart.Memo.create ~series in
  let header typ =
    let h = Header.init () in
    let h = Header.add h "Content-Type" typ in
    let h = Header.add h "Server" "prof_spacetime" in
    h
  in
  let header_html = header "text/html; charset=UTF-8" in
  let header_js = header "application/javascript; charset=UTF-8" in
  let header_json = header "application/json; charset=UTF-8" in
  let callback conn_id req body =
    let uri = Request.uri req in
    let path = Uri.path uri in
    match path with
    | "/" | "/index.html" ->
      let headers = header_html in
      let status = `OK in
      let body = Embed.html in
      Server.respond_string ~headers ~status ~body ()
    | "/graph.js" ->
      let headers = header_js in
      let status = `OK in
      let body = Embed.js ~title in
      Server.respond_string ~headers ~status ~body ()
    | _ -> begin
        match Url.of_string path with
        | Some url ->
          let path = Url.to_path url in
          let chart = Chart.Memo.chart memo ~path in
          let headers = header_json in
          let status = `OK in
          let json = Json.of_chart chart in
          let body = Yojson.Basic.pretty_to_string ~std:true json in
          Server.respond_string ~headers ~status ~body ()
        | None ->
          Server.respond_not_found ~uri ()
      end
  in
  let ctx = Conduit_lwt_unix.init ~src:address () in
  let mode = `TCP (`Port port) in
  let server = Server.make ~callback () in
  let body =
    ctx >>= fun ctx ->
    let ctx = Cohttp_lwt_unix.Net.init ~ctx () in
    Server.create ~ctx ~mode server
  in
  Printf.printf "Serving on http://%s:%d/\n%!" address port;
  Lwt_main.run body
