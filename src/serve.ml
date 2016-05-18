open Lwt
open Cohttp
module Server = Cohttp_lwt_unix.Server

let split_path path from until =
  let rec loop path segments prev n until =
    if prev >= until then begin
      List.rev segments
    end else if n >= until then begin
      let final = String.sub path prev (until - prev) in
      List.rev (final :: segments)
    end else if path.[n] = '/' then begin
      let segments =
        if prev >= n then segments
        else (String.sub path prev (n - prev)) :: segments
      in
      loop path segments (n + 1) (n + 1) until
    end else begin
      loop path segments prev (n + 1) until
    end
  in
  loop path [] from from until

let reduced_d = "/red"
let all_d = "/all"

let prefixd_len =
  let reduced_len = String.length reduced_d in
  let add_len = String.length all_d in
  assert (reduced_len = add_len);
  reduced_len

let jsonf = "series.json"
let jsonf_len = String.length jsonf

let serve ~address ~port series =
  let header typ =
    let h = Header.init () in
    let h = Header.add h "Content-Type" typ in
    let h = Header.add h "Server" "prof_alloc" in
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
      let body = Embed.js in
      Server.respond_string ~headers ~status ~body ()
    | _ ->
      let len = String.length path in
      if len >= (prefixd_len + jsonf_len) then begin
        let head = String.sub path 0 prefixd_len in
        let tail = String.sub path (len - jsonf_len) jsonf_len in
        let serve, reduced =
          if tail = jsonf then begin
            if head = reduced_d then true, true
            else if head = all_d then true, false
            else false, false
          end else false, false
        in
        if serve then begin
          try
            let segments = split_path path prefixd_len (len - jsonf_len) in
            let addrs =
              List.map
                (fun str -> Address.of_int64 (Int64.of_string str)) segments
            in
            let series =
              List.fold_left
                (fun acc addr -> Series.project acc addr) series addrs
            in
            let headers = header_json in
            let status = `OK in
            let json =
              if reduced then begin
                let path_body =
                  String.sub path prefixd_len (len - (prefixd_len + jsonf_len))
                in
                let other_path = all_d ^ path_body in
                Series.to_reduced_json other_path series
              end else begin
                Series.to_json series
              end
            in
            let body = Yojson.Basic.pretty_to_string ~std:true json in
            Server.respond_string ~headers ~status ~body ()
          with Failure _ -> Server.respond_not_found ~uri ()
        end else begin
          Server.respond_not_found ~uri ()
        end
      end else begin
        Server.respond_not_found ~uri ()
      end
  in
  let ctx = Conduit_lwt_unix.init ~src:address () in
  let mode = `TCP (`Port port) in
  let server = Server.make ~callback () in
  let body =
    ctx >>= fun ctx ->
    let ctx = Cohttp_lwt_unix_net.init ~ctx () in
    Server.create ~ctx ~mode server
  in
  Printf.printf "Serving on %s:%d\n%!" address port;
  Lwt_main.run body
