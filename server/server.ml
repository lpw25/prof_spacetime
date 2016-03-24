open Lwt
open Cohttp
module Server = Cohttp_lwt_unix.Server

let split_path path from =
  let rec loop path segments prev n =
    let length = String.length path in
    if prev >= length then begin
      List.rev segments
    end else if n >= length then begin
      let final = String.sub path prev (length - prev) in
      List.rev (final :: segments)
    end else if path.[n] = '/' then begin
      let segments =
        if prev >= n then segments
        else (String.sub path prev (n - prev)) :: segments
      in
      loop path segments (n + 1) (n + 1)
    end else begin
      loop path segments prev (n + 1)
    end
  in
  loop path [] from from

let serve ~address ~port profile =
  let series = AProf.Series.create profile in
  let data = Series.initial series in
  let series =
    Marshal.to_string (series : AProf.Series.t) [Marshal.Compat_32]
  in
  let header typ =
    let h = Header.init () in
    let h = Header.add h "Content-Type" typ in
    let h = Header.add h "Server" "prof_alloc" in
    h
  in
  let header_html = header "text/html; charset=UTF-8" in
  let header_js = header "application/javascript; charset=UTF-8" in
  let header_json = header "application/json; charset=UTF-8" in
  let header_trace = header "application/octet-stream; charset=binary" in
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
    | "/series" ->
      let headers = header_trace in
      let status = `OK in
      let body = series in
      Server.respond_string ~headers ~status ~body ()
    | _ ->
      if String.length path >= 5 then begin
        let head = String.sub path 0 5 in
        if head = "/data" then begin
          try
            let segments = split_path path 5 in
            let addrs =
              List.map
                (fun str -> Address.of_int64 (Int64.of_string str)) segments
            in
            let data =
              List.fold_left
                (fun acc addr -> Series.project acc addr) data addrs
            in
            let headers = header_json in
            let status = `OK in
            let body =
              Yojson.Basic.pretty_to_string ~std:true (Series.to_json data)
            in
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
  Lwt_main.run body

let main address port profile =
  serve ~address ~port profile

open Cmdliner

let default_address = "127.0.0.1"

let address =
  let doc = "Use $(docv) as address" in
  Arg.(value & opt string default_address
       & info ["address"] ~docv:"ADDRESS" ~doc)

let default_port = 8080

let port =
  let doc = "Use $(docv) as port" in
  Arg.(value & opt int default_port & info ["port"] ~docv:"PORT" ~doc)

let profile =
  let doc = "$(docv) to view" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"PROFILE" ~doc)

let main_t =
  let doc = "Serve allocation profile over HTTP" in
  Term.(Term.pure main $ address $ port $ profile, info "prof-alloc" ~doc)

let () =
  match Term.eval main_t with
  | `Error _ -> exit 1
  | `Ok () -> exit 0
  | `Help -> exit 0
  | `Version -> exit 0
