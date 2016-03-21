open Lwt
open Cohttp
open Cohttp_lwt_unix

let serve ~address ~port profile =
  let series = AProf.Series.create profile in
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
    | "/client.js" ->
      let headers = header_js in
      let status = `OK in
      let body = Embed.js in
      Server.respond_string ~headers ~status ~body ()
    | "/graph.js" ->
      let headers = header_js in
      let status = `OK in
      let body = Embed.graph in
      Server.respond_string ~headers ~status ~body ()
    | "/series" ->
      let headers = header_trace in
      let status = `OK in
      let body = series in
      Server.respond_string ~headers ~status ~body ()
    | _ ->
      Server.respond_not_found ~uri ()
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
