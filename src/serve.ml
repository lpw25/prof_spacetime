open Lwt
open Cohttp
module Server = Cohttp_lwt_unix.Server

let serve ~address ~port ~title series =
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
        match Path.of_string path with
        | Some path ->
          let addrs = Path.addresses path in
          let series =
            List.fold_right
              (fun addr acc -> Series.project acc addr) addrs series
          in
          let headers = header_json in
          let status = `OK in
          let json = Series.to_json path series in
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
    let ctx = Cohttp_lwt_unix_net.init ~ctx () in
    Server.create ~ctx ~mode server
  in
  Printf.printf "Serving on http://%s:%d/\n%!" address port;
  Lwt_main.run body
