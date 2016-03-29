
let main address port dump profile =
  let data = Spacetime_lib.Series.create profile in
  let series = Series.initial data in
  match dump with
  | None -> Serve.serve ~address ~port series
  | Some dir -> Dump.dump ~dir series

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

let dump =
  let doc = "Dump files to $(docv) instead of serving them" in
  Arg.(value & opt (some string) None & info ["dump"] ~docv:"DIRECTORY" ~doc)

let profile =
  let doc = "$(docv) to view" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"PROFILE" ~doc)

let main_t =
  let doc = "Serve allocation profile over HTTP" in
  Term.(Term.pure main $ address $ port $ dump $ profile, info "prof-alloc" ~doc)

let () =
  match Term.eval main_t with
  | `Error _ -> exit 1
  | `Ok () -> exit 0
  | `Help -> exit 0
  | `Version -> exit 0
