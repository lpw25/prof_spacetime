
type command =
  | Serve of { address: string; port: int }
  | Dump of { dir: string }
  | View

let main command profile =
  Printf.printf "Processsing series...%!";
  let data = Spacetime_lib.Series.create profile in
  Printf.printf "done\n%!";
  let series = Series.initial data in
  match command with
  | Serve { address; port } -> Serve.serve ~address ~port series
  | Dump { dir } -> Dump.dump ~dir series
  | View -> Viewer.show series

open Cmdliner

(* Common options *)

let profile =
  let doc = "$(docv) to view" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"PROFILE" ~doc)

(* Serve options *)

let default_address = "127.0.0.1"

let address =
  let doc = "Use $(docv) as address" in
  Arg.(value & opt string default_address
       & info ["address"] ~docv:"ADDRESS" ~doc)

let default_port = 8080

let port =
  let doc = "Use $(docv) as port" in
  Arg.(value & opt int default_port & info ["port"] ~docv:"PORT" ~doc)

let serve_arg =
  Term.(pure (fun address port -> Serve { address; port }) $ address $ port)

let serve_t =
  let doc = "Serve allocation profile over HTTP" in
  Term.(pure main $ serve_arg $ profile, info "serve" ~doc)

(* Dump options *)

let dir =
  let doc = "$(docv) in which to dump files" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"DIRECTORY" ~doc)

let dump_arg =
  Term.(pure (fun dir -> Dump { dir }) $ dir)

let dump_t =
  let doc = "Dump allocation profile as HTML" in
  Term.(pure main $ dump_arg $ profile, info "dump" ~doc)

(* View options *)

let view_arg =
  Term.(pure View)

let view_t =
  let doc = "View allocation profile in terminal" in
  Term.(pure main $ view_arg $ profile, info "view" ~doc)

(* Handle default case *)

let default =
  Term.(pure (`Error(true, "command expected.")))

let default_t =
  let doc = "OCaml allocation profile viewer" in
  Term.(ret default, info "prof-alloc" ~doc)

let () =
  match Term.eval_choice default_t [serve_t; view_t; dump_t] with
  | `Error _ -> exit 1
  | `Ok () -> exit 0
  | `Help -> exit 0
  | `Version -> exit 0
