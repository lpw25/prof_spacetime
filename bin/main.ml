
type command =
  | Serve of { address: string; port: int; elf_executable : string option; }
  | Dump of { dir: string }
  | View of { elf_executable : string option; }

let main command profile =
  Printf.printf "Examining ELF executable...%!";
  let resolve_foreign ~elf_executable =
    match elf_executable with
    | None -> fun _addr -> None
    | Some elf_executable ->
      let elf = Elf_locations.create ~elf_executable in
      fun address -> Elf_locations.resolve elf ~program_counter:address
  in
  Printf.printf "Processsing series...%!";
  let data = Spacetime_lib.Series.create profile in
  Printf.printf "done\n%!";
  match command with
  | Serve { address; port; elf_executable } ->
    let resolve_foreign = resolve_foreign ~elf_executable in
    let series = Series.initial data ~resolve_foreign in
    Serve.serve ~address ~port series
  | Dump { dir } ->
    let resolve_foreign = resolve_foreign ~elf_executable:None in
    let series = Series.initial data ~resolve_foreign in
    Dump.dump ~dir series
  | View { elf_executable } ->
    let resolve_foreign = resolve_foreign ~elf_executable in
    let series = Series.initial data ~resolve_foreign in
    Viewer.show series ~resolve_foreign

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

let elf_executable =
  let doc = "Specify the ELF executable that was profiled" in
  Arg.(value & opt string "" & info ["elf-executable"] ~docv:"PATH" ~doc)

let serve_arg =
  Term.(pure (fun address port elf_executable ->
    (* CR mshinwell: fix this to use an option type *)
    let elf_executable =
      if elf_executable = "" then None else Some elf_executable
    in
    Serve { address; port; elf_executable }) $ address $ port $ elf_executable)

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
  Term.(pure (fun elf_executable ->
    (* CR mshinwell: fix this to use an option type *)
    let elf_executable =
      if elf_executable = "" then None else Some elf_executable
    in
    View { elf_executable }) $ elf_executable)

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
