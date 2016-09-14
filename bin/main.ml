
type command =
  | Serve of { address: string; port: int; processed: bool; inverted: bool; }
  | Dump of { dir: string; processed: bool; inverted: bool }
  | View of { processed: bool; inverted: bool; }
  | Print_snapshot of { processed:         bool
                      ; snapshot_index:    int
                      ; print_filename:    bool
                      ; print_symbol:      bool
                      ; print_line_number: bool
                      }
  | Process

let unmarshal_profile file : Spacetime_lib.Series.t =
  let ic = open_in_bin file in
  match Marshal.from_channel ic with
  | data -> close_in ic; data
  | exception exn -> close_in ic; raise exn

let marshal_profile (profile : Spacetime_lib.Series.t) file =
  let oc = open_out_bin file in
  match Marshal.to_channel oc profile [] with
  | data -> close_out oc; data
  | exception exn -> close_out oc; raise exn

let main command profile executable =
  Printf.printf "Processing series...%!";
  let processed =
    match command with
    | Serve { processed; _ }
    | Dump { processed; _ }
    | View { processed; _ }
    | Print_snapshot { processed; _ } -> processed
    | Process -> false
  in
  let title =
    match executable with
    | None -> "Anonymous"
    | Some executable -> Filename.basename executable
  in
  let data =
    if processed then unmarshal_profile profile
    else Spacetime_lib.Series.create ?executable profile
  in
  Printf.printf "done\n%!";
  match command with
  | Serve { address; port; inverted } ->
    Serve.serve ~address ~port ~title (Series.initial data ~inverted)
  | Dump { dir; inverted } -> Dump.dump ~dir ~title (Series.initial data ~inverted)
  | View { inverted} -> Viewer.show (Series.initial data ~inverted)
  | Print_snapshot { snapshot_index
                   ; print_filename
                   ; print_symbol
                   ; print_line_number
                   } ->
    Print_snapshot.print
      (List.nth data snapshot_index)
      ~mode:`Words
      ~print_filename
      ~print_symbol
      ~print_line_number
  | Process -> marshal_profile data (profile ^ ".p")

open Cmdliner

(* Common options *)

let profile =
  let doc = "$(docv) to view" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"PROFILE" ~doc)

let executable =
  let doc = "Specify the ELF executable that was profiled" in
  Arg.(value & opt (some string) None
       & info ["e";"executable"] ~docv:"PATH" ~doc)

let processed =
  let doc = "Use an already processed allocation profile" in
  Arg.(value & flag & info ["p";"processed"] ~doc)

let inverted =
  let doc = "Aggregate traces by their outer-most frame" in
  Arg.(value & flag & info ["i";"inverted"] ~doc)

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
  Term.(pure
          (fun address port processed inverted ->
             Serve { address; port; processed; inverted })
        $ address $ port $ processed $ inverted)

let serve_t =
  let doc = "Serve allocation profile over HTTP" in
  Term.(pure main $ serve_arg $ profile $ executable, info "serve" ~doc)

(* Dump options *)

let dir =
  let doc = "$(docv) in which to dump files" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"DIRECTORY" ~doc)

let dump_arg =
  Term.(pure
          (fun dir processed inverted -> Dump { dir; processed; inverted })
        $ dir $ processed $ inverted)

let dump_t =
  let doc = "Dump allocation profile as HTML" in
  Term.(pure main $ dump_arg $ profile $ executable, info "dump" ~doc)

(* Print options *)

let print_snapshot_arg =
  let snapshot_index =
    let doc = "$(docv) which snapshot to print" in
    Arg.(required & pos 1 (some int) None & info [] ~docv:"SNAPSHOT-INDEX" ~doc)
  in
  let print_filename =
    let doc = "print out filename" in
    Arg.(value & flag & info ["filename"] ~doc)
  in
  let print_symbol =
    let doc = "print out symbol" in
    Arg.(value & flag & info ["symbol"] ~doc)
  in
  let print_line_number =
    let doc = "print out line_number" in
    Arg.(value & flag & info ["line-number"] ~doc)
  in
  Term.(pure
          (fun processed
            snapshot_index
            print_filename
            print_symbol
            print_line_number ->
            Print_snapshot
              { processed
              ; snapshot_index
              ; print_filename
              ; print_symbol
              ; print_line_number })
        $ processed
        $ snapshot_index
        $ print_filename
        $ print_symbol
        $ print_line_number
       )

let print_snapshot_t =
  let doc = "Print details of snapshot to stdout" in
  Term.(pure main $ print_snapshot_arg $ profile $ executable, info "print" ~doc)
;;

(* View options *)

let view_arg =
  Term.(pure
          (fun processed inverted -> View { processed; inverted })
        $ processed $ inverted)

let view_t =
  let doc = "View allocation profile in terminal" in
  Term.(pure main $ view_arg $ profile $ executable, info "view" ~doc)

(* Process options *)

let process_arg = Term.pure Process

let process_t =
  let doc = "Process allocation profile" in
  Term.(pure main $ process_arg $ profile $ executable, info "process" ~doc)

(* Handle default case *)

let default =
  Term.(pure (`Error(true, "command expected.")))

let default_t =
  let doc = "OCaml allocation profile viewer" in
  Term.(ret default, info "prof-alloc" ~doc)

let () =
  match Term.eval_choice default_t
          [serve_t; view_t; process_t; dump_t; print_snapshot_t] with
  | `Error _ -> exit 1
  | `Ok () -> exit 0
  | `Help -> exit 0
  | `Version -> exit 0
