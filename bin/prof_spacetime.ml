open Prof_spacetime_lib

type command =
  | Serve of { address: string; port: int; processed: bool; }
  | View of { processed: bool; }
  | Print of
      { processed:         bool;
        mode:              Print.Mode.t;
        inverted:          bool;
        print_filename:    bool;
        print_symbol:      bool;
        print_line_number: bool; }
  | Process
  | Diff of
      { processed: bool
      ; reference: string;
      }

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

let load_series processed executable profile =
  if processed then unmarshal_profile profile
  else begin
    Printf.printf "Processing series...%!";
    let series = Spacetime_lib.Series.create ?executable profile in
    Printf.printf "done\n%!";
    series
  end

let main command profile executable =
  let processed =
    match command with
    | Serve { processed; _ }
    | View { processed; _ }
    | Print { processed; _ } -> processed
    | Process -> false
    | Diff { processed; _ } -> processed
  in
  let data = load_series processed executable profile in
  match command with
  | Serve { address; port; } ->
      let title =
        match executable with
        | None -> "Anonymous"
        | Some executable -> Filename.basename executable
      in
      let series = Series.create data in
      Serve.serve ~address ~port ~title series
  | View _ ->
      let series = Series.create data in
      Viewer.show series
  | Print
      { mode; inverted;
        print_filename; print_symbol; print_line_number; } ->
    Print.print data
     ~mode ~inverted ~print_filename ~print_symbol ~print_line_number
  | Process ->
    marshal_profile data (profile ^ ".p")
  | Diff { reference } ->
    let ref_data = load_series processed executable reference in
    let series = Series.create data in
    Diff.diff (Series.create ref_data) series

open Cmdliner

(* Common options *)

let profile =
  let doc = "$(docv) to view" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"PROFILE" ~doc)

let reference =
  let doc = "$(docv) reference" in
  Arg.(required & pos 1 (some string) None & info [] ~docv:"REFERNCE" ~doc)

let executable =
  let doc = "Specify the ELF executable that was profiled" in
  Arg.(value & opt (some string) None
       & info ["e";"executable"] ~docv:"PATH" ~doc)

let processed =
  let doc = "Use an already processed allocation profile" in
  Arg.(value & flag & info ["p";"processed"] ~doc)

(* Serve options *)

let default_address = "127.0.0.1"

let serve_address =
  let doc = "Use $(docv) as address" in
  Arg.(value & opt string default_address
       & info ["address"] ~docv:"ADDRESS" ~doc)

let default_port = 8080

let serve_port =
  let doc = "Use $(docv) as port" in
  Arg.(value & opt int default_port & info ["port"] ~docv:"PORT" ~doc)

let serve_arg =
  Term.(pure
          (fun address port processed ->
             Serve { address; port; processed })
        $ serve_address $ serve_port $ processed)

let serve_t =
  let doc = "Serve allocation profile over HTTP" in
  Term.(pure main $ serve_arg $ profile $ executable, info "serve" ~doc)

(* Print options *)

let print_filename =
  let doc = "print out filename" in
  Arg.(value & flag & info ["filename"] ~doc)

let print_symbol =
  let doc = "print out symbol" in
  Arg.(value & flag & info ["symbol"] ~doc)

let print_line_number =
  let doc = "print out line_number" in
  Arg.(value & flag & info ["line-number"] ~doc)

type print_mode =
  | Words
  | Blocks
  | Allocations
  | Calls
  | Indirect_calls

let print_raw_mode =
  let mode =
    Arg.enum
      [ "words", Words;
        "blocks", Blocks;
        "allocations", Allocations;
        "calls", Calls;
        "indirect-calls", Indirect_calls; ]
  in
  let doc =
    "Numbers to output. $(docv) should be one of \
     words, blocks, allocations, calls or indirect-calls"
  in
  Arg.(value & opt mode Words & info ["mode"] ~docv:"MODE" ~doc)

let print_raw_index =
  let doc = "$(docv) which snapshot to print" in
  Arg.(value & pos 1 (some int) None & info [] ~docv:"SNAPSHOT-INDEX" ~doc)

let print_mode =
  let requires mode =
    `Error(true, "Mode \"" ^ mode ^ "\" requires a snapshot index")
  in
  let not_requires mode =
    `Error(true, "Mode \"" ^ mode ^ "\" does not require a snapshot index")
  in
  let convert mode index =
    match mode with
    | Words -> begin
        match index with
        | None -> requires "words"
        | Some index -> `Ok (Print.Mode.Words { index })
      end
    | Blocks -> begin
        match index with
        | None -> requires "blocks"
        | Some index -> `Ok (Print.Mode.Blocks { index })
      end
    | Allocations -> begin
        match index with
        | None -> requires "allocations"
        | Some index -> `Ok (Print.Mode.Allocations { index })
      end
    | Calls -> begin
        match index with
        | None -> `Ok Print.Mode.Calls
        | Some _ -> not_requires "calls"
      end
    | Indirect_calls -> begin
        match index with
        | None -> `Ok Print.Mode.Indirect_calls
        | Some _ -> not_requires "indirect-calls"
      end
  in
  Term.(ret (pure convert $ print_raw_mode $ print_raw_index))

let inverted =
  let doc = "Aggregate traces by their outer-most frame" in
  Arg.(value & flag & info ["i";"inverted"] ~doc)

let print_arg =
  Term.(pure
          (fun processed mode inverted
            print_filename print_symbol print_line_number ->
            Print
              { processed ; mode ; inverted
              ; print_filename ; print_symbol ; print_line_number })
        $ processed $ print_mode $ inverted
        $ print_filename $ print_symbol $ print_line_number)

let print_t =
  let doc = "Print data to stdout" in
  Term.(pure main $ print_arg $ profile $ executable, info "print" ~doc)
;;

let compare_arg =
  Term.(pure
      (fun processed reference -> Diff { reference; processed })
      $ processed
      $ reference)

let compare_t =
  let doc = "Compare two processed profiles" in
  Term.(pure main $ compare_arg $ profile $ executable, info "compare" ~doc)
;;

(* View options *)

let view_arg =
  Term.(pure
          (fun processed -> View { processed; })
        $ processed)

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
  let doc = "OCaml spacetime profile viewer" in
  Term.(ret default, info "prof-spacetime" ~doc)

let () =
  match Term.eval_choice default_t
          [serve_t; view_t; process_t; print_t; compare_t]
  with
  | `Error _ -> exit 1
  | `Ok () -> exit 0
  | `Help -> exit 0
  | `Version -> exit 0
