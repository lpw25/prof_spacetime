
let main profile =
  Printf.printf "Processsing series...%!";
  let data = Spacetime_lib.Series.create profile in
  Printf.printf "done\n%!";
  let series = Series.initial data in
  Viewer.show series

open Cmdliner

let profile =
  let doc = "$(docv) to view" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"PROFILE" ~doc)

let main_t =
  let doc = "Show allocation profile in terminal" in
  Term.(Term.pure main $ profile, info "prof-alloc-term" ~doc)

let () =
  match Term.eval main_t with
  | `Error _ -> exit 1
  | `Ok () -> exit 0
  | `Help -> exit 0
  | `Version -> exit 0
