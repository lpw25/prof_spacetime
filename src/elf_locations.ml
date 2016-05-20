type t = {
  map : Owee_buf.t;
  sections : Owee_elf.section array;
}

let create ~elf_executable =
  let fd = Unix.openfile elf_executable [Unix.O_RDONLY] 0 in
  let len = Unix.lseek fd 0 Unix.SEEK_END in
  let map =
    Bigarray.Array1.map_file fd
      Bigarray.int8_unsigned Bigarray.c_layout false len
  in
  Unix.close fd;
  let _header, sections = Owee_elf.read_elf map in
  { map; sections; }

(* CR mshinwell: tidy all this up *)

exception Found of string * int

let resolve t ~program_counter =
  try
    begin match Owee_elf.find_section t.sections ".debug_line" with
    | None -> ()
    | Some section ->
      let body = Owee_buf.cursor (Owee_elf.section_body t.map section) in
      let rec aux () =
        match Owee_debug_line.read_chunk body with
        | None -> ()
        | Some (header, chunk) ->
          let check header state () =
            let open Owee_debug_line in
            if not state.end_sequence then
            match get_filename header state with
            | None -> ()
            | Some filename -> raise (Found (filename, state.line))
          in
          Owee_debug_line.fold_rows (header, chunk) check ();
          aux ()
      in
      aux ()
    end;
    None
  with (Found (filename, line)) -> Some (filename, line)
