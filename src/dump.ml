
let ensure_dir dir =
  if not (Sys.file_exists dir) then Unix.mkdir dir 0o774
  else if not (Sys.is_directory dir) then
    failwith (dir ^ " exists and is not a directory.")

let write_string dir file str =
  let oc = open_out (dir ^ "/" ^ file) in
  output_string oc str;
  close_out oc

let rec dump_projections max_depth depth dir series path =
  if depth <= max_depth then begin
    ensure_dir dir;
    let json =
      Yojson.Basic.pretty_to_string ~std:true (Series.to_json path series)
    in
    write_string dir "series.json" json;
    let projections = Series.projections series in
    Address.Map.iter
      (fun addr series ->
         let dir = dir ^ "/" ^ (Address.to_string addr) in
         let path = Path.project path addr in
         dump_projections max_depth (depth + 1) dir series path)
      projections
  end

let dump ~dir ~title series =
  ensure_dir dir;
  write_string dir "index.html" Embed.html;
  write_string dir "graph.js" (Embed.js ~title);
  dump_projections 10 0 (dir ^ "/data") series Path.initial
