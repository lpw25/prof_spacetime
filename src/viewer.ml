open Lwt
open LTerm_style
open LTerm_text

module State = struct

  type t =
    { memo : Table.Memo.t;
      has_calls : bool;
      no_of_snapshots : int;
      snapshot : int;
      path : Table.Path.t;
      table : Table.t;
      no_of_visible_rows : int;
      first_visible_row : int;
      selected_row : int; }

  let table { table } = table

  let path { path } = path

  let first_visible_row { first_visible_row } = first_visible_row

  let selected_row { selected_row } = selected_row

  let no_of_snapshots { no_of_snapshots } = no_of_snapshots

  let pp ppf t =
    Format.fprintf ppf
      "@[<2>{@ memo = <memo>;@ \
               has_calls = %a;@  \
               no_of_snapshots = %i;@ \
               snapshot = %i;@ \
               table = <table>;@ \
               path = %a;@ \
               no_of_visible_rows = %i;@ \
               first_visible_row = %i;@ \
               selected_row = %i;@ }@]"
      Format.pp_print_bool t.has_calls
      t.no_of_snapshots
      t.snapshot
      Table.Path.pp t.path
      t.no_of_visible_rows
      t.first_visible_row
      t.selected_row

  let create series =
    let memo = Table.Memo.create series in
    let has_calls = Series.has_call_counts series in
    let no_of_snapshots = List.length (Series.snapshots series) in
    let snapshot = 0 in
    let mode =
      if snapshot < no_of_snapshots || not has_calls then
        Table.Mode.Allocation(snapshot, Table.Mode.Allocation.Bytes)
      else
        Table.Mode.Call Table.Mode.Call.Calls
    in
    let direction = Table.Direction.Normal in
    let path = Table.Path.create mode direction [] in
    let table = Table.Memo.table memo ~path in
    let no_of_visible_rows = 0 in
    let first_visible_row = 0 in
    let selected_row = 0 in
    { memo; has_calls; no_of_snapshots; snapshot; path; table;
      no_of_visible_rows; first_visible_row; selected_row; }

  let bound_snapshot t snapshot =
    let snapshot =
      if snapshot >= t.no_of_snapshots then t.no_of_snapshots - 1
      else snapshot
    in
    let snapshot =
      if snapshot < 0 then 0 else snapshot
    in
    snapshot

  let bound_selected_row t selected_row =
    let size = Table.size t.table in
    let selected_row =
      if selected_row >= size then size - 1 else selected_row
    in
    let selected_row =
      if selected_row < 0 then 0 else selected_row
    in
    selected_row

  let make_selected_visible t selected_row =
    let size = Table.size t.table in
    let diff = selected_row - t.first_visible_row in
    let first_visible_row =
      if diff > t.no_of_visible_rows || diff < 0 then
        t.selected_row - (t.no_of_visible_rows / 2)
      else
        t.first_visible_row
    in
    if first_visible_row < 0 then
      0
    else if t.no_of_visible_rows > size then
      0
    else if first_visible_row + t.no_of_visible_rows > size then
      size - t.no_of_visible_rows
    else
      first_visible_row

  let resize t no_of_visible_rows =
    let first_visible_row = make_selected_visible t t.selected_row in
    { t with no_of_visible_rows; first_visible_row }

  let selected_address t =
    match Table.row t.table t.selected_row with
    | None -> None
    | Some row ->
      let address = Table.Row.address row in
      Some address

  let update t address snapshot path =
    let table = Table.Memo.table t.memo path in
    let selected_row =
      match address with
      | None -> 0
      | Some address -> begin
          match
            Table.find
              (fun row -> Address.equal (Table.Row.address row) address)
              table
          with
          | index -> index
          | exception Not_found -> 0
        end
    in
    let first_visible_row = make_selected_visible t selected_row in
    { t with snapshot; path; table; selected_row; first_visible_row }

  let next_mode t =
    let mode = Table.Path.mode t.path in
    let mode =
      match mode with
      | Table.Mode.Allocation(_, Table.Mode.Allocation.Bytes) ->
          if (t.snapshot < t.no_of_snapshots) then
            Table.Mode.Allocation(t.snapshot, Table.Mode.Allocation.Blocks)
          else if t.has_calls then
            Table.Mode.Call Table.Mode.Call.Calls
          else
            mode
      | Table.Mode.Allocation(_, Table.Mode.Allocation.Blocks) ->
          if (t.snapshot < t.no_of_snapshots) then
            Table.Mode.Allocation(t.snapshot, Table.Mode.Allocation.Allocations)
          else if t.has_calls then
            Table.Mode.Call Table.Mode.Call.Calls
          else
            mode
      | Table.Mode.Allocation(_, Table.Mode.Allocation.Allocations) ->
          if t.has_calls then
            Table.Mode.Call Table.Mode.Call.Calls
          else if(t.snapshot < t.no_of_snapshots) then
            Table.Mode.Allocation(t.snapshot, Table.Mode.Allocation.Bytes)
          else
            mode
      | Table.Mode.Call Table.Mode.Call.Calls ->
          if t.has_calls then
            Table.Mode.Call Table.Mode.Call.Indirect_calls
          else if(t.snapshot < t.no_of_snapshots) then
            Table.Mode.Allocation(t.snapshot, Table.Mode.Allocation.Bytes)
          else
            mode
      | Table.Mode.Call Table.Mode.Call.Indirect_calls ->
          if(t.snapshot < t.no_of_snapshots) then
            Table.Mode.Allocation(t.snapshot, Table.Mode.Allocation.Bytes)
          else if t.has_calls then
            Table.Mode.Call Table.Mode.Call.Calls
          else
            mode
    in
    let path = Table.Path.with_mode t.path mode in
    let address = selected_address t in
    update t address t.snapshot path

  let previous_mode t =
    let mode = Table.Path.mode t.path in
    let mode =
      match mode with
      | Table.Mode.Allocation(_, Table.Mode.Allocation.Bytes) ->
          if t.has_calls then
            Table.Mode.Call Table.Mode.Call.Indirect_calls
          else if (t.snapshot < t.no_of_snapshots) then
            Table.Mode.Allocation
              (t.snapshot, Table.Mode.Allocation.Allocations)
          else
            mode
      | Table.Mode.Allocation(_, Table.Mode.Allocation.Blocks) ->
          if (t.snapshot < t.no_of_snapshots) then
            Table.Mode.Allocation(t.snapshot, Table.Mode.Allocation.Bytes)
          else if t.has_calls then
            Table.Mode.Call Table.Mode.Call.Indirect_calls
          else
            mode
      | Table.Mode.Allocation(_, Table.Mode.Allocation.Allocations) ->
          if (t.snapshot < t.no_of_snapshots) then
            Table.Mode.Allocation(t.snapshot, Table.Mode.Allocation.Blocks)
          else if t.has_calls then
            Table.Mode.Call Table.Mode.Call.Indirect_calls
          else
            mode
      | Table.Mode.Call Table.Mode.Call.Calls ->
          if (t.snapshot < t.no_of_snapshots) then
            Table.Mode.Allocation
              (t.snapshot, Table.Mode.Allocation.Allocations)
          else if t.has_calls then
            Table.Mode.Call Table.Mode.Call.Indirect_calls
          else
            mode
      | Table.Mode.Call Table.Mode.Call.Indirect_calls ->
          if t.has_calls then
            Table.Mode.Call Table.Mode.Call.Calls
          else if (t.snapshot < t.no_of_snapshots) then
            Table.Mode.Allocation
              (t.snapshot, Table.Mode.Allocation.Allocations)
          else
            mode
    in
    let path = Table.Path.with_mode t.path mode in
    let address = selected_address t in
    update t address t.snapshot path

  let next_snapshot t =
    let snapshot = bound_snapshot t (t.snapshot + 1) in
    match Table.Path.mode t.path with
    | Table.Mode.Allocation(_, amode) ->
      let mode = Table.Mode.Allocation(snapshot, amode) in
      let path = Table.Path.with_mode t.path mode in
      let address = selected_address t in
      update t address snapshot path
    | Table.Mode.Call _ -> t

  let previous_snapshot t =
    let snapshot = bound_snapshot t (t.snapshot - 1) in
    match Table.Path.mode t.path with
    | Table.Mode.Allocation(_, amode) ->
      let mode = Table.Mode.Allocation(snapshot, amode) in
      let path = Table.Path.with_mode t.path mode in
      let address = selected_address t in
      update t address snapshot path
    | Table.Mode.Call _ -> t

  let select t =
    match Table.row t.table t.selected_row with
    | None -> t
    | Some row -> begin
      match Table.Row.selection row with
      | None -> t
      | Some path ->
        update t None t.snapshot path
    end

  let parent t =
    match Table.frames t.table with
    | [] | [_] -> t
    | current :: parent :: _ ->
        let selected = Table.Frame.selected parent in
        let path = Table.Frame.path parent in
        update t selected t.snapshot path

  let invert t =
    let mode = Table.Path.mode t.path in
    let direction =
      match Table.Path.direction t.path with
      | Table.Direction.Normal -> Table.Direction.Inverted
      | Table.Direction.Inverted -> Table.Direction.Normal
    in
    let path = Table.Path.create mode direction [] in
    update t None t.snapshot path

  let next_row t =
    let selected_row = bound_selected_row t (t.selected_row + 1) in
    let first_visible_row = make_selected_visible t selected_row in
    { t with selected_row; first_visible_row }

  let previous_row t =
    let selected_row = bound_selected_row t (t.selected_row - 1) in
    let first_visible_row = make_selected_visible t selected_row in
    { t with selected_row; first_visible_row }

  let next_page t =
    let selected_row =
      bound_selected_row t (t.selected_row + t.no_of_visible_rows)
    in
    let first_visible_row = make_selected_visible t selected_row in
    { t with selected_row; first_visible_row }

  let previous_page t =
    let selected_row =
      bound_selected_row t (t.selected_row - t.no_of_visible_rows)
    in
    let first_visible_row = make_selected_visible t selected_row in
    { t with selected_row; first_visible_row }

end

let draw_header size ctx state =
  let header_bar =
    LTerm_style.{ none with foreground = Some white; background = Some blue }
  in
  let cols = LTerm_geom.cols size in
  LTerm_draw.draw_hline ctx 0 0 cols ~style:header_bar LTerm_draw.Blank;
  let table = State.table state in
  let time = Table.time table in
  let total = Table.total table in
  let path = State.path state in
  let snapshot, desc =
    match Table.Path.mode path with
    | Table.Mode.Allocation(snapshot, amode) ->
        let no_of_snapshots = State.no_of_snapshots state in
        let snapshot =
          if no_of_snapshots = 0 then 0 else snapshot + 1
        in
        let snapshot =
          Printf.sprintf " (%i/%i)" snapshot no_of_snapshots
        in
        let desc =
          match amode with
          | Table.Mode.Allocation.Bytes -> "live bytes"
          | Table.Mode.Allocation.Blocks -> "live blocks"
          | Table.Mode.Allocation.Allocations -> "allocated words"
        in
        snapshot, desc
    | Table.Mode.Call cmode ->
        let snapshot = "" in
        let desc =
          match cmode with
          | Table.Mode.Call.Calls -> "calls"
          | Table.Mode.Call.Indirect_calls -> "indirect calls"
        in
        snapshot, desc
  in
  let frames =
    let frames = Table.frames table in
    let frames =
      List.fold_right
        (fun frame acc ->
           match Table.Frame.display frame with
           | None -> acc
           | Some display -> display :: acc)
        frames []
    in
    String.concat ";" frames
  in
  LTerm_draw.draw_styled ctx 0 0 ~style:header_bar
    (eval [ LTerm_text.S
              (Printf.sprintf "Time %f%s, Total %d %s [%s]"
                 time snapshot total desc frames)])

let draw_row size ctx unit pos selected row =
  let value = Table.Row.value row in
  let percentage = Table.Row.percentage row in
  let display = Table.Row.display row in
  let colour =
    if percentage > 10. then
      LTerm_style.red
    else if percentage > 1. then
      LTerm_style.green
    else
      LTerm_style.default
  in
  let cols = LTerm_geom.cols size in
  let display_length = cols - 23 in
  let diff = display_length - (String.length display) in
  let display =
    if diff < 0 then
      String.sub display 0 display_length
    else
      display ^ String.make diff ' '
  in
  LTerm_draw.draw_styled ctx (pos + 1) 0
    (eval [ LTerm_text.B_reverse selected;
            LTerm_text.B_fg colour;
            LTerm_text.S (Printf.sprintf " %5.2f%% " percentage);
            LTerm_text.E_fg;
            LTerm_text.S (Printf.sprintf " %10d" value);
            LTerm_text.S unit;
            LTerm_text.S "  ";
            LTerm_text.S display;
            LTerm_text.E_reverse; ])

let draw ui matrix state =
  let size = LTerm_ui.size ui in
  let ctx = LTerm_draw.context matrix size in
  LTerm_draw.clear ctx;
  draw_header size ctx state;
  let path = State.path state in
  let unit =
    match Table.Path.mode path with
    | Table.Mode.Allocation(_, Table.Mode.Allocation.Bytes) -> "b"
    | Table.Mode.Allocation(_, Table.Mode.Allocation.Blocks) -> "b"
    | Table.Mode.Allocation(_, Table.Mode.Allocation.Allocations) -> "w"
    | Table.Mode.Call _ -> " "
  in
  let table = State.table state in
  let first_visible_row = State.first_visible_row state in
  let selected_row = State.selected_row state in
  for pos = 0 to (LTerm_geom.rows size - 1) do
    let idx = first_visible_row + pos in
    match Table.row table idx with
    | None -> ()
    | Some row ->
      let selected = idx = selected_row in
      draw_row size ctx unit pos selected row
  done

type response =
  | Update of State.t
  | Quit
  | Ignore

let process_event state event =
  match event with
  | LTerm_event.Resize size ->
      let rows = (LTerm_geom.rows size) - 1 in
      Update (State.resize state rows)
  | LTerm_event.Key { code = LTerm_key.Tab; shift = false } ->
      Update (State.next_mode state)
  | LTerm_event.Key { code = LTerm_key.Tab; shift = true } ->
      Update (State.previous_mode state)
  | LTerm_event.Key { code = LTerm_key.Left } ->
      Update (State.previous_snapshot state)
  | LTerm_event.Key { code = LTerm_key.Right } ->
      Update (State.next_snapshot state)
  | LTerm_event.Key { code = LTerm_key.Up } ->
      Update (State.previous_row state)
  | LTerm_event.Key { code = LTerm_key.Down } ->
      Update (State.next_row state)
  | LTerm_event.Key { code = LTerm_key.Prev_page } ->
      Update (State.previous_page state)
  | LTerm_event.Key { code = LTerm_key.Next_page } ->
      Update (State.next_page state)
  | LTerm_event.Key { code = LTerm_key.Enter } ->
      Update (State.select state)
  | LTerm_event.Key { code = LTerm_key.Escape }
  | LTerm_event.Key { code = LTerm_key.Backspace } ->
      Update (State.parent state)
  | LTerm_event.Key { code = LTerm_key.Char c; control } ->
      if CamomileLibrary.UChar.char_of c = 'q' then
        Quit
      else if control && CamomileLibrary.UChar.char_of c = 'c' then
        Quit
      else if CamomileLibrary.UChar.char_of c = 'i' then
        Update (State.invert state)
      else
        Ignore
  | _ ->
      Ignore

let rec event_loop ui state_ref =
  LTerm_ui.wait ui >>= fun event ->
  match process_event !state_ref event with
  | Quit ->
      Lwt.return ()
  | Update state ->
      state_ref := state;
      LTerm_ui.draw ui;
      event_loop ui state_ref
  | Ignore ->
      event_loop ui state_ref

let main series =
  let state_ref = ref (State.create series) in
  Lazy.force LTerm.stdout
  >>= fun term ->
  LTerm.set_escape_time term 0.2;
  LTerm_ui.create term (fun ui matrix -> draw ui matrix !state_ref)
  >>= fun ui ->
  let size = LTerm_ui.size ui in
  let rows = (LTerm_geom.rows size) - 1 in
  state_ref := State.resize !state_ref rows;
  LTerm_ui.draw ui;
  Lwt.finalize
    (fun () -> event_loop ui state_ref)
    (fun () -> LTerm_ui.quit ui)

let show series =
  Lwt_main.run (main series)
