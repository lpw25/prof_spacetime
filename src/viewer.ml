open Lwt
open LTerm_style
open LTerm_text

module View = struct
  type t =
    { snapshot : Snapshot.t
    ; projects : (Address.t * string) list
    ; summary : (Address.t * string * int) list
    ; total : int
    ; mutable row_cursor : int
    ; mutable top_row : int
    }

  let create series ?address ~index projects =
    let snapshots = Series.snapshots series in
    let snapshot = List.nth snapshots index in
    let snapshot =
      List.fold_left
        (fun acc (addr, _) -> Snapshot.project acc addr)
        snapshot
        (List.rev projects)
    in
    let locations = Snapshot.locations snapshot in
    let summary = Snapshot.to_summary_list locations snapshot in
    let total =
      List.fold_left (fun sum (_, _, value) -> sum + value) 0 summary
    in
    let row_cursor =
      match address with
      | None -> 1
      | Some addr ->
        match
          List.mapi (fun cnt (address, _, _) -> (cnt, address)) summary
          |> List.find (fun (_, address) -> Address.equal address addr)
        with
        | (cnt, _) -> cnt + 1
        | exception Not_found -> 1
    in
    { snapshot
    ; projects
    ; summary
    ; total
    ; row_cursor
    ; top_row = 1 (* This will get readjusted on drawing *)
    }

  let align_view t ~visible_rows =
    (* Remove 1 row for header *)
    if t.row_cursor - t.top_row > visible_rows - 2
    then t.top_row <- t.row_cursor - visible_rows + 2
    else begin
      if t.row_cursor < t.top_row
      then t.top_row <- t.row_cursor
    end

  let move_cursor t delta ~visible_rows =
    let prev_cursor = t.row_cursor in
    let cursor = prev_cursor + delta in
    let cursor =
      min (max cursor 1) (List.length t.summary)
    in
    if t.row_cursor = cursor
    then false
    else begin
      t.row_cursor <- cursor;
      true
    end
end

let projects_to_string projects =
  let rec concat = function
    | a :: b :: tl -> concat ((a ^ "/" ^ b) :: tl)
    | a :: [] -> a
    | [] -> ""
  in
  concat
    (List.rev_map (fun (_, s) -> s) projects)

type state =
  { series : Series.t
  ; mutable snapshot_index : int
  ; mutable view : View.t
  }

let update_view ?address ?projects state =
  let address =
    match address with
    | Some address -> address
    | None         ->
      List.nth state.view.View.summary (state.view.View.row_cursor - 1)
      |> fun (address, _, _) -> address
  in
  let projects =
    match projects with
    | Some projects -> projects
    | None         -> state.view.View.projects
  in
  let view =
    View.create ~address state.series ~index:state.snapshot_index projects
  in
  state.view <- view

let rec event_loop ui state =
  let visible_rows = (LTerm_ui.size ui).LTerm_geom.rows in
  let open LTerm_key in
  LTerm_ui.wait ui >>= function
  | LTerm_event.Resize _ ->
    LTerm_ui.draw ui;
    event_loop ui state
  | LTerm_event.Key { code = Left } ->
    if state.snapshot_index > 0
    then begin
      state.snapshot_index <- state.snapshot_index - 1;
      update_view state;
      LTerm_ui.draw ui
    end;
    event_loop ui state
  | LTerm_event.Key { code = Right } ->
    if state.snapshot_index < List.length (Series.snapshots state.series) - 1
    then begin
      state.snapshot_index <- state.snapshot_index + 1;
      update_view state;
      LTerm_ui.draw ui
    end;
    event_loop ui state
  | LTerm_event.Key { code = Up } ->
    if View.move_cursor state.view (-1) ~visible_rows then LTerm_ui.draw ui;
    event_loop ui state
  | LTerm_event.Key { code = Down } ->
    if View.move_cursor state.view 1 ~visible_rows then LTerm_ui.draw ui;
    event_loop ui state
  | LTerm_event.Key { code = Prev_page } ->
    if View.move_cursor state.view (-10) ~visible_rows then LTerm_ui.draw ui;
    event_loop ui state
  | LTerm_event.Key { code = Next_page } ->
    if View.move_cursor state.view 10 ~visible_rows then LTerm_ui.draw ui;
    event_loop ui state
  | LTerm_event.Key { code = Enter } ->
    let project =
      List.nth state.view.View.summary (state.view.View.row_cursor - 1)
      |> fun (address, s, _) -> address, s
    in
    let projects = project :: state.view.View.projects in
    update_view ~projects state;
    LTerm_ui.draw ui;
    event_loop ui state
  | LTerm_event.Key { code = Backspace } ->
    begin match state.view.View.projects with
    | (address, _) :: projects ->
      update_view ~address ~projects state;
      LTerm_ui.draw ui
    | [] -> ()
    end;
    event_loop ui state
  | LTerm_event.Key { code = Escape } -> Lwt.return ()
  | _ -> event_loop ui state

let draw ui matrix t =
  let size = LTerm_ui.size ui in
  let ctx = LTerm_draw.context matrix size in
  let view = t.view in
  LTerm_draw.clear ctx;
  let header_bar =
    LTerm_style.{ none with foreground = Some white; background = Some blue }
  in
  LTerm_draw.draw_hline ctx 0 0 size.LTerm_geom.cols ~style:header_bar LTerm_draw.Blank;
  let total = view.View.total in
  LTerm_draw.draw_styled ctx 0 0 ~style:header_bar
    (eval [ S(Printf.sprintf " [%s] Time %f, Total %d words"
                (projects_to_string view.View.projects)
                (Snapshot.time view.View.snapshot) total) ]);
  let rows = size.LTerm_geom.rows in
  View.align_view view ~visible_rows:rows;
  let rec loop row = function
    | (_address, key, value) :: tl ->
      if row > rows
      then ()
      else
      if value > 0 then begin
        let percentage = float(value * 100) /. float(total) in
        let (color : LTerm_style.color) =
          if percentage > 10.
          then red
          else if percentage > 1.
          then green
          else default
        in
        let reverse = view.View.row_cursor = (row + view.View.top_row - 1) in
        let cols = size.LTerm_geom.cols in
        LTerm_draw.draw_styled ctx row 0
          (eval [ B_reverse reverse
                ; B_fg color
                ; S (Printf.sprintf " %5.2f%% " percentage)
                ; E_fg
                ; S (Printf.sprintf " %10dw  " value)
                ; S key
                ; S (String.init (cols - 23 - (String.length key)) (fun _ -> ' '))
                ; E_reverse
                ]);
        loop (row + 1) tl
      end
      else loop row tl
    | [] -> ()
  in
  let rec jump_to_row row = function
    | _ :: tl when row > 1 ->  jump_to_row (row - 1) tl
    | lst -> lst
  in
  loop 1 (jump_to_row view.View.top_row view.View.summary)

let main state =
  Lazy.force LTerm.stdout
  >>= fun term ->
  LTerm_ui.create term (fun matrix size -> draw matrix size state)
  >>= fun ui ->
  Lwt.finalize (fun () -> event_loop ui state) (fun () -> LTerm_ui.quit ui)

let show series =
  let view = View.create series ~index:0 [] in
  let state = { series; snapshot_index = 0; view } in
  Lwt_main.run (main state)

