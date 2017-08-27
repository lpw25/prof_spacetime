
type mode =
  | Bytes
  | Blocks
  | Allocations
  | Indirect_calls
  | Direct_calls

type kind =
  | Reduced
  | All

type t = {
  mode : mode;
  kind : kind;
  addresses: Address.t list;
}

let initial =
  { mode = Bytes; kind = Reduced; addresses = [] }

let create mode kind addresses =
  { mode; kind; addresses }

let mode { mode; _ } = mode

let kind { kind; _ } = kind

let addresses { addresses; _ } = addresses

let with_mode t mode = { t with mode }

let with_kind t kind = { t with kind }

let project t addr = { t with addresses = addr :: t.addresses }

let bytes_mode = "/bytes"
let blocks_mode = "/block"
let allocations_mode = "/alloc"
let indirect_calls_mode = "/indirect-calls"
let direct_calls_mode = "/direct-calls"

let mode_len =
  let bytes_len = String.length bytes_mode in
  let blocks_len = String.length blocks_mode in
  let allocations_len = String.length allocations_mode in
  assert (bytes_len = blocks_len);
  assert (bytes_len = allocations_len);
  bytes_len

let reduced_kind = "/red"
let all_kind = "/all"

let kind_len =
  let reduced_len = String.length reduced_kind in
  let add_len = String.length all_kind in
  assert (reduced_len = add_len);
  reduced_len

let default_suffix = "series.json"

let suffix_len = String.length default_suffix

let initial_path = "/initial.json"

let () =
  let initial_len = String.length initial_path in
  assert (initial_len < mode_len + kind_len + suffix_len)

let split_string s from until =
  let rec loop s segments prev n until =
    if prev >= until then begin
      segments
    end else if n >= until then begin
      let final = String.sub s prev (until - prev) in
      (final :: segments)
    end else if s.[n] = '/' then begin
      let segments =
        if prev >= n then segments
        else (String.sub s prev (n - prev)) :: segments
      in
      loop s segments (n + 1) (n + 1) until
    end else begin
      loop s segments prev (n + 1) until
    end
  in
  loop s [] from from until

let of_string s =
  let len = String.length s in
  if len >= (mode_len + kind_len + suffix_len) then begin
    let mode = String.sub s 0 mode_len in
    let kind = String.sub s mode_len kind_len in
    let suffix = String.sub s (len - suffix_len) suffix_len in
    try
      if suffix = default_suffix then begin
        let mode =
          if mode = bytes_mode then Some Bytes
          else if mode = blocks_mode then Some Blocks
          else if mode = allocations_mode then Some Allocations
          else if mode = indirect_calls_mode then Some Indirect_calls
          else None
        in
        let kind =
          if kind = reduced_kind then Some Reduced
          else if kind = all_kind then Some All
          else None
        in
        let segments =
          split_string s (mode_len + kind_len) (len - suffix_len)
        in
        let addresses =
          List.map
            (fun str -> Address.of_int64 (Int64.of_string str))
            segments
        in
        match mode, kind with
        | None, _ -> None
        | _, None -> None
        | Some mode, Some kind -> Some { mode; kind; addresses }
      end else None
    with Failure _ -> None
  end else begin
    if s = initial_path then Some initial
    else None
  end

let mode_to_string = function
  | Bytes -> bytes_mode
  | Blocks -> blocks_mode
  | Allocations -> allocations_mode

let kind_to_string = function
  | Reduced -> reduced_kind
  | All -> all_kind

let to_string t =
  let mode = mode_to_string t.mode in
  let kind = kind_to_string t.kind in
  let rec loop = function
    | [] -> mode ^ kind
    | addr :: rest -> loop rest ^ "/" ^ (Address.to_string addr)
  in
  let body = loop t.addresses in
  body ^ "/" ^ default_suffix

let mode_to_display_string = function
  | Bytes -> "Live words"
  | Blocks -> "Live blocks"
  | Allocations -> "All allocated words"
  | Indirect_calls -> "Indirect calls"
