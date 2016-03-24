
module Graph = struct

  let map_object (map : 'a StringMap.t) =
    let bindings = Array.of_list (StringMap.bindings map) in
    let values = Array.map (fun (k, v) -> (k, Js.Unsafe.inject v)) bindings in
    Js.Unsafe.obj values

  type location =
    < display : Js.js_string Js.t Js.readonly_prop;
      foreign : bool Js.readonly_prop;
      depth: int Js.readonly_prop; > Js.t

  let location ~(depth : int) ~(display : string) ~(foreign : bool) =
    object%js
      val display = Js.string display
      val foreign = foreign
      val depth = depth
    end

  type names

  let names ~(names : location StringMap.t) : names =
    map_object names

  type values

  type snapshot =
    < time : float Js.readonly_prop;
      values : values Js.readonly_prop > Js.t

  let snapshot ~(time : float) ~(values: int StringMap.t) : snapshot =
    object%js
      val time = time
      val values = map_object values
    end

  type state =
    < names : names Js.readonly_prop;
      snapshots : snapshot Js.js_array Js.t Js.readonly_prop > Js.t

  let state ~(names : names) ~(snapshots : snapshot list) : state =
    object%js
      val names = names
      val snapshots = Js.array (Array.of_list snapshots)
    end

  let run (init : state) (change : string -> state) : unit =
    Js.Unsafe.global##run init (fun id -> change (Js.to_string id))

end
