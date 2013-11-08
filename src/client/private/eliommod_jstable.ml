type 'a t = < > Js.t
let obj = Js.Unsafe.global##_Object
let create () : 'a t = jsnew obj ()
let add (t:'a t) (k:Js.js_string Js.t) (v:'a) =
  (* '_' is added to avoid conflicts with objects methods *)
  Js.Unsafe.set t (k##concat(Js.string "_")) v
let find (t:'a t) (k:Js.js_string Js.t) : 'a Js.Optdef.t =
  Js.Unsafe.get t (k##concat(Js.string "_"))
let keys (t:'a t) : Js.js_string Js.t list =
  let key_array : Js.js_string Js.t Js.js_array Js.t =
    Js.Unsafe.global##_Object##keys(t)
  in
  let res = ref [] in
  for i = 0 to pred key_array##length do
    let key =
      Js.Optdef.get
        (Js.array_get key_array i)
        (fun () -> failwith "Eliommod_jstable.keys")
    in
    res := key##substring(0, pred key##length) :: !res
  done;
  List.rev !res
