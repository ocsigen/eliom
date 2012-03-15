type 'a t = < > Js.t
let obj = Js.Unsafe.variable "Object"
let create () : 'a t = jsnew obj ()
let add (t:'a t) (k:Js.js_string Js.t) (v:'a) =
  (* '_' is added to avoid conflicts with objects methods *)
  Js.Unsafe.set t (k##concat(Js.string "_")) v
let find (t:'a t) (k:Js.js_string Js.t) : 'a Js.Optdef.t =
  Js.Unsafe.get t (k##concat(Js.string "_"))
