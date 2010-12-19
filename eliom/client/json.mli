(** Js_of_ocaml **)

val unsafe_input: ?encoding:[ `Byte | `Unicode ] -> Js.js_string Js.t -> 'a
val output: ?encoding:[ `Byte | `Unicode ] -> 'a -> Js.js_string Js.t

