(** tables using javscript objetcs: faster than hashtables *)
type 'a t
val create : unit -> 'a t
val add : 'a t -> Js.js_string Js.t -> 'a -> unit
val find : 'a t -> Js.js_string Js.t -> 'a Js.Optdef.t
val keys : 'a t -> Js.js_string Js.t list
