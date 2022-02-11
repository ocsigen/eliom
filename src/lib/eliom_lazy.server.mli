(* The type of value whose evaluation may be postponed until the value
   is wrapped. *)
type 'a request

val from_fun : (unit -> 'a) -> 'a request
val from_val : 'a -> 'a request
val force : 'a request -> 'a
