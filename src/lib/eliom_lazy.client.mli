(* The type of value whose evaluation on the server is delayed until
   the value is wrapped.*)
type 'a request = 'a

val from_fun : (unit -> 'a) -> 'a request
val from_val : 'a -> 'a request
val force : 'a request -> 'a
