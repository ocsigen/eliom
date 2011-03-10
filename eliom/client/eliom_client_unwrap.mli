
type unwrap_id

type mark

val register_unwrapper : unwrap_id -> ('a -> 'b) -> unit

val unwrap : (mark*'a) -> 'b

val id_of_int : int -> unwrap_id
