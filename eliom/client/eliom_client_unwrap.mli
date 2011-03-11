
type unwrap_id

val register_unwrapper : unwrap_id -> ('a -> 'b) -> unit

val unwrap : ('b*'a) -> 'a

val id_of_int : int -> unwrap_id
