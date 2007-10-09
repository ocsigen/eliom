
type t

val create : int -> (unit -> unit) -> t

val reset : t -> unit

val remove : t -> unit

val change : t -> int -> unit
