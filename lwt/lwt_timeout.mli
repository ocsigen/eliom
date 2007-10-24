
type t

val create : int -> (unit -> unit) -> t

val start : t -> unit

val stop : t -> unit

val change : t -> int -> unit
