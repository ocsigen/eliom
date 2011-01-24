(* Copyright Grégoire Henry 2010.
   This file is free software, distributed under the MIT license.
   See the file COPYING for details.
*)

(** Json **)

type 'a t

val to_string: 'a t -> 'a -> string
val from_string: 'a t -> string -> 'a

(** Deriver *)

module type Json_min = sig
  type a
  val write: Buffer.t -> a -> unit
  val read: Deriving_Json_lexer.lexbuf -> a
end

module type Json_min' = sig
  type a
  val write: Buffer.t -> a -> unit
  val read: Deriving_Json_lexer.lexbuf -> a
  val match_variant: [`Cst of int | `NCst of int] -> bool
  val read_variant: Deriving_Json_lexer.lexbuf -> [`Cst of int | `NCst of int] -> a
end

module type Json = sig
  type a
  val t: a t
  val write: Buffer.t -> a -> unit
  val read: Deriving_Json_lexer.lexbuf -> a
  val to_string: a -> string
  (* val to_channel: out_channel -> a -> unit *)
  val from_string: string -> a
  (* val from_channel: in_channel -> a *)
  val match_variant: [`Cst of int | `NCst of int] -> bool
  val read_variant: Deriving_Json_lexer.lexbuf -> [`Cst of int | `NCst of int] -> a
end

module Defaults(J:Json_min) : Json with type a = J.a
module Defaults'(J:Json_min') : Json with type a = J.a

val wrap : 'a t -> (module Json with type a = 'a)

module Json_char : sig
  val make : (module Json with type a = char)
end
module Json_bool : sig
  val make: (module Json with type a = bool)
end
module Json_unit : sig
  val make: (module Json with type a = unit)
end
module Json_int : sig
  val make: (module Json with type a = int)
end
module Json_int32 : sig
  val make: (module Json with type a = int32)
end
module Json_int64 : sig
  val make: (module Json with type a = int64)
end
module Json_nativeint : sig
  val make: (module Json with type a = nativeint)
end
(* module Json_num       : sig *)
  (* val make: (module Json with type a = Num.num) *)
(* end *)
module Json_float : sig
  val make: (module Json with type a = float)
end
module Json_string : sig
  val make: (module Json with type a = string)
end

module Json_list : sig
  val make: (module Json with type a = 'a) -> (module Json with type a = 'a list)
end
module Json_ref : sig
  val make: (module Json with type a = 'a) -> (module Json with type a = 'a ref)
end
module Json_option : sig
    val make: (module Json with type a = 'a) -> (module Json with type a = 'a option)
end
module Json_array: sig
  val make: (module Json with type a = 'a) -> (module Json with type a = 'a array)
end
