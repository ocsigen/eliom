(* Copyright Jeremy Yallop 2007.
   Copyright Grégoire Henry 2010.
   This file is free software, distributed under the MIT license.
   See the file COPYING for details.
*)

module type Enum =
  sig
    type a
    val succ : a -> a
    val pred : a -> a
    val to_enum : int -> a
    val from_enum : a -> int
    val enum_from : a -> a list
    val enum_from_then : a -> a -> a list
    val enum_from_to : a -> a -> a list
    val enum_from_then_to : a -> a -> a -> a list
  end

module type Enum_min = sig
  type a
  val numbering : (a * int) list
end
module Defaults(E : Enum_min) : Enum with type a = E.a

module Defaults'
    (E : sig type a val from_enum : a -> int val to_enum : int -> a end)
    (B : sig val make: (module Deriving_Bounded.Bounded with type a = E.a) end)
    : Enum with type a = E.a

module Enum_bool : sig val make: (module Enum with type a = bool) end
module Enum_char : sig val make: (module Enum with type a = char) end
module Enum_int  : sig val make: (module Enum with type a = int) end
module Enum_unit : sig val make: (module Enum with type a = unit) end
