(* Copyright Jeremy Yallop 2007.
   Copyright Grégoire Henry 2010.
   This file is free software, distributed under the MIT license.
   See the file COPYING for details.
*)

module TypeRep : sig
  type t
  type delayed = t Lazy.t
  val compare : t -> t -> int
  val eq : t -> t -> bool
  val mkFresh : string -> delayed list -> t
  val mkTuple : delayed list -> t
  val mkPolyv : (string * delayed option) list -> delayed list -> t
end

exception CastFailure of string

type dynamic
val tagOf : dynamic -> TypeRep.t

module type Typeable_min = sig
  type a
  val type_rep : TypeRep.t Lazy.t
end

module type Typeable =
sig
  type a
  val type_rep : TypeRep.t Lazy.t
  val has_type : dynamic -> bool
  val cast : dynamic -> a option
  val throwing_cast : dynamic -> a
  val make_dynamic : a -> dynamic
  val mk : a -> dynamic
end

module Defaults (T : Typeable_min) : Typeable with type a = T.a

module Typeable_list   : sig
  val make: (module Typeable with type a = 'a) -> (module Typeable with type a = 'a list)
end
module Typeable_option : sig
  val make: (module Typeable with type a = 'a) -> (module Typeable with type a = 'a option)
end
module Typeable_ref    : sig
  val make: (module Typeable with type a = 'a) -> (module Typeable with type a = 'a ref)
end


module Typeable_unit   : sig val make: (module Typeable with type a = unit) end
module Typeable_int    : sig val make: (module Typeable with type a = int) end
(* module Typeable_num    : sig val make: (module Typeable with type a = Num.num) end *)
module Typeable_float  : sig val make: (module Typeable with type a = float) end
module Typeable_bool   : sig val make: (module Typeable with type a = bool) end
module Typeable_string : sig val make: (module Typeable with type a = string) end
module Typeable_char   : sig val make: (module Typeable with type a = char) end

