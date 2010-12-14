(* Copyright Jeremy Yallop 2007.
   Copyright Grégoire Henry 2010.
   This file is free software, distributed under the MIT license.
   See the file COPYING for details.
*)

(* A module for SML-style equality, i.e. where equality of mutables is
   physical equality and equality of immutables is structural equality.
*)

module type Eq =
sig
  type a
  val eq : a -> a -> bool
end

module Eq_immutable (S : sig type a end) : sig
  val make: (module Eq with type a = S.a)
end
module Eq_mutable (S : sig type a end) : sig
  val make: (module Eq with type a = S.a)
end

module Eq_unit   : sig val make: (module Eq with type a = unit) end
module Eq_bool   : sig val make: (module Eq with type a = bool) end
module Eq_char   : sig val make: (module Eq with type a = char) end
module Eq_int    : sig val make: (module Eq with type a = int) end
module Eq_float  : sig val make: (module Eq with type a = float) end
(* module Eq_num    : sig val make: (module Eq with type a = Num.num) end *)

module Eq_list   : sig
  val make: (module Eq with type a = 'a) -> (module Eq with type a = 'a list)
end
module Eq_option : sig
  val make: (module Eq with type a = 'a) -> (module Eq with type a = 'a option)
end

module Eq_string : sig val make: (module Eq with type a = string) end
module Eq_ref    : sig
  val make: (module Eq with type a = 'a) -> (module Eq with type a = 'a ref)
end
module Eq_array  : sig
  val make: (module Eq with type a = 'a) -> (module Eq with type a = 'a array)
end

