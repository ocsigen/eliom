(* Copyright Jeremy Yallop 2007.
   Copyright Grégoire Henry 2010.
   This file is free software, distributed under the MIT license.
   See the file COPYING for details.
*)

module type Bounded =
sig
  type a
  val min_bound : a
  val max_bound : a
end

module Bounded_unit      : sig val make: (module Bounded with type a = unit) end
module Bounded_bool      : sig val make: (module Bounded with type a = bool) end
module Bounded_char      : sig val make: (module Bounded with type a = char) end
module Bounded_int       : sig val make: (module Bounded with type a = int) end
module Bounded_int32     : sig val make: (module Bounded with type a = int32) end
module Bounded_int64     : sig val make: (module Bounded with type a = int64) end
module Bounded_nativeint : sig val make: (module Bounded with type a = nativeint) end
(* module Bounded_open_flag : sig val make: (module Bounded with type a = open_flag) end *)
(* module Bounded_fpclass   : sig val make: (module Bounded with type a = fpclass) end *)
