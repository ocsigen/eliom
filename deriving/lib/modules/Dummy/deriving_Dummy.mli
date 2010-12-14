(* Copyright Jeremy Yallop 2007.
   Copyright Grégoire Henry 2010.
   This file is free software, distributed under the MIT license.
   See the file COPYING for details.
*)

module type Dummy = sig
  type a
  val test : unit -> unit
end

module Dummy_unit      : sig val make: (module Dummy with type a = unit) end
module Dummy_char      : sig val make: (module Dummy with type a = char) end
module Dummy_bool      : sig val make: (module Dummy with type a = bool) end
module Dummy_int       : sig val make: (module Dummy with type a = int) end
module Dummy_int32     : sig val make: (module Dummy with type a = int32) end
module Dummy_int64     : sig val make: (module Dummy with type a = int64) end
module Dummy_nativeint : sig val make: (module Dummy with type a = nativeint) end
module Dummy_float     : sig val make: (module Dummy with type a = float) end
(* module Dummy_num       : sig val make: (module Dummy with type a = Num.num) end *)
module Dummy_string    : sig val make: (module Dummy with type a = string) end

module Dummy_list : sig
  val make: (module Dummy with type a = 'a) -> (module Dummy with type a = 'a list)
end
module Dummy_ref : sig
  val make: (module Dummy with type a = 'a) -> (module Dummy with type a = 'a ref)
end
module Dummy_option : sig
  val make: (module Dummy with type a = 'a) -> (module Dummy with type a = 'a option)
end
module Dummy_array : sig
  val make: (module Dummy with type a = 'a) -> (module Dummy with type a = 'a array)
end
