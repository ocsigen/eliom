(* Copyright Jeremy Yallop 2007.
   Copyright Grégoire Henry 2010.
   This file is free software, distributed under the MIT license.
   See the file COPYING for details.
*)

module type Show =
  sig
    type a
    val format : Format.formatter -> a -> unit
    val format_list : Format.formatter -> a list -> unit
    val show : a -> string
    val show_list : a list -> string
  end

module type Show_min =
  sig
    type a
    val format : Format.formatter -> a -> unit
  end
module Defaults (S : Show_min) : Show with type a = S.a

module Show_unprintable (S : sig type a end) : sig
  val make: (module Show with type a = S.a)
end

module Show_char      : sig val make: (module Show with type a = char) end
module Show_bool      : sig val make: (module Show with type a = bool) end
module Show_unit      : sig val make: (module Show with type a = unit) end
module Show_int       : sig val make: (module Show with type a = int) end
module Show_int32     : sig val make: (module Show with type a = int32) end
module Show_int64     : sig val make: (module Show with type a = int64) end
module Show_nativeint : sig val make: (module Show with type a = nativeint) end
(* module Show_num       : sig val make: (module Show with type a = Num.num_ ned *)
module Show_float     : sig val make: (module Show with type a = float) end
module Show_string    : sig val make: (module Show with type a = string) end

module Show_option  : sig
  val make: (module Show with type a = 'a) -> (module Show with type a = 'a option)
end
module Show_list : sig
  val make: (module Show with type a = 'a) -> (module Show with type a = 'a list)
end
module Show_array  : sig
  val make: (module Show with type a = 'a) -> (module Show with type a = 'a array)
end

module Show_ref  : sig
  val make: (module Show with type a = 'a) -> (module Show with type a = 'a ref)
end

(* module Show_open_flag : sig val make: (module Show with type a = open_flag) end *)
(* module Show_fpclass   : sig val make: (module Show with type a = fpclass) end *)
