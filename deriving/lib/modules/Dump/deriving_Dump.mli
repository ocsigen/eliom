(* Copyright Jeremy Yallop 2007.
   Copyright Grégoire Henry 2010.
   This file is free software, distributed under the MIT license.
   See the file COPYING for details.
*)

module type Dump =
  sig
    type a
    val to_buffer : Buffer.t -> a -> unit
    val to_string : a -> string
    val to_channel : out_channel -> a -> unit
    val from_stream : char Stream.t -> a
    val from_string : string -> a
    val from_channel : in_channel -> a
  end

module type Dump_min = sig
  type a
  val to_buffer : Buffer.t -> a -> unit
  val from_stream : char Stream.t -> a
end
module Defaults(P : Dump_min) : Dump with type a = P.a

exception Dump_error of string

module Dump_unit      : sig val make: (module Dump with type a = unit) end
module Dump_bool      : sig val make: (module Dump with type a = bool) end
module Dump_char      : sig val make: (module Dump with type a = char) end
module Dump_int       : sig val make: (module Dump with type a = int) end
module Dump_int32     : sig val make: (module Dump with type a = Int32.t) end
module Dump_int64     : sig val make: (module Dump with type a = Int64.t) end
module Dump_nativeint : sig val make: (module Dump with type a = Nativeint.t) end

module Dump_float     : sig val make: (module Dump with type a = float) end
module Dump_string    : sig val make: (module Dump with type a = string) end
(* module Dump_num       : sig val make: (module Dump with type a = Num.num) end *)
module Dump_list      : sig
  val make: (module Dump with type a = 'a) -> (module Dump with type a = 'a list)
end
module Dump_option    : sig
  val make: (module Dump with type a = 'a) -> (module Dump with type a = 'a option)
end

module Dump_undumpable (P : sig type a val tname : string end) : sig
  val make: (module Dump with type a = P.a)
end
module Dump_via_marshal (P : sig type a end) : sig
  val make: (module Dump with type a = P.a)
end
