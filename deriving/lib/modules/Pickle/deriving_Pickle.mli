(* Copyright Jeremy Yallop 2007.
   Copyright Grégoire Henry 2010.
   This file is free software, distributed under the MIT license.
   See the file COPYING for details.
*)

type id

(* representation of values of user-defined types *)
module Repr : sig
  type t
  val make : ?constructor:int -> id list -> t
end

(* Utilities for serialization *)
module Write : sig
  type s
  include Deriving_monad.Monad_state_type with type state = s
  module Utils (T : Deriving_Typeable.Typeable) (E : Deriving_Eq.Eq with type a = T.a) : sig
    val allocate : T.a -> (id -> unit m) -> id m
    val store_repr : id -> Repr.t -> unit m
  end
end

(* Utilities for deserialization *)
module Read : sig
  type s
  include Deriving_monad.Monad_state_type with type state = s
  module Utils (T : Deriving_Typeable.Typeable) : sig
    val sum    : (int * id list -> T.a m)  -> (id -> T.a m)
    val tuple  : (id list -> T.a m)        -> (id -> T.a m)
    val record : (T.a -> id list -> T.a m) -> int -> (id -> T.a m)
  end
end

exception UnpicklingError of string
exception UnknownTag of int * string

module type Pickle =
sig
  type a
  val typeable : (module Deriving_Typeable.Typeable with type a = a)
  val eq : (module Deriving_Eq.Eq with type a = a)
  val pickle : a -> id Write.m
  val unpickle : id -> a Read.m
  val to_buffer : Buffer.t -> a -> unit
  val to_string : a -> string
  val to_channel : out_channel -> a -> unit
  val from_stream : char Stream.t -> a
  val from_string : string -> a
  val from_channel : in_channel -> a
end

module type Pickle_min = sig
  type a
  val typeable : (module Deriving_Typeable.Typeable with type a = a)
  val eq : (module Deriving_Eq.Eq with type a = a)
  val pickle : a -> id Write.m
  val unpickle : id -> a Read.m
end
module Defaults(S : Pickle_min) : Pickle with type a = S.a

module Pickle_unit  : sig val make: (module Pickle with type a = unit) end
module Pickle_bool  : sig val make: (module Pickle with type a = bool) end
module Pickle_int   : sig val make: (module Pickle with type a = int) end
module Pickle_char  : sig val make: (module Pickle with type a = char) end
module Pickle_float : sig val make: (module Pickle with type a = float) end
(* module Pickle_num   : Pickle with type a = Num.num *)
module Pickle_string : sig val make: (module Pickle with type a = string) end
module Pickle_option : sig
  val make: (module Pickle with type a = 'a) -> (module Pickle with type a = 'a option)
end
module Pickle_list   : sig
  val make: (module Pickle with type a = 'a) -> (module Pickle with type a = 'a list)
end
module Pickle_ref    : sig
  val make: (module Pickle with type a = 'a) -> (module Pickle with type a = 'a ref)
end

module Pickle_from_dump : sig
  val make :
      (module Deriving_Dump.Dump with type a = 'a) ->
        (module Deriving_Eq.Eq with type a = 'a) ->
          (module Deriving_Typeable.Typeable with type a = 'a) ->
            (module Pickle with type a = 'a)
end

