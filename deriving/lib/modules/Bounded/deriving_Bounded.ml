(* Copyright Jeremy Yallop 2007.
   Copyright Grégoire Henry 2010.
   This file is free software, distributed under the MIT license.
   See the file COPYING for details.
*)

(** Primitive instances for Bounded **)

module type Bounded = sig
  type a
  val min_bound : a
  val max_bound : a
end

module Bounded_integer(B : sig type t
                               val max_int : t
                               val min_int : t
                       end) = struct
  let make = (module (struct
    type a = B.t
    let min_bound = B.min_int
    let max_bound = B.max_int
  end) : Bounded with type a = B.t)
end

module Bounded_int32 = Bounded_integer(Int32)
module Bounded_int64 = Bounded_integer(Int64)
module Bounded_nativeint = Bounded_integer(Nativeint)
module Bounded_int =  struct
  let make = (module (struct
    type a = int
    let min_bound = Pervasives.min_int
    let max_bound = Pervasives.max_int
  end) : Bounded with type a = int)
end
module Bounded_bool =  struct
  let make = (module (struct
    type a = bool
    let min_bound = false
    let max_bound = true
  end) : Bounded with type a = bool)
end
module Bounded_char =  struct
  let make = (module (struct
    type a = char
    let min_bound = Char.chr 0
    let max_bound = Char.chr 0xff (* Is this guaranteed? Yes it is. *)
  end) : Bounded with type a = char)
end
module Bounded_unit = struct
  let make = (module (struct
    type a = unit
    let min_bound = ()
    let max_bound = ()
  end) : Bounded with type a = unit)
end

(* module Bounded_open_flag = struct *)
  (* let make = (module struct *)
    (* type a = Pervasives.open_flag *)
    (* let min_bound = Open_rdonly *)
    (* let max_bound = Open_nonblock *)
  (* end : Bounded with type a = open_flag) *)
(* end *)

(* module Bounded_fpclass = struct *)
  (* let make = (module struct *)
    (* type a = Pervasives.fpclass *)
    (* let min_bound = FP_normal *)
    (* let max_bound = FP_nan *)
  (* end : Bounded with type a = fpclass) *)
(* end *)
