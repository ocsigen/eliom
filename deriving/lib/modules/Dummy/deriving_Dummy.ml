(* Copyright Jeremy Yallop 2007.
   Copyright Grégoire Henry 2010.
   This file is free software, distributed under the MIT license.
   See the file COPYING for details.
*)

module type Dummy = sig
  type a
  val test : unit -> unit
end

module Dummy_predefs (T : sig type a end) : Dummy  with type a = T.a = (struct
    type a = T.a
    let test () = print_endline "Dummy predefs"
  end)

module Dummy_unit = struct
  let make =
    (module Dummy_predefs(struct type a = unit end)
	: Dummy with type a = unit)
end
module Dummy_char = struct
  let make =
    (module Dummy_predefs(struct type a = char end)
	: Dummy with type a = char)
end
module Dummy_bool = struct
  let make =
    (module Dummy_predefs(struct type a = bool end)
	: Dummy with type a = bool)
end
module Dummy_int = struct
  let make =
    (module Dummy_predefs(struct type a = int end)
	: Dummy with type a = int)
end
module Dummy_int32 = struct
  let make =
    (module Dummy_predefs(struct type a = int32 end)
	: Dummy with type a = int32)
end
module Dummy_int64 = struct
  let make =
    (module Dummy_predefs(struct type a = int64 end)
	: Dummy with type a = int64)
end
module Dummy_nativeint = struct
  let make =
    (module Dummy_predefs(struct type a = nativeint end)
	: Dummy with type a = nativeint)
end
module Dummy_float = struct
  let make =
    (module Dummy_predefs(struct type a = float end)
	: Dummy with type a = float)
end
(* module Dummy_num = struct *)
  (* let make = *)
    (* (module Dummy_predefs(struct type a = Num.num end) *)
	(* : Dummy with type a = Num.num) *)
(* end *)
module Dummy_string = struct
  let make =
    (module Dummy_predefs(struct type a = string end)
	: Dummy with type a = string)
end

module Dummy_list = struct
  let make (type t_a) (m_a: (module Dummy with type a = t_a)) =
    (module Dummy_predefs(struct type a = t_a list end)
	: Dummy with type a = t_a list)
end
module Dummy_ref = struct
  let make (type t_a) (m_a: (module Dummy with type a = t_a)) =
    (module Dummy_predefs(struct type a = t_a ref end)
	: Dummy with type a = t_a ref)
end
module Dummy_option = struct
  let make (type t_a) (m_a: (module Dummy with type a = t_a)) =
    (module Dummy_predefs(struct type a = t_a option end)
	: Dummy with type a = t_a option)
end
module Dummy_array = struct
  let make (type t_a) (m_a: (module Dummy with type a = t_a)) =
    (module Dummy_predefs(struct type a = t_a array end)
	: Dummy with type a = t_a array)
end
