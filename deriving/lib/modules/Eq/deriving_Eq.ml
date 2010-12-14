(* Copyright Jeremy Yallop 2007.
   Copyright Grégoire Henry 2010.
   This file is free software, distributed under the MIT license.
   See the file COPYING for details.
*)

(** Primitive instances for Eq **)
module type Eq =
sig
  type a
  val eq : a -> a -> bool
end

module Eq_immutable(S : sig type a end) = struct
  let make = (module (struct
    type a = S.a
    let eq = (=)
  end) : Eq with type a = S.a)
end

module Eq_mutable(S : sig type a end) = struct
  let make = (module (struct
    type a = S.a
    let eq = (==)
  end) : Eq with type a = S.a)
end

module Eq_unit = Eq_immutable(struct type a = unit end)
module Eq_bool = Eq_immutable(struct type a = bool end)
module Eq_char = Eq_immutable(struct type a = char end)
module Eq_int = Eq_immutable(struct type a = int end)
module Eq_int32 = Eq_immutable(struct type a = int32 end)
module Eq_int64 = Eq_immutable(struct type a = int64 end)
module Eq_nativeintint = Eq_immutable(struct type a = nativeint end)
module Eq_float = Eq_immutable(struct type a = float end)
(* module Eq_num : Eq with type a = Num.num = *)
  (* struct type a = float let eq = Num.eq_num end *)

module Eq_option = struct
  let make (type t_a) (m_a: (module Eq with type a = t_a)) =
    (module (struct
      type a = t_a option
      let eq l r = match l, r with
      | None, None -> true
      | Some l, Some r -> (let module E = (val m_a : Eq with type a = t_a) in E.eq) l r
      | _ -> false
    end) :  Eq with type a = t_a option)
end

module Eq_list = struct
  let make (type t_a) (m_a: (module Eq with type a = t_a)) =
    (module (struct
      type a = t_a list
      let rec eq l r = match l, r with
      | [], [] -> true
      | (lfst::lrst), (rfst::rrst)
	when (let module E = (val m_a : Eq with type a = t_a) in E.eq) lfst rfst ->
	  eq lrst rrst
      | _ -> false
    end) :  Eq with type a = t_a list)
end

(* module Eq_map_s_t (E : Eq) (M : Map.S) : Eq with type a = E.a M.t = *)
(* struct *)
  (* type a = E.a M.t *)
  (* let eq = M.equal (E.eq) *)
(* end *)

module Eq_string = Eq_mutable(struct type a = string end)
module Eq_ref = struct
  let make (type t_a) (m_a: (module Eq with type a = t_a)) =
    (module (struct
      type a = t_a ref
      let eq = (==)
    end) :  Eq with type a = t_a ref)
end
module Eq_array = struct
  let make (type t_a) (m_a: (module Eq with type a = t_a)) =
    (module (struct
      type a = t_a array
      let eq = (==)
    end) :  Eq with type a = t_a array)
end
