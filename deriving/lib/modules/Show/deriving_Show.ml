(* Copyright Jeremy Yallop 2007.
   Copyright Grégoire Henry 2010.
   This file is free software, distributed under the MIT license.
   See the file COPYING for details.
*)

(** Primitive instances for Show **)
module type Show_min = sig
  type a
  val format : Format.formatter -> a -> unit
end

module type Show = sig
  type a
  val format : Format.formatter -> a -> unit
  val format_list : Format.formatter -> a list -> unit
  val show : a -> string
  val show_list : a list -> string
end

module type SimpleFormatter =
sig
  type a
  val format : Format.formatter -> a -> unit
end

module ShowFormatterDefault (S : SimpleFormatter) =
struct
  include S
  let format_list formatter items =
    let rec writeItems formatter = function
      | []      -> ()
      | [x]     -> S.format formatter x;
      | x :: xs -> Format.fprintf formatter "%a;@;%a" S.format x writeItems xs
    in
      Format.fprintf formatter "@[<hov 1>[%a]@]" writeItems items
end

module ShowDefaults'
  (S : (sig
          type a
          val format : Format.formatter -> a -> unit
          val format_list : Format.formatter -> a list -> unit
        end)) : Show with type a = S.a =
struct
  include S
  let showFormatted f item =
    let b = Buffer.create 16 in
    let formatter = Format.formatter_of_buffer b in
      Format.fprintf formatter "@[<hov 0>%a@]@?" f item;
      Buffer.sub b 0 (Buffer.length b)

  (* Warning: do not eta-reduce either of the following *)
  let show item = showFormatted S.format item
  let show_list items = showFormatted S.format_list items
end

module Defaults (S : SimpleFormatter) : Show with type a = S.a =
  ShowDefaults' (ShowFormatterDefault (S))

module Show_unprintable (S : sig type a end) = struct
  let make =
    (module (Defaults (struct
      type a = S.a
      let format formatter _ = Format.pp_print_string formatter "..."
    end)) : Show with type a = S.a)
end

(* instance Show a => Show [a] *)
module Show_list = struct
  let make (type t_a) (m_a : (module Show with type a = t_a)) =
    (module (Defaults (struct
      type a = t_a list
      let format =
	let module M = (val m_a : Show with type a = t_a) in M.format_list
    end)) : Show with type a = t_a list)
end

(* instance Show a => Show (a option) *)
module Show_option = struct
  let make (type t_a) (m_a : (module Show with type a = t_a)) =
    (module (Defaults (struct
      type a = t_a option
      let format formatter = function
	| None   -> Format.fprintf formatter "@[None@]"
	| Some s -> Format.fprintf formatter "@[Some@;<1 2>%a@]"
	      (let module M = (val m_a : Show with type a = t_a) in M.format) s
    end)) : Show with type a = t_a option)
end

(* instance Show a => Show (a array) *)
module Show_array = struct
  let make (type t_a) (m_a : (module Show with type a = t_a)) =
    (module (Defaults (struct
              type a = t_a array
              let format formatter obj =
                let writeItems formatter items =
                  let length = Array.length items in
                    for i = 0 to length - 2 do
                      Format.fprintf formatter "@[%a;@;@]"
			(let module M = (val m_a : Show with type a = t_a) in M.format)
			(Array.get items i)
                    done;
                    if length <> 0 then
		      (let module M = (val m_a : Show with type a = t_a) in
		       M.format) formatter (Array.get items (length -1));
                in
                  Format.fprintf formatter "@[[|%a|]@]" writeItems obj
            end)) : Show with type a = t_a array)
end

module Show_bool = struct
  let make = (module (Defaults (struct
    type a = bool
    let format formatter item =
      match item with
      | true  -> Format.pp_print_string formatter "true"
      | false -> Format.pp_print_string formatter "false"
  end)) : Show with type a = bool)
end

module Show_integer (S : sig type t val to_string : t -> string end) = struct
  let make = (module (Defaults (struct
    type a = S.t
    let format formatter item = Format.pp_print_string formatter (S.to_string item)
  end)) : Show with type a = S.t)
end

module Show_int32 = Show_integer(Int32)
module Show_int64 = Show_integer(Int64)
module Show_nativeint = Show_integer(Nativeint)

module Show_char = struct
  let make = (module (Defaults (struct
    type a = char
    let format formatter item =
      Format.pp_print_string formatter ("'" ^ Char.escaped item ^ "'")
  end)) : Show with type a = char)
end

module Show_int = struct
  let make = (module (Defaults (struct
    type a = int
    let format formatter item = Format.pp_print_string formatter (string_of_int item)
  end)) : Show with type a = int)
end

(* module Show_num = Defaults (struct *)
  (* type a = Num.num *)
  (* let format formatter item = Format.pp_print_string formatter (Num.string_of_num item) *)
(* end) *)

module Show_float = struct
  let make = (module (Defaults(struct
    type a = float
    let format formatter item = Format.pp_print_string formatter (string_of_float item)
  end)) : Show with type a = float)
end

module Show_string = struct
  let make = (module (Defaults (struct
    type a = string
    let format formatter item =
      Format.pp_print_char formatter '"';
      Format.pp_print_string formatter (String.escaped item);
      Format.pp_print_char formatter '"'
  end)) : Show with type a = string)
end

module Show_unit = struct
  let make = (module (Defaults(struct
    type a = unit
    let format formatter () = Format.pp_print_string formatter "()"
  end)) : Show with type a = unit)
end

(* module Show_open_flag = struct *)
  (* let make = (module Defaults(struct *)
    (* type a = open_flag *)

    (* let format formatter = function *)
      (* | Open_rdonly -> *)
          (* Format.pp_print_string formatter "Open_rdonly" *)
      (* | Open_wronly -> *)
          (* Format.pp_print_string formatter "Open_wronly" *)
      (* | Open_append -> *)
          (* Format.pp_print_string formatter "Open_append" *)
      (* | Open_creat -> *)
          (* Format.pp_print_string formatter "Open_creat" *)
      (* | Open_trunc -> *)
          (* Format.pp_print_string formatter "Open_trunc" *)
      (* | Open_excl -> *)
          (* Format.pp_print_string formatter "Open_excl" *)
      (* | Open_binary -> *)
          (* Format.pp_print_string formatter "Open_binary" *)
      (* | Open_text -> *)
          (* Format.pp_print_string formatter "Open_text" *)
      (* | Open_nonblock -> *)
          (* Format.pp_print_string formatter "Open_nonblock" *)

  (* end) : Show with type a = open_flag) *)

(* end *)

(* module Show_fpclass = struct *)
  (* let make = (module Defaults(struct *)

    (* type a = fpclass *)

    (* let format formatter = function *)
      (* | FP_normal -> *)
          (* Format.pp_print_string formatter "FP_normal" *)
      (* | FP_subnormal -> *)
          (* Format.pp_print_string formatter "FP_subnormal" *)
      (* | FP_zero -> Format.pp_print_string formatter "FP_zero" *)
      (* | FP_infinite -> *)
          (* Format.pp_print_string formatter "FP_infinite" *)
      (* | FP_nan -> Format.pp_print_string formatter "FP_nan" *)

  (* end) : Show with type a = fpclass) *)

(* end *)

module Show_ref = struct
  let make (type tm_a) (m_a : (module Show with type a = tm_a)) =
    (module Defaults(struct
      type a = tm_a ref

      let format formatter { contents = contents } =
        (Format.pp_open_hovbox formatter 0;
         Format.pp_print_char formatter '{';
         Format.pp_print_string formatter "contents =";
         (let module M = (val m_a : Show with type a = tm_a)
          in M.format) formatter contents;
         Format.pp_print_char formatter '}';
         Format.pp_close_box formatter ())

    end) : Show with type a = tm_a ref)

end
