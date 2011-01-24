(* Copyright Grégoire Henry 2010.
   This file is free software, distributed under the MIT license.
   See the file COPYING for details.
*)

(** Json **)

type 'a t = {
    write: Buffer.t -> 'a -> unit;
    read: Deriving_Json_lexer.lexbuf -> 'a
  }

let to_string t v =
  let buf = Buffer.create 50 in
  t.write buf v;
  Buffer.contents buf

let to_channel t oc v =
  let buf = Buffer.create 50 in
  t.write buf v;
  Buffer.output_buffer oc buf

let from_string t s =
  t.read (Deriving_Json_lexer.init_lexer (Lexing.from_string s))

let from_channel t ic =
  t.read (Deriving_Json_lexer.init_lexer (Lexing.from_channel ic))

(** Deriver **)

module type Json_min = sig
  type a
  val write: Buffer.t -> a -> unit
  val read: Deriving_Json_lexer.lexbuf -> a
end

module type Json_min' = sig
  type a
  val write: Buffer.t -> a -> unit
  val read: Deriving_Json_lexer.lexbuf -> a
  val match_variant: [`Cst of int | `NCst of int] -> bool
  val read_variant: Deriving_Json_lexer.lexbuf -> [`Cst of int | `NCst of int] -> a
end

module type Json = sig
  type a
  val t: a t
  val write: Buffer.t -> a -> unit
  val read: Deriving_Json_lexer.lexbuf -> a
  val to_string: a -> string
  (* val to_channel: out_channel -> a -> unit *)
  val from_string: string -> a
  (* val from_channel: in_channel -> a *)
  val match_variant: [`Cst of int | `NCst of int] -> bool
  val read_variant: Deriving_Json_lexer.lexbuf -> [`Cst of int | `NCst of int] -> a
end

module Defaults(J : Json_min) : Json with type a = J.a = struct
  include J
  let t = { write; read }
  let to_string v = to_string t v
  (* let to_channel oc v = to_channel t oc v *)
  let from_string s = from_string t s
  (* let from_channel ic = from_channel t ic *)
  let match_variant hash = assert false
  let read_variant buf hash = assert false
end

module Defaults'(J : Json_min') : Json with type a = J.a = struct
  include J
  let t = { write; read }
  let to_string v = to_string t v
  (* let to_channel oc v = to_channel t oc v *)
  let from_string s = from_string t s
  (* let from_channel ic = from_channel t ic *)
end

let wrap (type t) t =
  let module M =
      struct
	type a = t
	let write = t.write
	let read = t.read
      end
  in
  (module Defaults(M) : Json with type a = t)

(** Predefs *)

module Json_undef (T : sig type a end) = struct
  let make = (module Defaults(struct
    type a = T.a
    let write buf _ = failwith "Unimplemented"
    let read buf = failwith "Unimplemented"
  end) : Json with type a = T.a)
end

module Json_char = struct
  let make = (module Defaults(struct
    type a = char
    let write buffer c =
      Buffer.add_string buffer (string_of_int (int_of_char c))
    let read buf = char_of_int (Deriving_Json_lexer.read_bounded_int ~max:255 buf)
  end) : Json with type a = char)
end

module Json_bool = struct
  let make = (module  Defaults(struct
  type a = bool
  let write buffer b =
    Buffer.add_char buffer (if b then '1' else '0')
  let read buf = 1 = Deriving_Json_lexer.read_bounded_int ~max:1 buf
end) : Json with type a = bool)
end
module Json_unit = struct
  let make = (module Defaults(struct
  type a = unit
  let write buffer () = Buffer.add_char buffer '0'
  let read buf = ignore(Deriving_Json_lexer.read_bounded_int ~max:0buf)
end) : Json with type a = unit)
end
module Json_int = struct
  let make = (module  Defaults(struct
    type a = int
    let write buffer i = Format.bprintf buffer "%d" i
    let read buf = Deriving_Json_lexer.read_int buf
end) : Json with type a = int)
end
module Json_int32 = struct
  let make = (module  Defaults(struct
    type a = int32
    let write buffer i = Format.bprintf buffer "%ld" i
    let read buf = Deriving_Json_lexer.read_int32 buf
end) : Json with type a = int32)
end
module Json_int64 = struct
  let make = (module  Defaults(struct
  type a = int64
  let mask24 = Int64.of_int 0xffffff
  let mask16 = Int64.of_int 0xffff
  let write buffer i =
    Printf.bprintf buffer "[255,%Ld,%Ld,%Ld]"
      (Int64.logand i mask24)
      (Int64.logand (Int64.shift_right i 24) mask24)
      (Int64.logand (Int64.shift_right i 48) mask16)
  let read buf =
    Deriving_Json_lexer.read_lbracket buf;
    Deriving_Json_lexer.read_bounded_int buf ~min:255 ~max:255;
    Deriving_Json_lexer.read_comma buf;
    let h1 = Deriving_Json_lexer.read_int64 buf in
    Deriving_Json_lexer.read_comma buf;
    let h2 = Int64.shift_left (Deriving_Json_lexer.read_int64 buf) 24 in
    Deriving_Json_lexer.read_comma buf;
    let h3 = Int64.shift_left (Deriving_Json_lexer.read_int64 buf) 48 in
    Deriving_Json_lexer.read_rbracket buf;
    Int64.logor h3 (Int64.logor h2 h1)
end) : Json with type a = int64)
end

module Json_nativeint = Json_undef(struct type a = nativeint end)
(* module Json_num = Json_undef(struct type a = Num.num end) *)
module Json_float = struct
  let make = (module  Defaults(struct
    type a = float
    let write buffer f = Printf.bprintf buffer "%e" f
    let read buf = Deriving_Json_lexer.read_number buf
end) : Json with type a = float)
end
module Json_string = struct
  let make = (module  Defaults(struct
  type a = string
  let write buffer s =
    Buffer.add_char buffer '\"';
    for i = 0 to String.length s - 1 do
      match s.[i] with
      | '\"' -> Buffer.add_string buffer "\\\""
      | '\\' -> Buffer.add_string buffer "\\\\"
      | '\b' -> Buffer.add_string buffer "\\b"
      | '\x0C' -> Buffer.add_string buffer "\\f"
      | '\n' -> Buffer.add_string buffer "\\n"
      | '\r' -> Buffer.add_string buffer "\\r"
      | '\t' -> Buffer.add_string buffer "\\t"
      | c when c <= '\x1f' ->
	  Printf.bprintf buffer "\\u%04X" (int_of_char c)
      | c -> Buffer.add_char buffer s.[i]
    done;
    Buffer.add_char buffer '\"'
  let read buf = Deriving_Json_lexer.read_string buf
end) : Json with type a = string)
end

module Json_list = struct
  let make (type t_a) (m_a: (module Json with type a = t_a)) =
    (module Defaults(struct
      type a = t_a list
      let rec write buffer xs =
	match xs with
	| [] -> Buffer.add_char buffer '0'
	| x :: xs ->
	    Printf.bprintf buffer "[0,%a,%a]"
	      (let module M = (val m_a: Json with type a = t_a) in M.write) x
	      write xs
      let rec read buf =
	match Deriving_Json_lexer.read_case buf with
	| `Cst 0 -> []
	| `NCst 0 ->
	    Deriving_Json_lexer.read_comma buf;
	    let x =
	      (let module M = (val m_a: Json with type a = t_a)
	      in M.read) buf in
	    Deriving_Json_lexer.read_comma buf;
	    let xs = read buf in
	    Deriving_Json_lexer.read_rbracket buf;
	    x :: xs
	| _ -> failwith "Json_list.read: unexpected constructor."
    end) : Json with type a = t_a list)
end

module Json_ref = struct
  let make (type t_a) (m_a: (module Json with type a = t_a)) =
    (module Defaults(struct
      type a = t_a ref
      let rec write buffer r =
	Printf.bprintf buffer "[0,%a]"
	  (let module M = (val m_a: Json with type a = t_a) in M.write) !r
      let read buf =
	match Deriving_Json_lexer.read_case buf with
	| `NCst 0 ->
	    Deriving_Json_lexer.read_comma buf;
	    let x =
	      (let module M = (val m_a: Json with type a = t_a)
	      in M.read) buf in
	    Deriving_Json_lexer.read_rbracket buf;
	    ref x
	| _ -> failwith "Json_ref.read: unexpected constructor."
    end) : Json with type a = t_a ref)
end

module Json_option = struct
  let make (type t_a) (m_a: (module Json with type a = t_a)) =
    (module Defaults(struct
      type a = t_a option
      let rec write buffer o =
	match o with
	| None -> Buffer.add_char buffer '0'
	| Some x ->
	    Printf.bprintf buffer "[0,%a]"
	      (let module M = (val m_a: Json with type a = t_a) in M.write) x
      let read buf =
	match Deriving_Json_lexer.read_case buf with
	| `Cst 0 -> None
	| `NCst 0 ->
	    Deriving_Json_lexer.read_comma buf;
	    let x =
	      (let module M = (val m_a: Json with type a = t_a)
	      in M.read) buf in
	    Deriving_Json_lexer.read_rbracket buf;
	    Some x
	| _ -> failwith "Json_option.read: unexpected constructor."
    end) : Json with type a = t_a option)
end

module Json_array = struct
  let make (type t_a) (m_a: (module Json with type a = t_a)) =
    (module Defaults(struct
      type a = t_a array
      let write buffer a =
	Buffer.add_string buffer "[0";
	for i = 0 to Array.length a - 1 do
	  Buffer.add_char buffer ',';
	  (let module M = (val m_a: Json with type a = t_a)
	  in M.write) buffer a.(i);
	done;
	Buffer.add_char buffer ']'
      let rec read_list acc buf =
	match Deriving_Json_lexer.read_comma_or_rbracket buf with
	| `RBracket -> acc
	| `Comma ->
	    let x =
	      (let module M = (val m_a: Json with type a = t_a)
	      in M.read) buf in
	    read_list (x :: acc) buf
      let read buf =
	match Deriving_Json_lexer.read_case buf with
	| `NCst 0 -> Array.of_list (List.rev (read_list [] buf))
	| _ -> failwith "Json_array.read: unexpected constructor."
    end) : Json with type a = t_a array)
end
