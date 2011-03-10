(* $Id: xML.ml,v 1.14 2004/12/13 14:57:45 ohl Exp $

   Copyright (C) 2004 by Thorsten Ohl <ohl@physik.uni-wuerzburg.de>

   XHTML is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   XHTML is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  *)


type separator = Space | Comma

let separator_to_string = function
  | Space -> " "
  | Comma -> ", "

type aname = string
type attrib =
  | AFloat of aname * float (* Cecile *)
  | AInt of aname * int
  | AStr of aname * string
  | AStrL of separator * aname * string list
type attribs = attrib list

let float_attrib name value = AFloat (name, value) (* Cecile *)
let int_attrib name value = AInt (name, value)
let string_attrib name value = AStr (name, value)
let space_sep_attrib name values = AStrL (Space, name, values)
let comma_sep_attrib name values = AStrL (Comma, name, values)
let event_attrib name value = AStr (name, value)

let attrib_value_to_string encode = function
  | AFloat (_, f) -> Printf.sprintf "\"%e\"" f (* Cecile *)
  | AInt (_, i) -> Printf.sprintf "\"%d\"" i
  | AStr (_, s) -> Printf.sprintf "\"%s\"" (encode s)
  | AStrL (sep, _, slist) ->
      Printf.sprintf "\"%s\""
        (encode (String.concat (separator_to_string sep) slist))

let attrib_name = function
    | AFloat (n, _) (* Cecile *) | AInt (n, _) | AStr (n, _) | AStrL (_, n, _) -> n


let attrib_to_string encode a =
  Printf.sprintf "%s=%s" (attrib_name a) (attrib_value_to_string encode a)

type event = string

type ename = string
type elt_content =
  | Empty
  | Comment of string
  | EncodedPCDATA of string
  | PCDATA of string
  | Entity of string
  | Leaf of ename * attrib list
  | Node of ename * attrib list * elt list
and elt = {
  mutable ref : int ;
  elt : elt_content ;
}

let amap1 f n = {
  ref = 0;
  elt =
    match n.elt with
      | Empty | Comment _ | PCDATA _ | Entity _ as elt -> elt
      | Leaf (name, attribs) -> Leaf (name, f name attribs)
      | Node (name, attribs, elts) -> Node (name, f name attribs, elts)
      | _ -> failwith "not implemented for Ocsigen syntax extension"
}

let rec amap f n = {
  ref = 0;
  elt =
    match n.elt with
      | Empty | Comment _ | PCDATA _ | Entity _ as elt -> elt
      | Leaf (name, attribs) -> Leaf (name, f name attribs)
      | Node (name, attribs, elts) -> Node (name, f name attribs, List.map (amap f) elts)
      | _ -> failwith "not implemented for Ocsigen syntax extension"
}

  (* Cecile *)
let rec add_float_attrib name value = function
  | [] -> [AFloat (name, value)]
  | AFloat (name', _) :: tail when name' = name -> AFloat (name, value) :: tail
  | head :: tail -> head :: add_float_attrib name value tail

let rec map_float_attrib is_attrib f = function
  | [] -> []
  | AFloat (name, value) :: tail when is_attrib name ->
      AFloat (name, f value) :: map_float_attrib is_attrib f tail
  | head :: tail -> head :: map_float_attrib is_attrib f tail
  (* Cecile *)


let rec add_int_attrib name value = function
  | [] -> [AInt (name, value)]
  | AInt (name', _) :: tail when name' = name ->
      AInt (name, value) :: tail
  | head :: tail -> head :: add_int_attrib name value tail

let rec rm_attrib is_attrib = function
  | [] -> []
  | (AFloat (name, _) (* Cecile *) | AInt (name, _) | AStr (name, _) | AStrL (_, name, _)) :: tail
    when is_attrib name -> rm_attrib is_attrib tail
  | head :: tail -> head :: rm_attrib is_attrib tail

let rec map_int_attrib is_attrib f = function
  | [] -> []
  | AInt (name, value) :: tail when is_attrib name ->
      AInt (name, f value) :: map_int_attrib is_attrib f tail
  | head :: tail -> head :: map_int_attrib is_attrib f tail

let rec add_string_attrib name value = function
  | [] -> [AStr (name, value)]
  | AStr (name', _) :: tail when name' = name -> AStr (name, value) :: tail
  | head :: tail -> head :: add_string_attrib name value tail

let rec map_string_attrib is_attrib f = function
  | [] -> []
  | AStr (name, value) :: tail when is_attrib name ->
      AStr (name, f value) :: map_string_attrib is_attrib f tail
  | head :: tail -> head :: map_string_attrib is_attrib f tail

let rec add_space_sep_attrib name value = function
  | [] -> [AStrL (Space, name, [value])]
  | AStrL (Space, name', values') :: tail when name' = name ->
      AStrL (Space, name, value :: values') :: tail
  | head :: tail -> head :: add_space_sep_attrib name value tail

let rec add_comma_sep_attrib name value = function
  | [] -> [AStrL (Comma, name, [value])]
  | AStrL (Comma, name', values') :: tail when name' = name ->
      AStrL (Comma, name, value :: values') :: tail
  | head :: tail -> head :: add_comma_sep_attrib name value tail

let rec rm_attrib_from_list is_attrib is_value = function
  | [] -> []
  | AStrL (sep, name, values) :: tail when is_attrib name ->
      begin match List.filter (fun v -> not (is_value v)) values with
      |        [] -> tail
      |        values' -> AStrL (sep, name, values') :: tail
      end
  | head :: tail -> head :: rm_attrib_from_list is_attrib is_value tail

let rec map_string_attrib_in_list is_attrib f = function
  | [] -> []
  | AStrL (sep, name, values) :: tail when is_attrib name ->
      AStrL (sep, name, List.map f values) :: map_string_attrib_in_list is_attrib f tail
  | head :: tail -> head :: map_string_attrib_in_list is_attrib f tail

let rec fold of_empty of_comment of_pcdata of_entity of_leaf of_node n =
  match n.elt with
    | Empty -> of_empty ()
    | Comment s -> of_comment s
    | PCDATA s -> of_pcdata s
    | Entity s -> of_entity s
    | Leaf (name, attribs) -> of_leaf name attribs
    | Node (name, attribs, elts) ->
	of_node name attribs
          (List.map (fold of_empty of_comment of_pcdata of_entity of_leaf of_node) elts)
    | _ -> failwith "not implemented for Ocsigen syntax extension"
	
(* (* is this AT ALL useful??? *)
let rec foldx of_empty of_comment of_pcdata of_entity of_leaf of_node update_state state = function
  | Empty -> of_empty ()
  | Comment s -> of_comment s
  | PCDATA s -> of_pcdata s
  | Entity s -> of_entity s
  | Leaf (name, attribs) -> of_leaf state name attribs
  | Node (name, attribs, elts) ->
      of_node state name attribs
        (List.map (foldx of_empty of_comment of_pcdata of_entity of_leaf of_node
                     update_state (update_state name attribs state)) elts)
*)

let all_attribs access ?(is_elt = fun ename -> true) aname elt =
  let access' ename attribs =
    if is_elt ename then
      try [access aname attribs] with Not_found -> []
    else
      [] in
  fold (fun () -> []) (fun c -> []) (fun p -> []) (fun e -> []) access'
    (fun ename attribs elts -> access' ename attribs @ List.flatten elts)
    elt


let all_entities elt =
  fold (fun () -> []) (fun c -> []) (fun p -> []) (fun e -> [e])
    (fun ename attribs -> []) (fun ename attribs elts -> List.flatten elts)
    elt

let empty () = { elt = Empty ; ref = 0 }

let comment c = { elt = Comment c ; ref = 0 }

let pcdata d = { elt = PCDATA d ; ref = 0 }
let encodedpcdata d = { elt = EncodedPCDATA d ; ref = 0 }
let entity e = { elt = Entity e ; ref = 0 }

let cdata s = (* GK *)
  (* For security reasons, we do not allow "]]>" inside CDATA
     (as this string is to be considered as the end of the cdata)
  *)
  let s' = "\n<![CDATA[\n"^
    (Netstring_pcre.global_replace
       (Netstring_pcre.regexp_string "]]>") "" s)
    ^"\n]]>\n" in
  encodedpcdata s'

let cdata_script s = (* GK *)
  (* For security reasons, we do not allow "]]>" inside CDATA
     (as this string is to be considered as the end of the cdata)
  *)
  let s' = "\n//<![CDATA[\n"^
    (Netstring_pcre.global_replace
       (Netstring_pcre.regexp_string "]]>") "" s)
    ^"\n//]]>\n" in
  encodedpcdata s'

let cdata_style s = (* GK *)
  (* For security reasons, we do not allow "]]>" inside CDATA
     (as this string is to be considered as the end of the cdata)
  *)
  let s' = "\n/* <![CDATA[ */\n"^
    (Netstring_pcre.global_replace
       (Netstring_pcre.regexp_string "]]>") "" s)
    ^"\n/* ]]> */\n" in
  encodedpcdata s'



let leaf ?a name =
  { elt =
      (match a with
	 | Some a -> Leaf (name, a)
	 | None -> Leaf (name, [])) ;
    ref = 0 }

let node ?a name children =
  { elt =
      (match a with
	 | Some a -> Node (name, a, children)
	 | None -> Node (name, [], children)) ;
    ref = 0 }

let rec flatmap f = function
  | [] -> []
  | x :: rest -> f x @ flatmap f rest

let translate root_leaf root_node sub_leaf sub_node update_state state n =
  let rec translate' state  n =
    match n.elt with
      | (Empty | Comment _ | PCDATA _ | Entity _) -> [n]
      | Leaf (name, attribs) ->
          sub_leaf state name attribs
      | Node (name, attribs, elts) ->
          sub_node state name attribs
            (flatmap (translate' (update_state name attribs state)) elts)
      | _ -> failwith "not implemented for Ocsigen syntax extension"
  in
    match n.elt with
      | (Empty | Comment _ | PCDATA _ | Entity _) -> n
      | Leaf (name, attribs) ->
	  root_leaf name attribs
      | Node (name, attribs, elts) ->
	  root_node name attribs (flatmap (translate' state) elts)
      | _ -> failwith "not implemented for Ocsigen syntax extension"
	  
(** {1 Output} *)

module Elt_Set =
  Set.Make (struct type t = ename let compare = compare end)

let elt_set_of_list names =
  List.fold_right
    (fun n set -> Elt_Set.add (String.lowercase n) set) names Elt_Set.empty

type io_state =
    { preformatted : bool;
      preformatted_elts : Elt_Set.t;
      allow_break : bool;
      no_break_elts : Elt_Set.t }

let initial_io_state ?(preformatted = []) ?(no_break = []) () =
  let preformatted = elt_set_of_list preformatted
  and no_break = elt_set_of_list no_break in
  { preformatted = false;
    preformatted_elts = preformatted;
    allow_break = true;
    no_break_elts = Elt_Set.union no_break preformatted }

let update_io_state name attribs ios =
  { ios with
    allow_break = not (Elt_Set.mem (String.lowercase name) ios.no_break_elts);
    preformatted = Elt_Set.mem (String.lowercase name) ios.preformatted_elts }

(** {2 No Pretty Printing} *)

let is_control c =
  let cc = Char.code c in
  (cc <= 8 || cc = 11 || cc = 12 || (14 <= cc && cc <= 31) || cc = 127)

let encode_unsafe s =
  let b = Buffer.create (String.length s) in
  String.iter (function
    | '<' -> Buffer.add_string b "&lt;"
    | '>' -> Buffer.add_string b "&gt;"
    | '"' -> Buffer.add_string b "&quot;"
    | '&' -> Buffer.add_string b "&amp;"
    | c when is_control c ->
        Buffer.add_string b ("&#" ^ string_of_int (Char.code c) ^ ";")
    | c -> Buffer.add_char b c) s;
  Buffer.contents b

let encode_unsafe_and_at s =
  let b = Buffer.create (String.length s) in
  String.iter (function
    | '<' -> Buffer.add_string b "&lt;"
    | '>' -> Buffer.add_string b "&gt;"
    | '"' -> Buffer.add_string b "&quot;"
    | '&' -> Buffer.add_string b "&amp;"
    | '@' -> Buffer.add_string b "&#64;"
    | c when is_control c ->
        Buffer.add_string b ("&#" ^ string_of_int (Char.code c) ^ ";")
    | c -> Buffer.add_char b c) s;
  Buffer.contents b

let newline ios outs =
  if ios.allow_break then
    outs "\n"

let rec output' ios encode outs  n =
  match n.elt with
    | Empty -> ()
    | Comment c ->
	outs ("<!-- " ^ encode c ^ " -->");
	newline ios outs
    | PCDATA d ->
	outs (encode d);
	newline ios outs
    | Entity e ->
	outs ("&" ^ e ^ ";");  (* No {e not} encode these! *)
	newline ios outs
    | Leaf (name, attribs) ->
	outs ("<" ^ name);
	List.iter (fun a -> outs " "; outs (attrib_to_string encode a)) attribs;
	outs " />";
	newline ios outs
    | Node (name, attribs, children) ->
	let ios_elt = update_io_state name attribs ios in
	  outs ("<" ^ name);
	  List.iter (fun a -> outs " "; outs (attrib_to_string encode a)) attribs;
	  outs ">";
	  newline ios_elt outs;
	  List.iter (output' ios_elt encode outs) children;
	  outs ("</" ^ name ^ ">");
	  newline ios outs
    | _ -> failwith "not implemented for Ocsigen syntax extension"
	
let output ?preformatted ?no_break ?(encode = encode_unsafe) outs elt =
  output' (initial_io_state ?preformatted ?no_break ()) encode outs elt

let rec output_compact' encode outs  n =
  match n.elt with
    | Empty -> ()
    | Comment c ->
	outs ("<!-- " ^ encode c ^ " -->");
    | PCDATA d ->
	outs (encode d);
    | Entity e ->
	outs ("&" ^ e ^ ";");  (* No {e not} encode these! *)
    | Leaf (name, attribs) ->
	outs ("<" ^ name);
	List.iter (fun a -> outs " "; outs (attrib_to_string encode a)) attribs;
	outs " />";
    | Node (name, attribs, children) ->
	  outs ("<" ^ name);
	  List.iter (fun a -> outs " "; outs (attrib_to_string encode a)) attribs;
	  outs ">";
	  List.iter (output_compact' encode outs) children;
	  outs ("</" ^ name ^ ">");
    | _ -> failwith "not implemented for Ocsigen syntax extension"
	
let output_compact ?(encode = encode_unsafe) outs elt =
  output_compact' encode outs elt
(** {2 Pretty Printed} *)

let force_newline ios f () =
  if ios.allow_break then
    Format.pp_force_newline f ()

let print_cut ios f () =
  if ios.allow_break then
    Format.pp_print_cut f ()

let open_box ios f n =
  if ios.allow_break then
    Format.pp_open_box f n

let close_box ios f () =
  if ios.allow_break then
    Format.pp_close_box f ()

let is_white = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false

let iter_words fword fwhite s =
  let last = String.length s - 1 in
  let rec skip_non_white c =
    if c > last then
      c
    else if is_white s.[c] then
      c
    else
      skip_non_white (succ c) in
  let rec skip_white c =
    if c > last then
      c
    else if is_white s.[c] then
      skip_white (succ c)
    else
      c in
  let rec iter_words' c =
    if c > last then begin
      ()
    end else if is_white s.[c] then begin
      fwhite ();
      iter_words' (skip_white (succ c))
    end else begin
      let c' = skip_non_white (succ c) in
      fword (String.sub s c (c' - c));
      iter_words' c'
    end in
  iter_words' 0

let print_string ios f s =
  if ios.preformatted then
    Format.pp_print_string f s
  else
    iter_words (Format.pp_print_string f) (Format.pp_print_space f) s

let print_space ios f () =
  Format.pp_print_space f ()

let rec to_formatter ios encode f n =
  match n.elt with
  | Empty -> ()
  | Comment c ->
      force_newline ios f ();
      print_string ios f ("<!-- " ^ encode c ^ " -->");
      force_newline ios f ()
  | PCDATA d ->
      print_string ios f (encode d);
      print_cut ios f ()
  | Entity e ->
      print_string ios f ("&" ^ e ^ ";");  (* NO encoding! *)
      print_cut ios f ()
  | Leaf (name, attribs) ->
      print_cut ios f ();
      open_box ios f 4;
      print_string ios f ("<" ^ name);
      List.iter (fun a ->
        print_space ios f ();
        Format.pp_print_string f (attrib_to_string encode a)) attribs;
      print_string ios f " />";
      close_box ios f ();
      print_cut ios f ()
  | Node (name, attribs, children) ->
      print_cut ios f ();
      let ios_elt = update_io_state name attribs ios in
      open_box ios f 2;
      open_box ios f 4;
      print_string ios f ("<" ^ name);
      List.iter (fun a ->
        print_space ios f ();
        Format.pp_print_string f (attrib_to_string encode a)) attribs;
      print_string ios f ">";
      close_box ios f ();
      print_cut ios_elt f ();
      List.iter (to_formatter ios_elt encode f) children;
      close_box ios f ();
      print_cut ios_elt f ();
      print_string ios f ("</" ^ name ^ ">");
      print_cut ios f ()
  | _ -> failwith "Do not use this pretty printer with Ocsigen"

let pretty_print ?(width = 132) ?preformatted ?no_break
    ?(encode = encode_unsafe) outs element =
  Format.pp_set_margin Format.str_formatter width;
  to_formatter (initial_io_state ?preformatted ?no_break ())
    encode Format.str_formatter element;
  outs (Format.flush_str_formatter ())

let decl ?(version = "1.0") ?(encoding = "ISO-8859-1") outs () =
  outs ("<?xml version=\"" ^ version ^ "\" encoding=\"" ^ encoding ^ "\"?>\n")


let fresh_ref, next_ref = 
  let v = ref 0 in 
  ((fun () -> incr v ; !v),
   (fun () -> !v + 1))

let ref_node node =
  if node.ref = 0 then
    node.ref <- fresh_ref () ;
  node.ref

type ref_tree = Ref_tree of int option * (int * ref_tree) list


let rec make_ref_tree_list l =
  let rec map i = function
    | e :: es ->
	begin match make_ref_tree e with
	  | Ref_tree (None, []) -> map (succ i) es
	  | v -> (i, v) :: map (succ i) es
	end
    | [] -> []
  in map 0 l
and make_ref_tree root =
  let children =
    match root.elt with
        Node (n_, _, elts) ->
          make_ref_tree_list elts
      | Empty | EncodedPCDATA _ | PCDATA _ | Entity _
      | Leaf (_, _) | Comment _  ->
	  []
  in
  Ref_tree ((if root.ref = 0 then None else Some root.ref), children)


	
let register_event ?keep_default elt name f v =
  failwith "not implemented server side"

let class_name = "class" (* see xHTML.ml *)
