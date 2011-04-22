
exception Eliom_Internal_Error of string

external id : 'a -> 'a = "%identity"

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)
let (!!) = Lazy.force

type yesnomaybe = Yes | No | Maybe
type ('a, 'b) leftright = Left of 'a | Right of 'b

let map_option f = function
  | None -> None
  | Some v -> Some (f v)

let fst3 (a, _, _) = a
let snd3 (_, a, _) = a
let thd3 (_, _, a) = a

module Filename = Ocsigen_pervasives.Filename
module Printexc = Ocsigen_pervasives.Printexc

(*****************************************************************************)

module Url = struct

  include Ocsigen_pervasives.Url

  let remove_internal_slash u =
    let rec aux = function
      | [] -> []
      | [a] -> [a]
      | ""::l -> aux l
      | a::l -> a::(aux l)
    in match u with
    | [] -> []
    | a::l -> a::(aux l)

  let change_empty_list = function
    | [] -> [""] (* It is not possible to register an empty URL *)
    | l -> l
  let make_encoded_parameters = Netencoding.Url.mk_url_encoded_parameters

  let encode = Ocsigen_pervasives.Url.encode
  let decode = Ocsigen_pervasives.Url.decode

end

(*****************************************************************************)

module String = struct

  include Ocsigen_pervasives.String

  (* Cut a string to the next separator *)
  let basic_sep char s =
    try
      let seppos = String.index s char in
      ((String.sub s 0 seppos),
       (String.sub s (seppos+1)
          ((String.length s) - seppos - 1)))
    with Invalid_argument _ -> raise Not_found

  (* returns the index of the first difference between s1 and s2,
     starting from n and ending at last.
     returns (last + 1) if no difference is found.
   *)
  let rec first_diff s1 s2 n last =
    try
      if s1.[n] = s2.[n]
      then
	if n = last
	then last+1
	else first_diff s1 s2 (n+1) last
      else n
    with Invalid_argument _ -> n

  let may_append s1 ~sep = function
    | "" -> s1
    | s2 -> s1^sep^s2

  let may_concat s1 ~sep s2 = match s1, s2 with
  | _, "" -> s1
  | "", _ -> s2
  | _ -> String.concat sep [s1;s2]

  let make_cryptographic_safe = Ocsigen_pervasives.String.make_cryptographic_safe

end

(*****************************************************************************)

module List = struct
  include Ocsigen_pervasives.List
  let rec remove_all_assoc a = function
    | [] -> []
    | (b, _)::l when a = b -> remove_all_assoc a l
    | b::l -> b::(remove_all_assoc a l)
  let rec remove_first_if_any a = function
    |  [] -> []
    | b::l when a = b -> l
    | b::l -> b::(remove_first_if_any a l)
  let rec remove_first_if_any_q a = function
    |  [] -> []
    | b::l when a == b -> l
    | b::l -> b::(remove_first_if_any_q a l)
end

(*****************************************************************************)

module Ip_address = struct

  include Ocsigen_pervasives.Ip_address

  let network_of_ip ip mask4 (mask61, mask62) = match ip with
  | IPv4 a -> IPv4 (Int32.logand a mask4)
  | IPv6 (a, b) -> IPv6 (Int64.logand a mask61, Int64.logand b mask62)

  let inet6_addr_loopback =
    fst (parse (Unix.string_of_inet_addr Unix.inet6_addr_loopback))

end

(*****************************************************************************)

module Int = struct

  module Table = Map.Make(struct
    type t = int
    let compare = compare
  end)

end

(*****************************************************************************)

let to_json ?typ v =
  match typ with
    | Some typ -> Deriving_Json.to_string typ v
    | None -> assert false (* implemented only client side *)

let of_json ?typ s =
  match typ with
    | Some typ -> Deriving_Json.from_string typ s
    | None -> assert false (* implemented only client side *)

module XML = struct

  module M = struct

  type separator = Space | Comma

  let separator_to_string = function
    | Space -> " "
    | Comma -> ", "

  type aname = string
  type acontent =
    | AFloat of aname * float (* Cecile *)
    | AInt of aname * int
    | AStr of aname * string
    | AStrL of separator * aname * string list
  type attrib = acontent
  let aname = function
    | (AFloat (name, _) | AInt (name, _) | AStr (name, _) | AStrL (_, name, _)) -> name
  let acontent = id

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
  type econtent =
    | Empty
    | Comment of string
    | EncodedPCDATA of string
    | PCDATA of string
    | Entity of string
    | Leaf of ename * attrib list
    | Node of ename * attrib list * elt list
  and elt = {
      mutable ref : int ;
      elt : econtent ;
      elt_mark : Obj.t;
    }

  let content e = e.elt

  let make_mark = ref (fun () -> Obj.repr (ref 0))

  let make_node elt =
    { ref = 0; elt = elt; elt_mark = !make_mark () }

  let empty () = { elt = Empty ; ref = 0; elt_mark = !make_mark (); }

  let comment c = { elt = Comment c ; ref = 0; elt_mark = !make_mark (); }

  let pcdata d = { elt = PCDATA d ; ref = 0; elt_mark = !make_mark (); }
  let encodedpcdata d = { elt = EncodedPCDATA d ; ref = 0; elt_mark = !make_mark (); }
  let entity e = { elt = Entity e ; ref = 0; elt_mark = !make_mark (); }

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
      ref = 0;
      elt_mark = !make_mark (); }

  let node ?a name children =
    { elt =
      (match a with
      | Some a -> Node (name, a, children)
      | None -> Node (name, [], children)) ;
      ref = 0;
      elt_mark = !make_mark (); }

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

      (** TODO REMOVE FROM server side *)
  let lwt_register_event ?keep_default _ = assert false
  let register_event ?keep_default _ = assert false

  let class_name = "class" (* see xHTML.ml *)

  end

end

module SVG = struct
  module M = SVG_f.Make(XML.M)
  module P = XML_print.MakeTypedSimple(XML.M)(M)
end

module HTML5 = struct
  module M = HTML5_f.Make(XML.M)(SVG.M)
  module P = XML_print.MakeTypedSimple(XML.M)(M)
end

module XHTML = struct
  module M = XHTML_f.Make(XML.M)
  module M_01_00 = XHTML_f.Make_01_00(XML.M)
  module M_01_01 = XHTML_f.Make_01_01(XML.M)
  module P = XML_print.MakeTypedSimple(XML.M)(M)
end

type file_info = Ocsigen_extensions.file_info
