
include Eliom_pervasives_base

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
  include Eliom_pervasives_base.List_base
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

  include Eliom_pervasives_base.RawXML

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

  let make_unique ?copy elt =
    let id = match copy with
      | Some copy -> copy.unique_id
      | None -> Some (String.make_cryptographic_safe ()) in
    { elt with unique_id = id }

  (** Ref tree *)

  let cons_attrib att acc = match racontent att with
    | RACamlEventHandler (CE_registered_closure (id, client_expr)) ->
      ClosureMap.add id client_expr acc
    | _ -> acc

  let make_event_handler_table elt =
    let rec aux closure_acc elt =
      let make attribs =
	List.fold_right cons_attrib attribs closure_acc
      in
      match content elt with
	| Empty | EncodedPCDATA _ | PCDATA _
	| Entity _ | Comment _  -> closure_acc
	| Leaf (_, attribs) -> make attribs
	| Node (_, attribs, elts) ->
	  List.fold_left aux (make attribs) elts
    in
    aux ClosureMap.empty elt

  let filter_class (acc_class,acc_attr) = function
    | "class", RA value ->
      begin
	match value with
	  | AStr v ->
	    (v::acc_class,acc_attr)
	  | AStrL (Space,v) ->
	    (v@acc_class,acc_attr)
	  | _ -> failwith "attribute class is not a string"
      end
    | _, RACamlEventHandler (CE_registered_closure _) as attr ->
      (ce_registered_closure_class::acc_class,attr::acc_attr)
    | _, RACamlEventHandler (CE_call_service link_info) ->
      begin
	match Eliom_lazy.force link_info with
	  | None -> acc_class, acc_attr
	  | Some (kind,cookie_info) ->
	    ce_call_service_class::acc_class,
	    match cookie_info with
	      | None -> acc_attr
	      | Some v ->
		(ce_call_service_attrib, RA (AStr (Json.to_string<cookie_info> v)))
		::acc_attr
      end
    | attr -> (acc_class,attr::acc_attr)

  let filter_class_attribs unique_id attribs =
    let unique_id = match unique_id with
      | None -> [],[]
      | Some i -> [unique_class], [unique_attrib,RA (AStr i)]
    in
    let (classes,attribs) =
      List.fold_left filter_class (unique_id) attribs in
    match classes with
      | [] -> attribs
      | _ -> ("class",RA (AStrL(Space,classes)))::attribs

  let set_classes unique_id = function
    | Empty
    | Comment _
    | EncodedPCDATA _
    | PCDATA _
    | Entity _ as e -> e
    | Leaf (ename, attribs) ->
      Leaf (ename, filter_class_attribs unique_id attribs)
    | Node (ename, attribs, sons) ->
      Node (ename, filter_class_attribs unique_id attribs, sons)

  let content e =
    let c = match e.elt with
      | RE e -> e
      | RELazy e -> Eliom_lazy.force e
    in
    set_classes e.unique_id c

end

module SVG = struct
  module M = struct
      include SVG_f.Make(XML)
      let unique ?copy elt =
	tot (XML.make_unique ?copy:(map_option toelt copy) (toelt elt))
  end
  module P = XML_print.MakeTypedSimple(XML)(M)
end

module HTML5 = struct
  module M = struct
    include HTML5_f.Make(XML)(SVG.M)

    let unique ?copy elt =
      tot (XML.make_unique ?copy:(map_option toelt copy) (toelt elt))

    type ('a, 'b, 'c) lazy_plus =
      ?a: (('a attrib) list) -> 'b elt Eliom_lazy.request -> ('b elt) list Eliom_lazy.request -> 'c elt

    let lazy_form ?(a = []) elt1 elts =
      tot (XML.lazy_node ~a:(to_xmlattribs a) "form"
	     (Eliom_lazy.from_fun
		(fun () ->
		  toelt (Eliom_lazy.force elt1)
		  :: toeltl (Eliom_lazy.force elts))))

    (* Aliases use in Eliom_output_base (redefined on client side) *)
    let raw_a_onclick = a_onclick
    let raw_a_onsubmit = a_onsubmit

  end
  module P = XML_print.MakeTypedSimple(XML)(M)
end

module XHTML = struct

  module M = struct

    include XHTML_f.Make(XML)

    type ('a, 'b, 'c) lazy_plus =
      ?a: (('a attrib) list) -> 'b elt Eliom_lazy.request -> ('b elt) list Eliom_lazy.request -> 'c elt

    let lazy_form ~action ?(a = []) elt elts =
      tot (XML.lazy_node ~a:(XML.uri_attrib "action" action :: to_xmlattribs a) "form"
	     (Eliom_lazy.from_fun
		(fun () ->
		  toelt (Eliom_lazy.force elt)
		  :: toeltl (Eliom_lazy.force elts))))

  end

  module M_01_00 = struct

    include XHTML_f.Make_01_00(XML)

    type ('a, 'b, 'c) lazy_plus =
      ?a: (('a attrib) list) -> 'b elt Eliom_lazy.request -> ('b elt) list Eliom_lazy.request -> 'c elt

    let lazy_form ~action ?(a = []) elt elts =
      tot (XML.lazy_node ~a:(XML.uri_attrib "action" action :: to_xmlattribs a) "form"
	     (Eliom_lazy.from_fun
		(fun () ->
		  toelt (Eliom_lazy.force elt)
		  :: toeltl (Eliom_lazy.force elts))))

  end

  module M_01_01 = struct

    include XHTML_f.Make_01_01(XML)

    type ('a, 'b, 'c) lazy_plus =
      ?a: (('a attrib) list) -> 'b elt Eliom_lazy.request -> ('b elt) list Eliom_lazy.request -> 'c elt

    let lazy_form ~action ?(a = []) elt elts =
      tot (XML.lazy_node ~a:(XML.uri_attrib "action" action :: to_xmlattribs a) "form"
	     (Eliom_lazy.from_fun
		(fun () ->
		  toelt (Eliom_lazy.force elt)
		  :: toeltl (Eliom_lazy.force elts))))

  end

  module P = XML_print.MakeTypedSimple(XML)(M)
  module P_01_01 = XML_print.MakeTypedSimple(XML)(M_01_01)
  module P_01_00 = XML_print.MakeTypedSimple(XML)(M_01_00)

end

type file_info = Ocsigen_extensions.file_info


let debug f = Printf.ksprintf (fun s -> Printf.eprintf "%s\n%!" s) f
