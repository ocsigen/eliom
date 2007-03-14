(* Ocsigen
 * Copyright (C) 2007 Gabriel Kerneis
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception; 
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

open Camlp4.PreCast ;

open BasicTypes ;

module Make (Syntax:Camlp4.Sig.Camlp4Syntax with module Loc = Loc and module Ast = Ast) = struct
value blocktags = [ "fieldset"; "form"; "address"; "body"; "head"; "blockquote"; 
  "div"; "html"; "h1"; "h2"; "h3"; "h4"; "h5"; "h6"; "p"; "dd"; "dl"; "li"; "ol";
  "ul"; "colgroup"; "table"; "tbody"; "tfoot"; "thead"; "td"; "th"; "tr" ] ;

value semiblocktags = [ "pre"; "style"; "title" ] ;

type state = {
	stream : Stream.t (token * Loc.t)  ;
	stack :  Stack.t token;
	loc : Loc.t ;
} ;

type error_msg =
	[ UnterminatedComment
	| UnterminatedString
	| UnterminatedEntity
	| IdentExpected
	| CloseExpected
	| NodeExpected
	| AttributeNameExpected
	| AttributeValueExpected
	| EndOfTagExpected of string
	| EOFExpected ] ;

exception Internal_error of error_msg ;
exception NoMoreData ;
exception CamlListExc of string ;
 
(* Stack - the type of s is state *)
value pop s =
	try
		(Stack.pop s.stack, s)
	with
		[Stack.Empty ->
			let (t,l) = Stream.next s.stream in
			(t, {stream = s.stream ; stack = s.stack ; loc = l })] ;

value push t s =
	Stack.push t s.stack ;


value rec expr_of_list loc = fun
  [ [] -> <:expr< [] >>
  | [(`Elt a)::l] -> 
    <:expr< [ $a$ :: $expr_of_list loc l$ ] >>
  | [(`List a)::l] -> 
    <:expr< $a$ @ $expr_of_list loc l$ >>
  ] ;

  (* une version plus compacte ?
value list_of_mlast_expr loc el = 
  List.fold_right 
    (fun x l -> <:expr< [$x$ :: $l$] >>) el <:expr< [] >> *)
(* marche pas...
  value rec list_of_expr loc = fun
  [ <:expr< [] >> -> []
  | <:expr< [a::l] >> -> [ ( <:expr< a >> ) :: (list_of_expr loc <:expr< l >> ) ]
  | _ -> assert false
  ] ;
*)

value parse = Xmllexer.mk() ;

(* To parse antiquotations *)

value get_expr v loc = 
        Syntax.Gram.parse_string Syntax.expr_eoi loc v;


value xml_pat s = failwith "xml not allowed in patterns";
value xml_patl  = xml_pat ;


(* Convert a stream of tokens into an xhtml tree *)
value rec read_node s =
	let loc = s.loc in
	match pop s with
	[ (PCData s, _) -> 
	    <:expr< ((XHTML.M.tot (XML.EncodedPCDATA $str:s$)) 
      : XHTML.M.elt [> Xhtmltypes.pcdata ]) >>
	| (CamlString s, _) ->
	    <:expr< ((XHTML.M.tot (XML.EncodedPCDATA $get_expr s loc$)) 
      : XHTML.M.elt [> Xhtmltypes.pcdata ]) >>	  
        | (CamlList s, _) -> raise (CamlListExc s)
        | (CamlExpr s, _) -> get_expr s loc
        | (Comment s, _) ->
	   <:expr< XHTML.M.tot (XML.Comment $str:s$) >>
	| (Tag (tag, attlist, closed), s) -> 
      let constr =
          if List.mem tag blocktags
          then "BlockElement"
          else (if List.mem tag semiblocktags
          then "SemiBlockElement"
          else "Element")
        in
        (match closed with
        [ True ->
            <:expr< ((XHTML.M.tot (XML.$uid:constr$ $str:tag$
                $read_attlist s attlist$
                [])) : XHTML.M.elt [> `$uid: String.capitalize tag$])
            >>
        | False ->
            <:expr< ((XHTML.M.tot (XML.$uid:constr$ $str:tag$
               $read_attlist s attlist$
               (XHTML.M.toeltl 
                  ($read_elems ~tag s$ 
                   :> list (XHTML.M.elt 
                              [< Xhtmltypes.$lid:tag^"_content"$])))))
                   : XHTML.M.elt [> `$uid: String.capitalize tag$])
            >>
         ])
	| (t,_) ->
		do {push t s;
		raise NoMoreData}
	]

and read_elems ?tag s =
	let elems = ref [] in
	let loc = s.loc in
	let _ = (try
		while True do {
			try
                        match (read_node s, elems.val) with [
      (* FIXME: concaténer les retours à la ligne et $ des PCData en ajoutant :
  		| (PCData c , [(PCData c2) :: q]) ->
  		    elems.val := [PCData (Printf.sprintf "%s\n%s" c2 c) :: q]
  		il faut traduire les PCData du pattern matching et de l'expression en 
  		leur équivalent Ast.*)
                        (x,l) -> elems.val := [(`Elt x) :: l] ]
		        with [
                        CamlListExc e -> 
                                let l = get_expr e s.loc in
                                elems.val := [ (`List l) :: elems.val ]
                        ]
                }
	with
		[NoMoreData -> ()]) in
  match pop s with
	[ (Endtag s,_) when Some s = tag -> 
	  <:expr< $expr_of_list loc (List.rev elems.val) $ >> 
	| (Eof,_) when tag = None -> 
	  <:expr< $expr_of_list loc (List.rev elems.val) $ >>
	| (t,loc) ->
		match tag with
		[ None -> raise (Internal_error EOFExpected)
		| Some s -> raise (Internal_error (EndOfTagExpected s))
		]
  ] 


and read_attlist s = 
  let loc = s.loc in   
  fun
  [ [] -> <:expr< [] >>
  | [`Attribute (`Attr a, `Val v)::l] -> 
    <:expr< [ (XML.string_attrib $str:a$ $str:v$) :: $read_attlist s l$ ] >>
  | [`Attribute (`CamlAttr a, `Val v)::l] ->
    <:expr< [ (XML.string_attrib $get_expr a loc$ $str:v$) :: $read_attlist s l$ ] >>
  | [`Attribute (`Attr a, `CamlVal v)::l] ->
    <:expr< [ (XML.string_attrib $str:a$ $get_expr v loc$) :: $read_attlist s l$ ] >>
  | [`Attribute (`CamlAttr a, `CamlVal v)::l] ->
    <:expr< [ (XML.string_attrib $get_expr a loc$ $get_expr v loc$) :: $read_attlist s l$ ] >>
  | [`CamlList cl ::l] ->
    <:expr< [ (XHTML.M.to_xmlattribs $get_expr cl loc$) :: $read_attlist s l$ ] >>
  
  ] ;

value  to_expr stream loc = 
 let s = {stream = stream; stack = Stack.create() ; loc = loc } in
 read_node s ;

(*
 value to_expr_taglist stream loc = 
  let s = {stream = stream; stack = Stack.create() ; loc = loc } in
  let rec aux acc =
    let tag = read_node s in
    match fst(pop s) with 
    [ Eof -> [tag::acc]
    | t -> let _ = push t s in aux [tag::acc]
    ]
  in <:expr< $convert_list loc (aux [])$ >>;
*)

value to_expr_taglist stream loc = 
  let s = {stream = stream; stack = Stack.create() ; loc = loc } 
  in <:expr< $read_elems s$  >> ;


value xml_exp loc (x : option string) s = 
  try
  to_expr (parse loc (Stream.of_string s)) loc
   with 
   [NoMoreData ->  <:expr< XHTML.M.tot (XML.Comment "C'est vide là ! Revoir la
   gestion des commentaires !") >> ] ;

   value xml_expl loc (x : option string) s = 
  to_expr_taglist (parse loc (Stream.of_string s)) loc ;

(*
let remove_ws = 
  let rec remove_end_ws = function
      PLCons ((EPwhitespace _),PLEmpty _loc,_) -> PLEmpty _loc
    | PLCons (a,l,_loc) -> PLCons (a,(remove_end_ws l),_loc)
    | l -> l
  in function
      PLCons ((EPwhitespace _),l,_loc) -> remove_end_ws l
    | l -> remove_end_ws l

let xml_expl s = 
  (to_expr_taglist 
     (remove_ws 
        (Grammar.Entry.parse exprpatt_any_tag_list (Stream.of_string s))))
*)

end ;
