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

(* 
   Parseur camlp4 pour XML sans antiquotations

On colle à la forme de la version précédente (cf. ../oldocaml) par souci de
compatibilité uniquement. Je suis plus que perplexe face aux types employés.
*)

open Camlp4.PreCast ;

module ExprOrPatt = struct
  open BasicTypes ;

  type tvarval = [ EPVstr of string | EPVvar of string ];
  type tlist 'a = [ PLEmpty | PLCons of 'a and tlist 'a ];
  type texprpatt =
      [ EPanyattr of tvarval and tvarval
      | EPanytag of string and tlist texprpatt and tlist texprpatt
      | EPpcdata of string
      | EPwhitespace of string
      | EPcomment of string ];

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

value rec convert_list = fun
  [ [] -> PLEmpty
  | [a::l] -> PLCons (a,convert_list l)
  ] ;

(* Convert a stream of tokens into an xml tree *)
value rec read_node s =
	match pop s with
	[ (PCData s, _) -> EPpcdata s
	| (Comment s, _) -> EPcomment s
	| (Tag (tag, attlist, closed), s) -> 
      (match closed with
        [ True -> EPanytag (tag,read_attlist s attlist,PLEmpty)
        | False -> EPanytag (tag,read_attlist s attlist,read_elems ~tag s)
        ])
  | (CamlExpr _,_)|(CamlString _,_) -> assert False (* no antiquotation *)
	| (t,_) ->
		do {push t s;
		raise NoMoreData}
	]

and read_elems ?tag s =
	let elems = ref [] in
	let _ = (try
		while True do {
			match (read_node s, elems.val) with [
      (* FIXME: concaténer les retours à la ligne et $ des PCData en ajoutant :
  		| (PCData c , [(PCData c2) :: q]) ->
  		    elems.val := [PCData (Printf.sprintf "%s\n%s" c2 c) :: q]
  		il faut traduire les PCData du pattern matching et de l'expression en 
  		leur équivalent Ast.*)
			 (x,l) -> elems.val := [x :: l] ]
			}
	with
		[NoMoreData -> ()]) in
  match pop s with
	[ (Endtag s,_) when Some s = tag -> 
	  convert_list (List.rev elems.val)
	| (Eof,_) when tag = None -> 
    convert_list (List.rev elems.val)
	| (t,loc) ->
		match tag with
		[ None -> raise (Internal_error EOFExpected)
		| Some s -> raise (Internal_error (EndOfTagExpected s))
		]
  ] 

and read_attlist s = 
  fun
  [ [] -> PLEmpty
  | [`Attribute (`Attr a, `Val v)::l] -> 
    PLCons (EPanyattr(EPVstr a,EPVstr v),read_attlist s l)
  | [`Attribute (`CamlAttr _, `Val _)::_]| [`Attribute (_, `CamlVal _)::_]
  | [`CamlList _ ::_] -> assert False (* no antiquotation *)
  ] ;

value to_expr_taglist stream loc = 
  let s = {stream = stream; stack = Stack.create() ; loc = loc } in
  let rec aux acc =
    let tag = read_node s in
    match fst(pop s) with 
    [ Eof -> [tag::acc]
    | t -> let _ = push t s in aux [tag::acc]
    ]
 in convert_list (aux []);

end ;

open ExprOrPatt ;

value parse = Xmllexer.mk() ;

value xmlparser s =
  let chan = open_in s in
  let loc = Loc.ghost in
  let tree = to_expr_taglist (parse loc (Stream.of_channel chan)) loc in
  do {close_in chan; tree } ;


