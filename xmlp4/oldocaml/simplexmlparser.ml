(* Ocsigen
 * Copyright (C) 2005 Vincent Balat
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

(* 
   Parseur camlp4 pour XML sans antiquotations

   Attention c'est juste un essai
   Je ne colle peut-être pas à la syntaxe XML
   par ex il faut revoir si un attribut peut être vide en xml
   Si oui, il faut remplacer le "string" par "string option"

   Le typage des attributs n'est pas evident donc pour l'instant ils sont tous string
   exemple << <plop number="5" /> >> ----> `Number 5  (en fait `Number (int_of_string "5"))
           << <plop number=$n$ /> >> ----> `Number n o`u n est de type int ??? 

On pourrait decider d'ecrire << <plop number=$string_of_int n$ /> >>
Mais du coup cela fait int_of_string (string_of_int n) 
et ensuite encore string_of_int au moment de l'affichage

   Revoir aussi la gestion des commentaires ?

à revoir 

*)

open Pcaml

(* Instead of using Pcaml.gram, we use a new grammar, using xmllexer *)
let g = Grammar.gcreate (Xmllexer.gmake ())


module ExpoOrPatt = struct

  let loc = (Lexing.dummy_pos, Lexing.dummy_pos)

  type tvarval =
      EPVstr of string
    | EPVvar of string 

  type 'a tlist =
      PLEmpty
    | PLCons of 'a * 'a tlist

  type texprpatt = 
      EPanyattr of tvarval * tvarval
    | EPanytag of string * texprpatt tlist * texprpatt tlist
    | EPpcdata of string
    | EPwhitespace of string
    | EPcomment of string

  let list_of_mlast_expr el = 
    List.fold_right 
      (fun x l -> <:expr< [$x$ :: $l$] >>) el <:expr< [] >>

  let list_of_mlast_patt pl = 
    List.fold_right 
      (fun x l -> <:patt< [$x$ :: $l$] >>) pl <:patt< [] >>

  let expr_valorval = function
      EPVstr v -> <:expr< $str:v$ >>
    | EPVvar v -> <:expr< $lid:v$ >>

  let patt_valorval = function
      EPVstr v -> <:patt< $str:v$ >>
    | EPVvar v -> <:patt< $lid:v$ >>

  let rec to_expr = function

      EPanyattr (EPVstr aa, v) ->
	let vv = expr_valorval v in
	<:expr< (`$uid:String.capitalize aa$, $vv$) >>

    | EPanyattr (EPVvar aa, v) ->
	let vv = expr_valorval v in
	<:expr< ($lid:aa$, $vv$) >>

    | EPanytag (tag, attribute_list, child_list) ->
	<:expr< `$uid:String.capitalize tag$
	  $to_expr_attlist attribute_list$
          $to_expr_taglist child_list$
	>>
	
    | EPpcdata dt -> <:expr< `PCData $str:dt$ >>

    | EPwhitespace dt -> <:expr< `Whitespace $str:dt$ >>

    | EPcomment c -> <:expr< `Comment $str:c$ >>

  and to_expr_taglist = function
      PLEmpty -> <:expr< [] >>
    | PLCons (a,l) -> <:expr< [ $to_expr a$ :: $to_expr_taglist l$ ] >>

  and to_expr_attlist = function
      PLEmpty -> <:expr< [] >>
    | PLCons (a,l) -> <:expr< [ $to_expr a$ :: $to_expr_attlist l$ ] >>


  let rec to_patt = function

      EPanyattr (EPVstr a, v) -> 
	let vv = patt_valorval v in
	<:patt< ((`$uid:String.capitalize a$), $vv$) >>

    | EPanyattr (EPVvar a, v) ->
	let vv = patt_valorval v in
	<:patt< ($lid:a$, $vv$) >>

    | EPanytag (tag, attribute_list, child_list) ->
	<:patt< `$uid:String.capitalize tag$
	  $to_patt_attlist attribute_list$
          $to_patt_taglist child_list$
        >>

    | EPpcdata dt -> <:patt< `PCData $str:dt$ >>

    | EPwhitespace dt -> <:patt< `Whitespace $str:dt$ >>

    | EPcomment c -> <:patt< `Comment $str:c$ >>

  and to_patt_taglist = function
      PLEmpty -> <:patt< [] >>
    | PLCons (a,l) -> <:patt< [ $to_patt a$ :: $to_patt_taglist l$ ] >>

  and to_patt_attlist = function
      PLEmpty -> <:patt< [] >>
    | PLCons (a,l) -> <:patt< [ $to_patt a$ :: $to_patt_attlist l$ ] >>

end

open ExpoOrPatt

let exprpatt_xml = Grammar.Entry.create g "xml"
let exprpatt_any_tag = Grammar.Entry.create g "xml tag"
let exprpatt_any_tag_list = Grammar.Entry.create g "xml tag list"
let exprpatt_any_attribute_list = Grammar.Entry.create g "xml attribute list"
let exprpatt_attr_or_var = Grammar.Entry.create g "xml attribute or $var$"
let exprpatt_value_or_var = Grammar.Entry.create g "xml value or $var$"


EXTEND

  exprpatt_xml:
  [ [
    declaration_list = LIST0 [ DECL | XMLDECL ];
    OPT WHITESPACE;
    root_tag = exprpatt_any_tag;
    OPT WHITESPACE;
    EOI -> root_tag
  ] ];

  exprpatt_any_tag:
  [ [
    tag = TAG;
    attribute_list = OPT exprpatt_any_attribute_list;
    child_list = OPT exprpatt_any_tag_list;
    GAT -> 
      let attlist = match attribute_list with
	  None -> PLEmpty
	| Some l -> l
      in
      let taglist = match child_list with
	  None -> PLEmpty
	| Some l -> l
      in EPanytag
	(tag,
	 attlist, 
	 taglist)
  | dt = WHITESPACE -> EPwhitespace dt
  | dt = DATA -> EPpcdata dt
  | c = COMMENT -> EPcomment c
  ] ];

  exprpatt_any_attribute_list:
  [ [
     a = exprpatt_attr_or_var;
      "=";
      value = exprpatt_value_or_var;
      suite  = OPT exprpatt_any_attribute_list ->
      let suite = match suite with
	  None -> PLEmpty
	| Some l -> l
      in PLCons (EPanyattr (a,value), suite)
  ] ];

  exprpatt_any_tag_list:
  [ [
     anytag = exprpatt_any_tag;
      suite  = OPT exprpatt_any_tag_list ->
      let suite = match suite with
	  None -> PLEmpty
	| Some l -> l
      in PLCons (anytag, suite)
  ] ];

  exprpatt_value_or_var:
  [ [
    v = VALUE -> EPVstr v
  ] ];

  exprpatt_attr_or_var:
  [ [
    v = ATTR -> EPVstr v
  ] ];

END;;

let xml_exp s = to_expr (Grammar.Entry.parse exprpatt_xml (Stream.of_string s))
let xml_pat s = to_patt (Grammar.Entry.parse exprpatt_xml (Stream.of_string s))

let xmlparser s =
  let chan = open_in s in
  let tree = Grammar.Entry.parse exprpatt_any_tag_list (Stream.of_channel chan) in
  close_in chan;
  tree



