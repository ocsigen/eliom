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
   Parseur camlp4 pour XHTML

   Attention c'est juste un essai
   Je ne colle peut-être pas à la syntaxe XML

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

let blocktags = [ "fieldset"; "form"; "address"; "body"; "head"; "blockquote"; "div"; "html"; "h1"; "h2"; "h3"; "h4"; "h5"; "h6"; "p"; "dd"; "dl"; "li"; "ol"; "ul"; "colgroup"; "table"; "tbody"; "tfoot"; "thead"; "td"; "th"; "tr" ]

let semiblocktags = [ "pre"; "style"; "title" ]
    


(* Instead of using Pcaml.gram, we use a new grammar, using xmllexer *)
let g = Grammar.gcreate (Xmllexer.gmake ())


module ExpoOrPatt = struct

  type tvarval =
      EPVstr of string * MLast.loc
    | EPVvar of string * MLast.loc 

  type 'a tlist =
      PLEmpty of MLast.loc
    | PLExpr of string * MLast.loc
    | PLCons of 'a * 'a tlist * MLast.loc

  type texprpatt = 
      EPanyattr of tvarval * tvarval * MLast.loc
    | EPanytag of string * texprpatt tlist * texprpatt tlist * MLast.loc
    | EPpcdata of string * MLast.loc
    | EPwhitespace of string * MLast.loc
    | EPcomment of string * MLast.loc
    | EPanytagvar of string * MLast.loc
    | EPanytagvars of string * MLast.loc

  let locadd p11 (p21,p22) = 
    {Lexing.pos_fname = p11.Lexing.pos_fname;
     Lexing.pos_lnum = p11.Lexing.pos_lnum + p21.Lexing.pos_lnum;
     Lexing.pos_bol = p11.Lexing.pos_bol + p21.Lexing.pos_bol;
     Lexing.pos_cnum = p11.Lexing.pos_cnum + p21.Lexing.pos_cnum
   },{Lexing.pos_fname = p11.Lexing.pos_fname;
    Lexing.pos_lnum = p11.Lexing.pos_lnum + p22.Lexing.pos_lnum;
    Lexing.pos_bol = p11.Lexing.pos_bol + p22.Lexing.pos_bol;
    Lexing.pos_cnum = p11.Lexing.pos_cnum + p22.Lexing.pos_cnum
  }
      
  let get_expr v loc = (* <:expr< $lid:v$ >> *)
    let refpos1 = (fun a,b,c -> a) !Pcaml.position in
    let refpos2 = (fun a,b,c -> b) !Pcaml.position in
    let sauv1 = !refpos1 in
    let sauv2 = !refpos2 in
    refpos1 := 0;
    refpos2 := 1;
    let ast = try 
      Grammar.Entry.parse Pcaml.expr_eoi (Stream.of_string v)
    with
      Stdpp.Exc_located (locc, exc) ->
	let loc2 = locadd (fst loc) locc in
	Stdpp.raise_with_loc loc2 exc
    in  
    refpos1 := sauv1;
    refpos2 := sauv2;
    (* Pcaml.expr_reloc (locadd loc) 
       {Lexing.pos_fname = (fst loc).Lexing.pos_fname;
       Lexing.pos_lnum = 0;
       Lexing.pos_bol = 0;
       Lexing.pos_cnum = 0}*)
    <:expr<$anti:ast$>>
  
(*  let list_of_mlast_expr el loc = 
    List.fold_right 
      (fun x l -> <:expr< [$x$ :: $l$] >>) el <:expr< [] >>

  let list_of_mlast_patt pl loc = 
    List.fold_right 
      (fun x l -> <:patt< [$x$ :: $l$] >>) pl <:patt< [] >> *)

  let expr_valorval = function
      EPVstr (v, loc) -> <:expr< $str:v$ >>
    | EPVvar (v, loc) -> <:expr< $lid:v$ >>

  let rec to_expr = function

      EPanyattr (EPVstr (aa,_), v, loc) ->
	let vv = expr_valorval v in
	<:expr< XML.string_attrib $str:aa$ $vv$ >>

    | EPanyattr (EPVvar (aa,_), v, loc) ->
	let vv = expr_valorval v in
	<:expr< XML.string_attrib $lid:aa$ $vv$ >>

    | EPanytag (tag, attribute_list, child_list, loc) ->
	let constr =
	  if List.mem tag blocktags
	  then "BlockElement"
	  else (if List.mem tag semiblocktags
	  then "SemiBlockElement"
	  else "Element")
	in
        (match child_list with
	  PLEmpty loc ->
	    <:expr< ((XHTML.M.tot (XML.$uid:constr$ $str:tag$
                $to_expr_attlist attribute_list$
		[])) : XHTML.M.elt [> `$uid: String.capitalize tag$])
            >>
	| PLExpr (_, loc) | PLCons (_,_, loc) ->
	    <:expr< ((XHTML.M.tot (XML.$uid:constr$ $str:tag$
               $to_expr_attlist attribute_list$
               (XHTML.M.toeltl 
		  ($to_expr_taglist child_list$ 
		   :> list (XHTML.M.elt 
			      [< Xhtmltypes.$lid:tag^"_content"$])))))
		   : XHTML.M.elt [> `$uid: String.capitalize tag$])
	    >>)
	
    | EPpcdata (dt, loc) -> 
	<:expr< ((XHTML.M.tot 
		    (XML.EncodedPCDATA $str:dt$)) 
		   : XHTML.M.elt [> Xhtmltypes.pcdata ]) >>

    | EPwhitespace (dt, loc) -> 
	<:expr< XHTML.M.tot (XML.Whitespace $str:dt$) >>

    | EPanytagvar (v, loc) -> get_expr v loc
      (* <:expr< $lid:v$ >> *)

    | EPanytagvars (v, loc) -> 
	let s = get_expr v loc in
	<:expr< ((XHTML.M.tot (XML.EncodedPCDATA $s$)) 
		   : XHTML.M.elt [> Xhtmltypes.pcdata ]) >>

    | EPcomment (c, loc) -> <:expr< XHTML.M.tot (XML.Comment $str:c$) >>

  and to_expr_taglist = function
      PLEmpty loc -> <:expr< [] >>
    | PLExpr (v, loc) -> get_expr v loc
    | PLCons (a,l, loc) ->
	<:expr< [ $to_expr a$ :: $to_expr_taglist l$ ] >>

  and to_expr_attlist = function
      PLEmpty loc -> <:expr< [] >>
    | PLExpr (v, loc) -> let e = get_expr v loc in
      <:expr< XHTML.M.to_xmlattribs $e$ >>
    | PLCons (a,l, loc) -> <:expr< [ $to_expr a$ :: $to_expr_attlist l$ ] >>


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
	  None -> PLEmpty loc
	| Some l -> l
      in
      let taglist = match child_list with
	  None -> PLEmpty loc
	| Some l -> l
      in EPanytag
	(tag,
	 attlist, 
	 taglist,
	 loc)
  | dt = WHITESPACE -> EPwhitespace (dt, loc)
  | dt = DATA -> EPpcdata (dt, loc)
  | c = COMMENT -> EPcomment (c, loc)
  | v = CAMLEXPRXML -> EPanytagvar (v, loc)
  | v = CAMLEXPRXMLS -> EPanytagvars (v, loc)
  ] ];

  exprpatt_any_attribute_list:
  [ [
      v = CAMLEXPRL -> PLExpr (v, loc)
    | a = exprpatt_attr_or_var;
      "=";
      value = exprpatt_value_or_var;
      suite  = OPT exprpatt_any_attribute_list ->
      let suite = match suite with
	  None -> PLEmpty loc
	| Some l -> l
      in PLCons (EPanyattr (a,value, loc), suite, loc)
  ] ];

  exprpatt_any_tag_list:
  [ [
      v = CAMLEXPRXMLL;
      OPT WHITESPACE -> PLExpr (v, loc)
    | anytag = exprpatt_any_tag;
      suite  = OPT exprpatt_any_tag_list ->
      let suite = match suite with
	  None -> PLEmpty loc
	| Some l -> l
      in PLCons (anytag, suite, loc)
  ] ];

  exprpatt_value_or_var:
  [ [
    v = VALUE -> EPVstr (v, loc)
  | v = CAMLEXPR -> EPVvar (v, loc)
  ] ];

  exprpatt_attr_or_var:
  [ [
    v = ATTR -> EPVstr (v, loc)
  | v = CAMLEXPR -> EPVvar (v, loc)
  ] ];

END;;

let xml_exp s = 
  let refposbol = (fun a,b,c -> a) !Pcaml.position in
  let refposlnum = (fun a,b,c -> b) !Pcaml.position in
  let sauvbol = !refposbol in
  let sauvlnum = !refposlnum in
  let s = 
      (to_expr (Grammar.Entry.parse exprpatt_xml (Stream.of_string s))) in
  refposbol := sauvbol;
  refposlnum := sauvlnum;
  s

(*  let ep = Grammar.Entry.parse exprpatt_xml (Stream.of_string s) in
  match ep with
    EPanytag (tag,_,_) -> <:expr< (($to_expr ep$) : XHTML.M.elt [= `$uid: String.capitalize tag$]) >>
  | _ -> failwith "Prepocessor error in xhtmlparser: there must be only one root tag"
*)


let xmlparser s =
  let chan = open_in s in
  let tree = Grammar.Entry.parse exprpatt_any_tag_list (Stream.of_channel chan) in
  close_in chan;
  tree



(*
(* Pour les expressions et les patterns on peut écrire *)
let a = << a >> in
let b = << bb >> in
let c = `Cc in
let d = "dd" in
let e = `Ee in
let f = "ff" in
let g = << <ark> </ark> >> in
let s = << <youpi> $a$ $b$ $$ $g$ <bobo $c$=$d$ $e$=$f$> </bobo> </youpi> >> in
let la = [(`A, "popo");(`Ggg, "lkjl")] in
let l = [<< <ark $c$=$f$ %la%> </ark> >>; << <wow> </wow> >>] in
  << <youpi> $a$ zzz %l% </youpi> >>
(* $$ permet d'écrire un $ *)
(* %% permet d'écrire un % *)

function << <html %l1%> $a$ ljl %l2% </html> >> -> 1 | _ -> 2
function << <html $n$=$v$ a="b" %l1%> <body> %l2% </body> </html> >> 
    -> 1 | _ -> 2
function << <html %l1%> <body> %l2% </body> %l3% </html> >> -> 1 | _ -> 2
(*
(* mais pas : *)
fun << <html %l1%> %l2% %l3% </html> >> -> 1
(* ni : *)
fun << <html %l1%> %l2% $a$ </html> >> -> 1
(* ni : *)
fun << <html %l1%> %l3% <body> %l2% </body> </html> >> -> 1
(* car les %l% sont des listes *)
*)
*)
