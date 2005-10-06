
(* 
   Parseur camlp4 pour XML

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

à revoir 

*)

open Pcaml

module ExpoOrPatt = struct
  type tvarval =
      EPVstr of string
    | EPVvar of string 

  type 'a tlist =
      PLEmpty
    | PLVar of string
    | PLCons of 'a * 'a tlist

  type texprpatt = 
      EPanyattr of tvarval * tvarval
    | EPanytag1 of string * texprpatt tlist * texprpatt tlist
    | EPanytag2 of string
    | EPcomment of string
    | EPanytagvar of string
    | EPanytagvars of string

  let loc =
    let nowhere =
      {(Lexing.dummy_pos) with 
	 Lexing.pos_lnum = 111111; 
	 Lexing.pos_cnum = 111111 } in
    (nowhere,nowhere)

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

    | EPanytag1 (tag, attribute_list, child_list) ->
	<:expr< `$uid:String.capitalize tag$
	  $to_expr_attlist attribute_list$
          $to_expr_taglist child_list$
	>>
	
    | EPanytag2 dt -> <:expr< `PCData $str:dt$ >>

    | EPanytagvar v -> <:expr< $lid:v$ >>

    | EPanytagvars v -> <:expr< `PCData $lid:v$ >>

    | EPcomment c -> 
	let com = "<!--"^c^"-->" in <:expr< `PCData $str:com$ >>

  and to_expr_taglist = function
      PLEmpty -> <:expr< [] >>
    | PLVar v -> <:expr< $lid:v$ >>
    | PLCons (a,l) -> <:expr< [ $to_expr a$ :: $to_expr_taglist l$ ] >>

  and to_expr_attlist = function
      PLEmpty -> <:expr< [] >>
    | PLVar v -> <:expr< $lid:v$ >>
    | PLCons (a,l) -> <:expr< [ $to_expr a$ :: $to_expr_attlist l$ ] >>


  let rec to_patt = function

      EPanyattr (EPVstr a, v) -> 
	let vv = patt_valorval v in
	<:patt< ((`$uid:String.capitalize a$), $vv$) >>

    | EPanyattr (EPVvar a, v) ->
	let vv = patt_valorval v in
	<:patt< ($lid:a$, $vv$) >>

    | EPanytag1 (tag, attribute_list, child_list) ->
	<:patt< `$uid:String.capitalize tag$
	  $to_patt_attlist attribute_list$
          $to_patt_taglist child_list$
        >>

    | EPanytag2 dt -> <:patt< `PCData $str:dt$ >>

    | EPanytagvar v -> <:patt< $lid:v$ >>

    | EPanytagvars v -> <:patt< `PCData $lid:v$ >>

    | EPcomment c -> 
	let com = "<!--"^c^"-->" in <:patt< `PCData $str:com$ >>

  and to_patt_taglist = function
      PLEmpty -> <:patt< [] >>
    | PLVar v -> <:patt< $lid:v$ >>
    | PLCons (a,l) -> <:patt< [ $to_patt a$ :: $to_patt_taglist l$ ] >>

  and to_patt_attlist = function
      PLEmpty -> <:patt< [] >>
    | PLVar v -> <:patt< $lid:v$ >>
    | PLCons (a,l) -> <:patt< [ $to_patt a$ :: $to_patt_attlist l$ ] >>

end

open ExpoOrPatt

(* Instead of using Pcaml.gram, we use a new grammar, using xmllexer *)
let g = Grammar.gcreate (Xmllexer.gmake ())

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
    root_tag = exprpatt_any_tag;
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
      in EPanytag1
	(tag,
	 attlist, 
	 taglist)
  | dt = DATA -> EPanytag2 dt
  | c = COMMENT -> EPcomment c
  | v = CAMLVARXML -> EPanytagvar v
  | v = CAMLVARXMLS -> EPanytagvars v
  ] ];

  exprpatt_any_attribute_list:
  [ [
      v = CAMLVARL -> PLVar v
    | a = exprpatt_attr_or_var;
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
      v = CAMLVARXMLL -> PLVar v
    | anytag = exprpatt_any_tag;
      suite  = OPT exprpatt_any_tag_list ->
      let suite = match suite with
	  None -> PLEmpty
	| Some l -> l
      in PLCons (anytag, suite)
  ] ];

  exprpatt_value_or_var:
  [ [
    v = VALUE -> EPVstr v
  | v = CAMLVAR -> EPVvar v
  ] ];

  exprpatt_attr_or_var:
  [ [
    v = ATTR -> EPVstr v
  | v = CAMLVAR -> EPVvar v
  ] ];

END;;

let xml_exp s = to_expr (Grammar.Entry.parse exprpatt_xml (Stream.of_string s))
let xml_pat s = to_patt (Grammar.Entry.parse exprpatt_xml (Stream.of_string s)) 
let _ = Quotation.add "xml" (Quotation.ExAst (xml_exp, xml_pat))

let xml_expl s = to_expr_taglist 
  (Grammar.Entry.parse exprpatt_any_tag_list (Stream.of_string s))
let xml_patl s = to_patt_taglist 
  (Grammar.Entry.parse exprpatt_any_tag_list (Stream.of_string s)) 
let _ = Quotation.add "xmllist" (Quotation.ExAst (xml_expl, xml_patl))

let _ = Quotation.default := "xml"



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
