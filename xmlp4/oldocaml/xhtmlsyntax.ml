(* Ocsigen
 * Copyright (C) 2005 Vincent Balat
 * Laboratoire PPS - CNRS Université Paris Diderot
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
  Syntax extension for xml

*)

open Xhtmlparser
open ExpoOrPatt

let xml_pat s = failwith "Syntax extension not implemented for patterns"

let _ = Quotation.add "xml" (Quotation.ExAst (xml_exp, xml_pat))

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
let xml_patl s = failwith "Syntax extension not implemented for patterns"

let xml_of_stream s =
  (to_expr_taglist
     (remove_ws
        (Grammar.Entry.parse exprpatt_any_tag_list s)))

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
