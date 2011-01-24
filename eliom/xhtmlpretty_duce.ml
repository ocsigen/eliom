(* Ocsigen
 * Copyright (C) 
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

(**
   Pretty printer for XHTML with Ocamlduce.
   Handles browser specificities properly.
   
   @author Jaap Boender
*)

(* The following tags are written <br />, etc.
   The other empty tags are written <p></p> for html compatibility.
   See guidelines here:
   http://www.w3.org/TR/xhtml1/#guidelines
 *)
let emptytags = ["hr"; "br"; "img"; "meta"; "link"; "input";
                 "col"; "area"; "param"; "base"; "basefont";
                 "isindex"; "frame"]


let blocktags = [ "fieldset"; "form"; "address"; "body"; "head";
                  "blockquote"; "div"; "html";
                  "h1"; "h2"; "h3"; "h4"; "h5"; "h6";
                  "p"; "dd"; "dl"; "li"; "ol"; "ul";
                  "colgroup"; "table"; "tbody"; "tfoot"; "thead";
                  "td"; "th"; "tr" ]

let semiblocktags = [ "pre"; "style"; "title" ]


let pretty_print_xhtml (f: string -> unit) (v: Ocamlduce.Load.anyxml): unit =
  let escape f s =
    let rec aux b i =
      if i = String.length s then f (String.sub s b (i-b))
      else match s.[i] with
	| '&' -> f (String.sub s b (i-b)); f "&amp;"; aux (succ i) (succ i)
	| '<' -> f (String.sub s b (i-b)); f "&lt;"; aux (succ i) (succ i)
	| '>' -> f (String.sub s b (i-b)); f "&gt;"; aux (succ i) (succ i)
	| '"' -> f (String.sub s b (i-b)); f "&quot;"; aux (succ i) (succ i)
	| _ -> aux b (succ i) in
    aux 0 0 in
  begin
    let open_markup tag attrs =
      f ("<" ^ tag);
      List.iter (fun (n, v) -> f " "; f n; f "=\""; escape f v; f "\"") attrs
    in
    Ocamlduce.Print.serialize
      ~start_elem:(fun tag attrs -> open_markup tag attrs; f ">")
      ~end_elem:(fun tag -> f ("</" ^ tag ^ ">"))
      ~empty_elem:(fun tag attrs ->
		     if List.mem tag emptytags then
		       (open_markup tag attrs; f " />")
		     else
		       (open_markup tag attrs; f ("></" ^ tag ^ ">")))
      ~text:(escape f)
      v
  end

