(* Ocsigen
 * Copyright (C) 2008 Vincent Balat, Mauricio Fernandez
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

open Format
open XML

(* The following tags are written <br />, etc.
   The other empty tags are written <p></p> for html compatibility.
   See guidelines here:
   http://www.w3.org/TR/xhtml1/#guidelines
 *)
let emptytags = ["hr"; "br"; "img"; "meta"; "link"; "input";
                 "col"; "area"; "param"; "base"; "basefont";
                 "isindex"; "frame"]


let xh_string = str_formatter

let id x = x

let x_print, xh_print =

  let aux ~width ~encode ?(html_compat = false) doctype arbre =
    let endemptytag = if html_compat then ">" else " />" in
    let rec xh_print_attrs encode attrs = match attrs with
      [] ->  ();
    | attr::queue ->
        pp_print_string xh_string (" "^(XML.attrib_to_string encode attr));
        xh_print_attrs encode queue

    and xh_print_text texte =
      pp_print_string xh_string texte

    and xh_print_closedtag encode tag attrs =
      if List.mem tag emptytags
      then begin
        pp_open_tbox xh_string ();
        pp_print_string xh_string ("<"^tag);
        xh_print_attrs encode attrs;
        pp_print_string xh_string endemptytag;
        pp_close_tbox xh_string ();
      end
      else begin
        pp_open_tbox xh_string ();
        pp_print_string xh_string ("<"^tag);
        xh_print_attrs encode attrs;
        pp_print_string xh_string "></";
        pp_print_string xh_string tag;
        pp_print_string xh_string ">";
        pp_close_tbox xh_string ();
      end

    and xh_print_tag encode tag attrs taglist =
      if taglist = []
      then xh_print_closedtag encode tag attrs
      else begin
        pp_print_string xh_string ("<"^tag);
        xh_print_attrs encode attrs;
        pp_print_string xh_string ">";
        xh_print_taglist taglist;
        pp_print_string xh_string ("</"^tag^">")
      end

    and print_nodes name xh_attrs xh_taglist queue =
      xh_print_tag encode name xh_attrs xh_taglist;
      xh_print_taglist queue

    and xh_print_taglist taglist =
      match taglist with

      | [] -> pp_open_tbox xh_string ();
          pp_close_tbox xh_string ();

      | (Comment texte)::queue ->
          xh_print_text ("<!--"^(encode texte)^"-->");
          xh_print_taglist queue;

      | (Entity e)::queue ->
          xh_print_text ("&"^e^";"); (* no encoding *)
          xh_print_taglist queue;

      | (PCDATA texte)::queue ->
          xh_print_text (encode texte);
          xh_print_taglist queue;

      | (EncodedPCDATA texte)::queue ->
          xh_print_text texte;
          xh_print_taglist queue;

          (* Nodes and Leafs *)
      | (Element (name, xh_attrs, xh_taglist))::queue
      | (BlockElement (name, xh_attrs, xh_taglist))::queue
      | (SemiBlockElement (name, xh_attrs, xh_taglist))::queue
      | (Node (name, xh_attrs, xh_taglist))::queue ->
          print_nodes name xh_attrs xh_taglist queue

      | (Leaf (name,xh_attrs))::queue ->
          print_nodes name xh_attrs [] queue

            (* Whitespaces *)
      | (Whitespace(texte))::queue ->
          xh_print_text (encode texte);
          xh_print_taglist queue

      | Empty::queue ->
          xh_print_taglist queue

    in
    xh_print_taglist [arbre]
  in
  ((fun ?(width = 132) ?(encode = encode_unsafe)
      ?html_compat doctype foret ->

        pp_set_margin str_formatter width;

        pp_open_tbox xh_string ();

        List.iter (aux ?width ?encode ?html_compat doctype) foret;

        pp_force_newline xh_string ();
        pp_close_tbox xh_string ();

        flush_str_formatter ()),

   (fun ?(width = 132) ?(encode = encode_unsafe)
       ?html_compat doctype arbre ->

         pp_set_margin str_formatter width;
         pp_open_tbox xh_string ();
(*  pp_print_string xh_string Xhtmlpretty.xh_topxml; Does not work with IE ...
   pp_force_newline xh_string (); *)
         pp_print_string xh_string doctype;
         pp_force_newline xh_string ();

         pp_print_string xh_string Xhtmlpretty.ocsigenadv;
         pp_force_newline xh_string ();

         aux ?width ?encode ?html_compat doctype arbre;

         pp_force_newline xh_string ();
         pp_close_tbox xh_string ();

         flush_str_formatter ()))


let xhtml_print ?(version=`XHTML_01_01) ?width ?encode ?html_compat arbre =
  xh_print ?width ?encode ?html_compat
    (XHTML.M.doctype version) (XHTML.M.toelt arbre)

let xhtml_list_print ?(version=`XHTML_01_01)
    ?width ?encode ?html_compat foret =
  x_print ?width ?encode ?html_compat
    (XHTML.M.doctype version) (XHTML.M.toeltl foret)

