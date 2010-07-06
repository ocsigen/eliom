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
open Xhtmlcompact




(*****************************************************************************)
(* print to Ocsigen's streams *)

let x_stream, xh_stream =

  let aux ~width ~encode ?(html_compat = false) arbre cont =
    let endemptytag = if html_compat then ">" else " />" in
    let rec xh_print_attrs encode attrs cont = match attrs with
    | [] -> cont ();
    | attr::queue ->
        (Ocsigen_stream.cont (" "^XML.attrib_to_string encode attr)) (fun () ->
        xh_print_attrs encode queue cont)

    and xh_print_text texte cont =
      (Ocsigen_stream.cont texte) cont

    and xh_print_closedtag encode tag attrs cont =
      if List.mem tag emptytags
      then
        (Ocsigen_stream.cont ("<"^tag)) (fun () ->
        xh_print_attrs encode attrs (fun () ->
        (Ocsigen_stream.cont endemptytag) cont))
      else
        (Ocsigen_stream.cont ("<"^tag)) (fun () ->
        xh_print_attrs encode attrs (fun () ->
        (Ocsigen_stream.cont ("></"^tag^">")) cont))

    and xh_print_tag encode tag attrs taglist cont =
      if taglist = []
      then xh_print_closedtag encode tag attrs cont
      else begin
        (Ocsigen_stream.cont ("<"^tag)) (fun () ->
        xh_print_attrs encode attrs (fun () ->
        (Ocsigen_stream.cont ">") (fun () ->
        xh_print_taglist taglist (fun () ->
        (Ocsigen_stream.cont ("</"^tag^">") cont)))))
      end

    and print_nodes name xh_attrs xh_taglist queue cont =
      xh_print_tag encode name xh_attrs xh_taglist (fun () ->
      xh_print_taglist queue cont)

    and xh_print_taglist taglist cont =
      match taglist with

      | [] -> cont ()

      | { elt = Comment texte }::queue ->
          xh_print_text ("<!--"^(encode texte)^"-->")
          (fun () -> xh_print_taglist queue cont)

      | { elt = Entity e }::queue ->
          xh_print_text ("&"^e^";") (* no encoding *)
          (fun () -> xh_print_taglist queue cont)

      | { elt = PCDATA texte }::queue ->
          xh_print_text (encode texte)
          (fun () -> xh_print_taglist queue cont)

      | { elt = EncodedPCDATA texte }::queue ->
          xh_print_text texte
          (fun () -> xh_print_taglist queue cont)

              (* Nodes and Leafs *)
      | { elt = Element (name, xh_attrs, xh_taglist )}::queue
      | { elt = BlockElement (name, xh_attrs, xh_taglist )}::queue
      | { elt = SemiBlockElement (name, xh_attrs, xh_taglist )}::queue
      | { elt = Node (name, xh_attrs, xh_taglist )}::queue ->
          print_nodes name xh_attrs xh_taglist queue cont

      | { elt = Leaf (name,xh_attrs )}::queue ->
          print_nodes name xh_attrs [] queue cont

            (* Whitespaces *)
      | { elt = Whitespace(texte )}::queue ->
          xh_print_text (encode texte)
          (fun () -> xh_print_taglist queue cont)

      | { elt = Empty }::queue ->
          xh_print_taglist queue cont



    in
    xh_print_taglist [arbre] cont
  in
  ((fun ?(width = 132) ?(encode = encode_unsafe)
      ?html_compat doctype foret ->

         (List.fold_right
             (fun arbre cont () ->
               aux ?width ?encode ?html_compat arbre cont)
             foret

         (fun () -> Ocsigen_stream.empty None))),


   (fun ?(width = 132) ?(encode = encode_unsafe)
       ?html_compat doctype arbre ->

        Ocsigen_stream.cont doctype
        (fun () -> Ocsigen_stream.cont Xhtmlpretty.ocsigenadv
        (fun () ->

          aux ?width ?encode ?html_compat arbre

           (fun () -> Ocsigen_stream.empty None)))))

let xhtml_stream ?(version=`XHTML_01_01) ?width ?encode ?html_compat arbre =
  Ocsigen_stream.make
    (fun () ->
      xh_stream ?width ?encode ?html_compat
        (XHTML5.M.doctype version) (XHTML5.M.toelt arbre))

let xhtml_list_stream ?(version=`XHTML_01_01)
    ?width ?encode ?html_compat foret =
  Ocsigen_stream.make
    (fun () ->
      x_stream ?width ?encode ?html_compat
        (XHTML5.M.doctype version) (XHTML5.M.toeltl foret) ())
