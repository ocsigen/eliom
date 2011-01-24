(* Ocsigen
 * Copyright (C) 2007 Vincent Balat
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

module type F = 
sig
  val emptytags : string list
  val blocktags : string list
  val semiblocktags : string list
  type +'a elt
  type doctypes
  val default_doctype : doctypes
  val doctype : doctypes -> string
  val toeltl : 'a elt list -> XML.elt list
  val toelt : 'a elt -> XML.elt
  val ocsigenadv: string
 end
module MakePretty (F : F) = struct
  open F
  include F
  let xh_string = str_formatter
  let taille_tab = 2
  let id x = x

(* A pretty_printer that handles inline tags properly
   (based on code by Julien Mineraud) *)
  let x_print, xh_print =

    let aux ~width ~encode ?(html_compat = false) doctype arbre =
      let endemptytag = if html_compat then ">" else " />" in
      let rec xh_print_attrs encode attrs = match attrs with
          [] ->  ();
        | attr::queue ->
          pp_print_string xh_string (" "^(XML.attrib_to_string encode attr));
          xh_print_attrs encode queue

      and xh_print_text texte i is_first =
        pp_print_string xh_string texte

      and xh_print_closedtag encode tag attrs i is_first =
        if List.mem tag emptytags
        then begin
          pp_open_tbox xh_string ();
          if (i > 0) || is_first then
            pp_force_newline xh_string ();
          if ((i > 0) || is_first) then
            pp_print_tbreak xh_string (taille_tab*i) 0;
          pp_print_string xh_string ("<"^tag);
          xh_print_attrs encode attrs;
          pp_print_string xh_string endemptytag;
          pp_close_tbox xh_string ()
        end
        else begin
          pp_open_tbox xh_string ();
          if (i > 0) || is_first then
            pp_force_newline xh_string ();
          if ((i > 0) || is_first) then
            pp_print_tbreak xh_string (taille_tab*i) 0;
          pp_print_string xh_string ("<"^tag);
          xh_print_attrs encode attrs;
          pp_print_string xh_string "></";
          pp_print_string xh_string tag;
          pp_print_string xh_string ">";
          pp_close_tbox xh_string ()
        end

      and xh_print_inlinetag encode tag attrs taglist i is_first =
        if taglist = []
        then xh_print_closedtag encode tag attrs i true
        else begin
          pp_print_string xh_string ("<"^tag);
          xh_print_attrs encode attrs;
          pp_print_string xh_string ">";
          xh_print_taglist taglist 0 false false;
          pp_print_string xh_string ("</"^tag^">")
        end

      and xh_print_blocktag encode tag attrs taglist i =
        if taglist = []
        then xh_print_closedtag encode tag attrs i true
        else begin
          pp_open_tbox xh_string ();
          pp_force_newline xh_string ();
          if i > 0 then
            pp_print_tbreak xh_string (taille_tab*i) 0;
          pp_print_string xh_string ("<"^tag);
          xh_print_attrs encode attrs;
          pp_print_string xh_string ">";

          xh_print_taglist_removews taglist (i+1) true;

          pp_force_newline xh_string ();
          if i > 0 then
            pp_print_tbreak xh_string (taille_tab*i) 0;
          pp_print_string xh_string ("</"^tag^">");
          pp_close_tbox xh_string ()
        end

      and xh_print_semiblocktag encode tag attrs taglist i =
      (* New line before and after but not inside, for ex for <pre> *)
        if taglist = []
        then xh_print_closedtag encode tag attrs i true
        else begin
          pp_open_tbox xh_string ();
          pp_force_newline xh_string ();
          if i > 0 then
            pp_print_tbreak xh_string (taille_tab*i) 0;
          pp_print_string xh_string ("<"^tag);
          xh_print_attrs encode attrs;
          pp_print_string xh_string ">";

          xh_print_taglist taglist 0 false false;

          pp_print_string xh_string ("</"^tag^">");
          pp_close_tbox xh_string ()
        end

      and xh_print_taglist_removews taglist i is_first =
          xh_print_taglist taglist i is_first true


      and print_nodes
          ws1 name xh_attrs xh_taglist ws2 queue i is_first removetailingws =
        if (List.mem name blocktags)
        then xh_print_blocktag encode name xh_attrs xh_taglist i
        else
          (if (List.mem name semiblocktags)
           then xh_print_semiblocktag encode name xh_attrs xh_taglist i
           else begin
             xh_print_text (encode ws1) i is_first;
             xh_print_inlinetag encode name xh_attrs xh_taglist i is_first;
             xh_print_text (encode ws2) i is_first;
           end);
        xh_print_taglist queue i false removetailingws

      and xh_print_taglist taglist i is_first removetailingws = match taglist with

          [] -> pp_open_tbox xh_string ();
            pp_close_tbox xh_string ();

        | { elt = Comment texte }::queue ->
          xh_print_text ("<!--"^(encode texte)^"-->") i is_first;
          xh_print_taglist queue i false removetailingws;

        | { elt = Entity e }::queue ->
          xh_print_text ("&"^e^";") i is_first; (* no encoding *)
          xh_print_taglist queue i false removetailingws;

        | { elt = PCDATA texte }::queue ->
          xh_print_text (encode texte) i is_first;
          xh_print_taglist queue i false removetailingws;

        | { elt = EncodedPCDATA texte }::queue ->
          xh_print_text texte i is_first;
          xh_print_taglist queue i false removetailingws;

        | { elt = Node (name,xh_attrs,xh_taglist )}::queue ->
          print_nodes "" name xh_attrs xh_taglist "" queue i is_first removetailingws

        | { elt = Leaf (name,xh_attrs )}::queue ->
          print_nodes "" name xh_attrs [] "" queue i is_first removetailingws

    (* Whitespaces *)
        | { elt = Empty }::queue ->
          xh_print_taglist queue i false removetailingws



      in
      xh_print_taglist [arbre] 0 true false
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
         (*  pp_print_string xh_string xh_topxml; Does not work with IE ...
             pp_force_newline xh_string (); *)
         pp_print_string xh_string doctype;
         pp_force_newline xh_string ();

         pp_print_string xh_string ocsigenadv;
         pp_force_newline xh_string ();

         aux ?width ?encode ?html_compat doctype arbre;

         pp_force_newline xh_string ();
         pp_close_tbox xh_string ();

         flush_str_formatter ()))


  let opt_default x = function
    | Some x -> x
    | _ -> x

  let xhtml_print ?version ?width ?encode ?html_compat arbre =
    let version = opt_default default_doctype version in
    xh_print ?width ?encode ?html_compat
      (doctype version) (F.toelt arbre)

  let xhtml_list_print ?version
      ?width ?encode ?html_compat foret =
    let version = opt_default default_doctype version in
    x_print ?width ?encode ?html_compat
      (doctype version) (F.toeltl foret)
end

