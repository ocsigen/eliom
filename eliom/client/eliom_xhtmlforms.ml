(* Ocsigen
 * http://www.ocsigen.org
 * Module Eliom_xhtmlforms
 * Copyright (C) 2009 Vincent Balat
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

let str = Ocsigen_lib.id

module Xhtmlforms_ = struct

  type form_content_elt = JSOO.obj
  type form_content_elt_list = JSOO.obj list
  type form_elt = JSOO.obj
  type a_content_elt = JSOO.obj
  type a_content_elt_list = JSOO.obj list
  type a_elt = JSOO.obj
  type a_elt_list = JSOO.obj list
  type div_content_elt = JSOO.obj
  type div_content_elt_list = JSOO.obj list
  type uri = string
  type link_elt = JSOO.obj
  type script_elt = JSOO.obj
  type textarea_elt = JSOO.obj
  type input_elt = JSOO.obj
  type pcdata_elt = JSOO.obj
  type select_elt = JSOO.obj
  type select_content_elt = JSOO.obj
  type select_content_elt_list = JSOO.obj list
  type button_elt = JSOO.obj
  type button_content_elt = JSOO.obj
  type button_content_elt_list = JSOO.obj list
  type option_elt = JSOO.obj
  type option_elt_list = JSOO.obj list




  type a_attrib_t = (string * string) list
  type form_attrib_t = (string * string) list

  type input_attrib_t = (string * string) list
  type textarea_attrib_t = (string * string) list
  type select_attrib_t = (string * string) list
  type link_attrib_t = (string * string) list
  type script_attrib_t = (string * string) list

  type optgroup_attrib_t = (string * string) list
  type option_attrib_t = (string * string) list
  type button_attrib_t = (string * string) list

  type input_type_t = string
  type button_type_t = string

  let hidden = "hidden"
  let checkbox = "checkbox"
  let radio = "radio"
  let submit = "submit"
  let file = "file"
  let image = "image"

  let buttonsubmit = "submit"

  let uri_of_string x = x

  let empty_seq = []
  let cons_form a l = a::l
  let map_option f l = List.map f l
  let map_optgroup f a l = ((f a), (List.map f l))

  let select_content_of_option a = (a :> select_content_elt)
    
  let make_pcdata s = Js.Node.text (str s)

  let make_a ?(a=[]) ~href l : a_elt =
    let m = Js.Html.create "a" ~attrs:a () in
    Js.Node.set_attribute m "href" href ;
    List.iter (Js.Node.append m) l ;
    m

  let make_get_form ?(a=[]) ~(action : uri) elt1 elts : form_elt =
    let m = Js.Html.create "form" ~attrs:a () in
    Js.Node.set_attribute m "method" "get" ;
    Js.Node.set_attribute m "action" (str action) ;
    Js.Node.append m elt1;
    List.iter (Js.Node.append m) elts ;
    m

  let make_post_form ?(a=[]) ~(action : uri) ?id ?(inline = false) elt1 elts
      : form_elt =
    let m = Js.Html.create "form" ~attrs:a () in
    Js.Node.set_attribute m "method" "post" ;
    Js.Node.set_attribute m "enctype" "multipart/form-data" ;
    Js.Node.set_attribute m "action" (str action) ;
    (match id with
       | None -> ()
       | Some (i : string) -> Js.Node.set_attribute m "id" (str i));
    if inline then Js.Node.set_attribute m "class" "inline";
    Js.Node.append m elt1;
    List.iter (Js.Node.append m) elts ;
    m

  let make_hidden_field content =
    let m = Js.Html.create "div" () in
    Js.Node.set_attribute m "class" "eliom_nodisplay" ;
    (match content with
       | None -> ()
       | Some c -> Js.Node.append m c);
    m

  let make_div ~classe c =
    let m = Js.Html.create "div" () in
    let classe = List.fold_left (fun a b -> a^" "^b) "" classe in
    Js.Node.set_attribute m "class" (str classe) ;
    Js.Node.append m c;
    m

  let make_empty_form_content () =  Js.Html.create "p" () (**** à revoir !!!!! *)

  let remove_first = function
    | a::l -> a,l
    | [] -> (make_empty_form_content ()), []

  let make_input ?(a=[]) ?(checked=false) ~typ ?name ?src ?value () =
    let m = Js.Html.create "input" ~attrs:a () in
    Js.Node.set_attribute m "type" typ ;
    (match value with
       | None -> ()
       | Some (v : string) -> Js.Node.set_attribute m "value" (str v));
    (match name with
       | None -> ()
       | Some (v : string) -> Js.Node.set_attribute m "name" (str v));
    (match src with
       | None -> ()
       | Some (v : uri) -> Js.Node.set_attribute m "src" (str v));
    if checked then Js.Node.set_attribute m "checked" "checked" else ();
    m

  let make_button ?(a=[]) ~button_type ?name ?value c =
    let m = Js.Html.create "button" ~attrs:a () in
    Js.Node.set_attribute m "type" button_type ;
    (match value with
       | None -> ()
       | Some (v : string) -> Js.Node.set_attribute m "value" (str v));
    (match name with
       | None -> ()
       | Some (v : string) -> Js.Node.set_attribute m "name" (str v));
    List.iter (Js.Node.append m) c ;
    m

  let make_textarea ?(a=[]) ~name ?value ~rows ~cols () =
    let m = Js.Html.create "textarea" ~attrs:a () in
    Js.Node.set_attribute m "rows" (string_of_int rows) ;
    Js.Node.set_attribute m "cols" (string_of_int rows) ;
    Js.Node.set_attribute m "name" (str name) ;
    (match value with
       | None -> ()
       | Some v -> Js.Node.append m (make_pcdata v)) ;
    m

  let make_select ?(a=[]) ~multiple ~name elt elts =
    let m = Js.Html.create "select" ~attrs:a () in
    Js.Node.set_attribute m "name" (str name) ;
    if multiple then Js.Node.set_attribute m "multiple" "multiple";
    Js.Node.append m elt;
    List.iter (Js.Node.append m) elts ;
    m

  let make_option ?(a=[]) ~selected ?value c =
    let m = Js.Html.create "option" ~attrs:a () in
    (match value with
       | None -> ()
       | Some (v : string) -> Js.Node.set_attribute m "value" (str v));
    if selected then Js.Node.set_attribute m "selected" "selected";
    Js.Node.append m c;
    m

  let make_optgroup ?(a=[]) ~label elt elts =
    let m = Js.Html.create "optgroup" ~attrs:a () in
    Js.Node.set_attribute m "label" (str label) ;
    Js.Node.append m elt;
    List.iter (Js.Node.append m) elts ;
    m

  let make_css_link ?(a=[]) ~uri () =
    let m = Js.Html.create "link" ~attrs:a () in
    Js.Node.set_attribute m "href" (str uri) ;
    Js.Node.set_attribute m "rel" "stylesheet" ;
    Js.Node.set_attribute m "type" "text/css" ;
    m

  let make_js_script ?(a=[]) ~uri () =
    let m = Js.Html.create "script" ~attrs:a () in
    Js.Node.set_attribute m "src" (str uri) ;
    Js.Node.set_attribute m "type" "text/javascript" ;
    m

end

module Xhtmlforms = Eliom_mkforms.MakeForms(Xhtmlforms_)

include Xhtmlforms

