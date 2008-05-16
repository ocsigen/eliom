(* Ocsigen
 * http://www.ocsigen.org
 * Module ocsigenduce.ml
 * Copyright (C) 2007 Vincent Balat, Alain Frisch
 * CNRS - Université Paris Diderot Paris 7
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

open Ocsigen_http_frame
open Ocsigen_http_com
open Lwt
open Ocsigen_senders
open Xhtmltypes_duce

let add_css (a : html) : html =
  let css =
    {{ <style type="text/css"> "\n.inline {display: inline}\n.nodisplay {display: none}\n" }}
  in
  {{ match a with <html (al)>el ->
   <html (al)>
      (map* el with <head (al)> el -> [ <head (al)> [ css !el ] ])
   }}


module Ocamlduce_content =
  struct
    type t = {{ html }}

    let get_etag_aux x =
      Digest.to_hex (Digest.string x)

    let print x =
      let b = Buffer.create 256 in
      Ocamlduce.Print.print_xml (Buffer.add_string b) x;
      Buffer.contents b

    let get_etag c =
      let x = print (add_css c) in
      get_etag_aux x

    let stream_of_content c =
      let x = print (add_css c) in
      let md5 = get_etag_aux x in
      Lwt.return (Some (Int64.of_int (String.length x)),
                  md5,
                  Ocsigen_stream.make (fun () -> Ocsigen_stream.cont x
                      (fun () -> Ocsigen_stream.empty None))
                 )

    (*il n'y a pas encore de parser pour ce type*)
    let content_of_stream s = assert false
  end

module Ocamlduce_sender = FHttp_sender(Ocamlduce_content)


(** fonction that sends a xhtml page
 * code is the code of the http answer
 * keep_alive is a boolean value that set the field Connection
 * cookie is a string value that give a value to the session cookie
 * path is the path associated to the cookie
 * page is the page to send
 * xhtml_sender is the sender to be used *)
let send_ocamlduce_page =
  send_generic Ocamlduce_sender.send
    ~contenttype:"text/html"




module Xhtmlreg_ = struct

  type page = html

  let headers = Ocsigen_senders.dyn_headers
  let send = send_ocamlduce_page

end

module Xhtmlforms_ = struct

  type form_content_elt = form_content
  type form_content_elt_list = {{ [ form_content* ] }}
  type uri = string
  type a_content_elt = a_content
  type a_content_elt_list = {{ [ a_content* ] }}
  type div_content_elt = flows
  type div_content_elt_list = {{ [ flows* ] }}

  type a_elt = a
  type a_elt_list = {{ [ a* ] }}
  type form_elt = form

  type textarea_elt = textarea
  type select_elt = select
  type input_elt = input

  type link_elt = link
  type script_elt = script

  type pcdata_elt = {{ [ PCDATA ] }}

  type a_attrib_t = a_attrs
  type form_attrib_t =
    {{ attrs ++ { accept-charset=?String accept=?String
                  onreset=?String onsubmit=?String enctype=?String } }}

  type input_attrib_t = input_attrs
  type textarea_attrib_t = {{ attrs ++ focus ++
        { onchange=?String
            onselect=?String
            readonly=?"readonly"
            disabled=?"disabled"
            name=?String } }}
  type select_attrib_t = select_attrs
  type link_attrib_t = link_attrs
  type script_attrib_t = {{ id ++ { defer=?"defer" src=?String charset=?String } }}
(* {{ script_attrs -. type }} *)

  type input_type_t = input_type_values

  let hidden = {{ "hidden" }}
  let text = {{ "text" }}
  let password = {{ "password" }}
  let checkbox = {{ "checkbox" }}
  let radio = {{ "radio" }}
  let submit = {{ "submit" }}
  let file = {{ "file" }}

  let uri_of_string x = x

  let empty_seq = {{ [] }}
  let cons_form a l = {{ [ a !l ] }}

  let make_a ?(a={{ {} }}) ~href l : a_elt =
    {{ <a ({href={: uri_of_string href :} } ++ a)> l }}

  let make_get_form ?(a={{ {} }}) ~(action : uri) elt1 elts : form_elt =
    {{ <form ({method="get"
               action={: action :}}
              ++ a )>
       [ elt1 !elts ] }}

  let make_post_form ?(a={{ {} }}) ~(action : uri) ?id ?(inline = false) elt1 elts
      : form_elt =
    let id_attr = (match id with
      None -> {{ {} }}
    | Some (i : string) -> {{ { id={: i :} } }})
    in
    let inline_attr = if inline then {{ { class="inline" } }} else {{ {} }} in
    {{ <form ({action={: action :}
               enctype="multipart/form-data"
               method="post"}
              ++ inline_attr
              ++ id_attr
              ++ a)>
       [ elt1
         !elts ]
     }}

  let make_hidden_field content =
    {{ <div class="nodisplay"> [ content ] }}

  let make_div ~classe c =
    let classe = (List.fold_left (fun a b -> a^" "^b) "" classe) in
    {{ <div class={: classe :}> [ c ] }}

  let make_select ?(a={{ {} }}) ~name:name ?(selected=None) fp lp =
    let build_option selec p =
      let lsel = if selec then {{ {selected="selected"} }} else {{ {} }}
      in
        match p with
        | (None, (s : string)) -> {{ <option (lsel)> {: s :} }}
        | (Some (v : string), s) -> {{ <option ({value={: v :} } ++ lsel)> {: s :} }}
    in
      match selected with
      | None -> {{ <select ({ name={: name :} } ++ a)>
                   [ {: (build_option false fp) :}
                     !{: (List.map (build_option false) lp) :} ] }}
      | Some p -> {{ <select ({ name={: name :} } ++ a)>
                       [ {: (build_option true p) :}
                         {: (build_option false fp) :}
                         !{: (List.map (build_option false) lp) :} ] }}

  let make_empty_form_content () = {{ <p> [] }} (**** à revoir !!!!! *)

  let remove_first = function {{ (hd,tl) }} -> (hd,tl) | {{ [] }} -> {{ <p>[] }}, {{ [] }}

  let make_input ?(a={{ {} }}) ?(checked=false) ~typ ?name ?value () =
    let a2 = match value with
      None -> {{ {} }}
    | Some (v : string) -> {{ { value={: v :} } }}
    in
    let a3 = match name with
      None -> {{ {} }}
    | Some (v : string) -> {{ { name={: v :} } }}
    in
    let a4 = if checked then {{ { checked="checked" } }} else {{ {} }} in
    {{ <input ({type=typ} ++ a ++ a2 ++ a3 ++ a4)> [] }}

  let make_textarea ?(a={{ {} }}) ~name:name ~rows ~cols c =
    {{ <textarea ({ name={: name :}
                    rows={: string_of_int rows :}
                    cols={: string_of_int cols :}
                  } ++ a)> c }}

  let make_css_link ?(a={{ {} }}) uri =
    {{ <link ({href={: uri :}
            type="text/css"
            rel="stylesheet"}
            ++ a)> [] }}

  let make_js_script ?(a={{ {} }}) uri =
    {{ <script ({type="text/javascript"
                 src={: uri :} } ++ a)> [] }}

end

module Xhtmlreg = Ocsigen.MakeRegister(Xhtmlreg_)
module Xhtmlforms = Ocsigen.MakeForms(Xhtmlforms_)
module Xhtml = struct
  include Xhtmlreg
  include Xhtmlforms
end
