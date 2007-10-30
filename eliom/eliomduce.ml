(* Ocsigen
 * http://www.ocsigen.org
 * Module Eliomduce
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


open Http_frame
open Http_com
open Lwt
open Predefined_senders
open Ocsistream
open Xhtml1_strict
open Extensions
open Eliommkforms
open Eliommkreg

let add_css (a : html) : html = 
  let css = 
    {{ <style type="text/css"> "\n.eliom_inline {display: inline}\n.eliom_nodisplay {display: none}\n" }}
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
                  Ocsistream.make (fun () -> Ocsistream.cont x 
                      (fun () -> Ocsistream.empty None))
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

  let send ?(cookies=[]) ?charset ?code ~sp content = 
    Eliommod.EliomResult
      {res_cookies= cookies;
       res_lastmodified= None;
       res_etag= None;
       res_code= code;
       res_send_page= send_ocamlduce_page ~content:content;
       res_headers= Predefined_senders.dyn_headers;
       res_charset= (match charset with
       | None -> Eliomsessions.get_config_file_charset sp
       | _ -> charset);
       res_filter=None
     }

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
  type input_elt = input

  type select_elt = select
  type select_content_elt = select_content
  type select_content_elt_list = {{ [ select_content* ] }}
  type option_elt = option
  type option_elt_list = {{ [ option* ] }}

  type button_elt = button
  type button_content_elt = button_content
  type button_content_elt_list = {{ [ button_content* ] }}

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

  type optgroup_attrib_t = {{ attrs ++ { disabled=?"disabled" } }}
  type option_attrib_t = option_attrs
  type button_attrib_t = button_attrs

  type input_type_t = input_type_values
  type button_type_t = button_type_values

  let hidden = {{ "hidden" }}
  let checkbox = {{ "checkbox" }}
  let radio = {{ "radio" }}
  let submit = {{ "submit" }}
  let file = {{ "file" }}
  let image = {{ "image" }}

  let buttonsubmit = {{ "submit" }}

  let uri_of_string x = x 

  let empty_seq = {{ [] }}
  let cons_form a l = {{ [ a !l ] }}
  let map_option f l = {{ {: (List.map f l) :} }}
  let map_optgroup f a l = ((f a), {{ {: (List.map f l) :} }})

  let select_content_of_option a = (a :> select_content_elt)

  let make_pcdata s = {{ {: s :} }}

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
    | None -> {{ {} }}
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
    {{ <div class="eliom_nodisplay"> [ content ] }}

  let make_div ~classe c =
    let classe = (List.fold_left (fun a b -> a^" "^b) "" classe) in
    {{ <div class={: classe :}> [ c ] }}

  let make_empty_form_content () = {{ <p> [] }} (**** à revoir !!!!! *)

  let remove_first = function {{ (hd,tl) }} -> (hd,tl) | {{ [] }} -> {{ <p>[] }}, {{ [] }}

  let make_input ?(a={{ {} }}) ?(checked=false) ~typ ?name ?src ?value () = 
    let a2 = match value with
    | None -> {{ {} }}
    | Some (v : string) -> {{ { value={: v :} } }}
    in
    let a3 = match name with
    | None -> {{ {} }}
    | Some (v : string) -> {{ { name={: v :} } }}
    in
    let a4 = match src with
    | None -> {{ {} }}
    | Some (v : string) -> {{ { src={: v :} } }}
    in
    let a5 = if checked then {{ { checked="checked" } }} else {{ {} }} in
    {{ <input ({type=typ} ++ a ++ a2 ++ a3 ++ a4 ++ a5)> [] }}

  let make_button ?(a={{ {} }}) ~button_type ?name ?value c =
    let a2 = match value with
    | None -> {{ {} }}
    | Some (v : string) -> {{ { value={: v :} } }}
    in
    let a3 = match name with
    | None -> {{ {} }}
    | Some (v : string) -> {{ { name={: v :} } }}
    in
    {{ <button ({type=button_type} ++ a ++ a2 ++ a3)> c }}

  let make_textarea ?(a={{ {} }}) ~name ?(value={{ [] }}) ~rows ~cols () = 
    {{ <textarea ({ name={: name :}
		    rows={: string_of_int rows :}
		    cols={: string_of_int cols :}
		  } ++ a)> value }}

  let make_select ?(a={{ {} }}) ~multiple ~name elt elts =
    let a2 = if multiple then {{ { multiple="multiple" } }} else {{ {} }} in
    {{ <select ({name={: name :}} ++ a2 ++ a)> [ elt !elts ] }}

  let make_option ?(a={{ {} }}) ~selected ?value c =
    let a2 = match value with
    | None -> {{ {} }}
    | Some (v : string) -> {{ { value={: v :} } }}
    in
    let a3 = if selected then {{ { selected="selected" } }} else {{ {} }} in
    {{ <option (a3 ++ a2 ++ a)> c }}

  let make_optgroup ?(a={{ {} }}) ~label elt elts =
    {{ <optgroup ({label={: label :}} ++ a)> [ elt !elts ] }}
    
  let make_css_link ?(a={{ {} }}) ~uri () =
    {{ <link ({href={: uri :}
	    type="text/css"
            rel="stylesheet"}
            ++ a)> [] }}
      
  let make_js_script ?(a={{ {} }}) ~uri () =
    {{ <script ({type="text/javascript"
	         src={: uri :} } ++ a)> [] }}

end

module Xhtmlreg = Eliommkreg.MakeRegister(Xhtmlreg_)
module Xhtmlforms = Eliommkforms.MakeForms(Xhtmlforms_)
module Xhtml = struct
  include Xhtmlreg
  include Xhtmlforms
end


(****************************************************************************)
(****************************************************************************)

module Xml =
  (struct
    module Cont_content =
      struct
        type t = Ocamlduce.Load.anyxml

        let get_etag_aux x =
          Digest.to_hex (Digest.string x)

        let print x =
          let b = Buffer.create 256 in
          Ocamlduce.Print.print_xml (Buffer.add_string b) x;
          Buffer.contents b

        let get_etag c =
          let x = print c in
          get_etag_aux x

        let stream_of_content c = 
          let x = print c in
          let md5 = get_etag_aux x in
          Lwt.return (Some (Int64.of_int (String.length x)), 
                      md5, 
                      Ocsistream.make (fun () -> Ocsistream.cont x 
                          (fun () -> Ocsistream.empty None))
                     )

            (*il n'y a pas encore de parser pour ce type*)
        let content_of_stream s = assert false

      end
        
    module Cont_sender = FHttp_sender(Cont_content)
        
    let send_cont_page =
      Predefined_senders.send_generic Cont_sender.send 
        ~contenttype:"text/html"
        
    module Contreg_ = struct
      open XHTML.M
      open Xhtmltypes
        
      type page = Ocamlduce.Load.anyxml
            
      let send ?(cookies=[]) ?charset ?code ~sp content = 
        Eliommod.EliomResult 
          {res_cookies= cookies;
           res_lastmodified= None;
           res_etag= None;
           res_code= code;
           res_send_page= send_cont_page ~content:content;
           res_headers= Predefined_senders.dyn_headers;
           res_charset= (match charset with
           | None -> Eliomsessions.get_config_file_charset sp
           | _ -> charset);
           res_filter=None
         }
          
    end
        
    module Contreg = Eliommkreg.MakeRegister(Contreg_)

    include Xhtmlforms
    include Contreg

  end : sig

  include Eliommkreg.ELIOMREGSIG with type page = Ocamlduce.Load.anyxml
  include Eliommkforms.ELIOMFORMSIG

end)

module Blocks = Xml 

(*
: sig

  include Eliommkreg.ELIOMREGSIG with type page = {{ blocks }}
  include Eliommkforms.ELIOMFORMSIG

end *)

(*

+ faire un foncteur SubXhtml (variables de rangée privées et ocamlduce ??)

*)

