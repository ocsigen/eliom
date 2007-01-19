open Http_frame
open Http_com
open Lwt
open Predefined_senders
open Ocsistream
open Xhtml1_strict

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
    type t = html

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
      Lwt.return (Int64.of_int (String.length x), 
                  md5, 
                  (new_stream x 
                     (fun () -> Lwt.return (empty_stream None))),
                  Ocsimisc.id
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
let send_ocamlduce_page ~content waiter ?code ?etag ~keep_alive ?cookies ?path 
    ?last_modified ?location ?head xhtml_sender =
  send_generic waiter ?etag
    ?code ~keep_alive ?cookies ?path ?location ?last_modified
    ~content ?head xhtml_sender Ocamlduce_sender.send



module Xhtml_ = struct

  type page = html
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

  type link_elt = link
  type script_elt = script

  type pcdata_elt = {{ [ PCDATA ] }}

  type a_attrib_t = a_attrs
  type form_attrib_t = 
    {{ attrs ++ { accept-charset=?String accept=?String 
	          onreset=?String onsubmit=?String enctype=?String } }}

  type input_attrib_t = input_attrs
  type textarea_attrib_t = {{ attrs ++ focus ++ 
	{ onchange=?String onselect=?String 
	  readonly=?"readonly" disabled=?"disabled" 
	  name=?String } }}
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

  let make_uri_from_string x = x 

  let create_sender = Predefined_senders.create_xhtml_sender
  let send = send_ocamlduce_page

  let empty_seq = {{ [] }}
  let cons_form a l = {{ [ a !l ] }}

  let make_a ?(a={{ {} }}) ~href l : a_elt = 
    {{ <a ({href={: make_uri_from_string href :} } ++ a)> l }} 

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

module Xhtml = Ocsigen.Make(Xhtml_)
