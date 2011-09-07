(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2010 Vincent Balat
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

open Eliom_pervasives
open Ocsigen_cookies

exception Looping_redirection
exception Failed_request of int
exception Program_terminated
exception Non_xml_content

(* == ... *)

let max_redirection_level = 12

let short_url_re =
  jsnew Js.regExp (Js.bytestring "^([^\\?]*)(\\?(.*))?$")

let url_re =
  jsnew Js.regExp (Js.bytestring "^([Hh][Tt][Tt][Pp][Ss]?)://([0-9a-zA-Z.-]+|\\[[0-9A-Fa-f:.]+\\])(:([0-9]+))?/([^\\?]*)(\\?(.*))?$")

let get_cookie_info_for_uri_js uri_js =
  match Url.url_of_string (Js.to_string uri_js) with
    | None -> (* Decoding failed *)
      (Js.Opt.case (short_url_re##exec (uri_js))
         (fun () -> assert false)
         (fun res ->
            let match_result = Js.match_result res in
            let path =
              Url.path_of_path_string
                (Js.to_string
                   (Js.Optdef.get (Js.array_get match_result 1)
                      (fun () -> assert false)))
            in
            let path = match path with
              | ""::_ -> path (* absolute *)
              | _ -> Eliom_uri.make_actual_path (Eliom_request_info.get_csp_original_full_path () @ path)
            in
            (Eliom_request_info.get_csp_ssl (), path)
         )
      )
    | Some (Url.Https { Url.hu_path = path }) -> (true,  path)
    | Some (Url.Http  { Url.hu_path = path }) -> (false, path)
    | Some (Url.File  { Url.fu_path = path }) -> (false, path)

let get_cookie_info_for_uri uri =
  let uri_js = Js.bytestring uri in
  get_cookie_info_for_uri_js uri_js


type 'a result = XmlHttpRequest.http_frame -> 'a

let xml_result x =
  match x.XmlHttpRequest.content_xml () with
    | None -> raise Non_xml_content
    | Some v -> v

let string_result x = x.XmlHttpRequest.content

(*TODO: use Url.Current.set *)
let redirect_get url = Dom_html.window##location##href <- Js.string url

let redirect_post url params =
  let f = Dom_html.createForm Dom_html.document in
  f##action <- Js.string url;
  f##_method <- Js.string "post";
  List.iter
    (fun (n, v) ->
       let i =
         Dom_html.createInput
           ~_type:(Js.string "text") ~name:(Js.string n) Dom_html.document in
       i##value <- Js.string v;
       Dom.appendChild f i)
    params;
  f##style##display <- (Js.string "none");
  Dom.appendChild (Dom_html.document##body) f;
  (* firefox accepts submit only on forms in the document *)
  f##submit ()

let redirect_post_form_elt ?(post_args=[]) ?(form_arg=[]) url =
  redirect_post url
    ((List.map (function
      | (name,`String s) -> (name,Js.to_string s)
      | (name,`File f) ->
	Firebug.console##error(Js.string "can't do POST redirection with file parameters");
	failwith "can't do POST redirection with file parameters") form_arg)
     @post_args)

let rec send ?(expecting_process_page = false) ?cookies_info
    ?get_args ?post_args ?form_arg url result =
  let rec aux i ?cookies_info ?get_args ?post_args ?form_arg url =
    let (https, path) = match cookies_info with
      | Some c -> c
      | None -> get_cookie_info_for_uri url
    in
    let cookies = Eliommod_cookies.get_cookies_to_send https path in
    let headers = [ Eliom_common.tab_cookies_header_name,
                    encode_header_value cookies ] in
    let headers = if not Eliom_process.history_api
      then ( Eliom_common.tab_cpi_header_name,
	     encode_header_value Eliom_process.info ) :: headers
      else headers
    in
    let headers = if expecting_process_page
      then
	("Accept","application/xhtml+xml")::
	(Eliom_common.expecting_process_page_name,
         encode_header_value true)::
	headers
      else headers
    in
    let form_contents =
      match form_arg with
	| None -> None
	| Some form_arg ->
	  let contents = Form.empty_form_contents () in
	  List.iter (Form.append contents) form_arg;
	  Some contents
    in
    lwt r = XmlHttpRequest.perform_raw_url ?headers:(Some headers) ?content_type:None
      ?post_args ?get_args ?form_arg:form_contents url in
    ( match r.XmlHttpRequest.headers Eliom_common.set_tab_cookies_header_name with
      | None -> ()
      | Some tab_cookies ->
	let tab_cookies = Eliommod_cookies.cookieset_of_json tab_cookies in
	Eliommod_cookies.update_cookie_table tab_cookies; );
    if r.XmlHttpRequest.code = 204
    then
      match r.XmlHttpRequest.headers Eliom_common.full_xhr_redir_header with
        | Some uri ->
          if i < max_redirection_level
          then aux (i+1) uri
          else Lwt.fail Looping_redirection
        | None ->
          match r.XmlHttpRequest.headers Eliom_common.half_xhr_redir_header with
            | Some uri ->
	      (match post_args,form_arg with
		| None,None -> redirect_get uri
		| _,_ -> redirect_post_form_elt ?post_args ?form_arg url);
	      Lwt.fail Program_terminated
            | None -> Lwt.fail (Failed_request r.XmlHttpRequest.code)
    else
      if expecting_process_page
      then
	match r.XmlHttpRequest.headers Eliom_common.appl_name_header_name with
	  | None ->
	    debug "Eliom_request: non application content received";
	    Lwt.fail (Failed_request r.XmlHttpRequest.code)
	  | Some appl_name ->
	    match Eliom_process.get_application_name () with
	      | None ->
		debug "Eliom_request: no application name? please report this bug";
		assert false
	      | Some current_appl_name ->
		if appl_name = current_appl_name
		then Lwt.return (r.XmlHttpRequest.url, result r)
		else
		  (debug "Eliom_request: received content for application %s when running application %s"
		     appl_name current_appl_name;
		   Lwt.fail (Failed_request r.XmlHttpRequest.code))
      else
	if r.XmlHttpRequest.code = 200
	then Lwt.return (r.XmlHttpRequest.url, result r)
	else Lwt.fail (Failed_request r.XmlHttpRequest.code)
  in aux 0 ?cookies_info ?get_args ?post_args ?form_arg url

(** Same as XmlHttpRequest.perform_raw_url, but:
    - sends tab cookies in an HTTP header
    - does half and full XHR redirections according to headers

    The optional parameter [~cookies_info] is a pair
    containing the information (secure, path)
    that is taken into account for finding tab cookies to send.
    If not present, the path and protocol and taken from the URL.
*)

(* BEGIN FORMDATA HACK *)
let add_button_arg args form =
  let button = (Js.Unsafe.variable "window")##eliomLastButton in
  (Js.Unsafe.variable "window")##eliomLastButton <- None;
  match button with
    | None -> args
    | Some b ->
	let name,value,b_form =
          match Dom_html.tagged b with
            | Dom_html.Button b -> b##name,b##value,b##form
            | Dom_html.Input b -> b##name,b##value,b##form
            | _ -> assert false
	in
	let name = Js.to_string name in
	let value = Js.to_string value in
	if name <> "" && b_form = Js.some form
	then
	  match args with
	    | None -> Some [name,value]
	    | Some l -> Some ((name,value)::l)
	else args
(* END FORMDATA HACK *)


(** Send a GET form with tab cookies and half/full XHR.
    If [~post_params] is present, the HTTP method will be POST,
    with form data in the URL.
    If [~get_params] is present, it will be appended to the form fields.
*)
let send_get_form
    ?expecting_process_page ?cookies_info ?(get_args=[]) ?post_args form url =
  let get_args = get_args@(Form.get_form_contents form) in
  (* BEGIN FORMDATA HACK *)
  let get_args = add_button_arg (Some get_args) form in
  (* END FORMDATA HACK *)
  send ?expecting_process_page ?cookies_info ?get_args ?post_args url

(** Send a POST form with tab cookies and half/full XHR. *)
let send_post_form
    ?expecting_process_page ?cookies_info ?get_args ?post_args form url =
  (* BEGIN FORMDATA HACK *)
  let post_args = add_button_arg post_args form in
  (* END FORMDATA HACK *)
  send ?expecting_process_page ?cookies_info ?get_args ?post_args
    ~form_arg:(Form.form_elements form) url

let http_get ?expecting_process_page ?cookies_info url get_args =
  send ?expecting_process_page ?cookies_info ~get_args url

let http_post ?expecting_process_page ?cookies_info url post_args =
  send ?expecting_process_page ?cookies_info ~post_args url
