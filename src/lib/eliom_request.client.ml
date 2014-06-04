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

open Eliom_lib
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
      match v with
        | `String v ->
          let i =
            Dom_html.createTextarea
              ~_type:(Js.string "text") ~name:(Js.string n) Dom_html.document in
          i##value <- v;
          Dom.appendChild f i
        | `File i ->
          Firebug.console##error(Js.string "can't do POST redirection with file parameters");
          failwith "redirect_post not implemented for files")
    params;
  f##style##display <- (Js.string "none");
  Dom.appendChild (Dom_html.document##body) f;
  (* firefox accepts submit only on forms in the document *)
  f##submit ()

let redirect_post_form_elt ?(post_args=[]) ?(form_arg=[]) url =
  redirect_post url (form_arg@post_args)

(* Forms cannot use PUT http method: do not redirect *)
let redirect_put _url _params =
  Firebug.console##error(Js.string "can't do PUT redirection");
  failwith "redirect_put not implemented"

(* Forms cannot use DELETE http method: do not redirect *)
let redirect_delete _url _params =
  Firebug.console##error(Js.string "can't do DELETE redirection");
  failwith "redirect_delete not implemented"

let nl_template =
  Eliom_parameter.make_non_localized_parameters
    ~prefix:"eliom" ~name:"template"
    (Eliom_parameter.string "name")
(* Warning: it must correspond to [nl_template]. *)
let nl_template_string = "__nl_n_eliom-template.name"

let rec send ?(expecting_process_page = false) ?cookies_info
    ?get_args ?post_args ?form_arg url result =
  let rec aux i ?cookies_info ?(get_args=[]) ?post_args ?form_arg url =
    let (https, path) = match cookies_info with
      | Some c -> c
      (* CCC Is it really necessary to allow to specify cookie_info here?
         hence, is it necessary to send it with the links? (attribute data-eliom-cookie-info) *)
      | None -> get_cookie_info_for_uri url
    in
    let host = match Url.url_of_string url with
      | Some (Url.Http url)
      | Some (Url.Https url) -> Some url.Url.hu_host
      | Some (Url.File _) -> None
      | None -> (* decoding failed: it is a relative link *)
        Some Url.Current.host
    in
    let cookies = Eliommod_cookies.get_cookies_to_send host https path in
    let headers = match cookies with
      | [] -> []
      | _ -> [ Eliom_common.tab_cookies_header_name,
              encode_header_value cookies ] in
    (* CCC *
       For now we assume that an eliom application is not distributed
       among different server with different hostnames:
       to do that It is needed to change that part a bit to be able to
       send the process name to every host serving eliom pages.
       Do not send it to everybody: when doing a cross domain request
       with additionnal headers like thoose, an OPTION request is done
       before to check if the request is authorized. Some server does
       not support it ( like google ones for instance ) *)
    let headers =
      match host with
        | Some host when host = Url.Current.host ->
          ( Eliom_common.tab_cpi_header_name,
            encode_header_value (Eliom_process.get_info ())) :: headers
        | _ -> headers in
    let headers = if expecting_process_page
      then
        let content_type =
          if Dom_html.onIE &&
            not (Js.Optdef.test ((Js.Unsafe.coerce Dom_html.document)##adoptNode))
          then
            (* ie < 9 does not know xhtml+xml content type, but ie 9
               can use it and need it to use adoptNode *)
            "application/xml"
          else "application/xhtml+xml"
        in
        ("Accept",content_type)::
          (Eliom_common.expecting_process_page_name,
           encode_header_value true)::
          headers
      else headers
    in
    let get_args =
      if expecting_process_page
      (* we add this parameter to ensure that the xhr request is
         different from the normal ones: we can't ensure that the
         browser won't cache the content of the page ( for instance
         when clicking the back button ). That way we are sure that an
         xhr answer won't be used in place of a normal answer. *)
      then (Eliom_common.nl_get_appl_parameter, "true")::get_args
      else get_args
    in
    let form_contents =
      match form_arg with
        | None -> None
        | Some form_arg ->
          let contents = Form.empty_form_contents () in
          List.iter (Form.append contents) form_arg;
          Some contents
    in
    let check_headers code headers =
      if expecting_process_page
      then
        if code = 204
        then true
        else
          headers Eliom_common.appl_name_header_name
          = Eliom_process.get_application_name ()
      else true
    in
    try_lwt
      lwt r = XmlHttpRequest.perform_raw_url
        ?headers:(Some headers) ?content_type:None
        ?post_args ~get_args ?form_arg:form_contents ~check_headers url in
      (match r.XmlHttpRequest.headers Eliom_common.set_tab_cookies_header_name
       with
         | None | Some "" -> () (* Empty tab_cookies for IE compat *)
         | Some tab_cookies ->
           let tab_cookies = Eliommod_cookies.cookieset_of_json tab_cookies in
           Eliommod_cookies.update_cookie_table host tab_cookies; );
      if r.XmlHttpRequest.code = 204
      then
        match r.XmlHttpRequest.headers Eliom_common.full_xhr_redir_header with
          | None | Some "" ->
            (match r.XmlHttpRequest.headers Eliom_common.half_xhr_redir_header
             with
               | None | Some "" -> Lwt.return (r.XmlHttpRequest.url, None)
               | Some uri ->
                 (match post_args, form_arg with
                   | None, None -> redirect_get uri
                   | _, _ -> redirect_post_form_elt ?post_args ?form_arg url);
                 Lwt.fail Program_terminated)
          | Some uri ->
            if i < max_redirection_level
            then aux (i+1) uri
            else Lwt.fail Looping_redirection
      else
        if expecting_process_page
        then
          match r.XmlHttpRequest.headers Eliom_common.response_url_header with
            | None | Some "" -> error "Eliom_request: no location header"
            | Some url -> Lwt.return (url, Some (result r))
        else
          if r.XmlHttpRequest.code = 200
          then Lwt.return (r.XmlHttpRequest.url, Some (result r))
          else Lwt.fail (Failed_request r.XmlHttpRequest.code)
    with
      | XmlHttpRequest.Wrong_headers (code, headers) ->
        (* We are requesting application content and the headers tels
           us that the answer is not application content *)
        match headers Eliom_common.appl_name_header_name with
          | None | Some "" -> (* Empty appl_name for IE compat. *)
            (match post_args, form_arg with
              | None, None -> redirect_get url
              | _, _ -> error "Eliom_request: can't silently redirect a Post request to non application content");
            Lwt.fail Program_terminated
          | Some appl_name ->
            match Eliom_process.get_application_name () with
              | None ->
                debug "Eliom_request: no application name? please report this bug";
                assert false
              | Some current_appl_name ->
                if appl_name = current_appl_name
                then assert false (* we can't go here:
                                     this case is already handled before *)
                else
                  (debug "Eliom_request: received content for application %S when running application %s"
                     appl_name current_appl_name;
                   Lwt.fail (Failed_request code))
  in
  lwt (url, content) = aux 0 ?cookies_info ?get_args ?post_args ?form_arg url in
  let filter_url url =
    { url with Url.hu_arguments =
        List.filter (fun (e,_) -> e <> nl_template_string) url.Url.hu_arguments } in
  Lwt.return (
    (match Url.url_of_string url with
      | Some (Url.Http url) ->
        Url.string_of_url (Url.Http (filter_url url))
      | Some (Url.Https url) ->
        Url.string_of_url (Url.Https (filter_url url))
      | _ -> url),
    content)



(** Same as XmlHttpRequest.perform_raw_url, but:
    - sends tab cookies in an HTTP header
    - does half and full XHR redirections according to headers

    The optional parameter [~cookies_info] is a pair
    containing the information (secure, path)
    that is taken into account for finding tab cookies to send.
    If not present, the path and protocol and taken from the URL.
*)

(* BEGIN FORMDATA HACK *)
let add_button_arg inj args form =
  let button = Js.Unsafe.global##eliomLastButton in
  Js.Unsafe.global##eliomLastButton <- None;
  match button with
    | None -> args
    | Some b ->
        let name, value, b_form =
          match Dom_html.tagged b with
            | Dom_html.Button b -> b##name, b##value, b##form
            | Dom_html.Input b -> b##name, b##value, b##form
            | _ -> assert false
        in
        let name = Js.to_string name in
        if name <> "" && b_form = Js.some form
        then
          match args with
            | None -> Some [name, inj value]
            | Some l -> Some ((name, inj value)::l)
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
  let get_args = add_button_arg Js.to_string (Some get_args) form in
  (* END FORMDATA HACK *)
  send ?expecting_process_page ?cookies_info ?get_args ?post_args url

(** Send a POST form with tab cookies and half/full XHR. *)
let send_post_form
    ?expecting_process_page ?cookies_info ?get_args ?post_args form url =
  (* BEGIN FORMDATA HACK *)
  let post_args = add_button_arg (fun x -> `String x) post_args form in
  (* END FORMDATA HACK *)
  send ?expecting_process_page ?cookies_info ?get_args ?post_args
    ~form_arg:(Form.form_elements form) url

let http_get ?expecting_process_page ?cookies_info url get_args =
  send ?expecting_process_page ?cookies_info ~get_args url

let http_post ?expecting_process_page ?cookies_info url post_args =
  send ?expecting_process_page ?cookies_info ~post_args url

let http_put ?expecting_process_page ?cookies_info url post_args =
  send ?expecting_process_page ?cookies_info ~post_args url

let http_delete ?expecting_process_page ?cookies_info url post_args =
  send ?expecting_process_page ?cookies_info ~post_args url
