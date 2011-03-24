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
              | _ -> Eliom_uri.make_actual_path (Url.Current.path @ path)
            in
            (Eliom_state.ssl_, path)
         )
      )
    | Some (Url.Https { Url.hu_path = path }) -> (true,  path)
    | Some (Url.Http  { Url.hu_path = path }) -> (false, path)
    | Some (Url.File  { Url.fu_path = path }) -> (false, path)

let get_cookie_info_for_uri uri =
  let uri_js = Js.bytestring uri in
  get_cookie_info_for_uri_js uri_js




(*TODO: use Url.Current.set *)
let redirect_get url = Dom_html.window##location##href <- Js.string url

(*TODO: add Form to js_of_ocaml libs *)
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
  f##submit ()


let rec send_wraper f ?cookies_info ?get_args ?post_args url =
  let rec aux i f ?cookies_info ?get_args ?post_args url =
    let (https, path) = match cookies_info with
      | Some c -> c
      | None -> get_cookie_info_for_uri url
    in
    (* We add cookies in POST parameters *)
    let cookies = Eliommod_cookies.get_cookies_to_send https path in
    let headers = [(Eliom_common.tab_cookies_header_name, 
                    (Ocsigen_lib.encode_header_value cookies))]
    in
    (*** MERGE begin ***)
    let post_args =
      ((Eliom_common.tab_cookies_param_name,
        (encode_form_value (CookiesTable.fold (fun k v l -> (k, v) ::l) cookies [])))::
          post_args)
    in
    f ?headers:(Some headers) ?content_type:None ?post_args ?get_args url
    >>= fun r ->
    (*** MERGE end ***)
    if r.XmlHttpRequest.code = 204
    then
      match r.XmlHttpRequest.headers Eliom_common.full_xhr_redir_header with
        | Some uri ->
          if i < max_redirection_level
          then aux (i+1) XmlHttpRequest.send_string uri
          else Lwt.fail Looping_redirection
        | None ->
          match r.XmlHttpRequest.headers Eliom_common.half_xhr_redir_header with
            | Some uri -> redirect_get uri; Lwt.fail Program_terminated
            | None -> Lwt.fail (Failed_request r.XmlHttpRequest.code)
    else
      if r.XmlHttpRequest.code = 200
      then Lwt.return r.XmlHttpRequest.content
      else Lwt.fail (Failed_request r.XmlHttpRequest.code)
  in aux 0 f ?cookies_info ?get_args ?post_args url

(** Same as XmlHttpRequest.send_string, but:
    - sends tab cookies in an HTTP header
    - does half and full XHR redirections according to headers

    The optional parameter [~cookies_info] is a pair
    containing the information (secure, path)
    that is taken into account for finding tab cookies to send.
    If not present, the path and protocol and taken from the URL.
*)
let send ?cookies_info ?get_args ?post_args url =
  send_wraper XmlHttpRequest.send_string ?cookies_info ?get_args ?post_args url

(** Send a GET form with tab cookies and half/full XHR.
    If [~post_params] is present, the HTTP method will be POST,
    with form data in the URL.
    If [~get_params] is present, it will be appended to the form fields.
*)
let send_get_form ?cookies_info ?get_args ?post_args form url =
  send_wraper
    (fun ?headers ?content_type ?post_args ?get_args url ->
      XmlHttpRequest.send_get_form_string
        ?headers ?get_args ?post_args form url)
    ?cookies_info ?get_args ?post_args url

(** Send a POST form with tab cookies and half/full XHR. *)
let send_post_form ?cookies_info ?get_args ?post_args form url =
  send_wraper
    (fun ?headers ?content_type ?post_args ?get_args url ->
      XmlHttpRequest.send_post_form_string
        ?headers ?get_args ?post_args form url)
    ?cookies_info ?get_args ?post_args url


let get_eliom_appl_result a : Eliom_services.eliom_appl_answer =
  Marshal.from_string (Url.urldecode a) 0

let http_get ?cookies_info url get_args : string Lwt.t =
  send ?cookies_info ~get_args url

let http_post ?cookies_info url post_args : string Lwt.t =
  send ?cookies_info ~post_args url


