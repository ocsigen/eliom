(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2010 Vincent Balat
 * Copyright (C) 2011 Jérôme Vouillon, Grégoire Henry, Pierre Chambart
 * Copyright (C) 2012 Benedikt Becker
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

open Lwt.Syntax
open Js_of_ocaml

(* == Low-level: call service. *)

let create_request__
      ?absolute
      ?absolute_path
      ?https
      (type m)
      ~(service : (_, _, m, _, _, _, _, _, _, _, _) Service.t)
      ?hostname
      ?port
      ?fragment
      ?keep_nl_params
      ?nl_params
      ?keep_get_na_params
      get_params
      post_params
  =
  let path, get_params, fragment, post_params =
    Eliom_uri.make_post_uri_components__ ?absolute ?absolute_path ?https
      ~service ?hostname ?port ?fragment ?keep_nl_params ?nl_params
      ?keep_get_na_params get_params post_params
  in
  let uri =
    Eliom_uri.make_string_uri_from_components (path, get_params, fragment)
  in
  uri, get_params, post_params

let create_request_
      (type m)
      ?absolute
      ?absolute_path
      ?https
      ~(service : (_, _, m, _, _, _, _, _, _, _, _) Service.t)
      ?hostname
      ?port
      ?fragment
      ?keep_nl_params
      ?nl_params
      ?keep_get_na_params
      get_params
      post_params
  =
  (* TODO: allow get_get_or_post service to return also the service
     with the correct subtype. Then do use Eliom_uri.make_string_uri
     and Eliom_uri.make_post_uri_components instead of
     Eliom_uri.make_string_uri_ and
     Eliom_uri.make_post_uri_components__ *)
  match Service.which_meth service with
  | Service.Get' ->
      let ((_, get_params, _) as components) =
        Eliom_uri.make_uri_components ?absolute ?absolute_path ?https ~service
          ?hostname ?port ?fragment ?keep_nl_params ?nl_params get_params
      in
      let uri = Eliom_uri.make_string_uri_from_components components in
      `Get (uri, get_params)
  | Service.Post' ->
      `Post
        (create_request__ ?absolute ?absolute_path ?https ~service ?hostname
           ?port ?fragment ?keep_nl_params ?nl_params ?keep_get_na_params
           get_params post_params)
  | Service.Put' ->
      `Put
        (create_request__ ?absolute ?absolute_path ?https ~service ?hostname
           ?port ?fragment ?keep_nl_params ?nl_params ?keep_get_na_params
           get_params post_params)
  | Service.Delete' ->
      `Delete
        (create_request__ ?absolute ?absolute_path ?https ~service ?hostname
           ?port ?fragment ?keep_nl_params ?nl_params ?keep_get_na_params
           get_params post_params)

let raw_call_service
      ?absolute
      ?absolute_path
      ?https
      ~service
      ?hostname
      ?port
      ?fragment
      ?keep_nl_params
      ?nl_params
      ?keep_get_na_params
      ?progress
      ?upload_progress
      ?override_mime_type
      get_params
      post_params
  =
  (* with_credentials = true is necessary for client side apps when
     we want the Eliom server to be different from the server for
     static files (if any). For example when testing a mobile app
     in a browser, with Cordova's Web server.
     Also set with_credentials to true in CORS configuration.
  *)
  let with_credentials = not (Service.is_external service) in
  let request =
    create_request_ ?absolute ?absolute_path ?https ~service ?hostname ?port
      ?fragment ?keep_nl_params ?nl_params ?keep_get_na_params get_params
      post_params
  in
  let cookies_info = Eliom_uri.make_cookies_info (https, service) in
  let* uri, content =
    match request with
    | `Get (uri, _) ->
        Request.http_get ~with_credentials ?cookies_info uri [] ?progress
          ?upload_progress ?override_mime_type Request.string_result
    | `Post (uri, _, post_params) ->
        Request.http_post ~with_credentials ?cookies_info ?progress
          ?upload_progress ?override_mime_type uri post_params
          Request.string_result
    | `Put (uri, _, post_params) ->
        Request.http_put ~with_credentials ?cookies_info ?progress
          ?upload_progress ?override_mime_type uri post_params
          Request.string_result
    | `Delete (uri, _, post_params) ->
        Request.http_delete ~with_credentials ?cookies_info ?progress
          ?upload_progress ?override_mime_type uri post_params
          Request.string_result
  in
  match content with
  | None -> Lwt.fail (Request.Failed_request 204)
  | Some content -> Lwt.return (uri, content)

let call_service
      ?absolute
      ?absolute_path
      ?https
      ~service
      ?hostname
      ?port
      ?fragment
      ?keep_nl_params
      ?nl_params
      ?keep_get_na_params
      ?progress
      ?upload_progress
      ?override_mime_type
      get_params
      post_params
  =
  let* _, content =
    raw_call_service ?absolute ?absolute_path ?https ~service ?hostname ?port
      ?fragment ?keep_nl_params ?nl_params ?keep_get_na_params ?progress
      ?upload_progress ?override_mime_type get_params post_params
  in
  Lwt.return content

(* == Leave an application. *)

let exit_to
      ?window_name
      ?window_features
      ?absolute
      ?absolute_path
      ?https
      ~service
      ?hostname
      ?port
      ?fragment
      ?keep_nl_params
      ?nl_params
      ?keep_get_na_params
      get_params
      post_params
  =
  match
    create_request_ ?absolute ?absolute_path ?https ~service ?hostname ?port
      ?fragment ?keep_nl_params ?nl_params ?keep_get_na_params get_params
      post_params
  with
  | `Get (uri, _) -> Request.redirect_get ?window_name ?window_features uri
  | `Post (uri, _, post_params) ->
      Request.redirect_post ?window_name uri post_params
  | `Put (uri, _, post_params) ->
      Request.redirect_put ?window_name uri post_params
  | `Delete (uri, _, post_params) ->
      Request.redirect_delete ?window_name uri post_params

let window_open
      ~window_name
      ?window_features
      ?absolute
      ?absolute_path
      ?https
      ~service
      ?hostname
      ?port
      ?fragment
      ?keep_nl_params
      ?nl_params
      ?keep_get_na_params
      get_params
  =
  match
    create_request_ ?absolute ?absolute_path ?https ~service ?hostname ?port
      ?fragment ?keep_nl_params ?nl_params ?keep_get_na_params get_params ()
  with
  | `Get (uri, _) ->
      Dom_html.window##(open_ (Js.string uri) window_name
                          (Js.Opt.option window_features))
  | `Post (_, _, _) -> assert false
  | `Put (_, _, _) -> assert false
  | `Delete (_, _, _) -> assert false
