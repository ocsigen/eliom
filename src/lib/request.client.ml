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

open Lwt.Syntax
open Js_of_ocaml
open Lib

exception Looping_redirection
exception Failed_request of int
exception Program_terminated
exception Non_xml_content

module XmlHttpRequest = Js_of_ocaml_lwt.XmlHttpRequest

let section = Logs.Src.create "eliom:request"
(* == ... *)

let max_redirection_level = 12
let short_url_re = new%js Js.regExp (Js.bytestring "^([^\\?]*)(\\?(.*))?$")

let get_cookie_info_for_uri_js uri_js =
  match Url.url_of_string (Js.to_string uri_js) with
  | None ->
      (* Decoding failed *)
      Js.Opt.case
        short_url_re##(exec uri_js)
        (fun () -> assert false)
        (fun res ->
           let match_result = Js.match_result res in
           let path =
             Url.path_of_path_string
               (Js.to_string
                  (Js.Optdef.get (Js.array_get match_result 1) (fun () ->
                     assert false)))
           in
           let path =
             match path with
             | "" :: _ -> path (* absolute *)
             | _ ->
                 Common_base.make_actual_path
                   (Request_info.get_csp_original_full_path () @ path)
           in
           Request_info.get_csp_ssl (), path)
  | Some (Url.Https {Url.hu_path = path; _}) -> true, path
  | Some (Url.Http {Url.hu_path = path; _}) -> false, path
  | Some (Url.File {Url.fu_path = path; _}) -> false, path

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
let redirect_get ?window_name ?window_features url =
  match window_name with
  | None -> Dom_html.window##.location##.href := Js.string url
  | Some window_name ->
      ignore
        Dom_html.window##(open_ (Js.string url) (Js.string window_name)
                            (Js.Opt.map
                               (Js.Opt.option window_features)
                               Js.string))

let redirect_post ?window_name url params =
  let f = Dom_html.createForm Dom_html.document in
  f##.action := Js.string url;
  f##._method := Js.string "post";
  (match window_name with None -> () | Some wn -> f##.target := Js.string wn);
  List.iter
    (fun (n, v) ->
       match v with
       | `String v ->
           let i =
             Dom_html.createTextarea ~name:(Js.string n) Dom_html.document
           in
           i##.value := v;
           Dom.appendChild f i
       | `File _ ->
           raise_error ~section "redirect_post not implemented for files")
    params;
  f##.style##.display := Js.string "none";
  Dom.appendChild Dom_html.document##.body f;
  (* firefox accepts submit only on forms in the document *)
  f##submit

(* Forms cannot use PUT http method: do not redirect *)
let redirect_put ?window_name:_ _url _params =
  raise_error ~section "redirect_put not implemented"

(* Forms cannot use DELETE http method: do not redirect *)
let redirect_delete ?window_name:_ _url _params =
  raise_error ~section "redirect_delete not implemented"

let nl_template =
  Parameter.make_non_localized_parameters ~prefix:"eliom" ~name:"template"
    (Parameter.string "name")

(* Warning: it must correspond to [nl_template]. *)
let nl_template_string = "__nl_n_eliom-template.name"

module Additional_headers = struct
  module Headers = Map.Make (String)

  let headers = ref Headers.empty

  let add header value =
    headers :=
      Headers.update
        (String.lowercase_ascii header)
        (fun _ -> Some value)
        !headers

  let remove header = headers := Headers.remove header !headers
  let to_list () = Headers.bindings !headers
end

let locked, set_locked = React.S.create false
let lock () = set_locked true
let unlock () = set_locked false

(** Same as XmlHttpRequest.perform_raw_url, but:
    - sends tab cookies in an HTTP header
    - does half and full XHR redirections according to headers

    The optional parameter [~cookies_info] is a pair
    containing the information (secure, path)
    that is taken into account for finding tab cookies to send.
    If not present, the path and protocol are taken from the URL.

    The optional parameter [~override_method] forces the method of the
    request, as in XmlHttpRequest.perform_raw_url. Full XHR redirections
    are always followed with GET.
*)
let send
      ?with_credentials
      ?(expecting_process_page = false)
      ?cookies_info
      ?get_args
      ?post_args
      ?override_method
      ?progress
      ?upload_progress
      ?override_mime_type
      url
      result
  =
  let rec aux i ?cookies_info ?(get_args = []) ?post_args ?override_method url =
    let https, path =
      match cookies_info with
      | Some c -> c
      (* CCC Is it really necessary to allow to specify cookie_info here?
         hence, is it necessary to send it with the links? (attribute data-eliom-cookie-info) *)
      | None -> get_cookie_info_for_uri url
    in
    let host =
      match Url.url_of_string url with
      | Some (Url.Http url) | Some (Url.Https url) -> Some url.Url.hu_host
      | Some (Url.File _) -> None
      | None ->
          (* decoding failed: it is a relative link *)
          Some Url.Current.host
    in
    let host =
      match host with
      | Some host when host = Url.Current.host ->
          Some (Process.get_info ()).Common.cpi_hostname
      | _ -> host
    in
    let cookies = Mod_cookies.get_cookies_to_send host https path in
    let headers =
      match cookies with
      | [] -> []
      | _ ->
          [ ( Common.tab_cookies_header_name
            , encode_header_value ~typ:[%json: (string * string) list] cookies )
          ]
    in
    let headers =
      if Js.Optdef.test Js.Unsafe.global##.___eliom_use_cookie_substitutes_
      then
        (* Cookie substitutes are for iOS WKWebView *)
        let cookies =
          Mod_cookies.get_cookies_to_send ~in_local_storage:true host https path
        in
        ( Common.cookie_substitutes_header_name
        , encode_header_value ~typ:[%json: (string * string) list] cookies )
        :: headers
      else headers
    in
    let headers = Additional_headers.to_list () @ headers in
    (* CCC *
       For now we assume that an eliom application is not distributed
       among different server with different hostnames:
       to do that It is needed to change that part a bit to be able to
       send the process name to every host serving eliom pages.
       Do not send it to everybody: when doing a cross domain request
       with additional headers like thoose, an OPTION request is done
       before to check if the request is authorized. Some server does
       not support it ( like google ones for instance ) *)
    let headers =
      match host with
      | Some host when host = Url.Current.host ->
          ( Common.tab_cpi_header_name
          , encode_header_value ~typ:[%json: Common_base.client_process_info]
              (Process.get_info ()) )
          :: headers
      | _ -> headers
    in
    let headers =
      if expecting_process_page
      then
        ("Accept", "application/xhtml+xml")
        :: ( Common.expecting_process_page_name
           , encode_header_value ~typ:[%json: bool] true )
        :: headers
      else headers
    in
    let get_args =
      if
        expecting_process_page
        (* we add this parameter to ensure that the xhr request is
         different from the normal ones: we can't ensure that the
         browser won't cache the content of the page ( for instance
         when clicking the back button ). That way we are sure that an
         xhr answer won't be used in place of a normal answer. *)
      then (Common.nl_get_appl_parameter, "true") :: get_args
      else get_args
    in
    let check_headers code headers =
      if expecting_process_page
      then
        if code = 204
        then true
        else
          headers Common.appl_name_header_name
          = Some (Process.get_application_name ())
      else true
    in
    Lwt.catch
      (fun () ->
         let* r =
           let contents =
             match post_args with
             | Some post_args -> Some (`POST_form post_args)
             | None -> None
           in
           XmlHttpRequest.perform_raw_url ?with_credentials
             ?headers:(Some headers) ?content_type:None ?contents ~get_args
             ~check_headers ?progress ?upload_progress ?override_mime_type
             ?override_method url
         in
         let wait_for_unlock, unlock = Lwt.wait () in
         (if not @@ React.S.value locked
          then Lwt.wakeup unlock ()
          else
            let unlock_event = React.E.once @@ React.S.changes locked in
            Dom_reference.retain_generic wait_for_unlock
              ~keep:(React.E.map (fun _ -> Lwt.wakeup unlock ()) unlock_event));
         let* () = wait_for_unlock in
         (if Js.Optdef.test Js.Unsafe.global##.___eliom_use_cookie_substitutes_
          then
            match
              (* Cookie substitutes are for iOS WKWebView *)
              r.XmlHttpRequest.headers Common.set_cookie_substitutes_header_name
            with
            | None | Some "" -> ()
            | Some cookie_substitutes ->
                Mod_cookies.update_cookie_table ~in_local_storage:true host
                  (Mod_cookies.cookieset_of_json cookie_substitutes));
         (match r.XmlHttpRequest.headers Common.set_tab_cookies_header_name with
         | None | Some "" -> () (* No tab_cookies header *)
         | Some tab_cookies ->
             let tab_cookies = Mod_cookies.cookieset_of_json tab_cookies in
             Mod_cookies.update_cookie_table host tab_cookies);
         if r.XmlHttpRequest.code = 204
         then
           match r.XmlHttpRequest.headers Common.full_xhr_redir_header with
           | None | Some "" -> (
             match r.XmlHttpRequest.headers Common.half_xhr_redir_header with
             | None | Some "" -> Lwt.return (r.XmlHttpRequest.url, None)
             | Some _uri ->
                 redirect_post url (Option.value post_args ~default:[]);
                 Lwt.fail Program_terminated)
           | Some uri ->
               if i < max_redirection_level
               then aux (i + 1) (Url.resolve uri)
               else Lwt.fail Looping_redirection
         else if expecting_process_page
         then
           let url =
             match r.XmlHttpRequest.headers Common.response_url_header with
             | None | Some "" -> Url.add_get_args url (List.tl get_args)
             | Some url -> Url.resolve url
           in
           Lwt.return (url, Some (result r))
         else if
           r.XmlHttpRequest.code = 200
           || XmlHttpRequest.(r.code = 0 && r.content <> "")
           (* HACK for file access within Cordova which yields code 0.
                    Code 0 might mean a network error, but then we have no
                    content. *)
         then Lwt.return (r.XmlHttpRequest.url, Some (result r))
         else Lwt.fail (Failed_request r.XmlHttpRequest.code))
      (function
        | XmlHttpRequest.Wrong_headers (code, headers) -> (
          (* We are requesting application content and the headers tels
           us that the answer is not application content *)
          match headers Common.appl_name_header_name with
          | None | Some "" ->
              (* No application name header. *)
              (match post_args with
              | None -> redirect_get url
              | _ ->
                  raise_error ~section
                    "can't silently redirect a Post request to non application content");
              Lwt.fail Program_terminated
          | Some appl_name ->
              let current_appl_name = Process.get_application_name () in
              if appl_name = current_appl_name
              then
                assert false
                (* we can't go here:
                                     this case is already handled before *)
              else (
                Logs.warn ~src:section (fun fmt ->
                  fmt
                    "received content for application %S when running application %s"
                    appl_name current_appl_name);
                Lwt.fail (Failed_request code)))
        | exc -> Lwt.fail exc)
  in
  let* url, content =
    aux 0 ?cookies_info ?get_args ?post_args ?override_method url
  in
  let filter_url url =
    { url with
      Url.hu_arguments =
        List.filter (fun (e, _) -> e <> nl_template_string) url.Url.hu_arguments
    }
  in
  Lwt.return
    ( (match Url.url_of_string url with
      | Some (Url.Http url) -> Url.string_of_url (Url.Http (filter_url url))
      | Some (Url.Https url) -> Url.string_of_url (Url.Https (filter_url url))
      | _ -> url)
    , content )

(* The image button last clicked, with the point of the click in the
   image. The submit event gives the image button that submitted a form
   but not this point, which a native submission sends. *)
let image_button_click : (Dom_html.inputElement Js.t * (int * int)) option ref =
  ref None

class type clickEvent = object
  inherit Dom_html.mouseEvent
  method detail : int Js.readonly_prop
end

let track_image_button_clicks () =
  let record (ev : Dom_html.mouseEvent Js.t) =
    (* The target is not always an element: a script can dispatch a click
       on the document. *)
    let input =
      Js.Opt.bind
        (Dom_html.CoerceTo.element (Dom_html.eventTarget ev))
        Dom_html.CoerceTo.input
    in
    Js.Opt.iter input (fun i ->
      if i##._type = Js.string "image"
      then
        (* A click without a pointer, from the keyboard or from a script,
           has a [detail] of 0: the image button then sends (0, 0), as
           the HTML specification says. *)
        let point =
          if (Js.Unsafe.coerce ev : clickEvent Js.t)##.detail = 0
          then 0, 0
          else
            ( int_of_float (Js.to_float ev##.offsetX)
            , int_of_float (Js.to_float ev##.offsetY) )
        in
        image_button_click := Some (i, point));
    Js._true
  in
  ignore
    (Dom_html.addEventListener Dom_html.document Dom_html.Event.click
       (Dom_html.handler record) Js._true
     : Dom_html.event_listener_id)

(* The point that [image] sends when it submits a form. The click is
   forgotten once used, so that a later submission without a click sends
   (0, 0), and the image button, maybe no longer in the page, is not kept
   alive. *)
let selected_point image =
  let point =
    match !image_button_click with
    | Some (clicked, point) when clicked == image -> point
    | _ -> 0, 0
  in
  image_button_click := None;
  point

(* The name and value of the button that submitted a form. Eliom sends
   forms through XHR, serializing their fields with
   [Form.get_form_contents] and [Form.form_elements], which cannot know
   which button was used: a native submission sends it with the fields.
   An image button sends the point of the click, as [name.x] and
   [name.y], or [x] and [y] when it has no name. *)
let submitter_args inj = function
  | None -> []
  | Some submitter -> (
      let named name value =
        let name = Js.to_string name in
        if name = "" then [] else [name, inj value]
      in
      match Dom_html.tagged submitter with
      | Dom_html.Button b -> named b##.name b##.value
      | Dom_html.Input i when i##._type = Js.string "submit" ->
          named i##.name i##.value
      | Dom_html.Input i when i##._type = Js.string "image" ->
          let x, y = selected_point i in
          let prefix =
            match Js.to_string i##.name with "" -> "" | name -> name ^ "."
          in
          let coordinate c = inj (Js.string (string_of_int c)) in
          [prefix ^ "x", coordinate x; prefix ^ "y", coordinate y]
      | _ -> [])

let send_get_form
      ?with_credentials
      ?expecting_process_page
      ?cookies_info
      ?(get_args = [])
      ?post_args
      ?progress
      ?upload_progress
      ?override_mime_type
      ?submitter
      form
      url
  =
  let get_args =
    submitter_args Js.to_string submitter
    @ get_args
    @ Form.get_form_contents form
  in
  send ?with_credentials ?expecting_process_page ?cookies_info ~get_args
    ?post_args ?progress ?upload_progress ?override_mime_type url

let send_post_form
      ?with_credentials
      ?expecting_process_page
      ?cookies_info
      ?get_args
      ?(post_args = [])
      ?progress
      ?upload_progress
      ?override_mime_type
      ?submitter
      form
      url
  =
  let post_args =
    submitter_args (fun x -> `String x) submitter
    @ Form.form_elements form @ post_args
  in
  send ?with_credentials ?expecting_process_page ?cookies_info ?get_args
    ~post_args ?progress ?upload_progress ?override_mime_type url

let http_get
      ?with_credentials
      ?expecting_process_page
      ?cookies_info
      ?progress
      ?upload_progress
      ?override_mime_type
      url
      get_args
  =
  send ?with_credentials ?expecting_process_page ?cookies_info ?progress
    ?upload_progress ?override_mime_type ~get_args url

let http_post
      ?with_credentials
      ?expecting_process_page
      ?cookies_info
      ?progress
      ?upload_progress
      ?override_mime_type
      url
      post_args
  =
  send ?with_credentials ?expecting_process_page ?cookies_info ~post_args
    ?progress ?upload_progress ?override_mime_type url

let http_put
      ?with_credentials
      ?expecting_process_page
      ?cookies_info
      ?progress
      ?upload_progress
      ?override_mime_type
      url
      post_args
  =
  send ?with_credentials ?expecting_process_page ?cookies_info ~post_args
    ~override_method:`PUT ?progress ?upload_progress ?override_mime_type url

let http_delete
      ?with_credentials
      ?expecting_process_page
      ?cookies_info
      ?progress
      ?upload_progress
      ?override_mime_type
      url
      post_args
  =
  send ?with_credentials ?expecting_process_page ?cookies_info ~post_args
    ~override_method:`DELETE ?progress ?upload_progress ?override_mime_type url
