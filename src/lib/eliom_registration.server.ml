(* Ocsigen
 * http://www.ocsigen.org
 * Module Eliom_registration
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

let (>>=) = Lwt.(>>=)

let code_of_code_option = function
  | None -> 200
  | Some c -> c

module Result_types :
sig
  type 'a kind
  val cast_result : Ocsigen_http_frame.result -> 'a kind
  val cast_kind : 'a kind -> Ocsigen_http_frame.result
  val cast_kind_lwt : 'a kind Lwt.t -> Ocsigen_http_frame.result Lwt.t
  val cast_result_lwt : Ocsigen_http_frame.result Lwt.t -> 'a kind Lwt.t
  val cast_function_kind : ('c -> 'a kind Lwt.t) -> ('c -> 'd kind Lwt.t)
  val cast_function_http : ('c -> 'a kind Lwt.t) -> ('c -> Ocsigen_http_frame.result Lwt.t)
end
=
struct
  type 'a kind = Ocsigen_http_frame.result
  let cast_result x = x
  let cast_kind x = x
  let cast_kind_lwt x = x
  let cast_result_lwt x = x
  let cast_function_kind x = x
  let cast_function_http x = x
end

type 'a kind = 'a Result_types.kind
type 'a application_content = [`Appl of 'a]
type block_content
type browser_content = [`Browser]
type 'a ocaml_content
type unknown_content

type redirection_content

let cast_unknown_content_kind (x:unknown_content kind) : 'a kind =
  Result_types.cast_result (Result_types.cast_kind x)
let cast_http_result = Result_types.cast_result

module Html_make_reg_base
  (Html_content : Ocsigen_http_frame.HTTP_CONTENT
   with type t = Html_types.html Eliom_content.Html.elt
    and type options = Http_headers.accept Lazy.t)
  = struct

  type page = Html_types.html Eliom_content.Html.elt
  type options = unit
  type result = browser_content kind

  let result_of_http_result = Result_types.cast_result

  let send_appl_content = Eliom_service.XNever

  let send
      ?(options = ()) ?charset ?code
      ?content_type ?headers content =
    let accept =
      (Ocsigen_extensions.Ocsigen_request_info.accept (Eliom_request_info.get_ri ())) in
    let%lwt r = Html_content.result_of_content ~options:accept content in
    Lwt.return
      (Ocsigen_http_frame.Result.update r
         ~code:(code_of_code_option code)
         ~charset:(match charset with
                     | None -> Some (Eliom_config.get_config_default_charset ())
                     | _ -> charset)
         ~content_type:(match content_type with
                         | None -> Ocsigen_http_frame.Result.content_type r
                         | _ -> content_type)
         ~headers:(match headers with
                       | None -> Ocsigen_http_frame.Result.headers r
                       | Some headers ->
                           Http_headers.with_defaults headers (Ocsigen_http_frame.Result.headers r))
         ())

end

module Html =
  Eliom_mkreg.Make
    (Html_make_reg_base
       (Ocsigen_senders.Make_XML_Content
          (Eliom_content.Xml)
          (Eliom_content.Html.D)))

module Make_typed_xml_registration
  (Xml: Xml_sigs.Iterable)
  (Typed_xml: Xml_sigs.Typed_xml with module Xml := Xml)
  (E : sig type content end) = struct

    module Format = Xml_print.Make_typed(Xml)(Typed_xml)(Ocsigen_stream.StringStream)

    let result_of_content_subxhtml get_etag c =
      let encode x = fst (Xml_print.Utf8.normalize_html x) in
      let x = Format.print_list ~encode c in
      let default_result = Ocsigen_http_frame.Result.default () in
      Lwt.return
        (Ocsigen_http_frame.Result.update default_result
          ~content_length:None
          ~content_type:(Some "text/html")
          ~etag:(get_etag c)
          ~headers:(Http_headers.dyn_headers)
          ~stream:(x, None) ())

    module Cont_content =
      (* Pasted from ocsigen_senders.ml and modified *)
      struct

        let get_etag ?options c = None

        let result_of_content c = result_of_content_subxhtml get_etag c

      end

    module Cont_reg_base = struct

      type page = E.content Typed_xml.elt list
      type options = unit
      type result = block_content kind

      let result_of_http_result = Result_types.cast_result

      let send_appl_content = Eliom_service.XNever

      let send ?options ?charset ?code ?content_type ?headers content =
        let%lwt r = Cont_content.result_of_content content in
        Lwt.return
          (Ocsigen_http_frame.Result.update r
            ~code:(code_of_code_option code)
            ~charset:(match charset with
              | None -> Some (Eliom_config.get_config_default_charset ())
              | _ -> charset)
            ~content_type:(match content_type with
              | None -> Ocsigen_http_frame.Result.content_type r
              | _ -> content_type)
            ~headers:(match headers with
              | None -> Ocsigen_http_frame.Result.headers r
              | Some headers ->
                Http_headers.with_defaults
                  headers (Ocsigen_http_frame.Result.headers r))
            ())

    end

    include Eliom_mkreg.Make(Cont_reg_base)

  end

module Flow5 =
  Make_typed_xml_registration
    (Eliom_content.Xml)
    (Eliom_content.Html.D)
    (struct
      type content = Html_types.flow5
    end)

let (<-<) h (n, v) = Http_headers.replace n v h
let add_cache_header cache headers =
  match cache with
    | None -> headers
    | Some 0 ->
      headers
      <-< (Http_headers.cache_control, "no-cache")
      <-< (Http_headers.expires, "0")
    | Some duration ->
      headers
      <-< (Http_headers.cache_control, "max-age: "^ string_of_int duration)
      <-< (Http_headers.expires,
           Ocsigen_http_com.gmtdate (Unix.time () +. float_of_int duration))


module Text_reg_base = struct

  type page = (string * string)
  type options = int
  type return = Eliom_service.non_ocaml
  type result = unknown_content kind

  let result_of_http_result = Result_types.cast_result

  let send_appl_content = Eliom_service.XNever

  let send ?options ?charset ?code ?content_type ?headers content =
    let%lwt r = Ocsigen_senders.Text_content.result_of_content content in
    let headers = match headers with
      | None -> Ocsigen_http_frame.Result.headers r
      | Some headers ->
        Http_headers.with_defaults headers (Ocsigen_http_frame.Result.headers r) in
    let headers = add_cache_header options headers in
    Lwt.return
      (Ocsigen_http_frame.Result.update r
        ~code:(code_of_code_option code)
        ~charset:(match charset with
          | None ->  Some (Eliom_config.get_config_default_charset ())
          | _ -> charset)
        ~content_type:(match content_type with
          | None -> Ocsigen_http_frame.Result.content_type r
          | _ -> content_type)
        ~headers ())

end

module Text = Eliom_mkreg.Make(Text_reg_base)

module CssText_reg_base = struct

  type page = string
  type options = int
  type result = browser_content kind

  let result_of_http_result = Result_types.cast_result

  let send_appl_content = Eliom_service.XNever

  let send ?options ?charset ?code ?content_type ?headers content =
    let%lwt r =
      Ocsigen_senders.Text_content.result_of_content (content, "text/css") in
    let headers = match headers with
      | None -> Ocsigen_http_frame.Result.headers r
      | Some headers ->
        Http_headers.with_defaults headers (Ocsigen_http_frame.Result.headers r) in
    let headers = add_cache_header options headers in
    Lwt.return
      (Ocsigen_http_frame.Result.update r
        ~code:(code_of_code_option code)
        ~charset:(match charset with
          | None ->  Some (Eliom_config.get_config_default_charset ())
          | _ -> charset)
        ~content_type:(match content_type with
          | None -> Ocsigen_http_frame.Result.content_type r
          | _ -> content_type)
        ~headers ())

end

module CssText = Eliom_mkreg.Make(CssText_reg_base)

module HtmlText_reg_base = struct

  type page = string
  type options = unit
  type result = browser_content kind

  let result_of_http_result = Result_types.cast_result

  let send_appl_content = Eliom_service.XNever

  let send ?options ?charset ?code ?content_type ?headers content =
    let%lwt r =
      Ocsigen_senders.Text_content.result_of_content (content, "text/html") in
    Lwt.return
      (Ocsigen_http_frame.Result.update r
        ~code:(code_of_code_option code)
        ~charset:(match charset with
          | None -> Some (Eliom_config.get_config_default_charset ())
          | _ -> charset)
        ~content_type:(match content_type with
          | None -> Ocsigen_http_frame.Result.content_type r
          | _ -> content_type
        )
        ~headers:(match headers with
          | None -> Ocsigen_http_frame.Result.headers r
          | Some headers ->
            Http_headers.with_defaults headers (Ocsigen_http_frame.Result.headers r))
        ())

end


module HtmlText_registration = Eliom_mkreg.Make(HtmlText_reg_base)

module Html_text = HtmlText_registration

(** Actions are like services, but do not generate any page. The current
   page is reloaded (but if you give the optional parameter
    [~options:`NoReload] to the registration function).
 *)
module Action_reg_base = struct

  type page = unit
  type options = [ `Reload | `NoReload ]
  type result = browser_content kind

  let result_of_http_result = Result_types.cast_result

  let send_appl_content = Eliom_service.XAlways
  (* The post action service will decide later *)

  let send_directly =
  (* send bypassing the following directives
     in the configuration file (they have already been taken into account) *)
    fun ri res ->
      Polytables.set
        (Ocsigen_extensions.Ocsigen_request_info.request_cache ri) Eliom_common.found_stop_key ();
      res

  let send
      ?(options = `Reload) ?charset ?(code = 204)
      ?content_type ?headers () =
    let user_cookies = Eliom_request_info.get_user_cookies () in
    if options = `NoReload
    then
      let empty_result = Ocsigen_http_frame.Result.empty () in
      let h = match headers with
        | None -> Ocsigen_http_frame.Result.headers empty_result
        | Some headers ->
          Http_headers.with_defaults headers
            (Ocsigen_http_frame.Result.headers empty_result)
      in
      let h =
        match Eliom_request_info.get_sp_client_appl_name () with
          | Some anr ->
            Http_headers.replace
              (Http_headers.name Eliom_common_base.appl_name_header_name)
              anr
              h
          | _ -> h
      in
      Lwt.return
        (Ocsigen_http_frame.Result.update empty_result
          ~code:code
          ~content_type:(match content_type with
            | None -> Ocsigen_http_frame.Result.content_type empty_result
            | _ -> content_type
          )
          ~headers:h ())
    else
      (* It is an action, we reload the page.
         To do that, we retry without POST params.
         If no post param at all, we retry
         without GET non_att info.
         If no GET non_att info, we retry without
         GET state.
         If no GET state,
         we do not reload, otherwise it will loop.
      *)
      (* be very careful while re-reading this *)
      let sp = Eliom_common.get_sp () in
      let sitedata = Eliom_request_info.get_sitedata_sp sp in
      let si = Eliom_request_info.get_si sp in
      let ri = Eliom_request_info.get_request_sp sp in
      let open Ocsigen_extensions in
      match (si.Eliom_common.si_nonatt_info,
                       si.Eliom_common.si_state_info,
                       (Ocsigen_extensions.Ocsigen_request_info.meth ri.request_info)) with
        | (Eliom_common.RNa_no,
           (Eliom_common.RAtt_no, Eliom_common.RAtt_no),
           Ocsigen_http_frame.Http_header.GET) ->
          let empty_result = Ocsigen_http_frame.Result.empty () in
          Lwt.return empty_result
        | _ ->
          let all_cookie_info = sp.Eliom_common.sp_cookie_info in
          let%lwt ric = Eliommod_cookies.compute_new_ri_cookies
            (Unix.time ())
            (Ocsigen_request_info.sub_path ri.request_info)
            (Lazy.force (Ocsigen_request_info.cookies ri.request_info))
            all_cookie_info
            user_cookies
          in
          let%lwt all_new_cookies =
            Eliommod_cookies.compute_cookies_to_send
              sitedata
              all_cookie_info
              user_cookies in

          (* Now tab cookies:
             As tab cookies are sent only by Eliom_app services,
             we just need to keep them in rc.
             If the fallback service is not Eliom_app, they will
             be lost.
          *)
          let rc = Eliom_request_info.get_request_cache_sp sp in
          Polytables.set
            ~table:rc
            ~key:Eliom_common.tab_cookie_action_info_key
            ~value:(sp.Eliom_common.sp_tab_cookie_info,
                    sp.Eliom_common.sp_user_tab_cookies,
                    si.Eliom_common.si_tab_cookies
            );

          (* Now removing some parameters to decide the following service: *)
          match (si.Eliom_common.si_nonatt_info,
                 si.Eliom_common.si_state_info,
                 (Ocsigen_request_info.meth ri.request_info)) with
            | (Eliom_common.RNa_get_ _,
               (_, Eliom_common.RAtt_no),
               Ocsigen_http_frame.Http_header.GET)
            | (Eliom_common.RNa_get' _,
               (_, Eliom_common.RAtt_no),
               Ocsigen_http_frame.Http_header.GET)
            (* no post params, GET na coservice *)
            | (Eliom_common.RNa_no,
               (_, Eliom_common.RAtt_no),
               Ocsigen_http_frame.Http_header.GET)
              (* no post params, GET attached coservice *)
              ->
              Polytables.set
                (Ocsigen_extensions.Ocsigen_request_info.request_cache ri.Ocsigen_extensions.request_info)
                Eliom_common.eliom_params_after_action
                (si.Eliom_common.si_all_get_params,
                 si.Eliom_common.si_all_post_params, (* is Some [] *)
                 si.Eliom_common.si_all_file_params, (* is Some [] *)
                 si.Eliom_common.si_nl_get_params,
                 si.Eliom_common.si_nl_post_params,
                 si.Eliom_common.si_nl_file_params,
                 si.Eliom_common.si_all_get_but_nl);
                (*VVV Also put all_cookie_info in this,
                  to avoid update_cookie_table and get_cookie_info (?)
                *)
                let ri = Ocsigen_extensions.Ocsigen_request_info.update ri.request_info
                    ~cookies:(lazy ric)
                    ~get_params:
                      (lazy si.Eliom_common.si_other_get_params) ()
                  (* Here we modify ri,
                     thus the request can be taken by other extensions,
                     with its new parameters *)
                in
                let%lwt () = Eliommod_pagegen.update_cookie_table sitedata all_cookie_info in
                send_directly ri (Ocsigen_extensions.compute_result
                                    ~previous_cookies:all_new_cookies ri)

            | (Eliom_common.RNa_post_ _, (_, _), _)
            | (Eliom_common.RNa_post' _, (_, _), _) ->
              (* POST na coservice *)
              (* retry without POST params *)

              Polytables.set
                (Ocsigen_extensions.Ocsigen_request_info.request_cache ri.Ocsigen_extensions.request_info)
                Eliom_common.eliom_params_after_action
                (si.Eliom_common.si_all_get_params,
                 si.Eliom_common.si_all_post_params,
                 si.Eliom_common.si_all_file_params,
                 si.Eliom_common.si_nl_get_params,
                 si.Eliom_common.si_nl_post_params,
                 si.Eliom_common.si_nl_file_params,
                 si.Eliom_common.si_all_get_but_nl);
              let ri =
                Ocsigen_extensions.Ocsigen_request_info.update ri.request_info
                  ~meth:Ocsigen_http_frame.Http_header.GET
                  ~cookies:(lazy ric)
                  ~get_params:
                    (lazy si.Eliom_common.si_other_get_params)
                  ~post_params:(Some (fun _ -> Lwt.return []))
                  ~files:(Some (fun _ -> Lwt.return []))
                  ()
              in
              let%lwt () = Eliommod_pagegen.update_cookie_table sitedata all_cookie_info in
              send_directly ri (Ocsigen_extensions.compute_result
                                  ~previous_cookies:all_new_cookies ri)

            | _ ->
              (* retry without POST params *)
              (*VVV
                Warning: is it possible to have an Eliom service with POST method
                but no POST parameter?
                --> may loop...
                (we impose GET to prevent that)
              *)
              Polytables.set
                (Ocsigen_extensions.Ocsigen_request_info.request_cache ri.Ocsigen_extensions.request_info)
                Eliom_common.eliom_params_after_action
                (si.Eliom_common.si_all_get_params,
                 si.Eliom_common.si_all_post_params,
                 si.Eliom_common.si_all_file_params,
                 si.Eliom_common.si_nl_get_params,
                 si.Eliom_common.si_nl_post_params,
                 si.Eliom_common.si_nl_file_params,
                 si.Eliom_common.si_all_get_but_nl);
              let ri =
                Ocsigen_extensions.Ocsigen_request_info.update ri.request_info
                  ~meth:Ocsigen_http_frame.Http_header.GET
                  ~cookies:(lazy ric)
                  ~get_params:
                    (lazy si.Eliom_common.si_other_get_params)
                  ~post_params:(Some (fun _ -> Lwt.return []))
                  ~files:(Some (fun _ -> Lwt.return []))
                  ()
              in
              let%lwt () =
                Eliommod_pagegen.update_cookie_table sitedata all_cookie_info in
              send_directly ri (Ocsigen_extensions.compute_result
                                  ~previous_cookies:all_new_cookies ri)

end

module Action = Eliom_mkreg.Make(Action_reg_base)

(** Unit services are like services, do not generate any page, and do not
    reload the page. To be used carefully. Probably not usefull at all.
    (Same as {!Action} with [`NoReload] option).
 *)

module Unit_reg_base = struct

  type page = unit
  type options = unit
  type result = browser_content kind

  let result_of_http_result = Result_types.cast_result

  let send_appl_content = Eliom_service.XAlways

  let send ?options ?charset ?(code = 204)
      ?content_type ?headers content =
    let empty_result = Ocsigen_http_frame.Result.empty () in
    Lwt.return
      (Ocsigen_http_frame.Result.update empty_result
         ~code:code
         ~content_type:(match content_type with
                              | None -> Ocsigen_http_frame.Result.content_type empty_result
                              | _ -> content_type
                           )
         ~headers:(match headers with
                         | None -> Ocsigen_http_frame.Result.headers empty_result
                         | Some headers ->
                             Http_headers.with_defaults
                               headers (Ocsigen_http_frame.Result.headers empty_result)
                      )
         ())

end

module Unit = Eliom_mkreg.Make(Unit_reg_base)

(* Any is a module allowing to register services that decide
   themselves what they want to send.  *)
module Any_reg_base = struct

  type 'a page   = 'a kind
  type options   = unit
  type 'a return = Eliom_service.non_ocaml
  type 'a result = 'a kind

  let result_of_http_result = Result_types.cast_result

  (* let send_appl_content = Eliom_service.XNever *)
  let send_appl_content = Eliom_service.XAlways

  let send ?options ?charset ?code
      ?content_type ?headers (res:'a kind) =
    let res = Result_types.cast_kind res in
    Lwt.return
      (Ocsigen_http_frame.Result.update res
         ~charset:(match charset with
                         | None -> Ocsigen_http_frame.Result.charset res
                         | _ -> charset)
         ~content_type:(match content_type with
                              | None -> Ocsigen_http_frame.Result.content_type res
                              | _ -> content_type
                           )
         ~headers:(match headers with
                         | None -> Ocsigen_http_frame.Result.headers res
                         | Some headers ->
                             Http_headers.with_defaults
                               headers (Ocsigen_http_frame.Result.headers res)
                      )
         ())

end

module Any = struct

  include Eliom_mkreg.Make_poly(Any_reg_base)

  type 'a result = 'a kind

  let send ?options ?charset ?code ?content_type ?headers content =
    Result_types.cast_result_lwt
      (Any_reg_base.send
         ?options ?charset ?code ?content_type ?headers content)

end

type 'a application_name = string

let appl_self_redirect send page =
      if Eliom_request_info.expecting_process_page ()
      then
        let url = Eliom_request_info.get_full_url () in
        let empty_result = Ocsigen_http_frame.Result.empty () in
        Lwt.return
          (Result_types.cast_result (Ocsigen_http_frame.Result.update empty_result
            ~headers:
              (Http_headers.add
                (Http_headers.name Eliom_common.half_xhr_redir_header) url
                (Ocsigen_http_frame.Result.headers empty_result)) ()))
      else
        let%lwt r = (Result_types.cast_function_http send) page in
        Lwt.return (Result_types.cast_result r)

let http_redirect = appl_self_redirect

(* File is a module allowing to register services that send files *)
module File_reg_base = struct

  type page = string
  type options = int
  type result = browser_content kind

  let result_of_http_result = Result_types.cast_result

  let send_appl_content = Eliom_service.XNever

  let send ?options ?charset ?code
      ?content_type ?headers filename =
    let sp = Eliom_common.get_sp () in
    let request = Eliom_request_info.get_request_sp sp in
    let file =
      try Ocsigen_local_files.resolve request filename ()
      with
        | Ocsigen_local_files.Failed_403 (* XXXBY : maybe we should signal a true 403? *)
        | Ocsigen_local_files.Failed_404
        | Ocsigen_local_files.NotReadableDirectory ->
            raise Eliom_common.Eliom_404
    in
    let%lwt r = Ocsigen_local_files.content ~request ~file in
    let open Ocsigen_extensions in
    let headers = match headers with
      | None -> (Ocsigen_http_frame.Result.headers r)
      | Some headers -> Http_headers.with_defaults headers (Ocsigen_http_frame.Result.headers r)
    in
    let headers = add_cache_header options headers in
    Lwt.return
      (Ocsigen_http_frame.Result.update r
          ~code:(code_of_code_option code)
          ~charset:(match charset with
                       | None ->
                           Some (Ocsigen_charset_mime.find_charset
                                   filename
                                   (Eliom_config.get_config_info_sp sp).charset_assoc)
                       | _ -> charset)
          ~content_type:(match content_type with
                           | None -> Ocsigen_http_frame.Result.content_type r
                           | _ -> content_type
                        )
          ~headers ())

end

module File =
struct
  include Eliom_mkreg.Make(File_reg_base)
  let check_file filename =
    let sp = Eliom_common.get_sp () in
    let request = Eliom_request_info.get_request_sp sp in
    try
      ignore (Ocsigen_local_files.resolve request filename ()
                : Ocsigen_local_files.resolved);
      true
    with
      | Ocsigen_local_files.Failed_403
      | Ocsigen_local_files.Failed_404
      | Ocsigen_local_files.NotReadableDirectory ->
        false
end

module File_ct_reg_base = struct

  type page = string * string
  type options = int
  type result = browser_content kind

  let result_of_http_result = Result_types.cast_result

  let send_appl_content = Eliom_service.XNever

  let send ?options ?charset ?code
      ?content_type ?headers (filename, ct) =
    let sp = Eliom_common.get_sp () in
    let request = Eliom_request_info.get_request_sp sp in
    let file =
      try Ocsigen_local_files.resolve request filename ()
      with
        | Ocsigen_local_files.Failed_403 (* XXXBY : maybe we should signal a true 403? *)
        | Ocsigen_local_files.Failed_404
        | Ocsigen_local_files.NotReadableDirectory ->
            raise Eliom_common.Eliom_404
    in
    let%lwt r = Ocsigen_local_files.content ~request ~file in
    let open Ocsigen_extensions in
    let headers = match headers with
      | None -> Ocsigen_http_frame.Result.headers r
      | Some headers -> Http_headers.with_defaults headers (Ocsigen_http_frame.Result.headers r)
    in
    let headers = add_cache_header options headers in
    Lwt.return
      (Ocsigen_http_frame.Result.update r
          ~code:(code_of_code_option code)
          ~charset:(match charset with
                       | None ->
                         Some (Ocsigen_charset_mime.find_charset
                                 filename
                                 (Eliom_config.get_config_info_sp sp).charset_assoc)
                       | _ -> charset)
          ~content_type:(match content_type with
              | None -> Some ct
              | _ -> content_type
            )
          ~headers ())

end

module File_ct =
struct
  include Eliom_mkreg.Make(File_ct_reg_base)
  let check_file filename =
    let sp = Eliom_common.get_sp () in
    let request = Eliom_request_info.get_request_sp sp in
    try
      ignore (Ocsigen_local_files.resolve request filename ()
                : Ocsigen_local_files.resolved);
      true
    with
      | Ocsigen_local_files.Failed_403
      | Ocsigen_local_files.Failed_404
      | Ocsigen_local_files.NotReadableDirectory ->
        false
end

module Streamlist_reg_base = struct

  type page = (((unit -> (string Ocsigen_stream.t) Lwt.t) list) * string)
  type options = unit
  type result = unknown_content kind

  let result_of_http_result = Result_types.cast_result

  let send_appl_content = Eliom_service.XNever

  let send ?options ?charset ?code
      ?content_type ?headers content =
    Ocsigen_senders.Streamlist_content.result_of_content content >>= fun r ->
    Lwt.return
      (Ocsigen_http_frame.Result.update r
         ~code:(code_of_code_option code)
         ~charset:(match charset with
                     | None ->  Some (Eliom_config.get_config_default_charset ())
                     | _ -> charset)
         ~content_type:(match content_type with
                          | None -> Ocsigen_http_frame.Result.content_type r
                          | _ -> content_type
                       )
         ~headers:(match headers with
                     | None -> Ocsigen_http_frame.Result.headers r
                     | Some headers ->
                         Http_headers.with_defaults
                           headers (Ocsigen_http_frame.Result.headers r)
                  )
         ())

end

module Streamlist = Eliom_mkreg.Make(Streamlist_reg_base)

module Customize
  (R : Eliom_registration_sigs.S_with_create)
  (T : sig type page val translate : page -> R.page Lwt.t end) = struct

    type page = T.page
    type return = R.return
    type options = R.options
    type result = R.result

  let make_eh = function
    | None -> None
    | Some eh -> Some (fun l -> eh l >>= T.translate)

  let make_service_handler f g p = f g p >>= T.translate

  let send ?options ?charset ?code ?content_type ?headers content =
    T.translate content >>= fun c ->
    R.send ?options ?charset ?code ?content_type ?headers c

  let register
      ?app
      ?scope
      ?options
      ?charset
      ?code
      ?content_type
      ?headers
      ?secure_session
      ~service
      ?error_handler
      (f : ('get -> 'post -> 'return Lwt.t)) =
    R.register
      ?app
      ?scope
      ?options
      ?charset
      ?code
      ?content_type
      ?headers
      ?secure_session
      ~service
      ?error_handler:(make_eh error_handler)
      (make_service_handler f)

  let create
      ?app
      ?scope
      ?options
      ?charset
      ?code
      ?content_type
      ?headers
      ?secure_session
      ?https
      ?name
      ?csrf_safe
      ?csrf_scope
      ?csrf_secure
      ?max_use
      ?timeout
      ~meth
      ~path
      ?error_handler
      f =
    R.create
      ?app
      ?scope
      ?options
      ?charset
      ?code
      ?content_type
      ?headers
      ?secure_session
      ?https
      ?name
      ?csrf_safe
      ?csrf_scope
      ?csrf_secure
      ?max_use
      ?timeout
      ~meth
      ~path
      ?error_handler:(make_eh error_handler)
      (make_service_handler f)

  let attach_get
      ?app
      ?scope
      ?options
      ?charset
      ?code
      ?content_type
      ?headers
      ?secure_session
      ?https
      ?name
      ?csrf_safe
      ?csrf_scope
      ?csrf_secure
      ?max_use
      ?timeout
      ~fallback
      ~get_params
      ?error_handler
      f =
    R.attach_get
      ?app
      ?scope
      ?options
      ?charset
      ?code
      ?content_type
      ?headers
      ?secure_session
      ?https
      ?name
      ?csrf_safe
      ?csrf_scope
      ?csrf_secure
      ?max_use
      ?timeout
      ~fallback
      ~get_params
      ?error_handler:(make_eh error_handler)
      (make_service_handler f)

  let attach_post
      ?app
      ?scope
      ?options
      ?charset
      ?code
      ?content_type
      ?headers
      ?secure_session
      ?https
      ?name
      ?csrf_safe
      ?csrf_scope
      ?csrf_secure
      ?max_use
      ?timeout
      ~fallback
      ~post_params
      ?error_handler
      f =
    R.attach_post
      ?app
      ?scope
      ?options
      ?charset
      ?code
      ?content_type
      ?headers
      ?secure_session
      ?https
      ?name
      ?csrf_safe
      ?csrf_scope
      ?csrf_secure
      ?max_use
      ?timeout
      ~fallback
      ~post_params
      ?error_handler:(make_eh error_handler)
      (make_service_handler f)

end

module Ocaml_reg_base = struct

  type page = string
  type options = unit
  type result = Ocsigen_http_frame.result

  let result_of_http_result x = x

  let send_appl_content = Eliom_service.XNever

  let send ?options ?charset ?code
      ?content_type ?headers content =
    Result_types.cast_kind_lwt
      (Text.send ?charset ?code
         ?content_type ?headers
         (content,
          Eliom_service.eliom_appl_answer_content_type))

end


module Ocaml = struct

  type 'a page = 'a
  type options = unit
  type 'a return = 'a Eliom_service.ocaml
  type 'a result = 'a ocaml_content kind

  module M = Eliom_mkreg.Make(Ocaml_reg_base)

  let prepare_data data =
    let ecs_request_data =
      let data = Eliom_syntax.get_request_data () in
      if not (Ocsigen_config.get_debugmode()) then
        Array.iter (fun d ->
          Eliom_runtime.Client_value_server_repr.clear_loc
            d.Eliom_runtime.value) data;
      data
    in
    (*     debug_client_value_data (debug "%s") client_value_data; *)
    let r = { Eliom_runtime.ecs_request_data;
              ecs_data = data } in
    Lwt.return (Eliom_types.encode_eliom_data r)

  let make_eh = function
    | None -> None
    | Some eh ->
        Some (fun l -> eh l >>= prepare_data)

  let make_service_handler f =
    fun g p ->
      let%lwt data =
        try%lwt
          let%lwt res = f g p in
          Lwt.return (`Success res)
        with exc ->
          Lwt.return (`Failure (Printexc.to_string exc))
      in
      prepare_data data

  let send ?options ?charset ?code ?content_type ?headers content =
    let%lwt content = prepare_data content in
    Result_types.cast_result_lwt
      (M.send ?options ?charset ?code
         ?content_type ?headers content)

  let register
      ?app
      ?scope
      ?options
      ?charset
      ?code
      ?content_type
      ?headers
      ?secure_session
      ~(service :
          ('get, 'post, _, _, _, Eliom_service.non_ext, Eliom_service.reg,
           _, _, _, 'return Eliom_service.ocaml) Eliom_service.t)
      ?(error_handler : ((string * exn) list -> 'return Lwt.t) option)
      (f : ('get -> 'post -> 'return Lwt.t)) =
    M.register
      ?app
      ?scope
      ?options
      ?charset
      ?code
      ?content_type
      ?headers
      ?secure_session
      ~service:(Eliom_service.untype service)
      ?error_handler:(make_eh error_handler)
      (make_service_handler f)

  let create
      ?app
      ?scope
      ?options
      ?charset
      ?code
      ?content_type
      ?headers
      ?secure_session
      ?https
      ?name
      ?csrf_safe
      ?csrf_scope
      ?csrf_secure
      ?max_use
      ?timeout
      ~meth
      ~path
      ?error_handler
      f =
    Eliom_service.untype @@
    M.create
      ?app
      ?scope
      ?options
      ?charset
      ?code
      ?content_type
      ?headers
      ?secure_session
      ?https
      ?name
      ?csrf_safe
      ?csrf_scope
      ?csrf_secure
      ?max_use
      ?timeout
      ~meth
      ~path
      ?error_handler:(make_eh error_handler)
      (make_service_handler f)

  let attach_get
      ?app
      ?scope
      ?options
      ?charset
      ?code
      ?content_type
      ?headers
      ?secure_session
      ?https
      ?name
      ?csrf_safe
      ?csrf_scope
      ?csrf_secure
      ?max_use
      ?timeout
      ~fallback
      ~get_params
      ?error_handler
      f =
    Eliom_service.untype @@
    M.attach_get
      ?app
      ?scope
      ?options
      ?charset
      ?code
      ?content_type
      ?headers
      ?secure_session
      ?https
      ?name
      ?csrf_safe
      ?csrf_scope
      ?csrf_secure
      ?max_use
      ?timeout
      ~fallback:(Eliom_service.untype fallback)
      ~get_params
      ?error_handler:(make_eh error_handler)
      (make_service_handler f)

  let attach_post
      ?app
      ?scope
      ?options
      ?charset
      ?code
      ?content_type
      ?headers
      ?secure_session
      ?https
      ?name
      ?csrf_safe
      ?csrf_scope
      ?csrf_secure
      ?max_use
      ?timeout
      ~fallback
      ~post_params
      ?error_handler
      f =
    Eliom_service.untype @@
    M.attach_post
      ?app
      ?scope
      ?options
      ?charset
      ?code
      ?content_type
      ?headers
      ?secure_session
      ?https
      ?name
      ?csrf_safe
      ?csrf_scope
      ?csrf_secure
      ?max_use
      ?timeout
      ~fallback:(Eliom_service.untype fallback)
      ~post_params
      ?error_handler:(make_eh error_handler)
      (make_service_handler f)

end

(** [{do_not_launch = true}]: do not launch the client side program if
    it is not already launched.  Default: [false]. *)
type appl_service_options = { do_not_launch : bool }

let default_appl_service_options = {do_not_launch = false; }

let comet_service_key : unit Polytables.key = Polytables.make_key ()

let request_template =
  Eliom_reference.eref ~scope:Eliom_common.request_scope None

let global_data_unwrapper =
  Eliom_wrap.create_unwrapper
    (Eliom_wrap.id_of_int Eliom_runtime.global_data_unwrap_id_int)

module Eliom_appl_reg_make_param
  (Html_content
     : Ocsigen_http_frame.HTTP_CONTENT
       with type t = [ `Html ] Eliom_content.Html.elt
       and type options = Http_headers.accept Lazy.t)
  (App_param : Eliom_registration_sigs.APP_PARAM) = struct

  type app_id

  type page = Html_types.html Eliom_content.Html.elt
  type options = appl_service_options
  type result = app_id application_content kind

  let result_of_http_result = Result_types.cast_result

  let is_initial_request () =
    let sp = Eliom_common.get_sp () in
    sp.Eliom_common.sp_client_appl_name <> Some App_param.application_name

  let eliom_appl_script_id : [ `Script ] Eliom_content.Html.Id.id =
    Eliom_content.Html.Id.new_elt_id ~global:true ()
  let application_script ?(defer = false) ?(async = false) () =
    let a =
      (if defer then [Eliom_content.Html.D.a_defer ()] else [])
        @
      (if async then [Eliom_content.Html.D.a_async ()] else [])
    in
    Eliom_content.Html.Id.create_named_elt
      ~id:eliom_appl_script_id
      (Eliom_content.Html.D.js_script ~a
         ~uri:(Eliom_content.Html.D.make_uri
                 ~service:(Eliom_service.static_dir ())
                 [App_param.application_name ^ ".js"])
         ())
  let application_script =
    (application_script
     : ?defer:_ -> ?async:_ -> _ -> [ `Script ] Eliom_content.Html.elt
     :> ?defer:_ -> ?async:_ -> _ -> [> `Script ] Eliom_content.Html.elt)
  let is_eliom_appl_script elt =
    Eliom_content.Html.Id.have_id eliom_appl_script_id elt

  let eliom_appl_data_script_id =
    Eliom_content.Html.Id.new_elt_id ~global:true ()

  let make_eliom_appl_data_script ~sp =

    let script =
      Printf.sprintf
        "var __eliom_appl_sitedata = \'%s\';\n\
         var __eliom_appl_process_info = \'%s\'\n\
         var __eliom_request_data;\n\
         var __eliom_request_cookies;\n\
         var __eliom_request_template;\n"
        (Eliom_lib.jsmarshal (Eliommod_cli.client_sitedata sp))
        (Eliom_lib.jsmarshal (sp.Eliom_common.sp_client_process_info))
    in

    Lwt.return
      Eliom_content.Html.(
        Id.create_named_elt ~id:eliom_appl_data_script_id
          (F.script (F.cdata_script script)))

  let queue_map (q : 'a Queue.t) (f : 'a -> 'b) : 'b Queue.t =
    let q2 = Queue.create () in
    Queue.iter (fun x ->
        let y = f x in
        Queue.add y q2
      ) q;
    q2

  let make_eliom_data_script ?(keep_debug=false) ~sp page =

    let ejs_global_data =
      if is_initial_request () then
        let data = Eliom_syntax.get_global_data () in
        let data =
          if keep_debug
          then data
          else
            Eliom_lib.String_map.map
              (fun {Eliom_runtime.server_sections_data;
                    client_sections_data} ->
                 Array.iter
                   (Array.iter (fun d ->
                      Eliom_runtime.Client_value_server_repr.clear_loc
                        d.Eliom_runtime.value))
                   server_sections_data;
              { Eliom_runtime.server_sections_data;
                client_sections_data = Array.map
                    (
                      Array.map (fun x ->
                        {x with
                         Eliom_runtime.injection_dbg = None})
                    )
                    client_sections_data
              }) data
        in
        Some (data, global_data_unwrapper)
      else None
    in
    let ejs_request_data =
      let data = Eliom_syntax.get_request_data () in
      if not keep_debug then
        Array.iter (fun d ->
          Eliom_runtime.Client_value_server_repr.clear_loc
            d.Eliom_runtime.value) data;
      data
    in

    (* wrapping of values could create eliom references that may
       create cookies that needs to be sent along the page. Hence,
       cookies should be calculated after wrapping. *)
    let eliom_data =
      Eliom_content.Xml.wrap (Eliom_content.Html.D.toelt page) { Eliom_common.
        ejs_global_data;
        ejs_request_data;
        ejs_event_handler_table = Eliom_content.Xml.make_event_handler_table (Eliom_content.Html.D.toelt page);
        ejs_client_attrib_table = Eliom_content.Xml.make_client_attrib_table (Eliom_content.Html.D.toelt page);
        ejs_sess_info           = Eliommod_cli.client_si sp.Eliom_common.sp_si;
      } in

    let%lwt tab_cookies =
      Eliommod_cookies.compute_cookies_to_send
        sp.Eliom_common.sp_sitedata
        sp.Eliom_common.sp_tab_cookie_info
        sp.Eliom_common.sp_user_tab_cookies
    in

    let%lwt template = Eliom_reference.get request_template in

    let script =
      Printf.sprintf
        "__eliom_request_data = \'%s\';\n\
         __eliom_request_cookies = \'%s\';\n\
         __eliom_request_template = \'%s\';"
        (Eliom_lib.jsmarshal eliom_data)
        (Eliom_lib.jsmarshal tab_cookies)
        (Eliom_lib.jsmarshal (template: string option))
    in
    Lwt.return Eliom_content.Html.(F.script (F.cdata_script script))

  let split_page page :
      (Html_types.html_attrib Eliom_content.Html.attrib list
        * (Html_types.head_attrib Eliom_content.Html.attrib list
           * Html_types.title Eliom_content.Html.elt
            * Html_types.head_content_fun Eliom_content.Html.elt list)
        * Html_types.body Eliom_content.Html.elt ) =
    match Eliom_content.Xml.content page with
      | Eliom_content.Xml.Node (_, html_attribs, [head; body]) ->
        begin match Eliom_content.Xml.content head with
          | Eliom_content.Xml.Node (_, head_attribs, head_elts) ->
            ( List.map Eliom_content.Html.D.to_attrib html_attribs,
              ( List.map Eliom_content.Html.D.to_attrib head_attribs,
                Eliom_content.Html.D.tot (List.hd head_elts),
                Eliom_content.Html.D.totl (List.tl head_elts) ),
              Eliom_content.Html.D.tot body )
          | _ -> assert false
        end
      | _ -> assert false

  let add_eliom_scripts ~sp page =

    let%lwt appl_data_script = make_eliom_appl_data_script ~sp in

    (* First we build a fake page to build the ref_tree... *)
    let (html_attribs, (head_attribs, title, head_elts), body) =
      split_page (Eliom_content.Html.D.toelt page) in
    let encode_slashs = List.map (Eliom_lib.Url.encode ~plus:false) in
    let base_url =
      Eliom_uri.make_proto_prefix
        (Eliom_config.default_protocol_is_https () ||
         Eliom_request_info.get_csp_ssl_sp sp)
      ^
      (String.concat "/"
         (encode_slashs (Eliom_request_info.get_csp_original_full_path ()))
      )
    in
    let head_elts =
      if List.exists is_eliom_appl_script head_elts
      then head_elts
      else (head_elts @ [application_script ()])
    in
    let head_elts =
      appl_data_script
      (* <base> elt is added only for xhr done by client process,
         because in that case, URLs are relative to the URL of
         the first page, not the current URL.
         We don't want to put base for non-xhr,
         to make it possible to have truly relative URLs in HTML pages.
      *)
      :: if Eliom_request_info.expecting_process_page ()
      then
        Eliom_content.Html.(
          F.base
            ~a:[F.a_id Eliom_common_base.base_elt_id;
                F.a_href (Eliom_content.Xml.uri_of_string base_url)]
            ())
        :: head_elts
      else head_elts
    in
    let fake_page =
      Eliom_content.Html.F.html ~a:html_attribs
        (Eliom_content.Html.F.head ~a:head_attribs title head_elts)
        body
    in
    let%lwt data_script = make_eliom_data_script
        ~keep_debug:(Ocsigen_config.get_debugmode ())
        ~sp fake_page
    in

    (* Then we replace the faked data_script *)
    let head_elts =
      List.hd head_elts :: data_script :: (List.tl head_elts) in
    Lwt.return
      (Eliom_content.Html.F.html ~a:html_attribs
         (Eliom_content.Html.F.head ~a:head_attribs title head_elts)
         body )

  let remove_eliom_scripts page =
    let (html_attribs, (head_attribs, title, head_elts), body) =
      split_page (Eliom_content.Html.D.toelt page) in
    let head_elts = List.filter (fun x -> not (is_eliom_appl_script x)) head_elts in
    Lwt.return
      (Eliom_content.Html.F.html ~a:html_attribs
         (Eliom_content.Html.F.head ~a:head_attribs title head_elts)
         body )

  let send_appl_content = Eliom_service.XSame_appl (App_param.application_name, None)

  let send ?(options = default_appl_service_options) ?charset ?code
      ?content_type ?headers content =


    let sp = Eliom_common.get_sp () in

    (* GRGR FIXME et si le nom de l'application diffère ?? Il faut
       renvoyer un full_redirect... TODO *)
    if sp.Eliom_common.sp_client_appl_name <> Some App_param.application_name then

      Eliom_state.set_cookie
        ~cookie_level:`Client_process
        ~name:Eliom_common.appl_name_cookie_name
        ~value:App_param.application_name ();

    let%lwt page =
      match sp.Eliom_common.sp_client_appl_name, options.do_not_launch with
        | None, true -> remove_eliom_scripts content
        | _ -> add_eliom_scripts ~sp content in

    let ri = Eliom_request_info.get_ri () in
    let accept = Ocsigen_extensions.Ocsigen_request_info.accept ri in
    let%lwt r = Html_content.result_of_content ~options:accept page in

    let headers =
      match headers with
        | None -> Ocsigen_http_frame.Result.headers r
        | Some headers ->
          Http_headers.with_defaults headers (Ocsigen_http_frame.Result.headers r)
    in
    let headers = Http_headers.replace
      (Http_headers.name Eliom_common_base.appl_name_header_name)
      App_param.application_name
      headers
    in

    let rc = Eliom_request_info.get_request_cache () in
    let headers =
      try
        (* If it is a suffix service with redirection,
           we may have to normalize the uri *)
        Http_headers.replace
          (Http_headers.name Eliom_common_base.response_url_header)
          (Polytables.get ~table:rc ~key:Eliom_mkreg.suffix_redir_uri_key)
        headers
      with Not_found ->
        headers
    in

    Lwt.return
      (Ocsigen_http_frame.Result.update r
        ~code:(code_of_code_option code)
        ~charset:(match charset with
          | None -> Some (Eliom_config.get_config_default_charset ())
          | _ -> charset
        )
        ~content_type:
          (if Eliom_request_info.expecting_process_page ()
           then Ocsigen_http_frame.Result.content_type r
           else (match content_type with
            | None -> Ocsigen_http_frame.Result.content_type r
            | _ -> content_type))
        ~headers ())

  end

module type APP = sig
  val application_script :
    ?defer:bool -> ?async:bool -> unit -> [> `Script ] Eliom_content.Html.elt
  val application_name : string
  val is_initial_request : unit -> bool
  type app_id
  type page = Html_types.html Eliom_content.Html.elt
  type options = appl_service_options
  type return = Eliom_service.non_ocaml
  type result = app_id application_content kind
  include Eliom_registration_sigs.S_with_create
    with type page    := page
     and type options := options
     and type return  := return
     and type result  := result
  val typed_name : app_id application_name
end

module App

    (App_param : Eliom_registration_sigs.APP_PARAM) : APP =

struct

  module P =
    Eliom_appl_reg_make_param
      (Ocsigen_senders.Make_XML_Content
         (Eliom_content.Xml)
         (Eliom_content.Html.D))
      (App_param)

  type app_id = P.app_id

  module Eliom_appl_registration = Eliom_mkreg.Make(P)

  include Eliom_appl_registration

  (** Unique identifier for this application.
      It is the application name.
      Warning: do not mix up with the "application instance id",
      that is unique for each instance of the application.
  *)
  let application_name = App_param.application_name
  let typed_name = App_param.application_name
  let is_initial_request = P.is_initial_request

  let application_script = P.application_script

  let data_service_handler () () =
    Lwt.return (Eliom_syntax.get_global_data (), global_data_unwrapper)

  let _ =
    match App_param.global_data_path with
    | Some global_data_path ->
      ignore @@
      Ocaml.create
        ~path:(Eliom_service.Path global_data_path)
        ~meth:(Get Eliom_parameter.unit)
        ~https:true
        data_service_handler
    | None ->
      ()

end

module type TMPL_PARAMS = sig
  type t
  val name: string
  val make_page: t -> Html_types.html Eliom_content.Html.elt Lwt.t
  val update: t -> unit Eliom_client_value.t
end

module Eliom_tmpl_reg_make_param
    (Appl : APP)
    (Tmpl_param : TMPL_PARAMS) = struct

  type page = Tmpl_param.t
  type options = appl_service_options
  type result = Appl.app_id application_content kind

  let result_of_http_result = Result_types.cast_result

  let send_appl_content = Eliom_service.XSame_appl (Appl.application_name, Some Tmpl_param.name)

  let nl_template =
    Eliom_parameter.make_non_localized_parameters
      ~prefix:"eliom" ~name:"template"
      (Eliom_parameter.string "name")

  let send ?(options = default_appl_service_options) ?charset ?code
      ?content_type ?headers content =
    match
      Eliom_parameter.get_non_localized_get_parameters nl_template
    with
    | None ->
      let%lwt () = Eliom_reference.set request_template (Some Tmpl_param.name) in
      let%lwt content = Tmpl_param.make_page content in
      Result_types.cast_kind_lwt
        (Appl.send ~options ?charset ?code ?content_type ?headers content)
    | Some _ ->
      ignore (Tmpl_param.update content);
      Result_types.cast_kind_lwt
        (Ocaml.send ?charset ?code ?content_type ?headers ())

end

module Eliom_tmpl(App : APP)(Tmpl_param : TMPL_PARAMS) =
  Eliom_mkreg.Make(Eliom_tmpl_reg_make_param(App)(Tmpl_param))

(** Redirection services are like services, but send a redirection
    instead of a page.

    The HTTP/1.1 RFC says: If the 301 status code is received in
    response to a request other than GET or HEAD, the user agent MUST
    NOT automatically redirect the request unless it can be confirmed
    by the user, since this might change the conditions under which
    the request was issued.

    Here redirections are done towards services without parameters.
    (possibly preapplied). *)
module String_redir_reg_base = struct

  type page = Eliom_lib.Url.uri
  type options =
    [ `MovedPermanently
    | `Found
    | `SeeOther
    | `NotNodifed
    | `UseProxy
    | `TemporaryRedirect ]
  type result = browser_content kind

  let result_of_http_result = Result_types.cast_result

  let send_appl_content = Eliom_service.XAlways
  (* actually, the service will decide itself *)

  let send ?(options = `Found) ?charset ?code
      ?content_type ?headers content =
    let uri = content in
    let empty_result = Ocsigen_http_frame.Result.empty () in
    let content_type = match content_type with
      | None -> Ocsigen_http_frame.Result.content_type empty_result
      | _ -> content_type
    in
    let headers = match headers with
      | None -> Ocsigen_http_frame.Result.headers empty_result
      | Some headers ->
        Http_headers.with_defaults
          headers (Ocsigen_http_frame.Result.headers empty_result)
    in

    (* Now we decide the kind of redirection we do.
       If the request is an xhr done by a client side Eliom program
       expecting a process page,
       we do not send an HTTP redirection.
       In that case, we send a half xhr redirection.
    *)
    if not (Eliom_request_info.expecting_process_page ())
    then (* the browser did not ask application eliom data,
            we send a regular redirection *)
      let code = match code with
        | Some c -> c
        | None ->
          match options with
          | `MovedPermanently -> 301
          | `Found -> 302
          | `SeeOther -> 303
          | `NotNodifed -> 304
          | `UseProxy -> 305
          | `TemporaryRedirect -> 307
      in
      Lwt.return
        (Ocsigen_http_frame.Result.update empty_result
          ~code
          ~location:(Some uri)
          ~content_type
          ~headers ())
    else
      Lwt.return
        (Ocsigen_http_frame.Result.update empty_result
          ~content_type
          ~headers:
            (Http_headers.add
              (Http_headers.name Eliom_common.half_xhr_redir_header)
              uri headers)
          ())


end

module String_redirection = Eliom_mkreg.Make(String_redir_reg_base)

type _ redirection =
    Redirection :
      (unit, unit, Eliom_service.get , _, _, _, _,
       [ `WithoutSuffix ], unit, unit, 'a) Eliom_service.t ->
    'a redirection

module Redir_reg_base = struct

  type 'a page = 'a redirection

  type options =
    [ `MovedPermanently
    | `Found
    | `SeeOther
    | `NotNodifed
    | `UseProxy
    | `TemporaryRedirect ]

  type 'a return = 'a

  type 'a result = unknown_content kind

  let result_of_http_result = Result_types.cast_result

  let send_appl_content = Eliom_service.XAlways
  (* actually, the service will decide itself *)

  let send ?(options = `Found) ?charset ?code
      ?content_type ?headers (Redirection service) =
    let uri = Eliom_uri.make_string_uri ~service () in
    let empty_result = Ocsigen_http_frame.Result.empty () in
    let content_type = match content_type with
      | None -> Ocsigen_http_frame.Result.content_type empty_result
      | _ -> content_type
    in
    let headers = match headers with
      | None -> Ocsigen_http_frame.Result.headers empty_result
      | Some headers ->
        Http_headers.with_defaults
          headers (Ocsigen_http_frame.Result.headers empty_result)
    in

    (* Now we decide the kind of redirection we do.
       If the request is an xhr done by a client side Eliom program
       expecting a process page,
       we do not send an HTTP redirection.
       In that case, we send:
       - a full xhr redirection if the application to which belongs
       the destination service is the same (thus it will send back tab cookies)
       (simulate a redirection without stopping the client process)
       - a half xhr redirection otherwise (i.e. ask the browser to do an
       actual redirection).
    *)
    match Eliom_request_info.expecting_process_page (),
      Eliom_request_info.get_sp_client_appl_name () with
      (* the appl name as sent by browser *)
      | true, None (* should not happen *)
      | false, _ -> (* the browser did not ask for process data,
                       we send a regular redirection *)
        let code = match code with
          | Some c -> c
          | None ->
          match options with
          | `MovedPermanently -> 301
          | `Found -> 302
          | `SeeOther -> 303
          | `NotNodifed -> 304
          | `UseProxy -> 305
          | `TemporaryRedirect -> 307
        in
        Lwt.return
          (Ocsigen_http_frame.Result.update empty_result
            ~code
            ~location:(Some uri)
            ~content_type
            ~headers ())

      | true, Some anr ->
        (* the browser asked application eliom data
           for the application called anr *)
        (* If it comes from an xhr, we use answer with a special header field *)
        let headers = Http_headers.replace
          (Http_headers.name Eliom_common_base.appl_name_header_name)
          anr
          headers
        in
        match Eliom_service.send_appl_content service with
          (* the appl name of the destination service *)
            | Eliom_service.XSame_appl (an,_) when (an = anr) ->
            (* Same appl, we do a full xhr redirection
               (not an http redirection, because we want to
               send back tab cookies) *)
              Lwt.return
                (Ocsigen_http_frame.Result.update empty_result
                  ~content_type
                  ~headers:
                    (Http_headers.add
                      (Http_headers.name Eliom_common.full_xhr_redir_header)
                      uri headers) ())

            | Eliom_service.XAlways ->
            (* It is probably an action, or a void coservice. Full xhr again *)
              Lwt.return
                (Ocsigen_http_frame.Result.update empty_result
                  ~content_type
                  ~headers:
                    (Http_headers.add
                      (Http_headers.name Eliom_common.full_xhr_redir_header)
                      uri headers) ())

            | _ -> (* No application, or another application.
                      We ask the browser to do an HTTP redirection. *)
              Lwt.return
                (Ocsigen_http_frame.Result.update empty_result
                  ~content_type
                  ~headers:
                    (Http_headers.add
                      (Http_headers.name Eliom_common.half_xhr_redir_header)
                      uri headers) ())

end

module Redirection = struct

  include Eliom_mkreg.Make_poly(Redir_reg_base)

  let send ?options ?charset ?code ?content_type ?headers content =
    Result_types.cast_result_lwt
      (Redir_reg_base.send
         ?options ?charset ?code ?content_type ?headers content)

end

let set_exn_handler h =
  let sitedata = Eliom_request_info.find_sitedata "set_exn_handler" in
  Eliom_request_info.set_site_handler sitedata (Result_types.cast_function_http h)

module String = Text
