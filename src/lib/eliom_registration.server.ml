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

open Lwt.Infix

let headers_with_content_type ?charset ?content_type headers =
  match content_type with
  | Some content_type ->
      let charset =
        if charset <> None
        then charset
        else if String.length content_type >= 5
                && (String.sub content_type 0 5 = "text/"
                   ||
                   let suffix =
                     String.sub content_type (String.length content_type - 4) 4
                   in
                   suffix = "/xml" || suffix = "=xml")
        then Some (Eliom_config.get_config_default_charset ())
        else None
      in
      Cohttp.Header.replace headers
        Ocsigen_header.Name.(to_string content_type)
        (match charset with
        | Some charset -> Printf.sprintf "%s; charset=%s" content_type charset
        | None -> content_type)
  | None -> headers

let result_of_content ?charset ?content_type ?headers ?status body =
  let headers =
    match content_type with
    | Some content_type ->
        let headers = Ocsigen_header.of_option headers in
        Some (headers_with_content_type ?charset ~content_type headers)
    | None -> headers
  in
  let headers =
    Cohttp.Header.add_unless_exists
      (Ocsigen_header.of_option headers)
      "cache-control" "no-cache"
  in
  let response = Cohttp.Response.make ?status ~headers () in
  Lwt.return (Ocsigen_response.make ~body response)

module Result_types : sig
  type 'a kind

  val cast_result : Ocsigen_response.t -> 'a kind
  val cast_kind : 'a kind -> Ocsigen_response.t
  val cast_kind_lwt : 'a kind Lwt.t -> Ocsigen_response.t Lwt.t
  val cast_result_lwt : Ocsigen_response.t Lwt.t -> 'a kind Lwt.t

  val cast_function_http :
     ('c -> 'a kind Lwt.t)
    -> 'c
    -> Ocsigen_response.t Lwt.t
end = struct
  type 'a kind = Ocsigen_response.t

  let cast_result x = x
  let cast_kind x = x
  let cast_kind_lwt x = x
  let cast_result_lwt x = x
  let cast_function_http x = x
end

type 'a kind = 'a Result_types.kind
type 'a application_content = [`Appl of 'a]
type block_content
type browser_content = [`Browser]
type 'a ocaml_content
type unknown_content

let cast_unknown_content_kind (x : unknown_content kind) : 'a kind =
  Result_types.cast_result (Result_types.cast_kind x)

let cast_http_result = Result_types.cast_result

let content_type_html content_type =
  let sp = Eliom_common.get_sp () in
  match
    content_type, sp.Eliom_common.sp_sitedata.Eliom_common.html_content_type
  with
  | None, Some content_type -> content_type
  | Some content_type, _ -> content_type
  | None, None ->
      let accept =
        Ocsigen_request.header_multi
          (Eliom_request_info.get_ri ())
          Ocsigen_header.Name.accept
      in
      let accept = Ocsigen_header.Accept.parse accept in
      Ocsigen_header.Content_type.choose accept
        Eliom_content.Html.D.Info.content_type
        Eliom_content.Html.D.Info.alternative_content_types

module Html_base = struct
  type page = Html_types.html Eliom_content.Html.elt
  type options = unit
  type result = browser_content kind

  let result_of_http_result = Result_types.cast_result
  let send_appl_content = Eliom_service.XNever

  let out =
    let encode x = fst (Xml_print.Utf8.normalize_html x) in
    Eliom_content.Html.Printer.pp ~encode ()

  let send ?options:_ ?charset ?code ?content_type ?headers c =
    let status = Eliom_lib.Option.map Cohttp.Code.status_of_code code
    and content_type = content_type_html content_type
    and body = Cohttp_lwt.Body.of_string (Format.asprintf "%a" out c) in
    result_of_content ?charset ?headers ?status ~content_type body
end

module Html = Eliom_mkreg.Make (Html_base)

module Flow5_base = struct
  type options = unit
  type result = block_content kind
  type page = Html_types.flow5 Eliom_content.Html.elt list

  let result_of_http_result = Result_types.cast_result
  let send_appl_content = Eliom_service.XNever

  let out =
    let encode x = fst (Xml_print.Utf8.normalize_html x) in
    Eliom_content.Html.Printer.pp_elt ~encode ()

  let body l =
    Lwt_stream.of_list l
    |> Lwt_stream.map (Format.asprintf "%a" out)
    |> Cohttp_lwt.Body.of_stream

  let send ?options:_ ?charset ?code ?content_type ?headers c =
    let status = Eliom_lib.Option.map Cohttp.Code.status_of_code code
    and content_type = content_type_html content_type
    and body = body c in
    result_of_content ?charset ?headers ?status ~content_type body
end

module Flow5 = Eliom_mkreg.Make (Flow5_base)

let add_cache_header cache headers =
  let ( <-< ) h (n, v) =
    Cohttp.Header.replace h (Ocsigen_header.Name.to_string n) v
  in
  match cache with
  | None -> headers
  | Some 0 -> headers <-< (Ocsigen_header.Name.cache_control, "no-cache")
  | Some duration ->
      headers
      <-< ( Ocsigen_header.Name.cache_control
          , "max-age: " ^ string_of_int duration )

module String_base = struct
  type page = string * string
  type options = int
  type result = unknown_content kind

  let result_of_http_result = Result_types.cast_result
  let send_appl_content = Eliom_service.XNever

  let send ?options ?charset ?code ?content_type:_ ?headers (c, content_type) =
    let status = Eliom_lib.Option.map Cohttp.Code.status_of_code code
    and body = Cohttp_lwt.Body.of_string c
    and headers = add_cache_header options (Ocsigen_header.of_option headers) in
    result_of_content ?charset ?status ~content_type ~headers body
end

module String = Eliom_mkreg.Make (String_base)

module CssText_base = struct
  type page = string
  type options = int
  type result = browser_content kind

  let result_of_http_result = Result_types.cast_result
  let send_appl_content = Eliom_service.XNever

  let send ?options ?charset ?code ?content_type ?headers content =
    String_base.send ?options ?charset ?code ?content_type ?headers
      (content, "text/css")
end

module CssText = Eliom_mkreg.Make (CssText_base)

module Html_text_base = struct
  type page = string
  type options = unit
  type result = browser_content kind

  let result_of_http_result = Result_types.cast_result
  let send_appl_content = Eliom_service.XNever

  let send ?options:_ ?charset ?code ?content_type ?headers content =
    String_base.send ?charset ?code ?content_type ?headers (content, "text/html")
end

module Html_text = Eliom_mkreg.Make (Html_text_base)

(** Actions are like services, but do not generate any page. The current
   page is reloaded (but if you give the optional parameter
    [~options:`NoReload] to the registration function).
 *)
module Action_base = struct
  type page = unit
  type options = [`Reload | `NoReload]
  type result = browser_content kind

  let result_of_http_result = Result_types.cast_result
  let send_appl_content = Eliom_service.XAlways
  (* The post action service will decide later *)

  let send_directly ri res =
    (* send bypassing the following directives in the configuration
       file (they have already been taken into account) *)
    Polytables.set
      ~table:(Ocsigen_request.request_cache ri)
      ~key:Eliom_common.found_stop_key ~value:();
    res

  let update_request ri si cookies_override =
    Ocsigen_request.update ri ~post_data:None ~meth:`GET ~cookies_override
      ~get_params_flat:si.Eliom_common.si_other_get_params

  let send ?(options = `Reload) ?charset ?(code = 204) ?content_type ?headers ()
    =
    let user_cookies = Eliom_request_info.get_user_cookies () in
    match options with
    | `NoReload ->
        let headers = Ocsigen_header.of_option headers in
        let headers =
          match Eliom_request_info.get_sp_client_appl_name () with
          | Some anr ->
              Cohttp.Header.replace headers
                Eliom_common_base.appl_name_header_name anr
          | _ -> headers
        and status = Cohttp.Code.status_of_code code in
        result_of_content ?charset ?content_type ~headers ~status
          Cohttp_lwt.Body.empty
    | `Reload -> (
        (* It is an action, we reload the page. To do that, we retry
         without POST params.

         If no post param at all, we retry without GET non_att info.

         If no GET non_att info, we retry without GET state.

         If no GET state, we do not reload, otherwise it will
         loop.

         Be very careful while re-reading this. *)
        let sp = Eliom_common.get_sp () in
        let sitedata = Eliom_request_info.get_sitedata_sp ~sp in
        let si = Eliom_request_info.get_si sp in
        let ri = Eliom_request_info.get_request_sp sp in
        let open Ocsigen_extensions in
        match
          ( si.Eliom_common.si_nonatt_info
          , si.Eliom_common.si_state_info
          , Ocsigen_request.meth ri.request_info )
        with
        | ( Eliom_common.RNa_no
          , (Eliom_common.RAtt_no, Eliom_common.RAtt_no)
          , `GET ) ->
            Lwt.return (Ocsigen_response.make (Cohttp.Response.make ()))
        | _ ->
            let all_cookie_info = sp.Eliom_common.sp_cookie_info in
            let%lwt ric =
              Eliommod_cookies.compute_new_ri_cookies (Unix.time ())
                (Ocsigen_request.sub_path ri.request_info)
                (Ocsigen_request.cookies ri.request_info)
                all_cookie_info user_cookies
            in
            let%lwt all_new_cookies =
              Eliommod_cookies.compute_cookies_to_send sitedata all_cookie_info
                user_cookies
            in
            (* Now tab cookies:

           As tab cookies are sent only by Eliom_app services,
           we just need to keep them in rc.

           If the fallback service is not Eliom_app, they will be
           lost. *)
            let rc = Eliom_request_info.get_request_cache_sp sp in
            Polytables.set ~table:rc
              ~key:Eliom_common.tab_cookie_action_info_key
              ~value:
                ( sp.Eliom_common.sp_tab_cookie_info
                , sp.Eliom_common.sp_user_tab_cookies
                , si.Eliom_common.si_tab_cookies );
            (* Remove some parameters to choose the following service *)
            Polytables.set
              ~table:
                (Ocsigen_request.request_cache
                   ri.Ocsigen_extensions.request_info)
              ~key:Eliom_common.eliom_params_after_action
              ~value:
                ( si.Eliom_common.si_all_get_params
                , si.Eliom_common.si_all_post_params
                , (* is Some [] *)
                  si.Eliom_common.si_all_file_params
                , (* is Some [] *)
                  si.Eliom_common.si_nl_get_params
                , si.Eliom_common.si_nl_post_params
                , si.Eliom_common.si_nl_file_params
                , si.Eliom_common.si_all_get_but_nl
                , si.Eliom_common.si_ignored_get_params
                , si.Eliom_common.si_ignored_post_params );
            (*VVV Also put all_cookie_info in this, to avoid
          update_cookie_table and get_cookie_info (?) *)
            let ri = update_request ri.request_info si ric in
            let%lwt () =
              Eliommod_pagegen.update_cookie_table sitedata all_cookie_info
            in
            send_directly ri
              (Ocsigen_extensions.compute_result
                 ~previous_cookies:all_new_cookies ri))
end

module Action = Eliom_mkreg.Make (Action_base)

(** Unit services are like services, do not generate any page, and do not
    reload the page. To be used carefully. Probably not useful at all.
    (Same as {!Action} with [`NoReload] option).
 *)

module Unit_base = struct
  type page = unit
  type options = unit
  type result = browser_content kind

  let result_of_http_result = Result_types.cast_result
  let send_appl_content = Eliom_service.XAlways

  let send ?options:_ ?charset ?(code = 204) ?content_type ?headers _content =
    let status = Cohttp.Code.status_of_code code in
    result_of_content ?charset ?content_type ?headers ~status
      Cohttp_lwt.Body.empty
end

module Unit = Eliom_mkreg.Make (Unit_base)

(* Any is a module allowing to register services that decide
   themselves what they want to send.  *)
module Any_base = struct
  type 'a page = 'a kind
  type options = unit
  type 'a return = Eliom_service.non_ocaml

  (* let send_appl_content = Eliom_service.XNever *)
  let send_appl_content = Eliom_service.XAlways

  let send ?options:_ ?charset ?code:_ ?content_type ?headers:_
      (result : 'a kind)
    =
    let result = Result_types.cast_kind result in
    let cohttp_response = fst (Ocsigen_response.to_cohttp result) in
    let headers =
      headers_with_content_type ?charset ?content_type
        (Cohttp.Response.headers cohttp_response)
    in
    let response = {cohttp_response with Cohttp.Response.headers} in
    Lwt.return (Ocsigen_response.update ~response result)
end

module Any = struct
  include Eliom_mkreg.Make_poly (Any_base)

  type 'a result = 'a kind

  let send ?options ?charset ?code ?content_type ?headers content =
    Result_types.cast_result_lwt
      (Any_base.send ?options ?charset ?code ?content_type ?headers content)
end

type 'a application_name = string

let appl_self_redirect send page =
  if Eliom_request_info.expecting_process_page ()
  then
    let response =
      let headers =
        Cohttp.Header.(add (init ()))
          Eliom_common.half_xhr_redir_header
          (Eliom_request_info.get_full_url ())
      in
      Cohttp.Response.make ~headers ()
    in
    Ocsigen_response.make response |> Result_types.cast_result |> Lwt.return
  else
    let%lwt r = (Result_types.cast_function_http send) page in
    Lwt.return (Result_types.cast_result r)

(* File is a module allowing to register services that send files *)
module File_base = struct
  type page = string
  type options = int
  type result = browser_content kind

  let result_of_http_result = Result_types.cast_result
  let send_appl_content = Eliom_service.XNever

  let send ?options ?charset ?code:_ ?content_type ?headers filename =
    let sp = Eliom_common.get_sp () in
    let request = Eliom_request_info.get_request_sp sp in
    match
      try Ocsigen_local_files.resolve ~request ~filename ()
      with
      | Ocsigen_local_files.Failed_403
      (* XXXBY : maybe we should signal a true 403 ? *)
      | Ocsigen_local_files.Failed_404
      | Ocsigen_local_files.NotReadableDirectory
      ->
        raise Eliom_common.Eliom_404
    with
    | Ocsigen_local_files.RFile fname ->
        let%lwt response, body =
          let headers =
            Ocsigen_header.of_option headers
            |> add_cache_header options
            |> headers_with_content_type ?charset ?content_type
          in
          Cohttp_lwt_unix.Server.respond_file ~headers ~fname ()
        in
        Lwt.return (Ocsigen_response.make ~body response)
    | Ocsigen_local_files.RDir _ ->
        (* FIXME COHTTP TRANSITION: implement directories *)
        raise Ocsigen_local_files.Failed_404
end

module File = struct
  include Eliom_mkreg.Make (File_base)

  let check_file filename =
    let sp = Eliom_common.get_sp () in
    let request = Eliom_request_info.get_request_sp sp in
    try
      ignore (Ocsigen_local_files.resolve ~request ~filename ());
      true
    with
    | Ocsigen_local_files.Failed_403 | Ocsigen_local_files.Failed_404
    | Ocsigen_local_files.NotReadableDirectory
    ->
      false
end

module File_ct_base = struct
  type page = string * string
  type options = int
  type result = browser_content kind

  let result_of_http_result = Result_types.cast_result
  let send_appl_content = Eliom_service.XNever

  let send ?options ?charset ?code ?content_type ?headers
      (filename, content_type')
    =
    let content_type =
      match content_type with
      | Some content_type -> content_type
      | None -> content_type'
    in
    File_base.send ?options ?charset ?code ?headers ~content_type filename
end

module File_ct = struct
  include Eliom_mkreg.Make (File_ct_base)

  let check_file = File.check_file
end

(* FIXME COHTTP TRANSITION: Streamlist (temporarily?) removed *)

module Customize
    (R : Eliom_registration_sigs.S_with_create)
    (T : sig
       type page

       val translate : page -> R.page Lwt.t
     end) =
struct
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

  let register ?app ?scope ?options ?charset ?code ?content_type ?headers
      ?secure_session ~service ?error_handler
      (f : 'get -> 'post -> 'return Lwt.t)
    =
    R.register ?app ?scope ?options ?charset ?code ?content_type ?headers
      ?secure_session ~service ?error_handler:(make_eh error_handler)
      (make_service_handler f)

  let create ?app ?scope ?options ?charset ?code ?content_type ?headers
      ?secure_session ?https ?name ?csrf_safe ?csrf_scope ?csrf_secure ?max_use
      ?timeout ~meth ~path ?error_handler f
    =
    R.create ?app ?scope ?options ?charset ?code ?content_type ?headers
      ?secure_session ?https ?name ?csrf_safe ?csrf_scope ?csrf_secure ?max_use
      ?timeout ~meth ~path ?error_handler:(make_eh error_handler)
      (make_service_handler f)

  let create_attached_get ?app ?scope ?options ?charset ?code ?content_type
      ?headers ?secure_session ?https ?name ?csrf_safe ?csrf_scope ?csrf_secure
      ?max_use ?timeout ~fallback ~get_params ?error_handler f
    =
    R.create_attached_get ?app ?scope ?options ?charset ?code ?content_type
      ?headers ?secure_session ?https ?name ?csrf_safe ?csrf_scope ?csrf_secure
      ?max_use ?timeout ~fallback ~get_params
      ?error_handler:(make_eh error_handler) (make_service_handler f)

  let create_attached_post ?app ?scope ?options ?charset ?code ?content_type
      ?headers ?secure_session ?https ?name ?csrf_safe ?csrf_scope ?csrf_secure
      ?max_use ?timeout ~fallback ~post_params ?error_handler f
    =
    R.create_attached_post ?app ?scope ?options ?charset ?code ?content_type
      ?headers ?secure_session ?https ?name ?csrf_safe ?csrf_scope ?csrf_secure
      ?max_use ?timeout ~fallback ~post_params
      ?error_handler:(make_eh error_handler) (make_service_handler f)
end

module Ocaml_base = struct
  type page = string
  type options = unit
  type result = Ocsigen_response.t

  let result_of_http_result x = x
  let send_appl_content = Eliom_service.XNever

  let send ?options:_ ?charset ?code ?content_type ?headers content =
    Result_types.cast_kind_lwt
      (String.send ?charset ?code ?content_type ?headers
         (content, Eliom_service.eliom_appl_answer_content_type))
end

module Ocaml = struct
  type 'a page = 'a
  type options = unit
  type 'a return = 'a Eliom_service.ocaml
  type 'a result = 'a ocaml_content kind

  module M = Eliom_mkreg.Make (Ocaml_base)

  let prepare_data data =
    let ecs_request_data =
      let data = Eliom_syntax.get_request_data () in
      if not (Ocsigen_config.get_debugmode ())
      then
        Array.iter
          (fun d ->
             Eliom_runtime.Client_value_server_repr.clear_loc
               d.Eliom_runtime.value)
          data;
      data
    in
    (*     debug_client_value_data (debug "%s") client_value_data; *)
    let r = {Eliom_runtime.ecs_request_data; ecs_data = data} in
    Lwt.return (Eliom_types.encode_eliom_data r)

  let make_eh = function
    | None -> None
    | Some eh -> Some (fun l -> eh l >>= prepare_data)

  let string_regexp = Str.regexp "\"\\([^\\\"]\\|\\\\.\\)*\""

  let make_service_handler ~name f g p =
    let%lwt data =
      try%lwt
        let%lwt res = f g p in
        Lwt.return (`Success res)
      with exn ->
        let code = Printf.sprintf "%06x" (Random.int 0x1000000) in
        let argument =
          let sp = Eliom_common.get_sp () in
          let si = Eliom_request_info.get_si sp in
          let post_params =
            match si.Eliom_common.si_all_post_params with
            | None -> []
            | Some l -> l
          in
          try Printf.sprintf " (%s)" (List.assoc "argument" post_params)
          with Not_found -> ""
        in
        (match name with
        | Some name ->
            Lwt_log_core.ign_error_f ~exn
              "Uncaught exception in service %s [%s]%s" name code
              (Str.global_replace string_regexp "\"xxx\"" argument)
        | None ->
            Lwt_log_core.ign_error_f ~exn "Uncaught exception [%s]%s" code
              argument);
        Lwt.return (`Failure code)
    in
    prepare_data data

  let send ?options ?charset ?code ?content_type ?headers content =
    let%lwt content = prepare_data content in
    Result_types.cast_result_lwt
      (M.send ?options ?charset ?code ?content_type ?headers content)

  let register ?app ?scope ?options ?charset ?code ?content_type ?headers
      ?secure_session
      ~(service :
         ( 'get
           , 'post
           , _
           , _
           , _
           , Eliom_service.non_ext
           , Eliom_service.reg
           , _
           , _
           , _
           , 'return Eliom_service.ocaml )
           Eliom_service.t)
      ?(error_handler : ((string * exn) list -> 'return Lwt.t) option)
      (f : 'get -> 'post -> 'return Lwt.t)
    =
    M.register ?app ?scope ?options ?charset ?code ?content_type ?headers
      ?secure_session
      ~service:(Eliom_service.untype service)
      ?error_handler:(make_eh error_handler)
      (make_service_handler ~name:None f)

  let create ?app ?scope ?options ?charset ?code ?content_type ?headers
      ?secure_session ?https ?name ?csrf_safe ?csrf_scope ?csrf_secure ?max_use
      ?timeout ~meth ~path ?error_handler f
    =
    Eliom_service.untype
    @@ M.create ?app ?scope ?options ?charset ?code ?content_type ?headers
         ?secure_session ?https ?name ?csrf_safe ?csrf_scope ?csrf_secure
         ?max_use ?timeout ~meth ~path ?error_handler:(make_eh error_handler)
         (make_service_handler ~name f)

  let create_attached_get ?app ?scope ?options ?charset ?code ?content_type
      ?headers ?secure_session ?https ?name ?csrf_safe ?csrf_scope ?csrf_secure
      ?max_use ?timeout ~fallback ~get_params ?error_handler f
    =
    Eliom_service.untype
    @@ M.create_attached_get ?app ?scope ?options ?charset ?code ?content_type
         ?headers ?secure_session ?https ?name ?csrf_safe ?csrf_scope
         ?csrf_secure ?max_use ?timeout
         ~fallback:(Eliom_service.untype fallback)
         ~get_params ?error_handler:(make_eh error_handler)
         (make_service_handler ~name f)

  let create_attached_post ?app ?scope ?options ?charset ?code ?content_type
      ?headers ?secure_session ?https ?name ?csrf_safe ?csrf_scope ?csrf_secure
      ?max_use ?timeout ~fallback ~post_params ?error_handler f
    =
    Eliom_service.untype
    @@ M.create_attached_post ?app ?scope ?options ?charset ?code ?content_type
         ?headers ?secure_session ?https ?name ?csrf_safe ?csrf_scope
         ?csrf_secure ?max_use ?timeout
         ~fallback:(Eliom_service.untype fallback)
         ~post_params ?error_handler:(make_eh error_handler)
         (make_service_handler ~name f)
end

type appl_service_options = {do_not_launch : bool}
(** [{do_not_launch = true}]: do not launch the client side program if
    it is not already launched.  Default: [false]. *)

let default_appl_service_options = {do_not_launch = false}

let request_template =
  Eliom_reference.eref ~scope:Eliom_common.request_scope None

let global_data_unwrapper =
  Eliom_wrap.create_unwrapper
    (Eliom_wrap.id_of_int Eliom_runtime.global_data_unwrap_id_int)

let get_global_data ~keep_debug =
  let data = Eliom_syntax.get_global_data () in
  let data =
    if keep_debug
    then data
    else
      Eliom_lib.String_map.map
        (fun {Eliom_runtime.server_sections_data; client_sections_data} ->
           Array.iter
             (Array.iter (fun d ->
                Eliom_runtime.Client_value_server_repr.clear_loc
                  d.Eliom_runtime.value))
             server_sections_data;
           { Eliom_runtime.server_sections_data
           ; client_sections_data =
               Array.map
                 (Array.map (fun x ->
                    {x with Eliom_runtime.injection_dbg = None}))
                 client_sections_data })
        data
  in
  data, global_data_unwrapper

let transform_global_app_uri = ref (fun x -> x)

module type APP = sig
  val application_script :
     ?defer:bool
    -> ?async:bool
    -> unit
    -> [> `Script] Eliom_content.Html.elt

  val application_name : string
  val is_initial_request : unit -> bool

  type app_id
  type page = Html_types.html Eliom_content.Html.elt
  type options = appl_service_options
  type return = Eliom_service.non_ocaml
  type result = app_id application_content kind

  include
    Eliom_registration_sigs.S_with_create
    with type page := page
     and type options := options
     and type return := return
     and type result := result

  val typed_name : app_id application_name
end

module App_base (App_param : Eliom_registration_sigs.APP_PARAM) = struct
  type app_id
  type page = Html_types.html Eliom_content.Html.elt
  type options = appl_service_options
  type result = app_id application_content kind

  let result_of_http_result = Result_types.cast_result

  let is_initial_request () =
    Eliom_common.((get_sp ()).sp_client_appl_name)
    <> Some App_param.application_name

  let global_data_cache_options () =
    (Eliom_request_info.get_sitedata ()).Eliom_common.cache_global_data

  let eliom_appl_script_id : [`Script] Eliom_content.Html.Id.id =
    Eliom_content.Html.Id.new_elt_id ~global:true ()

  let application_script ?defer ?async () =
    let defer', async' =
      (Eliom_request_info.get_sitedata ()).Eliom_common.application_script
    in
    let defer = match defer with Some b -> b | None -> defer' in
    let async = match async with Some b -> b | None -> async' in
    let a =
      (if defer then [Eliom_content.Html.D.a_defer ()] else [])
      @ if async then [Eliom_content.Html.D.a_async ()] else []
    in
    Eliom_content.Html.Id.create_named_elt ~id:eliom_appl_script_id
      (Eliom_content.Html.D.js_script ~a
         ~uri:
           (Eliom_content.Html.D.make_uri
              ~service:(Eliom_service.static_dir ())
              [App_param.application_name ^ ".js"])
         ())

  let application_script =
    (application_script
      : ?defer:_ -> ?async:_ -> _ -> [`Script] Eliom_content.Html.elt
      :> ?defer:_ -> ?async:_ -> _ -> [> `Script] Eliom_content.Html.elt)

  let is_eliom_appl_script elt =
    Eliom_content.Html.Id.have_id eliom_appl_script_id elt

  let eliom_appl_data_script_id =
    Eliom_content.Html.Id.new_elt_id ~global:true ()

  let make_eliom_appl_data_script ~sp =
    let script =
      Printf.sprintf
        "var __eliom_appl_sitedata = \'%s\';\nvar __eliom_appl_process_info = \'%s\'\nvar __eliom_request_data;\nvar __eliom_request_cookies;\nvar __eliom_request_template;\n"
        (Eliom_lib.jsmarshal (Eliommod_cli.client_sitedata sp))
        (Eliom_lib.jsmarshal sp.Eliom_common.sp_client_process_info)
    in
    Lwt.return
      Eliom_content.Html.(
        Id.create_named_elt ~id:eliom_appl_data_script_id
          (F.script (F.cdata_script script)))

  let make_eliom_data_script ?(keep_debug = false) ~sp page =
    let ejs_global_data =
      if is_initial_request () && global_data_cache_options () = None
      then Some (get_global_data ~keep_debug)
      else None
    in
    let ejs_request_data =
      let data = Eliom_syntax.get_request_data () in
      if not keep_debug
      then
        Array.iter
          (fun d ->
             Eliom_runtime.Client_value_server_repr.clear_loc
               d.Eliom_runtime.value)
          data;
      data
    in
    (* wrapping of values could create eliom references that may
       create cookies that needs to be sent along the page. Hence,
       cookies should be calculated after wrapping. *)
    let eliom_data =
      Eliom_content.Xml.wrap
        (Eliom_content.Html.D.toelt page)
        { Eliom_common.ejs_global_data
        ; ejs_request_data
        ; ejs_event_handler_table =
            Eliom_content.Xml.make_event_handler_table
              (Eliom_content.Html.D.toelt page)
        ; ejs_client_attrib_table =
            Eliom_content.Xml.make_client_attrib_table
              (Eliom_content.Html.D.toelt page)
        ; ejs_sess_info = Eliommod_cli.client_si sp.Eliom_common.sp_si }
    in
    let%lwt tab_cookies =
      Eliommod_cookies.compute_cookies_to_send sp.Eliom_common.sp_sitedata
        sp.Eliom_common.sp_tab_cookie_info sp.Eliom_common.sp_user_tab_cookies
    in
    let%lwt template = Eliom_reference.get request_template in
    let script =
      Printf.sprintf
        "__eliom_request_data = \'%s\';\n__eliom_request_cookies = \'%s\';\n__eliom_request_template = \'%s\';"
        (Eliom_lib.jsmarshal eliom_data)
        (Eliom_lib.jsmarshal tab_cookies)
        (Eliom_lib.jsmarshal (template : string option))
    in
    Lwt.return Eliom_content.Html.(F.script (F.cdata_script script))

  let global_data_service =
    lazy
      (let path, max_age =
         match global_data_cache_options () with
         | Some v -> v
         | None -> assert false
       in
       let global_data =
         get_global_data ~keep_debug:(Ocsigen_config.get_debugmode ())
         |> Eliom_wrap.wrap |> Eliom_lib.jsmarshal
       in
       let script =
         Printf.sprintf "__eliom_global_data = \'%s\';" global_data
       in
       let name = Digest.to_hex (Digest.string global_data) ^ ".js" in
       String.create ~options:max_age
         ~path:(Eliom_service.Path (path @ [name]))
         ~meth:(Get Eliom_parameter.unit)
         (fun _ _ -> Lwt.return (script, "application/x-javascript")))

  let add_eliom_global_data_script rem =
    if global_data_cache_options () <> None
    then
      (* Using the async flag does not make sense here as we need to
         be sure that this is executed before the application script. *)
      let defer, _ =
        (Eliom_request_info.get_sitedata ()).Eliom_common.application_script
      in
      let uri =
        Eliom_content.Html.F.make_uri ~absolute:false
          ~service:(Lazy.force global_data_service)
          ()
      in
      let a =
        (if defer then [Eliom_content.Html.F.a_defer ()] else [])
        @ [Eliom_content.Html.F.a_src @@ !transform_global_app_uri uri]
      in
      Eliom_content.Html.F.script ~a (Eliom_content.Html.F.txt "") :: rem
    else rem

  let split_page page :
      Html_types.html_attrib Eliom_content.Html.attrib list
      * (Html_types.head_attrib Eliom_content.Html.attrib list
        * Html_types.title Eliom_content.Html.elt
        * Html_types.head_content_fun Eliom_content.Html.elt list)
      * Html_types.body Eliom_content.Html.elt
    =
    match Eliom_content.Xml.content page with
    | Eliom_content.Xml.Node (_, html_attribs, [head; body]) -> (
      match Eliom_content.Xml.content head with
      | Eliom_content.Xml.Node (_, head_attribs, head_elts) ->
          ( List.map Eliom_content.Html.D.to_attrib html_attribs
          , ( List.map Eliom_content.Html.D.to_attrib head_attribs
            , Eliom_content.Html.D.tot (List.hd head_elts)
            , Eliom_content.Html.D.totl (List.tl head_elts) )
          , Eliom_content.Html.D.tot body )
      | _ -> assert false)
    | _ -> assert false

  let add_eliom_scripts ~sp page =
    let%lwt appl_data_script = make_eliom_appl_data_script ~sp in
    (* First we build a fake page to build the ref_tree... *)
    let html_attribs, (head_attribs, title, head_elts), body =
      split_page (Eliom_content.Html.D.toelt page)
    in
    let encode_slashs = List.map (Eliom_lib.Url.encode ~plus:false) in
    let base_url =
      Eliom_uri.make_proto_prefix
        (Eliom_config.default_protocol_is_https ()
        || Eliom_request_info.get_csp_ssl_sp sp)
      ^ Eliom_lib.String.concat "/"
          (encode_slashs (Eliom_request_info.get_csp_original_full_path ()))
    in
    let head_elts =
      if List.exists is_eliom_appl_script head_elts
      then head_elts
      else head_elts @ [application_script ()]
    in
    let head_elts =
      appl_data_script
      (* <base> elt is added only for xhr done by client process,
         because in that case, URLs are relative to the URL of
         the first page, not the current URL.
         We don't want to put base for non-xhr,
         to make it possible to have truly relative URLs in HTML pages.
      *)
      ::
      (if Eliom_request_info.expecting_process_page ()
       then
         Eliom_content.Html.(
           F.base
             ~a:
               [ F.a_id Eliom_common_base.base_elt_id
               ; F.a_href (Eliom_content.Xml.uri_of_string base_url) ]
             ())
         :: head_elts
       else head_elts)
    in
    let fake_page =
      Eliom_content.Html.F.html ~a:html_attribs
        (Eliom_content.Html.F.head ~a:head_attribs title head_elts)
        body
    in
    let%lwt data_script =
      make_eliom_data_script
        ~keep_debug:(Ocsigen_config.get_debugmode ())
        ~sp fake_page
    in
    (* Then we replace the faked data_script *)
    let head_elts =
      (* Eliom_client_core.load_data_script expects data_script to be
         second in this list *)
      List.hd head_elts :: data_script
      :: add_eliom_global_data_script (List.tl head_elts)
    in
    Lwt.return
      (Eliom_content.Html.F.html ~a:html_attribs
         (Eliom_content.Html.F.head ~a:head_attribs title head_elts)
         body)

  let remove_eliom_scripts page =
    let html_attribs, (head_attribs, title, head_elts), body =
      split_page (Eliom_content.Html.D.toelt page)
    in
    let head_elts =
      List.filter (fun x -> not (is_eliom_appl_script x)) head_elts
    in
    Lwt.return
      (Eliom_content.Html.F.html ~a:html_attribs
         (Eliom_content.Html.F.head ~a:head_attribs title head_elts)
         body)

  let send_appl_content =
    Eliom_service.XSame_appl (App_param.application_name, None)

  let out =
    let encode x = fst (Xml_print.Utf8.normalize_html x) in
    Eliom_content.Html.Printer.pp ~encode ()

  let send ?(options = default_appl_service_options) ?charset ?code
      ?content_type ?headers content
    =
    let sp = Eliom_common.get_sp () in
    (* GRGR FIXME et si le nom de l'application diffère ?? Il faut
       renvoyer un full_redirect... TODO *)
    if sp.Eliom_common.sp_client_appl_name <> Some App_param.application_name
    then
      Eliom_state.set_cookie ~cookie_level:`Client_process
        ~name:Eliom_common.appl_name_cookie_name
        ~value:App_param.application_name ();
    let%lwt body =
      (match sp.Eliom_common.sp_client_appl_name, options.do_not_launch with
        | None, true -> remove_eliom_scripts content
        | _ -> add_eliom_scripts ~sp content)
      >|= fun body -> Cohttp_lwt.Body.of_string (Format.asprintf "%a" out body)
    in
    let headers =
      let h = Ocsigen_header.of_option headers in
      let h =
        Cohttp.Header.replace h Eliom_common_base.appl_name_header_name
          App_param.application_name
      in
      try
        (* If it is a suffix service with redirection, we may have to
           normalize the uri *)
        let table = Eliom_request_info.get_request_cache () in
        Cohttp.Header.replace h Eliom_common_base.response_url_header
          (Polytables.get ~table ~key:Eliom_mkreg.suffix_redir_uri_key)
      with Not_found -> h
    and status = Eliom_lib.Option.map Cohttp.Code.status_of_code code
    and content_type = content_type_html content_type in
    result_of_content ?charset ?status ~content_type ~headers body
end

module App (App_param : Eliom_registration_sigs.APP_PARAM) = struct
  module Base = App_base (App_param)

  type app_id = Base.app_id

  let is_initial_request = Base.is_initial_request
  let application_script = Base.application_script

  include Eliom_mkreg.Make (Base)

  let application_name = App_param.application_name
  let typed_name = App_param.application_name

  let data_service_handler () () =
    Lwt.return (get_global_data ~keep_debug:(Ocsigen_config.get_debugmode ()))

  let _ =
    match App_param.global_data_path with
    | Some global_data_path ->
        ignore
        @@ Ocaml.create ~path:(Eliom_service.Path global_data_path)
             ~meth:(Get Eliom_parameter.unit) ~https:true data_service_handler
    | None -> ()
end

module type TMPL_PARAMS = sig
  type t

  val name : string
  val make_page : t -> Html_types.html Eliom_content.Html.elt Lwt.t
  val update : t -> unit Eliom_client_value.t
end

module Eliom_tmpl_reg_make_param (Appl : APP) (Tmpl_param : TMPL_PARAMS) =
struct
  type page = Tmpl_param.t
  type options = appl_service_options
  type result = Appl.app_id application_content kind

  let result_of_http_result = Result_types.cast_result

  let send_appl_content =
    Eliom_service.XSame_appl (Appl.application_name, Some Tmpl_param.name)

  let nl_template =
    Eliom_parameter.make_non_localized_parameters ~prefix:"eliom"
      ~name:"template"
      (Eliom_parameter.string "name")

  let send ?(options = default_appl_service_options) ?charset ?code
      ?content_type ?headers content
    =
    match Eliom_parameter.get_non_localized_get_parameters nl_template with
    | None ->
        let%lwt () =
          Eliom_reference.set request_template (Some Tmpl_param.name)
        in
        let%lwt content = Tmpl_param.make_page content in
        Result_types.cast_kind_lwt
          (Appl.send ~options ?charset ?code ?content_type ?headers content)
    | Some _ ->
        ignore (Tmpl_param.update content);
        Result_types.cast_kind_lwt
          (Ocaml.send ?charset ?code ?content_type ?headers ())
end

module Eliom_tmpl (App : APP) (Tmpl_param : TMPL_PARAMS) =
  Eliom_mkreg.Make (Eliom_tmpl_reg_make_param (App) (Tmpl_param))

type redirection_options =
  [ `MovedPermanently
  | `Found
  | `SeeOther
  | `NotNodifed
  | `UseProxy
  | `TemporaryRedirect ]

let status_of_redirection_options options code =
  match code with
  | Some code -> Cohttp.Code.status_of_code code
  | None -> (
    match (options : redirection_options) with
    | `MovedPermanently -> `Moved_permanently
    | `Found -> `Found
    | `SeeOther -> `See_other
    | `NotNodifed -> `Not_modified
    | `UseProxy -> `Use_proxy
    | `TemporaryRedirect -> `Temporary_redirect)

(* Redirection services are like services, but send a redirection
   instead of a page.

   The HTTP/1.1 RFC says: If the 301 status code is received in
   response to a request other than GET or HEAD, the user agent MUST
   NOT automatically redirect the request unless it can be confirmed
   by the user, since this might change the conditions under which the
   request was issued.

   Here redirections are done towards services without parameters.
   (possibly preapplied). *)
module String_redirection_base = struct
  type page = Eliom_lib.Url.uri
  type options = redirection_options
  type result = browser_content kind

  let result_of_http_result = Result_types.cast_result
  let send_appl_content = Eliom_service.XAlways
  (* actually, the service will decide itself *)

  let send ?(options = `Found) ?charset ?code ?content_type ?headers uri =
    let headers = Ocsigen_header.of_option headers
    and header_id, status =
      (* We decide the kind of redirection we do. If the request is an
         XHR done by a client side Eliom program expecting a process
         page, we do not send an HTTP redirection. In that case, we
         send a half XHR redirection.  *)
      if not (Eliom_request_info.expecting_process_page ())
      then
        (* the browser did not ask application eliom data, we send a
           regular redirection *)
        ( Ocsigen_header.Name.(to_string location)
        , status_of_redirection_options options code )
      else Eliom_common.half_xhr_redir_header, `OK
    in
    let headers = Cohttp.Header.replace headers header_id uri in
    result_of_content ?charset ?content_type ~status ~headers
      (Cohttp.Body.empty :> Cohttp_lwt.Body.t)
end

module String_redirection = Eliom_mkreg.Make (String_redirection_base)

type _ redirection =
  | Redirection :
      ( unit
        , unit
        , Eliom_service.get
        , _
        , _
        , _
        , _
        , [`WithoutSuffix]
        , unit
        , unit
        , 'a )
        Eliom_service.t
      -> 'a redirection

module Redirection_base = struct
  type 'a page = 'a redirection
  type options = redirection_options
  type 'a return = 'a

  let send_appl_content = Eliom_service.XAlways
  (* actually, the service will decide itself *)

  let send ?(options = `Found) ?charset ?code ?content_type ?headers
      (Redirection service)
    =
    let uri = Eliom_uri.make_string_uri ~service ()
    and headers = Ocsigen_header.of_option headers in
    (* Now we decide the kind of redirection we do.

       If the request is an xhr done by a client side Eliom program
       expecting a process page, we do not send an HTTP redirection.
       In that case, we send:

       - a full xhr redirection if the application to which belongs
         the destination service is the same (thus it will send back
         tab cookies) (simulate a redirection without stopping the
         client process)

       - a half xhr redirection otherwise (i.e. ask the browser to do
         an actual redirection).  *)
    match
      ( Eliom_request_info.expecting_process_page ()
      , Eliom_request_info.get_sp_client_appl_name () )
    with
    | true, None (* should not happen *) | false, _ ->
        (* the browser did not ask for process data,we
                     send a regular redirection *)
        let status = status_of_redirection_options options code
        and headers =
          Cohttp.Header.replace headers
            Ocsigen_header.Name.(to_string location)
            uri
        in
        result_of_content ?charset ?content_type ~status ~headers
          (Cohttp.Body.empty :> Cohttp_lwt.Body.t)
    | true, Some anr ->
        let headers =
          Cohttp.Header.replace headers Eliom_common_base.appl_name_header_name
            anr
        in
        let headers =
          Cohttp.Header.replace headers
            (match Eliom_service.send_appl_content service with
            (* the appl name of the destination service *)
            | Eliom_service.XSame_appl (an, _) when an = anr ->
                (* Same appl, we do a full XHR redirection (not an HTTP
                redirection, because we want to send back tab cookies) *)
                Eliom_common.full_xhr_redir_header
            | Eliom_service.XAlways ->
                (* It is probably an action, or a void coservice. Full XHR
                again *)
                Eliom_common.full_xhr_redir_header
            | _ ->
                (* No application, or another application. We ask the
                browser to do an HTTP redirection. *)
                Eliom_common.half_xhr_redir_header)
            uri
        in
        result_of_content ?charset ?content_type ~status:`No_content ~headers
          (Cohttp.Body.empty :> Cohttp_lwt.Body.t)
end

module Redirection = struct
  include Eliom_mkreg.Make_poly (Redirection_base)

  let send ?options ?charset ?code ?content_type ?headers content =
    Result_types.cast_result_lwt
      (Redirection_base.send ?options ?charset ?code ?content_type ?headers
         content)
end

let set_exn_handler h =
  let sitedata = Eliom_request_info.find_sitedata "set_exn_handler" in
  Eliom_request_info.set_site_handler sitedata
    (Result_types.cast_function_http h)

let default_app_name = Eliommod.default_app_name
let set_app_name = Eliommod.set_app_name

let instruction ?xhr_links ?(app = default_app_name) ?data_timeout
    ?service_timeout ?persistent_timeout ?max_service_sessions_per_group
    ?max_volatile_data_sessions_per_group
    ?max_persistent_data_sessions_per_group ?max_service_tab_sessions_per_group
    ?max_volatile_data_tab_sessions_per_group
    ?max_persistent_data_tab_sessions_per_group
    ?max_anonymous_services_per_session ?secure_cookies ?application_script
    ?global_data_caching ?html_content_type ?ignored_get_params
    ?ignored_post_params ?omitpersistentstorage () vh conf_info site_dir
  =
  let sitedata = Eliommod.create_sitedata vh site_dir conf_info in
  (* customize sitedata according to optional parameters: *)
  Option.iter
    (fun v ->
       sitedata.Eliom_common.default_links_xhr#set ~override_tenable:true v)
    xhr_links;
  Option.iter
    (fun (level, hierarchyname, v) ->
       Eliommod.set_timeout
         (Eliommod_timeouts.set_global_ ~kind:`Data)
         sitedata level hierarchyname v)
    data_timeout;
  Option.iter
    (fun (level, hierarchyname, v) ->
       Eliommod.set_timeout
         (Eliommod_timeouts.set_global_ ~kind:`Service)
         sitedata level hierarchyname v)
    service_timeout;
  Option.iter
    (fun (level, hierarchyname, v) ->
       Eliommod.set_timeout
         (Eliommod_timeouts.set_global_ ~kind:`Persistent)
         sitedata level hierarchyname v)
    persistent_timeout;
  Option.iter
    (fun v -> sitedata.max_service_sessions_per_group <- v)
    max_service_sessions_per_group;
  Option.iter
    (fun v -> sitedata.max_volatile_data_sessions_per_group <- v)
    max_volatile_data_sessions_per_group;
  Option.iter
    (fun v -> sitedata.max_persistent_data_sessions_per_group <- Some v, true)
    max_persistent_data_sessions_per_group;
  Option.iter
    (fun v -> sitedata.max_service_tab_sessions_per_group <- v)
    max_service_tab_sessions_per_group;
  Option.iter
    (fun v -> sitedata.max_volatile_data_tab_sessions_per_group <- v)
    max_volatile_data_tab_sessions_per_group;
  Option.iter
    (fun v ->
       sitedata.max_persistent_data_tab_sessions_per_group <- Some v, true)
    max_persistent_data_tab_sessions_per_group;
  Option.iter
    (fun v -> sitedata.max_anonymous_services_per_session <- v)
    max_anonymous_services_per_session;
  Option.iter (fun v -> sitedata.secure_cookies <- v) secure_cookies;
  Option.iter (fun v -> sitedata.application_script <- v) application_script;
  Option.iter (fun v -> sitedata.cache_global_data <- v) global_data_caching;
  Option.iter (fun v -> sitedata.html_content_type <- Some v) html_content_type;
  Option.iter
    (fun v -> sitedata.ignored_get_params <- v :: sitedata.ignored_get_params)
    ignored_get_params;
  Option.iter
    (fun v -> sitedata.ignored_post_params <- v :: sitedata.ignored_post_params)
    ignored_post_params;
  Option.iter
    (fun v -> sitedata.omitpersistentstorage <- v)
    omitpersistentstorage;
  (* end sitedata *)
  Eliom_common.absolute_change_sitedata sitedata;
  Eliommod.site_init (ref true);
  (* Load app: *)
  Eliommod.load_eliom_module sitedata (Eliommod.Name app) "" [];
  Eliommod_pagegen.gen None sitedata
