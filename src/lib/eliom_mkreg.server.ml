open Lwt.Syntax

(* Ocsigen
 * http://www.ocsigen.org
 * Module Eliom_mkreg
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

module S = Eliom_service
open Lwt.Infix

let suffix_redir_uri_key = Polytables.make_key ()

type ('options, 'page, 'result) param =
  { send :
      ?options:'options
      -> ?charset:string
      -> ?code:int
      -> ?content_type:string
      -> ?headers:Ocsigen_header.t
      -> 'page
      -> Ocsigen_response.t Lwt.t
  ; send_appl_content : S.send_appl_content
    (** Whether the service is capable to send application content when
          required. This field is usually [Eliom_service.XNever]. This
          value is recorded inside each service just after
          registration.  *)
  ; result_of_http_result : Ocsigen_response.t -> 'result }

(* If it is an xmlHTTPrequest who asked for an internal application
   service but the current service
   does not belong to the same application,
   we ask the browser to stop the program and do a redirection.
   This can happen for example after an action,
   when the fallback service does not belong to the application.
   We can not do a regular redirection because
   it is an XHR. We use our own redirections.
*)
(*VVV
  An alternative, to avoid the redirection with rc,
  would be to answer the full page and to detect on client side
  that it is not the answer of an XRH (using content-type)
  and ask the browser to act as if it were a regular request.
  Is it possible to do that?
  Drawback: The URL will be wrong

  Other solution: send the page and ask the browser to put it in the cache
  during a few seconds. Then redirect. But can we trust the browser cache?
*)

(* the test to know before page generation if the page can contain
   application data. This test is not exhaustif: services declared as
   XAlways can contain classical content, but we can't know it at this
   point: we must wait for the page to be generated and then see if it
   is effectively application content. *)
let check_before name service =
  match S.send_appl_content service (* the appl name of the service *) with
  | S.XSame_appl (an, _) when an = name -> (* Same appl, it is ok *) false
  | S.XAlways -> (* It is an action *) false
  | _ -> true

(* This test check if there is a header set only by
   Eliom_registration.App. This test is sufficient, but it is better
   to stop page generation as soon as we know that the content won't
   be needed: hence we test what we can before page generation. *)
let check_after name result =
  match
    Ocsigen_response.header result
      (Ocsigen_header.Name.of_string Eliom_common_base.appl_name_header_name)
  with
  | Some appl_name -> not (appl_name = name)
  | None ->
      (* not an application content *)
      true

let check_process_redir sp f param =
  let redir =
    if Eliom_request_info.expecting_process_page ()
    then
      match sp.Eliom_common.sp_client_appl_name with
      (* the appl name as sent by browser *)
      | None -> false (* should not happen *)
      | Some anr -> f anr param
      (* the browser asked application eliom data
             (content only) for application anr *)
    else false
  in
  if redir
  then
    let ri = Eliom_request_info.get_ri_sp sp in
    Lwt.fail
      (* we answer to the xhr
         by asking an HTTP redirection *)
      (Eliom_common.Eliom_do_half_xhr_redirection
         ("/"
         ^ Eliom_lib.String.may_concat
             (Ocsigen_request.original_full_path_string ri)
             ~sep:"?"
             (Eliom_parameter.construct_params_string
                (Ocsigen_request.get_params_flat ri))))
  (* We do not put hostname and port.
     It is ok with half or full xhr redirections. *)
  (* If an action occurred before,
     it may have removed some get params form ri *)
  else Lwt.return_unit

let send_with_cookies
      sp
      pages
      ?options
      ?charset
      ?code
      ?content_type
      ?headers
      content
  =
  let* result =
    pages.send ?options ?charset ?code ?content_type ?headers content
  in
  let* () = check_process_redir sp check_after result in
  let* tab_cookies =
    Eliommod_cookies.compute_cookies_to_send sp.Eliom_common.sp_sitedata
      sp.Eliom_common.sp_tab_cookie_info sp.Eliom_common.sp_user_tab_cookies
  in
  (* TODO: do not add header when no cookies *)
  let response =
    let response, _ = Ocsigen_response.to_cohttp result in
    let headers =
      Cohttp.Header.add
        (Cohttp.Response.headers response)
        Eliom_common_base.set_tab_cookies_header_name
        (Eliommod_cookies.cookieset_to_json tab_cookies)
    in
    {response with Cohttp.Response.headers}
  and cookies =
    Ocsigen_cookie_map.add_multi
      (Eliom_request_info.get_user_cookies ())
      (Ocsigen_response.cookies result)
  in
  Lwt.return (Ocsigen_response.update result ~cookies ~response)

let register_aux
      pages
      ?options
      ?charset
      ?code
      ?content_type
      ?headers
      table
      (type a)
      ~(service : (_, _, _, a, _, _, _, _, _, _, _) S.t)
      ?(error_handler = fun l -> raise (Eliom_common.Eliom_Typing_Error l))
      page_generator
  =
  S.set_send_appl_content service pages.send_appl_content;
  match S.info service with
  | S.Attached attser -> (
      let key_meth = S.which_meth_untyped service in
      let attserget = S.get_name attser in
      let attserpost = S.post_name attser in
      let suffix_with_redirect = S.redirect_suffix attser in
      let priority = S.priority attser in
      let sgpt = S.get_params_type service
      and sppt = S.post_params_type service in
      let s_id =
        if attserget = Eliom_common.SAtt_no || attserpost = Eliom_common.SAtt_no
        then
          Eliom_parameter.(
            anonymise_params_type sgpt, anonymise_params_type sppt)
        else 0, 0
      and s_max_use = S.max_use service
      and s_expire =
        match S.timeout service with
        | None -> None
        | Some t -> Some (t, ref (t +. Unix.time ()))
      in
      let f table ((_attserget, _attserpost) as attsernames) =
        Eliom_route.add_service priority table (S.sub_path attser)
          { Eliom_common.key_state = attsernames
          ; Eliom_common.key_meth :> Eliom_common.meth }
          { s_id
          ; s_max_use
          ; s_expire
          ; s_f =
              (fun nosuffixversion sp ->
                Lwt.with_value Eliom_common.sp_key (Some sp) (fun () ->
                  let ri = Eliom_request_info.get_ri_sp sp
                  and suff = Eliom_request_info.get_suffix_sp sp in
                  Lwt.catch
                    (fun () ->
                       Eliom_parameter.reconstruct_params ~sp sgpt
                         (Some (Lwt.return (Ocsigen_request.get_params_flat ri)))
                         (Some (Lwt.return []))
                         nosuffixversion suff
                       >>= fun g ->
                       let post_params =
                         Eliom_request_info.get_post_params_sp sp
                       in
                       let files = Eliom_request_info.get_files_sp sp in
                       Eliom_parameter.reconstruct_params ~sp sppt post_params
                         files false None
                       >>= fun p ->
                       (* GRGR TODO: avoid
                           Eliom_uri.make_string_uri_. But we need to
                           "downcast" the type of service to the
                           correct "get service". *)
                       (if
                          Eliom_request_info.get_http_method () = `GET
                          && nosuffixversion && suffix_with_redirect
                        then (
                          if
                            (* it is a suffix service in version
                               without suffix. We redirect. *)
                            not (Eliom_request_info.expecting_process_page ())
                          then
                            let redir_uri =
                              Eliom_uri.make_string_uri_ ~absolute:true
                                ~service:
                                  (service
                                    : ( 'a
                                        , 'b
                                        , _
                                        , _
                                        , _
                                        , S.non_ext
                                        , S.reg
                                        , _
                                        , 'c
                                        , 'd
                                        , 'return )
                                        S.t
                                    :> ( 'a
                                         , 'b
                                         , _
                                         , _
                                         , _
                                         , _
                                         , _
                                         , _
                                         , 'c
                                         , 'd
                                         , 'return )
                                         S.t)
                                g
                            in
                            Lwt.fail
                              (Eliom_common.Eliom_do_redirection redir_uri)
                          else
                            (* It is an internal application form.
                               We don't redirect but we set this
                               special information for url to be displayed
                               by the browser
                               (see Eliom_request_info.rebuild_uri_without_iternal_form_info_)
                            *)
                            let redir_uri =
                              Eliom_uri.make_string_uri_ ~service g
                            in
                            let rc =
                              Eliom_request_info.get_request_cache_sp sp
                            in
                            Polytables.set ~table:rc ~key:suffix_redir_uri_key
                              ~value:redir_uri;
                            Lwt.return_unit)
                        else Lwt.return_unit)
                       >>= fun () ->
                       check_process_redir sp check_before service >>= fun () ->
                       page_generator g p)
                    (function
                      | Eliom_common.Eliom_Typing_Error l -> error_handler l
                      | e -> Lwt.fail e)
                  >>= fun content ->
                  send_with_cookies sp pages ?options ?charset ?code
                    ?content_type ?headers content)) }
      in
      match key_meth, attserget, attserpost with
      | ( (`Post | `Put | `Delete)
        , _
        , Eliom_common.SAtt_csrf_safe (id, scope, secure_session) ) ->
          let tablereg, forsession =
            match table with
            | Eliom_lib.Left globtbl -> globtbl, false
            | Eliom_lib.Right (sp, ct, sec) ->
                if secure_session <> sec || scope <> ct
                then raise S.Wrong_session_table_for_CSRF_safe_coservice;
                ( !(Eliom_state.get_session_service_table ?secure:secure_session
                      ~scope ~sp ())
                , true )
          in
          S.set_delayed_post_registration_function tablereg id
            (fun ~sp attserget ->
               let n = S.new_state () in
               let attserpost = Eliom_common.SAtt_anon n in
               let table =
                 if forsession
                 then tablereg
                 else
                   (* we do not register in global table,
                         but in the table specified while creating
                         the csrf safe service *)
                   !(Eliom_state.get_session_service_table
                       ?secure:secure_session ~scope ~sp ())
               in
               f table (attserget, attserpost);
               n)
      | `Get, Eliom_common.SAtt_csrf_safe (id, scope, secure_session), _ ->
          let tablereg, forsession =
            match table with
            | Left globtbl -> globtbl, false
            | Right (sp, ct, sec) ->
                if secure_session <> sec || ct <> scope
                then raise S.Wrong_session_table_for_CSRF_safe_coservice;
                ( !(Eliom_state.get_session_service_table ?secure:secure_session
                      ~scope ~sp ())
                , true )
          in
          S.set_delayed_get_or_na_registration_function tablereg id (fun ~sp ->
            let n = S.new_state () in
            let attserget = Eliom_common.SAtt_anon n in
            let table =
              if forsession
              then tablereg
              else
                (* we do not register in global table,
                         but in the table specified while creating
                         the csrf safe service *)
                !(Eliom_state.get_session_service_table ?secure:secure_session
                    ~scope ~sp ())
            in
            f table (attserget, attserpost);
            n)
      | _ ->
          let tablereg =
            match table with
            | Left globtbl -> globtbl
            | Right (sp, scope, secure_session) ->
                !(Eliom_state.get_session_service_table ?secure:secure_session
                    ~scope ~sp ())
          in
          f tablereg (attserget, attserpost))
  | S.Nonattached naser -> (
      let na_name = S.na_name naser in
      let f table na_name =
        Eliom_route.add_naservice table na_name
          ( (match S.max_use service with
            | None -> None
            | Some i -> Some (ref i))
          , (match S.timeout service with
            | None -> None
            | Some t -> Some (t, ref (t +. Unix.time ())))
          , fun sp ->
              Lwt.with_value Eliom_common.sp_key (Some sp) (fun () ->
                let ri = Eliom_request_info.get_ri_sp sp in
                Lwt.catch
                  (fun () ->
                     Eliom_parameter.reconstruct_params ~sp
                       (S.get_params_type service)
                       (Some (Lwt.return (Ocsigen_request.get_params_flat ri)))
                       (Some (Lwt.return []))
                       false None
                     >>= fun g ->
                     let post_params =
                       Eliom_request_info.get_post_params_sp sp
                     in
                     let files = Eliom_request_info.get_files_sp sp in
                     Eliom_parameter.reconstruct_params ~sp
                       (S.post_params_type service)
                       post_params files false None
                     >>= fun p ->
                     check_process_redir sp check_before service >>= fun () ->
                     page_generator g p)
                  (function
                    | Eliom_common.Eliom_Typing_Error l -> error_handler l
                    | e -> Lwt.fail e)
                >>= fun content ->
                send_with_cookies sp pages ?options ?charset ?code ?content_type
                  ?headers content) )
      in
      match na_name with
      | Eliom_common.SNa_get_csrf_safe (id, scope, secure_session) ->
          (* CSRF safe coservice: we'll do the registration later *)
          let tablereg, forsession =
            match table with
            | Left globtbl -> globtbl, false
            | Right (sp, ct, sec) ->
                if secure_session <> sec || ct <> scope
                then raise S.Wrong_session_table_for_CSRF_safe_coservice;
                ( !(Eliom_state.get_session_service_table ?secure:secure_session
                      ~scope ~sp ())
                , true )
          in
          S.set_delayed_get_or_na_registration_function tablereg id (fun ~sp ->
            let n = S.new_state () in
            let na_name = Eliom_common.SNa_get' n in
            let table =
              if forsession
              then tablereg
              else
                (* we do not register in global table,
                         but in the table specified while creating
                         the csrf safe service *)
                !(Eliom_state.get_session_service_table ?secure:secure_session
                    ~scope ~sp ())
            in
            f table na_name; n)
      | Eliom_common.SNa_post_csrf_safe (id, scope, secure_session) ->
          (* CSRF safe coservice: we'll do the registration later *)
          let tablereg, forsession =
            match table with
            | Left globtbl -> globtbl, false
            | Right (sp, ct, sec) ->
                if secure_session <> sec || ct <> scope
                then raise S.Wrong_session_table_for_CSRF_safe_coservice;
                ( !(Eliom_state.get_session_service_table ?secure:secure_session
                      ~scope ~sp ())
                , true )
          in
          S.set_delayed_get_or_na_registration_function tablereg id (fun ~sp ->
            let n = S.new_state () in
            let na_name = Eliom_common.SNa_post' n in
            let table =
              if forsession
              then tablereg
              else
                (* we do not register in global table,
                         but in the table specified while creating
                         the csrf safe service *)
                !(Eliom_state.get_session_service_table ?secure:secure_session
                    ~scope ~sp ())
            in
            f table na_name; n)
      | _ ->
          let tablereg =
            match table with
            | Left globtbl -> globtbl
            | Right (sp, scope, secure_session) ->
                !(Eliom_state.get_session_service_table ?secure:secure_session
                    ~scope ~sp ())
          in
          f tablereg na_name)

let send pages ?options ?charset ?code ?content_type ?headers content =
  let* result =
    pages.send ?options ?charset ?code ?content_type ?headers content
  in
  Lwt.return (pages.result_of_http_result result)

let register
      pages
      ?app:_
      ?scope
      ?options
      ?charset
      ?code
      ?content_type
      ?headers
      ?secure_session
      (type a)
      ~(service : (_, _, _, a, _, _, S.reg, _, _, _, _) S.t)
      ?error_handler
      page_gen
  =
  let sp = Eliom_common.get_sp_option () in
  match scope, sp with
  | None, None | Some `Site, None -> (
      let aux sitedata =
        (match S.info service with
        | S.Attached attser ->
            Eliom_common.remove_unregistered sitedata (S.sub_path attser)
        | S.Nonattached naser ->
            Eliom_common.remove_unregistered_na sitedata (S.na_name naser));
        register_aux pages ?options ?charset ?code ?content_type ?headers
          (Left sitedata.Eliom_common.global_services) ~service ?error_handler
          page_gen
      in
      match Eliom_common.global_register_allowed () with
      | Some get_current_sitedata ->
          let sitedata = get_current_sitedata () in
          if sitedata.Eliom_common.site_dir <> None
          then aux sitedata
          else
            (* I suppose that it's a statically linked module
               that is not associated with a site yet.
               I will defer the registration until app is initialised. *)
            Ocsigen_loader.add_module_init_function
              (Eliom_common.get_app_name ()) (fun () -> aux sitedata)
      | _ ->
          raise (Eliom_common.Eliom_site_information_not_available "register"))
  | None, Some _ | Some `Site, Some _ ->
      register_aux pages ?options ?charset ?code ?content_type ?headers
        ?error_handler
        (Eliom_lib.Left (Eliom_state.get_global_table ()))
        ~service page_gen
  | _, None -> raise (failwith "Missing sp while registering service")
  | Some (#Eliom_common.user_scope as scope), Some sp ->
      register_aux pages ?options ?charset ?code ?content_type ?headers
        ?error_handler
        (Right (sp, scope, secure_session))
        ~service page_gen

(* WARNING: if we create a new service without registering it,
     we can have a link towards a page that does not exist!!! :-(
     That's why I impose to register all service during init.
     The only other way I see to avoid this is to impose a syntax extension
     like "let rec" for service...
*)

let create
      pages
      ?scope
      ?app
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
      page
  =
  let service =
    S.create_unsafe ?name ?csrf_safe
      ?csrf_scope:(csrf_scope :> Eliom_common.user_scope option)
      ?csrf_secure ?max_use ?timeout ?https ~meth ~path ()
  in
  register pages ?scope ?app ?options ?charset ?code ?content_type ?headers
    ?secure_session ~service ?error_handler page;
  service

let create_attached_get
      pages
      ?scope
      ?app
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
      page
  =
  let service =
    S.create_attached_get_unsafe ?name ?csrf_safe
      ?csrf_scope:(csrf_scope :> Eliom_common.user_scope option)
      ?csrf_secure ?max_use ?timeout ?https ~fallback ~get_params ()
  in
  register pages ?scope ?app ?options ?charset ?code ?content_type ?headers
    ?secure_session ~service ?error_handler page;
  service

let create_attached_post
      pages
      ?scope
      ?app
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
      page
  =
  let service =
    S.create_attached_post_unsafe ?name ?csrf_safe
      ?csrf_scope:(csrf_scope :> Eliom_common.user_scope option)
      ?csrf_secure ?max_use ?timeout ?https ~fallback ~post_params ()
  in
  register pages ?scope ?app ?options ?charset ?code ?content_type ?headers
    ?secure_session ~service ?error_handler page;
  service

module Make
    (Pages : Eliom_registration_sigs.PARAM with type frame := Ocsigen_response.t) =
struct
  type page = Pages.page
  type options = Pages.options
  type return = Eliom_service.non_ocaml
  type result = Pages.result

  let pages =
    { send = Pages.send
    ; send_appl_content = Pages.send_appl_content
    ; result_of_http_result = Pages.result_of_http_result }

  let send ?options = send pages ?options
  let register ?app = register pages ?app
  let create ?app = create pages ?app
  let create_attached_get ?app = create_attached_get pages ?app
  let create_attached_post ?app = create_attached_post pages ?app
end

module Make_poly
    (Pages :
       Eliom_registration_sigs.PARAM_POLY with type frame := Ocsigen_response.t) =
struct
  type 'a page = 'a Pages.page
  type options = Pages.options
  type 'a return = 'a Pages.return

  let pages =
    { send = Pages.send
    ; send_appl_content = Pages.send_appl_content
    ; result_of_http_result = (fun x -> x) }

  let register ?app = register pages ?app
  let create ?app = create pages ?app
  let create_attached_get ?app = create_attached_get pages ?app
  let create_attached_post ?app = create_attached_post pages ?app
end
