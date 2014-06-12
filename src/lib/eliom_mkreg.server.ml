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

open Eliom_lib

open Lwt
open Ocsigen_extensions
open Eliom_state
open Eliom_parameter
open Eliom_service
open Lazy

let suffix_redir_uri_key = Polytables.make_key ()

(****************************************************************************)

type ('options,'page,'result) param =
    { send :
	?options:'options ->
	?charset:string ->
	?code: int ->
	?content_type:string ->
	?headers: Http_headers.t ->
	'page ->
	Ocsigen_http_frame.result Lwt.t;

      send_appl_content : Eliom_service.send_appl_content;
      (** Whether the service is capable to send application content when
	  required. This field is usually [Eliom_service.XNever]. This
	  value is recorded inside each service just after
	  registration.  *)

      result_of_http_result : Ocsigen_http_frame.result -> 'result; }


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
  match Eliom_service.get_send_appl_content service
  (* the appl name of the service *)
  with
    | Eliom_service.XSame_appl (an, _)
	when (an = name)
	  -> (* Same appl, it is ok *) false
    | Eliom_service.XAlways -> (* It is an action *) false
    | _ -> true

(* This test check if there is a header set only by
   Eliom_registration.App. This test is sufficient, but it is better
   to stop page generation as soon as we know that the content won't
   be needed: hence we test what we can before page generation. *)
let check_after name result =
  try
    let appl_name = Http_headers.find
      (Http_headers.name Eliom_common_base.appl_name_header_name)
      (Ocsigen_http_frame.Result.headers result)
    in
    not (appl_name = name)
  with
    (* not an application content *)
    | Not_found -> true

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
    raise_lwt
      (* we answer to the xhr
	 by asking an HTTP redirection *)
      (Eliom_common.Eliom_do_half_xhr_redirection
	 ("/"^
             String.may_concat
                  (Ocsigen_extensions.Ocsigen_request_info.original_full_path_string ri)
                  ~sep:"?"
                  (Eliom_parameter.construct_params_string
                     (Lazy.force
                        (Ocsigen_extensions.Ocsigen_request_info.get_params ri))
                  )))
  (* We do not put hostname and port.
     It is ok with half or full xhr redirections. *)
  (* If an action occured before,
     it may have removed some get params form ri *)
  else Lwt.return ()

let send_with_cookies
    sp
    pages
    ?options
    ?charset
    ?code
    ?content_type
    ?headers
    content =
  lwt result =
    pages.send
      ?options
      ?charset
      ?code
      ?content_type
      ?headers
      content
  in
  lwt () = check_process_redir sp check_after result in
  lwt tab_cookies =
    Eliommod_cookies.compute_cookies_to_send
      sp.Eliom_common.sp_sitedata
      sp.Eliom_common.sp_tab_cookie_info
      sp.Eliom_common.sp_user_tab_cookies
  in
  (* TODO: do not add header when no cookies *)
  let tab_cookies = Eliommod_cookies.cookieset_to_json tab_cookies in
  Lwt.return
    (Ocsigen_http_frame.Result.update result
       ~cookies:
         (Ocsigen_cookies.add_cookies
          (Eliom_request_info.get_user_cookies ())
          (Ocsigen_http_frame.Result.cookies result))
       ~headers:
         (Http_headers.add
          (Http_headers.name Eliom_common_base.set_tab_cookies_header_name)
          tab_cookies
          (Ocsigen_http_frame.Result.headers result))
       ())

let register_aux pages
      ?options
      ?charset
      ?code
      ?content_type
      ?headers
      table
      ~service
      ?(error_handler = fun l -> raise (Eliom_common.Eliom_Typing_Error l))
      page_generator =
    Eliom_service.set_send_appl_content service (pages.send_appl_content);
    begin
      match get_info_ service with
	| `Attached attser ->
          let key_kind = get_or_post_ service in
          let attserget = get_get_name_ attser in
          let attserpost = get_post_name_ attser in
          let suffix_with_redirect = get_redirect_suffix_ attser in
          let priority = get_priority_ attser in
          let sgpt = get_get_params_type_ service in
          let sppt = get_post_params_type_ service in
          let f table ((attserget, attserpost) as attsernames) =
            Eliommod_services.add_service
              priority
              table
              (get_sub_path_ attser)
              {Eliom_common.key_state = attsernames;
               Eliom_common.key_kind = key_kind}
              ((if attserget = Eliom_common.SAtt_no
                || attserpost = Eliom_common.SAtt_no
                then (anonymise_params_type sgpt,
                      anonymise_params_type sppt)
                else (0, 0)),
               ((match get_max_use_ service with
                 | None -> None
                 | Some i -> Some (ref i)),
                (match get_timeout_ service with
                  | None -> None
                  | Some t -> Some (t, ref (t +. Unix.time ()))),
                (fun nosuffixversion sp ->
                  Lwt.with_value Eliom_common.sp_key (Some sp)
                    (fun () ->
                      let ri = Eliom_request_info.get_ri_sp sp
                      and suff = Eliom_request_info.get_suffix_sp sp in
                      (catch (fun () ->
			reconstruct_params
			  ~sp
			  sgpt
			  (Some (Lwt.return (force (Ocsigen_extensions.Ocsigen_request_info.get_params ri))))
			  (Some (Lwt.return []))
			  nosuffixversion
			  suff
			>>= fun g ->
			let post_params =
			  Eliom_request_info.get_post_params_sp sp
			in
			let files =
			  Eliom_request_info.get_files_sp sp
			in
			reconstruct_params
			  ~sp
			  sppt
			  post_params
			  files
			  false
			  None
			>>= fun p ->
			(* GRGR TODO: avoid
			   Eliom_uri.make_string_uri_. But we need to
			   "downcast" the type of service to the
			   correct "get service". *)
			(if Eliom_request_info.get_http_method () =
                           Ocsigen_http_frame.Http_header.GET
                         && nosuffixversion && suffix_with_redirect
			 then
			    (* it is a suffix service in version
			       without suffix. We redirect. *)
			    if not (Eliom_request_info.expecting_process_page ())
			    then
			      let redir_uri =
			        Eliom_uri.make_string_uri_
				  ~absolute:true
				  ~service:
				  (service :
				     ('a, 'b, [< service_method],[<attached],[< Eliom_service.internal_service_kind ],
				      [< Eliom_service.suff ], 'c, 'd, [ `Registrable ],
				      'return) Eliom_service.service :>
				     ('a, 'b, [< service_method],[<attached],Eliom_service.service_kind,
				      [< Eliom_service.suff ], 'c, 'd,
				      [< Eliom_service.registrable ], 'return)
				     Eliom_service.service)
				  g
			      in
			      Lwt.fail
                                (Eliom_common.Eliom_do_redirection redir_uri)
			    else begin
			    (* It is an internal application form.
			       We don't redirect but we set this
			       special information for url to be displayed
			       by the browser
			       (see Eliom_request_info.rebuild_uri_without_iternal_form_info_)
			    *)
			      let redir_uri =
			        Eliom_uri.make_string_uri_
				  ~absolute:false
				  ~absolute_path:true
				  ~service:
				  (service :
				     ('a, 'b, [< service_method],[<attached],[< Eliom_service.internal_service_kind ],
				      [< Eliom_service.suff ], 'c, 'd, [ `Registrable ],
				      'return) Eliom_service.service :>
				     ('a, 'b, [< service_method],[<attached],Eliom_service.service_kind,
				      [< Eliom_service.suff ], 'c, 'd,
				      [< Eliom_service.registrable ], 'return)
				     Eliom_service.service)
				  g
			      in
                              let redir_uri =
                                if String.length redir_uri > 0
                                then String.sub redir_uri 1
                                  (String.length redir_uri - 1)
                                else redir_uri
                              in
			      let rc = Eliom_request_info.get_request_cache_sp sp in
			      Polytables.set ~table:rc ~key:suffix_redir_uri_key ~value:redir_uri;
			      Lwt.return ()
			    end
			 else Lwt.return ())
			>>= fun () ->
                        check_process_redir sp check_before service >>= fun () ->
                        page_generator g p)
                         (function
                           | Eliom_common.Eliom_Typing_Error l ->
                             error_handler l
                           | e -> fail e)
                        >>= fun content ->
		       send_with_cookies sp pages
                         ?options
                         ?charset
                         ?code
                         ?content_type
                         ?headers
                         content)))))
          in
          (match (key_kind, attserget, attserpost) with
            | (Ocsigen_http_frame.Http_header.POST, _,
               Eliom_common.SAtt_csrf_safe (id, scope, secure_session))
            | (Ocsigen_http_frame.Http_header.PUT, _,
               Eliom_common.SAtt_csrf_safe (id, scope, secure_session))
            | (Ocsigen_http_frame.Http_header.DELETE, _,
               Eliom_common.SAtt_csrf_safe (id, scope, secure_session)) ->
              let tablereg, forsession =
                match table with
                  | Left globtbl -> globtbl, false
                  | Right (sp, ct, sec) ->
                    if secure_session <> sec || scope <> ct
                    then raise
                      Wrong_session_table_for_CSRF_safe_coservice;
                    !(Eliom_state.get_session_service_table
                        ?secure:secure_session ~scope ~sp ()),
                      true
              in
              Eliom_service.set_delayed_post_registration_function
                tablereg
                id
                (fun ~sp attserget ->
                  let n = Eliom_service.new_state () in
                  let attserpost = Eliom_common.SAtt_anon n in
                  let table =
                    if forsession
                    then tablereg
                    else
                      (* we do not register in global table,
                         but in the table specified while creating
                         the csrf safe service *)
                      !(Eliom_state.get_session_service_table
                          ?secure:secure_session
                          ~scope ~sp ())
                  in
                  f table (attserget, attserpost);
                  n)
            | (Ocsigen_http_frame.Http_header.GET,
               Eliom_common.SAtt_csrf_safe (id, scope, secure_session),
               _) ->
              let tablereg, forsession =
                match table with
                  | Left globtbl -> globtbl, false
                  | Right (sp, ct, sec) ->
                    if secure_session <> sec || ct <> scope
                    then raise
                      Wrong_session_table_for_CSRF_safe_coservice;
                    !(Eliom_state.get_session_service_table
                        ?secure:secure_session ~scope ~sp ()), true
              in
              Eliom_service.set_delayed_get_or_na_registration_function
                tablereg
                id
                (fun ~sp ->
                  let n = Eliom_service.new_state () in
                  let attserget = Eliom_common.SAtt_anon n in
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
            | _ ->
              let tablereg =
                match table with
                  | Left globtbl -> globtbl
                  | Right (sp, scope, secure_session) ->
                    !(Eliom_state.get_session_service_table
                        ?secure:secure_session ~scope ~sp ())
              in
              f tablereg (attserget, attserpost))
	| `Nonattached naser ->
          let na_name = get_na_name_ naser in
          let f table na_name =
            Eliommod_naservices.add_naservice
              table
              na_name
              ((match get_max_use_ service with
                | None -> None
                | Some i -> Some (ref i)),
               (match get_timeout_ service with
                 | None -> None
                 | Some t -> Some (t, ref (t +. Unix.time ()))),
               (fun sp ->
                 Lwt.with_value Eliom_common.sp_key (Some sp)
                   (fun () ->
                     let ri = Eliom_request_info.get_ri_sp sp in
                     catch
                       (fun () ->
                         reconstruct_params
                           ~sp
                           (get_get_params_type_ service)
                           (Some (Lwt.return
                              (force (Ocsigen_extensions
                                      .Ocsigen_request_info.get_params ri))))
                           (Some (Lwt.return []))
                           false
                           None
                         >>= fun g ->
                         let post_params =
			   Eliom_request_info.get_post_params_sp sp
                         in
                         let files = Eliom_request_info.get_files_sp sp in
                         reconstruct_params
			   ~sp
			   (get_post_params_type_ service)
			   post_params
			   files
			   false
			   None
                         >>= fun p ->
                         check_process_redir sp check_before service >>= fun () ->
			 page_generator g p)
                       (function
                         | Eliom_common.Eliom_Typing_Error l ->
                           error_handler l
                         | e -> fail e) >>= fun content ->
		     send_with_cookies sp pages
                       ?options
                       ?charset
                       ?code
                       ?content_type
                       ?headers
                       content)
               ))
          in
          match na_name with
            | Eliom_common.SNa_get_csrf_safe (id, scope, secure_session) ->
              (* CSRF safe coservice: we'll do the registration later *)
              let tablereg, forsession =
                match table with
                  | Left globtbl -> globtbl, false
                  | Right (sp, ct, sec) ->
                    if secure_session <> sec || ct <> scope
                    then raise
                      Wrong_session_table_for_CSRF_safe_coservice;
                    !(Eliom_state.get_session_service_table
                        ?secure:secure_session ~scope ~sp ()), true
              in
              set_delayed_get_or_na_registration_function
                tablereg
                id
                (fun ~sp ->
                  let n = Eliom_service.new_state () in
                  let na_name = Eliom_common.SNa_get' n in
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
                  f table na_name;
                  n)
            | Eliom_common.SNa_post_csrf_safe (id, scope, secure_session) ->
              (* CSRF safe coservice: we'll do the registration later *)
              let tablereg, forsession =
                match table with
                  | Left globtbl -> globtbl, false
                  | Right (sp, ct, sec) ->
                    if secure_session <> sec || ct <> scope
                    then raise
                      Wrong_session_table_for_CSRF_safe_coservice;
                    !(Eliom_state.get_session_service_table
                        ?secure:secure_session ~scope ~sp ()), true
              in
              set_delayed_get_or_na_registration_function
                tablereg
                id
                (fun ~sp ->
                  let n = Eliom_service.new_state () in
                  let na_name = Eliom_common.SNa_post' n in
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
                  f table na_name;
                  n)
            | _ ->
              let tablereg =
                match table with
                  | Left globtbl -> globtbl
                  | Right (sp, scope, secure_session) ->
                    !(Eliom_state.get_session_service_table
                        ?secure:secure_session ~scope ~sp ())
              in
              f tablereg na_name
    end

let send pages
    ?options
    ?charset
    ?code
    ?content_type
    ?headers
    content =
  lwt result =
    pages.send
      ?options
      ?charset
      ?code
      ?content_type
      ?headers
      content
  in
  Lwt.return (pages.result_of_http_result result)

let register pages
    ?scope
    ?options
    ?charset
    ?code
    ?content_type
    ?headers
    ?secure_session
    ~service
    ?error_handler
    page_gen =
  let sp = Eliom_common.get_sp_option () in
  match scope, sp with
    | None, None
    | Some `Site, None ->
      (match Eliom_common.global_register_allowed () with
        | Some get_current_sitedata ->
          let sitedata = get_current_sitedata () in
          (match get_info_ service with
            | `Attached attser ->
              Eliom_common.remove_unregistered
                sitedata (get_sub_path_ attser)
            | `Nonattached naser ->
              Eliom_common.remove_unregistered_na
                sitedata (get_na_name_ naser));
          register_aux pages
            ?options
            ?charset
            ?code
            ?content_type
            ?headers
            (Left sitedata.Eliom_common.global_services)
            ~service ?error_handler page_gen
        | _ -> raise
          (Eliom_common.Eliom_site_information_not_available
             "register"))
    | None, Some sp
    | Some `Site, Some sp ->
      register_aux pages
        ?options
        ?charset
        ?code
        ?content_type
        ?headers
        ?error_handler
        (Left (get_global_table ()))
        ~service
        page_gen
    | _, None ->
      raise (failwith "Missing sp while registering service")
    | Some (#Eliom_common.user_scope as scope), Some sp ->
      register_aux pages
        ?options
        ?charset
        ?code
        ?content_type
        ?headers
        ?error_handler
        (Right (sp, scope, secure_session))
        ~service page_gen

  (* WARNING: if we create a new service without registering it,
     we can have a link towards a page that does not exist!!! :-(
     That's why I impose to register all service during init.
     The only other way I see to avoid this is to impose a syntax extension
     like "let rec" for service...
  *)


let register_service pages
    ?scope
    ?options
    ?charset
    ?code
    ?content_type
    ?headers
    ?secure_session
    ?https
    ?priority
    ~path
    ~get_params
    ?error_handler
    page =
  let u = Unsafe.service ?https ?priority ~path ~get_params () in
  register pages
    ?scope
    ?options
    ?charset
    ?code
    ?content_type
    ?headers
    ?secure_session
    ~service:u ?error_handler page;
  u

let register_coservice pages
    ?scope
    ?options
    ?charset
    ?code
    ?content_type
    ?headers
    ?secure_session
    ?name
    ?csrf_safe
    ?csrf_scope
    ?csrf_secure
    ?max_use
    ?timeout
    ?https
    ~(fallback: (unit, unit, [< Eliom_service.service_method > `Get ],
                     [> Eliom_service.attached_kind ],
                     [< Eliom_service.service_kind > `Service ],
                     [ `WithoutSuffix ], unit, unit,
                     [< Eliom_service.registrable ], 'returnT)
                    Eliom_service.service)
    ~get_params
    ?error_handler
    page =
  let u =
    Unsafe.coservice ?name
      ?csrf_safe
      ?csrf_scope:(csrf_scope:>Eliom_common.user_scope option)
      ?csrf_secure
      ?max_use ?timeout ?https
      ~fallback ~get_params ()
  in
  register pages
    ?scope
    ?options
    ?charset
    ?code
    ?content_type
    ?headers
    ?secure_session
    ~service:u ?error_handler page;
  u

let register_coservice' pages
    ?scope
    ?options
    ?charset
    ?code
    ?content_type
    ?headers
    ?secure_session
    ?name
    ?csrf_safe
    ?csrf_scope
    ?csrf_secure
    ?max_use
    ?timeout
    ?https
    ~get_params
    ?error_handler
    page =
  let u =
    Unsafe.coservice'
      ?name
      ?csrf_safe
      ?csrf_scope:(csrf_scope:>Eliom_common.user_scope option)
      ?csrf_secure
      ?max_use ?timeout ?https ~get_params ()
  in
  register pages
    ?scope
    ?options
    ?charset
    ?code
    ?content_type
    ?headers
    ?secure_session
    ~service:u ?error_handler page;
  u

let register_post_service pages
    ?scope
    ?options
    ?charset
    ?code
    ?content_type
    ?headers
    ?secure_session
    ?https
    ?priority
    ~fallback
    ~post_params
    ?error_handler
    page_gen =
  let u = Unsafe.post_service ?https ?priority
    ~fallback:fallback ~post_params:post_params ()
  in
  register pages
    ?scope
    ?options
    ?charset
    ?code
    ?content_type
    ?headers
    ?secure_session
    ~service:u ?error_handler page_gen;
  u

let register_post_coservice pages
    ?scope
    ?options
    ?charset
    ?code
    ?content_type
    ?headers
    ?secure_session
    ?name
    ?csrf_safe
    ?csrf_scope
    ?csrf_secure
    ?max_use
    ?timeout
    ?https
    ~fallback
    ~post_params
    ?error_handler
    page_gen =
  let u =
    Unsafe.post_coservice ?name
      ?csrf_safe
      ?csrf_scope:(csrf_scope:>Eliom_common.user_scope option)
      ?csrf_secure
      ?max_use ?timeout ?https
      ~fallback ~post_params () in
  register pages
    ?scope
    ?options
    ?charset
    ?code
    ?content_type
    ?headers
    ?secure_session
    ~service:u ?error_handler page_gen;
  u

let register_post_coservice' pages
    ?scope
    ?options
    ?charset
    ?code
    ?content_type
    ?headers
    ?secure_session
    ?name
    ?csrf_safe
    ?csrf_scope
    ?csrf_secure
    ?max_use
    ?timeout
    ?keep_get_na_params
    ?https
    ~post_params
    ?error_handler
    page_gen =
  let u =
    Unsafe.post_coservice'
      ?name
      ?csrf_safe
      ?csrf_scope:(csrf_scope:>Eliom_common.user_scope option)
      ?csrf_secure
      ?keep_get_na_params
      ?max_use
      ?timeout
      ?https
      ~post_params ()
  in
  register pages
    ?scope
    ?options
    ?charset
    ?code
    ?content_type
    ?headers
    ?secure_session
    ~service:u ?error_handler page_gen;
  u

let register_put_service pages
    ?scope
    ?options
    ?charset
    ?code
    ?content_type
    ?headers
    ?secure_session
    ?https
    ?priority
    ~path
    ~get_params
    ?error_handler
    page_gen =
  let u = Unsafe.put_service ?https ?priority ~path ~get_params () in
  register pages
    ?scope
    ?options
    ?charset
    ?code
    ?content_type
    ?headers
    ?secure_session
    ~service:u ?error_handler page_gen;
  u

let register_put_coservice pages
    ?scope
    ?options
    ?charset
    ?code
    ?content_type
    ?headers
    ?secure_session
    ?name
    ?csrf_safe
    ?csrf_scope
    ?csrf_secure
    ?max_use
    ?timeout
    ?https
    ~fallback
    ~get_params
    ?error_handler
    page_gen =
  let u =
    Unsafe.put_coservice ?name
      ?csrf_safe
      ?csrf_scope:(csrf_scope:>Eliom_common.user_scope option)
      ?csrf_secure
      ?max_use ?timeout ?https
      ~fallback ~get_params ()
  in
  register pages
    ?scope
    ?options
    ?charset
    ?code
    ?content_type
    ?headers
    ?secure_session
    ~service:u ?error_handler page_gen;
  u

let register_put_coservice' pages
    ?scope
    ?options
    ?charset
    ?code
    ?content_type
    ?headers
    ?secure_session
    ?name
    ?csrf_safe
    ?csrf_scope
    ?csrf_secure
    ?max_use
    ?timeout
    ?https
    ~get_params
    ?error_handler
    page_gen =
  let u =
    Unsafe.put_coservice'
      ?name
      ?csrf_safe
      ?csrf_scope:(csrf_scope:>Eliom_common.user_scope option)
      ?csrf_secure
      ?max_use
      ?timeout
      ?https
      ~get_params ()
  in
  register pages
    ?scope
    ?options
    ?charset
    ?code
    ?content_type
    ?headers
    ?secure_session
    ~service:u ?error_handler page_gen;
  u

let register_delete_service pages
    ?scope
    ?options
    ?charset
    ?code
    ?content_type
    ?headers
    ?secure_session
    ?https
    ?priority
    ~path
    ~get_params
    ?error_handler
    page_gen =
  let u = Unsafe.delete_service ?https ?priority ~path ~get_params () in
  register pages
    ?scope
    ?options
    ?charset
    ?code
    ?content_type
    ?headers
    ?secure_session
    ~service:u ?error_handler page_gen;
  u

let register_delete_coservice pages
    ?scope
    ?options
    ?charset
    ?code
    ?content_type
    ?headers
    ?secure_session
    ?name
    ?csrf_safe
    ?csrf_scope
    ?csrf_secure
    ?max_use
    ?timeout
    ?https
    ~fallback
    ~get_params
    ?error_handler
    page_gen =
  let u =
    Unsafe.delete_coservice ?name
      ?csrf_safe
      ?csrf_scope:(csrf_scope:>Eliom_common.user_scope option)
      ?csrf_secure
      ?max_use ?timeout ?https
      ~fallback ~get_params () in
  register pages
    ?scope
    ?options
    ?charset
    ?code
    ?content_type
    ?headers
    ?secure_session
    ~service:u ?error_handler page_gen;
  u

let register_delete_coservice' pages
    ?scope
    ?options
    ?charset
    ?code
    ?content_type
    ?headers
    ?secure_session
    ?name
    ?csrf_safe
    ?csrf_scope
    ?csrf_secure
    ?max_use
    ?timeout
    ?https
    ~get_params
    ?error_handler
    page_gen =
  let u =
    Unsafe.delete_coservice'
      ?name
      ?csrf_safe
      ?csrf_scope:(csrf_scope:>Eliom_common.user_scope option)
      ?csrf_secure
      ?max_use
      ?timeout
      ?https
      ~get_params ()
  in
  register pages
    ?scope
    ?options
    ?charset
    ?code
    ?content_type
    ?headers
    ?secure_session
    ~service:u ?error_handler page_gen;
  u

module type REG_PARAM = "sigs/eliom_reg_param.mli"

module MakeRegister(Pages : REG_PARAM) = struct

  type page = Pages.page
  type options = Pages.options
  type return = Pages.return
  type result = Pages.result

  let pages =
    { send = Pages.send;
      send_appl_content = Pages.send_appl_content;
      result_of_http_result = Pages.result_of_http_result; }

  let send ?options = send pages ?options
  let register ?scope = register pages ?scope
  let register_service ?scope = register_service pages ?scope
  let register_coservice ?scope = register_coservice pages ?scope
  let register_coservice' ?scope = register_coservice' pages ?scope
  let register_post_service ?scope = register_post_service pages ?scope
  let register_post_coservice ?scope = register_post_coservice pages ?scope
  let register_post_coservice' ?scope = register_post_coservice' pages ?scope
  let register_put_service ?scope = register_put_service pages ?scope
  let register_put_coservice ?scope = register_put_coservice pages ?scope
  let register_put_coservice' ?scope = register_put_coservice' pages ?scope
  let register_delete_service ?scope = register_delete_service pages ?scope
  let register_delete_coservice ?scope = register_delete_coservice pages ?scope
  let register_delete_coservice' ?scope = register_delete_coservice' pages ?scope

end

module type REG_PARAM_ALPHA_RETURN =
sig
  type ('a, 'b) page
  type 'a return
  type 'a result
  include "sigs/eliom_reg_param.mli"
    subst type page := ('a, 'b) page
      and type return := 'b return
      and type result := 'a result
end

module MakeRegister_AlphaReturn(Pages : REG_PARAM_ALPHA_RETURN) = struct

  type ('a, 'b) page = ('a, 'b) Pages.page
  type options = Pages.options
  type 'b return = 'b Pages.return
  type 'a result = 'a Pages.result

  let pages =
    { send = Pages.send;
      send_appl_content = Pages.send_appl_content;
      result_of_http_result = Pages.result_of_http_result; }

  let send ?options = send pages ?options
  let register ?scope = register pages ?scope
  let register_service ?scope = register_service pages ?scope
  let register_coservice ?scope = register_coservice pages ?scope
  let register_coservice' ?scope = register_coservice' pages ?scope
  let register_post_service ?scope = register_post_service pages ?scope
  let register_post_coservice ?scope = register_post_coservice pages ?scope
  let register_post_coservice' ?scope = register_post_coservice' pages ?scope
  let register_put_service ?scope = register_put_service pages ?scope
  let register_put_coservice ?scope = register_put_coservice pages ?scope
  let register_put_coservice' ?scope = register_put_coservice' pages ?scope
  let register_delete_service ?scope = register_delete_service pages ?scope
  let register_delete_coservice ?scope = register_delete_coservice pages ?scope
  let register_delete_coservice' ?scope = register_delete_coservice' pages ?scope

end
