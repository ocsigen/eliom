(* Ocsigen
 * http://www.ocsigen.org
 * Module Eliom_mkreg
 * Copyright (C) 2007 Vincent Balat
 * Laboratoire PPS - CNRS Université Paris Diderot
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




open Lwt
open Ocsigen_extensions
open Eliom_sessions
open Eliom_services
open Eliom_parameters
open Lazy





(****************************************************************************)

let get_or_post = function
  | `Internal (_, `Get) -> Http_frame.Http_header.GET
  | _ -> Http_frame.Http_header.POST
(*  | `External -> POST ? *)


module type REGCREATE = 
  sig

    type page

    type options

    val send : 
        ?options:options ->
          ?cookies:Eliom_services.cookie list -> 
            ?charset:string ->
              ?code: int ->
                sp:Eliom_sessions.server_params -> 
                  page -> Eliom_services.result_to_send Lwt.t

  end


module type ELIOMREGSIG1 =
(* pasted from mli *)
  sig

    type page

    type options

    val send : 
        ?options:options ->
          ?cookies:Eliom_services.cookie list -> 
            ?charset:string ->
              ?code: int ->
                sp:Eliom_sessions.server_params -> 
                  page -> result_to_send Lwt.t

    val register :
        ?options:options ->
        ?sp: Eliom_sessions.server_params ->
        service:('get, 'post,
                 [< internal_service_kind ],
                 [< suff ], 'gn, 'pn, [ `Registrable ]) service ->
        ?error_handler:(Eliom_sessions.server_params ->
                               (string * exn) list -> page Lwt.t) ->
        (Eliom_sessions.server_params -> 'get -> 'post -> page Lwt.t) ->
          unit
(** registers an service in the public service table
   with the associated handler function.
   [register service t f] will associate the service [service] 
   to the function [f].
   [f] is the function that creates a page, called {e service handler}. 

   That function takes three parameters. 
    - The first one has type [Eliom_sessions.server_params]
   and allows to have acces to informations about the request and the session.
    - The second and third ones are respectively GET and POST parameters.

   For example if [t] is [Eliom_parameters.int "s"], then [ 'get] is [int].

    {e Warning: If you want to register a service in the global table
    after the initialisation phase,
    you must give the [~sp] parameter, otherwise it will raise the
    exception {!Eliom_common.Eliom_function_forbidden_outside_site_loading}.}

   Registering services and coservices is always done in memory as there is
   no means of marshalling closures.

    Registering after initialization is not encouraged for coservices
    without timeout, as such services will be available only until the end
    of the server process!
    If you use that for main services, you will dynamically create new URLs!
    This may be dangerous as they will disappear if you stop the server.
    Be very careful to re-create these URLs when you relaunch the server,
    otherwise, some external links or bookmarks may be broken!

    Some output modules (for example Redirectmod) define their own options
    for that function.
    Use the [?options] parameter to set them.

 *)


    val register_for_session :
        ?options:options ->
        ?session_name:string ->
        sp:Eliom_sessions.server_params ->
          service:('get, 'post, [< internal_service_kind ],
                   [< suff ], 'gn, 'pn, [ `Registrable ]) service ->
              ?error_handler:(Eliom_sessions.server_params -> (string * exn) list -> 
                page Lwt.t) ->
                  (Eliom_sessions.server_params -> 'get -> 'post -> page Lwt.t) -> unit
(** registers a handler for a service in the session table.
   If the same client does a request to this service, this function will be
   used instead of the one from the public table.

   Warning:
   - All main services created during initialization must be
   registered in the public table during initialisation,
   but never after,
   - You can't register a service in a session table 
   when no session is active (i.e. outside a service handler)
 *)


    val register_new_service :
        ?options:options ->
        ?sp: Eliom_sessions.server_params ->
        path:url_path ->
            get_params:('get, [< suff ] as 'tipo, 'gn) params_type ->
                ?error_handler:(Eliom_sessions.server_params -> (string * exn) list -> 
                  page Lwt.t) ->
                    (Eliom_sessions.server_params -> 'get -> unit -> page Lwt.t) ->
                      ('get, unit, 
                       [> `Attached of 
                         [> `Internal of [> `Service ] * [> `Get] ] a_s ],
                       'tipo, 'gn, unit, 
                       [> `Registrable ]) service
(** Same as [new_service] followed by [register] *)
                      
    val register_new_service' :
        ?options:options ->
        ?sp: Eliom_sessions.server_params ->
          name:string ->
            get_params:('get, [ `WithoutSuffix ], 'gn) params_type ->
              ?error_handler:(Eliom_sessions.server_params -> (string * exn) list -> 
                page Lwt.t) ->
                  (Eliom_sessions.server_params -> 'get -> unit -> page Lwt.t) ->
                    ('get, unit, 
                     [> `Nonattached of [> `Get ] na_s ],
                     [ `WithoutSuffix ], 'gn, unit, 
                     [> `Registrable ]) service
(** Same as [new_service'] followed by [register] *)
                      
    val register_new_coservice :
        ?options:options ->
        ?sp: Eliom_sessions.server_params ->
        ?max_use:int ->
        ?timeout:float ->
        fallback:(unit, unit, 
                  [ `Attached of [ `Internal of [ `Service ] * [`Get]] a_s ],
                   [ `WithoutSuffix ] as 'tipo, 
                   unit, unit, [< registrable ])
        service ->
          get_params: 
            ('get, [`WithoutSuffix], 'gn) params_type ->
              ?error_handler:(Eliom_sessions.server_params -> 
                (string * exn) list -> page Lwt.t) ->
                  (Eliom_sessions.server_params -> 'get -> unit -> page Lwt.t) ->
                    ('get, unit, 
                     [> `Attached of 
                       [> `Internal of [> `Coservice ] * [> `Get]] a_s ], 
                     'tipo, 'gn, unit, 
                     [> `Registrable ])
                      service
(** Same as [new_coservice] followed by [register] *)

    val register_new_coservice' :
      ?options:options ->
      ?sp: Eliom_sessions.server_params ->
      ?max_use:int ->
      ?timeout:float ->
        get_params: 
        ('get, [`WithoutSuffix] as 'tipo, 'gn) params_type ->
          ?error_handler:(Eliom_sessions.server_params -> 
            (string * exn) list -> page Lwt.t) ->
              (Eliom_sessions.server_params -> 'get -> unit -> page Lwt.t) ->
                ('get, unit, 
                 [> `Nonattached of [> `Get] na_s ],
                 'tipo, 'gn, unit, [> `Registrable ])
                  service
(** Same as [new_coservice'] followed by [register] *)

    val register_new_coservice_for_session :
        ?options:options ->
        ?session_name:string ->
        sp:Eliom_sessions.server_params ->
        ?max_use:int ->
        ?timeout:float ->
          fallback:(unit, unit, 
                    [ `Attached of [ `Internal of [ `Service ] * [`Get]] a_s ],
                    [ `WithoutSuffix ] as 'tipo, 
                    unit, unit, [< registrable ])
            service ->
              get_params: 
                ('get, [`WithoutSuffix] as 'tipo, 'gn) params_type ->
                  ?error_handler:(Eliom_sessions.server_params -> (string * exn) list -> 
                    page Lwt.t) ->
                      (Eliom_sessions.server_params -> 'get -> unit -> page Lwt.t) ->
                        ('get, unit, 
                         [> `Attached of 
                           [> `Internal of [> `Coservice ] * [> `Get] ] a_s ], 
                         'tipo, 'gn, unit, 
                         [> `Registrable ])
                          service
(** Same as [new_coservice] followed by [register_for_session] *)

    val register_new_coservice_for_session' :
        ?options:options ->
        ?session_name:string ->
        sp:Eliom_sessions.server_params ->
        ?max_use:int ->
        ?timeout:float ->
          get_params: 
            ('get, [`WithoutSuffix] as 'tipo, 'gn) params_type ->
              ?error_handler:(Eliom_sessions.server_params -> (string * exn) list -> 
                page Lwt.t) ->
                  (Eliom_sessions.server_params -> 'get -> unit -> page Lwt.t) ->
                    ('get, unit, [> `Nonattached of [> `Get] na_s ], 
                     'tipo, 'gn, unit, 
                     [> `Registrable ])
                      service
(** Same as [new_coservice'] followed by [register_for_session] *)

    val register_new_post_service :
        ?options:options ->
        ?sp: Eliom_sessions.server_params ->
        fallback:('get, unit, 
                  [ `Attached of [ `Internal of 
                    ([ `Service | `Coservice ] as 'kind) * [`Get] ] a_s ],
                  [< suff ] as 'tipo, 'gn,
                  unit, [< `Registrable ])
        service ->
          post_params:('post, [ `WithoutSuffix ], 'pn) params_type ->
            ?error_handler:(Eliom_sessions.server_params -> (string * exn) list -> 
              page Lwt.t) ->
                (Eliom_sessions.server_params -> 'get -> 'post -> page Lwt.t) ->
                  ('get, 'post, [> `Attached of
                    [> `Internal of 'kind * [> `Post] ] a_s ], 
                   'tipo, 'gn, 'pn, [> `Registrable ])
                    service
(** Same as [new_post_service] followed by [register] *)

    val register_new_post_service' :
        ?options:options ->
        ?sp: Eliom_sessions.server_params ->
          name: string ->
            post_params:('post, [ `WithoutSuffix ], 'pn) params_type ->
              ?error_handler:(Eliom_sessions.server_params -> (string * exn) list -> 
                page Lwt.t) ->
                  (Eliom_sessions.server_params -> unit -> 'post -> page Lwt.t) ->
                    (unit, 'post, [> `Nonattached of [> `Post ] na_s ], 
                     [ `WithoutSuffix ], unit, 'pn, [> `Registrable ])
                      service
(** Same as [new_post_service'] followed by [register] *)

    val register_new_post_coservice :
        ?options:options ->
        ?sp: Eliom_sessions.server_params ->
        ?max_use:int ->
        ?timeout:float ->
        fallback:('get, unit , 
                  [ `Attached of 
                    [ `Internal of [< `Service | `Coservice ] * [`Get] ] a_s ],
                   [< suff ] as 'tipo, 
                   'gn, unit, [< `Registrable ])
        service ->
          post_params:('post, [ `WithoutSuffix ], 'pn) params_type ->
            ?error_handler:(Eliom_sessions.server_params -> (string * exn) list -> 
              page Lwt.t) ->
                (Eliom_sessions.server_params -> 'get -> 'post -> page Lwt.t) ->
                  ('get, 'post, 
                   [> `Attached of 
                     [> `Internal of [> `Coservice ] * [> `Post] ] a_s ], 
                     'tipo, 'gn, 'pn, [> `Registrable ])
                    service
(** Same as [new_post_coservice] followed by [register] *)

    val register_new_post_coservice' :
        ?options:options ->
        ?sp: Eliom_sessions.server_params ->
        ?max_use:int ->
        ?timeout:float ->
        post_params:('post, [ `WithoutSuffix ], 'pn) params_type ->
          ?error_handler:(Eliom_sessions.server_params -> (string * exn) list -> 
            page Lwt.t) ->
              (Eliom_sessions.server_params -> unit -> 'post -> page Lwt.t) ->
                (unit, 'post, [> `Nonattached of [> `Post] na_s ], 
                 [ `WithoutSuffix ], unit, 'pn,
                 [> `Registrable ])
                  service
(** Same as [new_post_coservice'] followed by [register] *)

(*
    val register_new_get_post_coservice' :
        ?options:options ->
        ?sp: Eliom_sessions.server_params ->
        ?max_use:int ->
        ?timeout:float ->
        fallback:('get, unit , 
                  [ `Nonattached of [`Get] na_s ],
                   [< suff ] as 'tipo, 
                   'gn, unit, [< `Registrable ])
        service ->
          post_params:('post, [ `WithoutSuffix ], 'pn) params_type ->
            ?error_handler:(Eliom_sessions.server_params -> (string * exn) list -> 
              page Lwt.t) ->
                (Eliom_sessions.server_params -> 'get -> 'post -> page Lwt.t) ->
                  ('get, 'post, [> `Nonattached of [> `Post] na_s ], 
                   [> 'tipo], 'gn, 'pn, [> `Registrable ])
                    service
(* * Same as [new_get_post_coservice'] followed by [register] *)
*)

    val register_new_post_coservice_for_session :
        ?options:options ->
        ?session_name:string ->
        sp:Eliom_sessions.server_params ->
        ?max_use:int ->
        ?timeout:float ->
          fallback:('get, unit, 
                    [ `Attached of [ `Internal of
                      [< `Service | `Coservice ] * [`Get] ] a_s ],
                    [< suff ] as 'tipo, 
                    'gn, unit, [ `Registrable ])
            service ->
              post_params:('post, [ `WithoutSuffix ], 'pn) params_type ->
                ?error_handler:(Eliom_sessions.server_params -> 
                  (string * exn) list -> page Lwt.t) ->
                    (Eliom_sessions.server_params -> 'get -> 'post -> page Lwt.t) ->
                      ('get, 'post, 
                       [> `Attached of 
                         [> `Internal of [> `Coservice ] * [> `Post]] a_s ], 
                       'tipo, 'gn, 'pn, [> `Registrable ])
                        service
(** Same as [new_post_coservice] followed by [register_for_session] *)

    val register_new_post_coservice_for_session' :
        ?options:options ->
        ?session_name:string ->
        sp:Eliom_sessions.server_params ->
        ?max_use:int ->
        ?timeout:float ->
          post_params:('post, [ `WithoutSuffix ], 'pn) params_type ->
            ?error_handler:(Eliom_sessions.server_params -> 
              (string * exn) list -> page Lwt.t) ->
                (Eliom_sessions.server_params -> unit -> 'post -> page Lwt.t) ->
                  (unit, 'post, [> `Nonattached of [> `Post] na_s ], 
                   [ `WithoutSuffix ], unit, 'pn, 
                   [> `Registrable ])
                    service
(** Same as [new_post_coservice'] followed by [register_for_session] *)

(*
    val register_new_get_post_coservice_for_session' :
        ?options:options ->
        ?session_name:string ->
        sp:Eliom_sessions.server_params ->
        ?max_use:int ->
        ?timeout:float ->
          fallback:('get, unit, [ `Nonattached of [`Get] na_s ],
                    [< suff ] as 'tipo, 
                    'gn, unit, [< `Registrable ])
            service ->
              post_params:('post, [ `WithoutSuffix ], 'pn) params_type ->
                ?error_handler:(Eliom_sessions.server_params -> 
                  (string * exn) list -> page Lwt.t) ->
                    (Eliom_sessions.server_params -> 'get -> 'post -> page Lwt.t) ->
                      ('get, 'post, [> `NonAttached of [> `Post] na_s ], 
                       'tipo, 'gn, 'pn, [> `Registrable ])
                        service
(* * Same as [new_get_post_coservice] followed by [register_for_session] *)
*)


  end



module type ELIOMREGSIG =
  sig
    include ELIOMREGSIG1
    module Cookies : ELIOMREGSIG1 
    with type page = page * Eliom_services.cookie list
  end





module MakeRegister = functor
  (Pages : REGCREATE) ->
    (struct

      type page = Pages.page

      type options = Pages.options

      let send = Pages.send

      module Cookies = struct
        
        type page = Pages.page * Eliom_services.cookie list

        type options = Pages.options
              
        let send ?options ?(cookies=[]) ?charset ?code ~sp (p, cl) =
          Pages.send ?options ~cookies:(cookies@cl) ?charset ?code ~sp p

        let register_aux
            ?options
            table
            duringsession (* registering during session? *)
            ~service
            ?(error_handler = fun sp l -> 
              raise (Eliom_common.Eliom_Typing_Error l))
            page_generator =
          match get_kind_ service with
          | `Attached attser ->
              let key_kind = get_or_post (get_att_kind_ attser) in
              let attserget = get_get_state_ attser in
              let attserpost = get_post_state_ attser in
              let sgpt = get_get_params_type_ service in
              let sppt = get_post_params_type_ service in
              Eliommod_services.add_service
                table 
	        duringsession
	        (get_sub_path_ attser)
                ({Eliom_common.key_state = (attserget, attserpost);
                  Eliom_common.key_kind = key_kind},
                 ((if attserget = None || attserpost = None 
                 then (anonymise_params_type sgpt,
                       anonymise_params_type sppt)
                 else (0, 0)),
                  (match get_max_use_ service with
                  | None -> None
                  | Some i -> Some (ref i)),
                  (match get_timeout_ service with
                  | None -> None
                  | Some t -> Some (t, ref (t +. Unix.time ()))),
                  (fun sp ->
                    let sp2 = Eliom_sessions.sp_of_esp sp in
                    let ri = get_ri ~sp:sp2 in
                    let suff = get_suffix ~sp:sp2 in
                    (catch (fun () -> 
                      (force ri.ri_post_params) >>=
                      (fun post_params ->
                        (force ri.ri_files) >>=
                        (fun files ->
                          (page_generator sp2
                             (reconstruct_params 
                                sgpt
                                (force ri.ri_get_params)
                                []
                                suff)
                             (reconstruct_params
                                sppt
                                post_params
                                files
                                [])))))
                       (function
                         | Eliom_common.Eliom_Typing_Error l -> 
                             error_handler sp2 l
                         | e -> fail e)) >>=
                    (fun (content, cookies_to_set) -> 
                      Pages.send
                        ?options ~cookies:cookies_to_set ~sp:sp2 content >>=
                      fun r -> Lwt.return (Eliom_services.erts_of_rst r)
                    )
                  )
                 )
                )
          | `Nonattached naser ->
              Eliommod_naservices.add_naservice 
	        table
	        duringsession
	        (get_na_name_ naser)
                ((match get_max_use_ service with
                | None -> None
                | Some i -> Some (ref i)),
                 (match get_timeout_ service with
                 | None -> None
                 | Some t -> Some (t, ref (t +. Unix.time ()))),
	         (fun sp ->
                   let sp2 = Eliom_sessions.sp_of_esp sp in
                   let ri = get_ri sp2 in
	           (catch
	              (fun () ->
	                (get_post_params sp2) >>=
	                (fun post_params ->
		          (force ri.ri_files) >>=
		          (fun files ->
                            (page_generator sp2
                               (reconstruct_params
                                  (get_get_params_type_ service)
                                  (force ri.ri_get_params)
                                  []
                                  [])
                               (reconstruct_params
                                  (get_post_params_type_ service)
                                  post_params
                                  files
                                  [])))))
	              (function
                        | Eliom_common.Eliom_Typing_Error l -> 
                            error_handler sp2 l
                        | e -> fail e)) >>=
                   (fun (content, cookies_to_set) -> 
                     Pages.send ?options ~cookies:cookies_to_set ~sp:sp2 content
                     >>= fun r -> Lwt.return (Eliom_services.erts_of_rst r))))


        let register ?options ?sp ~service ?error_handler page_gen =
          match sp with
          | None ->
              (match Eliom_common.global_register_allowed () with
              | Some get_current_sitedata ->
                  let sitedata = get_current_sitedata () in
                  (match get_kind_ service with
                  | `Attached attser -> 
                      Eliom_common.remove_unregistered
                        sitedata (get_sub_path_ attser)
                  | `Nonattached naser ->
                      Eliom_common.remove_unregistered_na
                        sitedata (get_na_name_ naser));
                  register_aux 
                    ?options
                    sitedata.Eliom_common.global_services
                    false 
                    ~service ?error_handler page_gen
              | _ -> raise 
                    (Eliom_common.Eliom_function_forbidden_outside_site_loading
                       "register"))
          | Some sp ->
              register_aux
                ?options
                ?error_handler
                (get_global_table sp)
                true
                ~service
                page_gen


(* WARNING: if we create a new service without registering it,
   we can have a link towards a page that does not exist!!! :-(
   That's why I impose to register all service during init.
   The only other way I see to avoid this is to impose a syntax extension
   like "let rec" for service...
 *)



        let register_for_session
            ?options
            ?session_name
            ~sp
            ~service
            ?error_handler
            page =
          register_aux
            ?options
            ?error_handler
            !(get_session_service_table ?session_name ~sp ())
            true 
            ~service page



        let register_new_service 
            ?options
            ?sp
            ~path
            ~get_params
            ?error_handler
            page =
          let u = new_service ?sp ~path ~get_params () in
          register ?options ?sp ~service:u ?error_handler page;
          u
            
        let register_new_service'
            ?options
            ?sp
            ~name
            ~get_params
            ?error_handler
            page =
          let u = new_service' ?sp ~name ~get_params () in
          register ?options ?sp ~service:u ?error_handler page;
          u
            
        let register_new_coservice
            ?options
            ?sp
            ?max_use
            ?timeout
            ~fallback
            ~get_params
            ?error_handler
            page =
          let u = new_coservice ?max_use ?timeout ~fallback ~get_params () in
          register ?options ?sp ~service:u ?error_handler page;
          u

        let register_new_coservice'
            ?options 
            ?sp
            ?max_use
            ?timeout
            ~get_params
            ?error_handler
            page =
          let u = new_coservice' ?max_use ?timeout ~get_params () in
          register ?options ?sp ~service:u ?error_handler page;
          u

        let register_new_coservice_for_session
            ?options 
            ?session_name
            ~sp
            ?max_use
            ?timeout
            ~fallback
            ~get_params
            ?error_handler
            page =
          let u = new_coservice ?max_use ?timeout ~fallback ~get_params () in
          register_for_session 
            ?options ?session_name ~sp ~service:u ?error_handler page;
          u

        let register_new_coservice_for_session'
            ?options 
            ?session_name
            ~sp
            ?max_use
            ?timeout
            ~get_params
            ?error_handler
            page =
          let u = new_coservice' ?max_use ~get_params () in
          register_for_session 
            ?options ?session_name ~sp ~service:u ?error_handler page;
          u


        let register_new_post_service 
            ?options 
            ?sp
            ~fallback
            ~post_params
            ?error_handler
            page_gen =
          let u = new_post_service ?sp
              ~fallback:fallback ~post_params:post_params () in
          register ?options ?sp ~service:u ?error_handler page_gen;
          u

        let register_new_post_service'
            ?options 
            ?sp 
            ~name
            ~post_params
            ?error_handler
            page_gen =
          let u = new_post_service' ~name ~post_params:post_params () in
          register ?options ?sp ~service:u ?error_handler page_gen;
          u

        let register_new_post_coservice
            ?options 
            ?sp
            ?max_use
            ?timeout
            ~fallback
            ~post_params
            ?error_handler
            page_gen =
          let u = 
            new_post_coservice ?max_use ?timeout ~fallback ~post_params () in
          register ?options ?sp ~service:u ?error_handler page_gen;
          u

        let register_new_post_coservice'
            ?options 
            ?sp
            ?max_use
            ?timeout
            ~post_params
            ?error_handler
            page_gen =
          let u = new_post_coservice' ?max_use ?timeout ~post_params () in
          register ?options ?sp ~service:u ?error_handler page_gen;
          u

(*
   let register_new_get_post_coservice'
  ?options 
  ?sp
   ?max_use
   ?timeout
   ~fallback
   ~post_params
   ?error_handler
   page_gen =
   let u = new_get_post_coservice'
   ?max_use ?timeout ~fallback ~post_params () in
   register ?options ?sp ~service:u ?error_handler page_gen;
   u
 *)

        let register_new_post_coservice_for_session
            ?options 
            ?session_name
            ~sp
            ?max_use
            ?timeout
            ~fallback
            ~post_params
            ?error_handler
            page_gen =
          let u = new_post_coservice 
              ?max_use ?timeout ~fallback ~post_params () in
          register_for_session
            ?options ?session_name ~sp ~service:u ?error_handler page_gen;
          u

        let register_new_post_coservice_for_session'
            ?options 
            ?session_name
            ~sp
            ?max_use
            ?timeout
            ~post_params
            ?error_handler
            page_gen =
          let u = new_post_coservice' ?max_use ?timeout ~post_params () in
          register_for_session
            ?options ?session_name ~sp ~service:u ?error_handler page_gen;
          u

(*
   let register_new_get_post_coservice_for_session'
  ?options 
  ?session_name
   ~sp
   ?max_use
   ?timeout
   ~fallback
   ~post_params
   ?error_handler
   page_gen =
   let u = new_get_post_coservice'
   ?max_use ?timeout ~fallback ~post_params () in
   register_for_session 
  ?options ?session_name ~sp ~service:u ?error_handler page_gen;
   u
 *)


      end


      let make_error_handler ?error_handler () = 
        match error_handler with
          None -> None
        | Some eh -> Some (fun sp l -> eh sp l >>= (fun r -> return (r, [])))

      let register ?options ?sp ~service ?error_handler page_gen =
        Cookies.register
          ?options
          ?sp
          ~service
          ?error_handler:(make_error_handler ?error_handler ())
          (fun sp g p -> page_gen sp g p >>= (fun r -> return (r, [])))

      let register_for_session
          ?options
          ?session_name
          ~sp
          ~service
          ?error_handler
          page =
        Cookies.register_for_session
          ?options
          ?session_name
          ~sp
          ~service
          ?error_handler:(make_error_handler ?error_handler ())
          (fun sp g p -> page sp g p >>= (fun r -> return (r, [])))

      let register_new_service 
          ?options
          ?sp
          ~path
          ~get_params
          ?error_handler
          page =
        Cookies.register_new_service 
          ?options
          ?sp
          ~path
          ~get_params
          ?error_handler:(make_error_handler ?error_handler ())
          (fun sp g p -> page sp g p >>= (fun r -> return (r, [])))
          
      let register_new_service'
          ?options
          ?sp
          ~name
          ~get_params
          ?error_handler
          page =
        Cookies.register_new_service'
          ?options
          ?sp
          ~name
          ~get_params
          ?error_handler:(make_error_handler ?error_handler ())
          (fun sp g p -> page sp g p >>= (fun r -> return (r, [])))
          
      let register_new_coservice
          ?options
          ?sp
          ?max_use
          ?timeout
          ~fallback
          ~get_params
          ?error_handler
          page =
        Cookies.register_new_coservice
          ?options
          ?sp
          ?max_use
          ?timeout
          ~fallback
          ~get_params
          ?error_handler:(make_error_handler ?error_handler ())
          (fun sp g p -> page sp g p >>= (fun r -> return (r, [])))

      let register_new_coservice'
          ?options
          ?sp
          ?max_use
          ?timeout
          ~get_params
          ?error_handler
          page =
        Cookies.register_new_coservice'
          ?options
          ?sp
          ?max_use
          ?timeout
          ~get_params
          ?error_handler:(make_error_handler ?error_handler ())
          (fun sp g p -> page sp g p >>= (fun r -> return (r, [])))

      let register_new_coservice_for_session
          ?options
          ?session_name
          ~sp
          ?max_use
          ?timeout
          ~fallback
          ~get_params
          ?error_handler
          page =
      Cookies.register_new_coservice_for_session
          ?options
          ?session_name
          ~sp
          ?max_use
          ?timeout
          ~fallback
          ~get_params
          ?error_handler:(make_error_handler ?error_handler ())
          (fun sp g p -> page sp g p >>= (fun r -> return (r, [])))

      let register_new_coservice_for_session'
          ?options
          ?session_name
          ~sp
          ?max_use
          ?timeout
          ~get_params
          ?error_handler
          page =
      Cookies.register_new_coservice_for_session'
          ?options
          ?session_name
          ~sp
          ?max_use
          ?timeout
          ~get_params
          ?error_handler:(make_error_handler ?error_handler ())
          (fun sp g p -> page sp g p >>= (fun r -> return (r, [])))

      let register_new_post_service 
          ?options
          ?sp
          ~fallback
          ~post_params
          ?error_handler
          page_gen =
      Cookies.register_new_post_service 
          ?options
          ?sp
          ~fallback
          ~post_params
          ?error_handler:(make_error_handler ?error_handler ())
          (fun sp g p -> page_gen sp g p >>= (fun r -> return (r, [])))

      let register_new_post_service'
          ?options
          ?sp
          ~name
          ~post_params
          ?error_handler
          page_gen =
      Cookies.register_new_post_service'
          ?options
          ?sp
          ~name
          ~post_params
          ?error_handler:(make_error_handler ?error_handler ())
          (fun sp g p -> page_gen sp g p >>= (fun r -> return (r, [])))

      let register_new_post_coservice
          ?options
          ?sp
          ?max_use
          ?timeout
          ~fallback
          ~post_params
          ?error_handler
          page_gen =
        Cookies.register_new_post_coservice
          ?options
          ?sp
          ?max_use
          ?timeout
          ~fallback
          ~post_params
          ?error_handler:(make_error_handler ?error_handler ())
          (fun sp g p -> page_gen sp g p >>= (fun r -> return (r, [])))

      let register_new_post_coservice'
          ?options
          ?sp
          ?max_use
          ?timeout
          ~post_params
          ?error_handler
          page_gen =
        Cookies.register_new_post_coservice'
          ?options
          ?sp
          ?max_use
          ?timeout
          ~post_params
          ?error_handler:(make_error_handler ?error_handler ())
          (fun sp g p -> page_gen sp g p >>= (fun r -> return (r, [])))

(*
   let register_new_get_post_coservice'
  ?options
  ?sp
          ?max_use
          ?timeout
   ~fallback
   ~post_params
   ?error_handler
   page_gen =
   Cookies.register_new_get_post_coservice'
  ?options
   ?sp
          ?max_use
          ?timeout
   ~fallback
   ~post_params
   ?error_handler:(make_error_handler ?error_handler ())
          (fun sp g p -> page_gen sp g p >>= (fun r -> return (r, [])))

 *)

      let register_new_post_coservice_for_session
          ?options
          ?session_name
          ~sp
          ?max_use
          ?timeout
          ~fallback
          ~post_params
          ?error_handler
          page_gen =
        Cookies.register_new_post_coservice_for_session
          ?options
          ?session_name
          ~sp
          ?max_use
          ?timeout
          ~fallback
          ~post_params
          ?error_handler:(make_error_handler ?error_handler ())
          (fun sp g p -> page_gen sp g p >>= (fun r -> return (r, [])))

      let register_new_post_coservice_for_session'
          ?options
          ?session_name
          ~sp
          ?max_use
          ?timeout
          ~post_params
          ?error_handler
          page_gen =
      Cookies.register_new_post_coservice_for_session'
          ?options
          ?session_name
          ~sp
          ?max_use
          ?timeout
          ~post_params
          ?error_handler:(make_error_handler ?error_handler ())
          (fun sp g p -> page_gen sp g p >>= (fun r -> return (r, [])))

(*
   let register_new_get_post_coservice_for_session'
  ?options
  ?session_name
   sp
  ?max_use
  ?timeout
   ~fallback
   ~post_params
   ?error_handler
   page_gen =
   Cookies.register_new_get_post_coservice_for_session'
  ?options
  ?session_name
  ~sp
  ?max_use
  ?timeout
  ~fallback
  ~post_params
  ?error_handler:(make_error_handler ?error_handler ())
          (fun sp g p -> page_gen sp g p >>= (fun r -> return (r, [])))

 *)


    end : ELIOMREGSIG with 
                 type page = Pages.page
                 and type options = Pages.options)

