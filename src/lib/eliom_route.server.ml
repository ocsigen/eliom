open Eliom_lib
open Lwt
open Ocsigen_extensions

include Eliom_route_base

include Eliom_route_base.Make (struct

    type site_data = Eliom_common.sitedata

    type info = Eliom_common.info

    let sess_info_of_info (_, i, _, _, _) = i

    let request_of_info (i, _, _, _, _) = i

    type tables = Eliom_common.tables

    let set_tables_services t v = t.Eliom_common.table_services <- v

    let tables_services {Eliom_common.table_services} = table_services

    let service_dlist_add ?sp tables lr =
      tables.Eliom_common.service_dlist_add ?sp lr

    let set_contains_timeout tables b =
      tables.Eliom_common.table_contains_services_with_timeout <- b

    type result = Ocsigen_http_frame.result

    module Table = struct

      type t = Eliom_common.page_table

      type node =
        (t ref * Eliom_common.page_table_key,
         Eliom_common.na_key_serv) leftright

      let remove = Eliom_common.Serv_Table.remove

      let add k v t = Eliom_common.Serv_Table.add k (`Ptc v) t

      let find k t =
        let `Ptc v = Eliom_common.Serv_Table.find k t in v

      let empty () = Eliom_common.Serv_Table.empty

    end

    let make_server_params = Eliom_common.make_server_params

  end)

let find_aux now sitedata info e sci : Ocsigen_http_frame.Result.result Lwt.t =
  Eliom_common.Full_state_name_table.fold
    (fun fullsessname (_, r) beg ->
       try_lwt
         beg
       with
       | Eliom_common.Eliom_404 | Eliom_common.Eliom_Wrong_parameter ->
         (match !r with
          | Eliom_common.SCData_session_expired
          | Eliom_common.SCNo_data (* cookie removed *) ->
            beg
          | Eliom_common.SC c ->
            find_service
              now !(c.Eliom_common.sc_table) (Some fullsessname)
              sitedata info)
       | e ->
         fail e)
    sci
    (fail Eliom_common.Eliom_404)

let get_page
    now
    ((ri,
      si,
      (((service_cookies_info, _, _), secure_ci) as all_cookie_info),
      (((service_cookies_info_tab, _, _), secure_ci_tab)
       as all_tab_cookie_info),
      user_tab_cookies) as info)
    sitedata :
  Ocsigen_http_frame.result Lwt.t
  =

  let tables = [] in
  let tables = (!service_cookies_info, "session table") :: tables in
  let tables =
    match secure_ci with
    | Some (service_cookies_info, _, _) ->
      (!service_cookies_info, "secure session table") :: tables
    | _ ->
      tables
  in
  let tables = (!service_cookies_info_tab, "tab session table") :: tables in
  let tables =
    match secure_ci_tab with
      | Some (service_cookies_info, _, _) ->
        (!service_cookies_info, "secure tab session table") :: tables
      | _ ->
        tables
  in
  (catch
     (fun () ->
        List.fold_left
          (fun beg (table, table_name) ->
             Lwt.catch
               (fun () -> beg)
               (function
                 | Eliom_common.Eliom_404
                 | Eliom_common.Eliom_Wrong_parameter ->
                   Lwt_log.ign_info_f ~section "Looking for %a in the %s:"
                     (fun _ ri ->
                        (Url.string_of_url_path
                           ~encode:true
                           (Ocsigen_request_info.sub_path ri.request_info)))
                     ri
                     table_name;
                   find_aux now sitedata info Eliom_common.Eliom_404 table
                 | e -> Lwt.fail e))
          (Lwt.fail Eliom_common.Eliom_404)
          tables
     )
     (function
       | Eliom_common.Eliom_404
       | Eliom_common.Eliom_Wrong_parameter ->
         catch (* ensuite dans la table globale *)
           (fun () ->
             Lwt_log.ign_info ~section "Searching in the global table:";
             find_service
               now
               sitedata.Eliom_common.global_services
               None
               sitedata
               info)
           (function
             | Eliom_common.Eliom_404
             | Eliom_common.Eliom_Wrong_parameter as exn ->
                    (* si pas trouvé avec, on essaie sans l'état *)
               (match si.Eliom_common.si_state_info with
                 | (Eliom_common.RAtt_no, Eliom_common.RAtt_no) -> fail exn
                 | (g, Eliom_common.RAtt_anon _)
                 | (g, Eliom_common.RAtt_named _) ->
                   (* There was a POST state.
                           We remove it, and remove POST parameters.
                        *)
                   Lwt_log.ign_info ~section "Link too old. Try without POST parameters:";
                   Polytables.set
                     (Ocsigen_request_info.request_cache ri.request_info)
                     Eliom_common.eliom_link_too_old
                     true;
                   fail (Eliom_common.Eliom_retry_with
                           ({ri with request_info =
                             Ocsigen_request_info.update ri.request_info
                               ~post_params:
                                 (match Ocsigen_request_info.post_params ri.request_info with
                                  | None -> None
                                  | Some _ -> Some (fun _ -> Lwt.return []))
                               ~files:
                                 (match Ocsigen_request_info.files ri.request_info with
                                  | None -> None
                                  | Some _ -> Some (fun _ -> Lwt.return []))
                               ~meth:Ocsigen_http_frame.Http_header.GET
                             ()},
                            {si with
                              Eliom_common.si_nonatt_info=
                                Eliom_common.RNa_no;
                              Eliom_common.si_state_info=
                                (g, Eliom_common.RAtt_no);
                            },
                            all_cookie_info,
                            all_tab_cookie_info,
                            user_tab_cookies
                           ))
                 | (Eliom_common.RAtt_named _, Eliom_common.RAtt_no)
                 | (Eliom_common.RAtt_anon _, Eliom_common.RAtt_no) ->
                        (* There was a GET state, but no POST state.
                           We remove it with its parameters,
                           and remove POST parameters.
                        *)
                   Lwt_log.ign_info ~section "Link to old. Trying without GET state parameters and POST parameters:";
                   Polytables.set
                     (Ocsigen_request_info.request_cache ri.request_info)
                     Eliom_common.eliom_link_too_old
                     true;
                   fail (Eliom_common.Eliom_retry_with
                           ({ri with request_info =
                             Ocsigen_request_info.update ri.request_info
                               ~get_params:(lazy si.Eliom_common.si_other_get_params)
                               ~post_params:(match Ocsigen_request_info.post_params ri.request_info with
                                             | None -> None
                                             | Some _ -> Some (fun _ -> Lwt.return []))
                               ~files:(match Ocsigen_request_info.files ri.request_info with
                                       | None -> None
                                       | Some _ -> Some (fun _ -> Lwt.return []))
                               ~meth:Ocsigen_http_frame.Http_header.GET
                               ()
                            },
                            {si with
                              Eliom_common.si_nonatt_info=
                                Eliom_common.RNa_no;
                              Eliom_common.si_state_info=
                                (Eliom_common.RAtt_no,
                                 Eliom_common.RAtt_no);
                              Eliom_common.si_other_get_params=[];
                            },
                            all_cookie_info,
                            all_tab_cookie_info,
                            user_tab_cookies))
               )
             | e -> fail e)
       | e -> fail e)
  )
