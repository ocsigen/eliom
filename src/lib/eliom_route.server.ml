open Eliom_lib
open Lwt
open Ocsigen_extensions

include Eliom_route_base

include Eliom_route_base.Make (struct

    type site_data = Eliom_common.sitedata

    type info = Eliom_common.info

    let sess_info_of_info (_, i, _, _, _) = i

    let meth_of_info ({request_info}, _, _, _, _) =
      match Ocsigen_request.meth request_info with
      | `GET ->
        `Get
      | `POST ->
        `Post
      | `PUT ->
        `Put
      | `DELETE ->
        `Delete
      | _ ->
        `Other

    let subpath_of_info ({request_info}, _, _, _, _) =
      Ocsigen_request.sub_path request_info

    module Container = struct

      type t = Eliom_common.tables

      let set t v = t.Eliom_common.table_services <- v

      let get {Eliom_common.table_services} = table_services

      let dlist_add ?sp tables lr =
        tables.Eliom_common.service_dlist_add ?sp lr

      let set_contains_timeout tables b =
        tables.Eliom_common.table_contains_services_with_timeout <- b

    end

    type params = Eliom_common.server_params

    type result = Ocsigen_response.t

    module Node = struct

      type t =
        (Eliom_common.page_table ref * Eliom_common.page_table_key,
         Eliom_common.na_key_serv) leftright Ocsigen_cache.Dlist.node

      let up = Ocsigen_cache.Dlist.up

      let remove = Ocsigen_cache.Dlist.remove

    end

    module Table = struct

      type t = Eliom_common.page_table

      let remove = Eliom_common.Serv_Table.remove

      let add k v t = Eliom_common.Serv_Table.add k (`Ptc v) t

      let find k t =
        let `Ptc v = Eliom_common.Serv_Table.find k t in v

      let empty () = Eliom_common.Serv_Table.empty

    end

    let make_params = Eliom_common.make_server_params

    let handle_directory (r, _, _, _, _) =
      Lwt.fail @@
      Ocsigen_extensions.Ocsigen_is_dir
        (Ocsigen_extensions.new_url_of_directory_request r)

    let get_number_of_reloads () =
      Ocsigen_extensions.get_numberofreloads ()

  end)

let find_aux now sitedata info e sci : Ocsigen_response.t Lwt.t =
  Eliom_common.Full_state_name_table.fold
    (fun fullsessname (_, r) beg ->
       try%lwt
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

let session_tables
    (_,
     _,
     ((service_cookies_info    , _, _),
      (secure_service_cookies_info, _, _)),
     ((service_cookies_info_tab, _, _),
      (secure_service_cookies_info_tab, _, _)),
     _) =

  [ !secure_service_cookies_info_tab, "secure tab session table" ;
    !service_cookies_info_tab, "tab session table" ;
    !secure_service_cookies_info, "secure session table" ;
    !service_cookies_info, "session table"]

let unflatten_get_params l =
  let module M = Eliom_lib.String.Table in
  M.bindings
    (List.fold_left
       (fun acc (id, v) ->
          M.add id (try v :: M.find id acc with Not_found -> [v]) acc)
       M.empty
       l)

let drop_most_params ri si =
  Ocsigen_request.update ri
    ~post_data:None
    ~meth:`GET
    ~get_params_flat:si.Eliom_common.si_other_get_params

let get_page
    now
    ((ri, si, all_cookie_info, all_tab_cookie_info, user_tab_cookies)
     as info)
    sitedata :
  Ocsigen_response.t Lwt.t =
  let tables = session_tables info in
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
                           (Ocsigen_request.sub_path ri.request_info)))
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
                    (Ocsigen_request.request_cache ri.request_info)
                    Eliom_common.eliom_link_too_old
                    true;
                  fail (Eliom_common.Eliom_retry_with
                          ({ri with
                            request_info =
                              Ocsigen_request.update
                                ri.request_info
                                ~post_data:None
                                ~meth:`GET
                           } ,
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
                    (Ocsigen_request.request_cache ri.request_info)
                    Eliom_common.eliom_link_too_old
                    true;
                  fail (Eliom_common.Eliom_retry_with
                          ({ri with
                            request_info =
                              drop_most_params ri.request_info si},
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

let add_naservice_table at (key, elt) =
  match at with
  | Eliom_common.AVide ->
    Eliom_common.ATable
      (Eliom_common.NAserv_Table.add
         key elt Eliom_common.NAserv_Table.empty)
  | Eliom_common.ATable t ->
    Eliom_common.ATable (Eliom_common.NAserv_Table.add key elt t)

let find_naservice_table at k =
  match at with
  | Eliom_common.AVide -> raise Not_found
  | Eliom_common.ATable t -> Eliom_common.NAserv_Table.find k t

let add_naservice
    tables
    name
    (max_use, expdate, naservice) =
  let sp = Eliom_common.get_sp_option () in
  let generation = Ocsigen_extensions.get_numberofreloads () in
  (if (sp = None) (* not duringsession *)
   then
     try
       let (g, _, _, _, _) =
         find_naservice_table !(tables.Eliom_common.table_naservices) name
       in
       if g = generation then
         match name with
         | Eliom_common.SNa_no
         | Eliom_common.SNa_get' _
         | Eliom_common.SNa_post' _ ->
           raise (Eliom_common.Eliom_duplicate_registration
                    "<non-attached coservice>")
         | Eliom_common.SNa_get_ n ->
           raise (Eliom_common.Eliom_duplicate_registration
                    ("GET non-attached service "^n))
         | Eliom_common.SNa_post_ n ->
           raise (Eliom_common.Eliom_duplicate_registration
                    ("POST non-attached service "^n))
         | Eliom_common.SNa_void_dontkeep
         | Eliom_common.SNa_void_keep ->
           raise (Eliom_common.Eliom_duplicate_registration
                    "<void coservice>")
         | Eliom_common.SNa_get_csrf_safe _
         | Eliom_common.SNa_post_csrf_safe _
           -> assert false
     with Not_found -> ());

  (match expdate with
   | Some _ -> tables.Eliom_common.table_contains_naservices_with_timeout
     <- true
   | _ -> ());

  let node = match name with
    | Eliom_common.SNa_get' _ | Eliom_common.SNa_post' _ ->
      Some
        (tables.Eliom_common.service_dlist_add ?sp (Right name))
    | _ -> None
  in

  tables.Eliom_common.table_naservices :=
    add_naservice_table !(tables.Eliom_common.table_naservices)
      (name, (generation, max_use, expdate, naservice, node))

let remove_naservice_ tables name nodeopt =
  match nodeopt with
  | None ->
    tables.Eliom_common.table_naservices :=
      Eliom_common.remove_naservice_table
        !(tables.Eliom_common.table_naservices) name
  | Some node ->
    Ocsigen_cache.Dlist.remove node

let find_naservice now tables name =
  let ((_, _, expdate, _, nodeopt) as p) =
    find_naservice_table !(tables.Eliom_common.table_naservices) name
  in
  match expdate with
  | Some (_, e) when !e < now ->
    (* Service expired. Removing it. *)
    Lwt_log.ign_info ~section "Non attached service expired. Removing it";
    remove_naservice_ tables name nodeopt;
    raise Not_found
  | _ ->
    (match nodeopt with
     | Some node -> Ocsigen_cache.Dlist.up node
     | None -> ());
    p

let remove_naservice tables name =
  let (_, _, _, _, nodeopt) =
    find_naservice_table !(tables.Eliom_common.table_naservices) name
  in
  remove_naservice_ tables name nodeopt

let make_naservice
    now
    ((ri, si, all_cookie_info, all_tab_cookie_info, user_tab_cookies)
     as info)
    sitedata =

  let find_aux sci =
    match
      Eliom_common.Full_state_name_table.fold
        (fun fullsessname (_, r) beg ->
           match beg with
           | Eliom_common.Found _ -> beg
           | Eliom_common.Notfound _ ->
             match !r with
             | Eliom_common.SCNo_data
             | Eliom_common.SCData_session_expired -> beg
             | Eliom_common.SC c ->
               try
                 Eliom_common.Found
                   ((find_naservice
                       now !(c.Eliom_common.sc_table)
                       (Eliom_common.na_key_serv_of_req
                          si.Eliom_common.si_nonatt_info)),
                    !(c.Eliom_common.sc_table),
                    Some fullsessname)
               with Not_found -> beg
        )
        sci
        (Eliom_common.Notfound ())
    with
    | Eliom_common.Found v -> v
    | Eliom_common.Notfound _ -> raise Not_found
  in

  let tables = session_tables info in

  (try
     try
       let rec f = function
         | [] -> raise Not_found
         | (table, table_name)::l ->
           Lwt_log.ign_info_f ~section
             "Looking for a non attached service in the %s:" table_name;
           try return (find_aux table)
           with Not_found -> f l
       in f tables
     with Not_found ->
       begin
         Lwt_log.ign_info ~section "Looking for a non attached service in the global table";
         return
           (find_naservice now sitedata.Eliom_common.global_services
              (Eliom_common.na_key_serv_of_req si.Eliom_common.si_nonatt_info),
            sitedata.Eliom_common.global_services,
            None)
       end
   with Not_found ->
   (* The non-attached service has not been found.
      We call the same URL without non-attached parameters.
   *)
   match si.Eliom_common.si_nonatt_info with
   | Eliom_common.RNa_no -> assert false
   | Eliom_common.RNa_post_ _
   | Eliom_common.RNa_post' _ ->
     (*VVV (Some, Some) or (_, Some)? *)
     Lwt_log.ign_info ~section "Link too old to a non-attached POST coservice. Try without POST parameters:";
     Polytables.set
       (Ocsigen_request.request_cache ri.request_info)
       Eliom_common.eliom_link_too_old
       true;
     Eliom_common.get_session_info
       { ri with
         Ocsigen_extensions.request_info =
           drop_most_params ri.request_info si }
       si.Eliom_common.si_previous_extension_error
     >>= fun (ri', si', previous_tab_cookies_info) ->
     Lwt.fail (Eliom_common.Eliom_retry_with (ri',
                                              si',
                                              all_cookie_info,
                                              all_tab_cookie_info,
                                              user_tab_cookies))

   | Eliom_common.RNa_get_ _
   | Eliom_common.RNa_get' _ ->
     Lwt_log.ign_info ~section "Link too old. Try without non-attached parameters:";
     Polytables.set
       (Ocsigen_request.request_cache ri.request_info)
       Eliom_common.eliom_link_too_old
       true;
     Eliom_common.get_session_info
       { ri with
         Ocsigen_extensions.request_info =
           drop_most_params ri.request_info si }
       si.Eliom_common.si_previous_extension_error
     >>= fun (ri', si', previous_tab_cookies_info) ->
     Lwt.fail (Eliom_common.Eliom_retry_with (ri', si',
                                              all_cookie_info,
                                              all_tab_cookie_info,
                                              user_tab_cookies)))

  >>= fun ((_, max_use, expdate, naservice, node),
           tablewhereithasbeenfound,
           fullsessname) ->
  let sp =
    Eliom_common.make_server_params sitedata info None fullsessname in
  naservice sp >>= fun r ->
  Lwt_log.ign_info ~section "Non attached page found and generated successfully";
  (match expdate with
   | Some (timeout, e) -> e := timeout +. now
   | None -> ());
  (match max_use with
   | None -> ()
   | Some r ->
     if !r = 1
     then
       remove_naservice_
         tablewhereithasbeenfound
         (Eliom_common.na_key_serv_of_req si.Eliom_common.si_nonatt_info)
         node
     else r := !r - 1);
  return r
