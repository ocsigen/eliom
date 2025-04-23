open Lwt.Syntax

(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2014 Hugo Heuzard
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

(* provides infos about the server (number of sessions, uptime...) *)

let uptime =
  let launchtime = Unix.time () in
  fun () -> Unix.time () -. launchtime

let pid () = Unix.getpid ()

let fd ~pid =
  try
    let a = Array.length (Sys.readdir (Printf.sprintf "/proc/%d/fd" pid)) - 2 in
    `Ok a
  with e -> `Error (Printexc.to_string e)

let ppf fmt = Printf.ksprintf Eliom_content.Html.D.txt fmt

let format_duration t =
  let open Unix in
  let tm = gmtime t in
  let year =
    if tm.tm_year > 0 then string_of_int (tm.tm_year - 70) ^ " years, " else ""
  in
  let days = string_of_int tm.tm_yday ^ " days, " in
  let hour = string_of_int tm.tm_hour ^ " hours, " in
  let min = string_of_int tm.tm_min ^ " min, " in
  let sec = string_of_int tm.tm_sec ^ " seconds" in
  year ^ days ^ hour ^ min ^ sec

open Eliom_content.Html.F

let general_stats () =
  let pid = pid () in
  div
    [ dl
        [ dt [ppf "Version of Ocsigen"]
        ; dd [ppf "%s" Ocsigen_config.version_number]
        ; dt [ppf "Uptime"]
        ; dd [ppf "%s" (format_duration (uptime ()))]
        ; dt [ppf "PID"]
        ; dd [ppf "%d" pid]
        ; dt [ppf "Numbers of file descriptors"]
        ; dd
            [ (match fd ~pid with
              | `Error s ->
                  ppf "Information on file descriptors not accessible (%s)." s
              | `Ok fd -> ppf "%d file descriptors opened." fd) ] ] ]

let gc_stats () =
  let stat = Gc.quick_stat () in
  div
    [ ul
        [ li [ppf "%d minor garbage collections." stat.Gc.minor_collections]
        ; li [ppf "%d major collections." stat.Gc.major_collections]
        ; li [ppf "%d compactions of the heap." stat.Gc.compactions]
        ; li
            [ ppf "%d words in the the major heap (max %d)." stat.Gc.heap_words
                stat.Gc.top_heap_words ] ] ]

let lwt_stats () =
  div
    [ ul
        [ li
            [ ppf "%d lwt threads waiting for inputs"
                (Lwt_engine.readable_count ()) ]
        ; li
            [ ppf "%d lwt threads waiting for outputs"
                (Lwt_engine.writable_count ()) ]
        ; li [ppf "%d sleeping lwt threads" (Lwt_engine.timer_count ())] ] ]

let preemptive_thread_stats () =
  div
    [ ul
        [ li
            [ ppf "%d detached threads (min %d,max %d)."
                (Lwt_preemptive.nbthreads ())
                (Ocsigen_config.get_minthreads ())
                (Ocsigen_config.get_maxthreads ()) ]
        ; li [ppf "%d are busy threads." (Lwt_preemptive.nbthreadsbusy ())]
        ; li
            [ ppf "%d computations queued (max %d)."
                (Lwt_preemptive.nbthreadsqueued ())
                (Ocsigen_config.get_max_number_of_threads_queued ()) ] ] ]

let http_stats () =
  let hosts = Ocsigen_extensions.get_hosts () in
  div
    [ ul
        [ li
            [ ppf "%d open connection"
                (Ocsigen_extensions.get_number_of_connected ()) ] ]
    ; h3 [txt "Hosts"]
    ; ul
        (List.map
           (fun (vhosts, config, _) ->
              let all_vhost =
                String.concat ", "
                  (List.map
                     (fun (vhost, _, vport) ->
                        let optport =
                          match vport with
                          | None -> ""
                          | Some p -> Printf.sprintf ":%d" p
                        in
                        Printf.sprintf
                          "%s%s ( default: %s,  http: %d, https: %d )" vhost
                          optport config.Ocsigen_extensions.default_hostname
                          config.Ocsigen_extensions.default_httpport
                          config.Ocsigen_extensions.default_httpsport)
                     vhosts)
              in
              li [txt (all_vhost : string)])
           hosts) ]

let eliom_stats () =
  let* persist_nb_of_groups = Eliommod_sessiongroups.Pers.nb_of_groups () in
  let* number_of_persistent_data_cookies =
    Eliom_state.number_of_persistent_data_cookies ()
  in
  Lwt.return
    (div
       [ h3 [ppf "Sessions"]
       ; ul
           [ li
               [ ppf "%d service cookies."
                   (Eliom_state.number_of_service_cookies ()) ]
           ; li
               [ ppf "%d volatile data cookies."
                   (Eliom_state.number_of_volatile_data_cookies ()) ]
           ; li
               [ ppf "%d volatile data tables (volatile Eliom references)."
                   (Eliom_state.number_of_tables ()) ]
           ; li
               [ ppf "%d persistent data cookies."
                   number_of_persistent_data_cookies ]
           ; li
               [ ppf "%d persistent data tables (persistent data reference)."
                   (Eliom_state.number_of_persistent_tables ()) ] ]
       ; h3 [ppf "Client processes"]
       ; p [em [txt "Not implemented yet"]]
       ; h3 [ppf "Session groups"]
       ; ul
           [ li
               [ ppf "%d service session groups."
                   (Eliommod_sessiongroups.Serv.nb_of_groups ()) ]
           ; li
               [ ppf "%d volatile data session groups."
                   (Eliommod_sessiongroups.Data.nb_of_groups ()) ]
           ; li [ppf "%d persistent data session groups." persist_nb_of_groups]
           ; li
               [ ppf "Session groups: %s"
                   (String.concat ", "
                      (Eliom_state.Ext.get_session_group_list ())) ] ] ])

let content_div () =
  let* eliom_stats = eliom_stats () in
  Lwt.return
    (div
       [ h1 [ppf "Ocsigen server monitoring"]
       ; general_stats ()
       ; h2 [ppf "HTTP connexions"]
       ; http_stats ()
       ; h2 [ppf "Eliom sessions"]
       ; eliom_stats
       ; h2 [ppf "GC"]
       ; gc_stats ()
       ; h2 [ppf "Lwt threads"]
       ; lwt_stats ()
       ; h2 [ppf "Preemptive threads"]
       ; preemptive_thread_stats () ])

let content_html () =
  let* content_div = content_div () in
  Lwt.return
    (html
       (head
          (title (txt "Server monitoring"))
          [ link ~rel:[`Stylesheet]
              ~href:
                (uri_of_string (fun () ->
                   "//netdna.bootstrapcdn.com/bootstrap/3.1.1/css/bootstrap.min.css"))
              ()
          ; link ~rel:[`Stylesheet]
              ~href:
                (uri_of_string (fun () ->
                   "//netdna.bootstrapcdn.com/bootstrap/3.1.1/css/bootstrap-theme.min.css"))
              () ])
       (body [div ~a:[a_class ["container"]] [content_div]]))
