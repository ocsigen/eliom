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

let (>>=) = Lwt.bind
let (!!) = Lazy.force

let current_fragment = ref ""
let url_fragment_prefix = "!"
let url_fragment_prefix_with_sharp = "#!"

let create_request_
    ?absolute ?absolute_path ?https
    ~(service : ('get, 'post, 
                 [< `Attached of (Eliom_services.attached_service_kind, [< Eliom_services.getpost]) Eliom_services.a_s
                 | `Nonattached of [< Eliom_services.getpost] Eliom_services.na_s ],
                 [< Eliom_services.suff ], 'h, 'i,
                 [< Eliom_services.registrable ], 'j)
        Eliom_services.service)
    ?hostname ?port ?fragment ?keep_nl_params ?nl_params ?keep_get_na_params
    g p =
  match Eliom_services.get_get_or_post service with
    | `Get ->
        let uri =
          Eliom_uri.make_string_uri
            ?absolute ?absolute_path ?https
            ~service
            ?hostname ?port ?fragment ?keep_nl_params ?nl_params g
        in
        Ocsigen_lib.Left uri
    | `Post ->
        let path, g, fragment, p =
          Eliom_uri.make_post_uri_components
            ?absolute ?absolute_path ?https
            ~service
            ?hostname ?port ?fragment ?keep_nl_params ?nl_params
            ?keep_get_na_params g p
        in
        let uri = 
          Eliom_uri.make_string_uri_from_components (path, g, fragment) 
        in
        Ocsigen_lib.Right (uri, p)



let exit_to
    ?absolute ?absolute_path ?https
    ~(service : ('get, 'post, 
                 [< `Attached of (Eliom_services.attached_service_kind, [< Eliom_services.getpost]) Eliom_services.a_s
                 | `Nonattached of [< Eliom_services.getpost] Eliom_services.na_s ],
                 [< Eliom_services.suff ], 'h, 'i,
                 [< Eliom_services.registrable ], 'j)
        Eliom_services.service)
    ?hostname ?port ?fragment ?keep_nl_params ?nl_params ?keep_get_na_params
    g p =
  (match create_request_
     ?absolute ?absolute_path ?https
     ~service
     ?hostname ?port ?fragment ?keep_nl_params ?nl_params ?keep_get_na_params
     g p
   with
     | Ocsigen_lib.Left uri -> Eliom_request.redirect_get uri
     | Ocsigen_lib.Right (uri, p) -> Eliom_request.redirect_post uri p)


(** This will change the URL, without doing a request.
    As browsers do not not allow to change the URL,
    we write the new URL in the fragment part of the URL.
    A script must do the redirection if there is something in the fragment.
    Usually this function is only for internal use.
*)
let change_url_string uri =
  current_fragment := url_fragment_prefix_with_sharp^uri;
  Dom_html.window##location##hash <- Js.string (url_fragment_prefix^uri)

let change_url
(*VVV is it safe to have absolute URLs? do we accept non absolute paths? *)
    ?absolute ?absolute_path ?https
    ~(service : ('get, 'post, 
                 [< `Attached of (Eliom_services.attached_service_kind, [< Eliom_services.getpost]) Eliom_services.a_s
                 | `Nonattached of [< Eliom_services.getpost] Eliom_services.na_s ],
                 [< Eliom_services.suff ], 'h, 'i,
                 [< Eliom_services.registrable ], 'j)
        Eliom_services.service)
    ?hostname ?port ?fragment ?keep_nl_params ?nl_params ?keep_get_na_params
    g p =
(*VVV only for GET services? *)
  let uri =
    (match create_request_
       ?absolute ?absolute_path ?https
       ~service
       ?hostname ?port ?fragment ?keep_nl_params ?nl_params ?keep_get_na_params
       g p
     with
       | Ocsigen_lib.Left uri -> uri
       | Ocsigen_lib.Right (uri, p) -> uri)
  in
  change_url_string uri


(* lazy because we want the page to be loaded *)
let container_node = 
  lazy ((Eliommod_cli.unwrap_node (Ocsigen_lib.unmarshal_js_var "container_node"))
           : Dom_html.element Js.t)

let on_unload_scripts = ref []
let at_exit_scripts = ref []

(*
let _ =
  Dom_html.window##onbeforeunload <-
    (Dom_html.handler
       (fun ev ->
(* We cannot wait for the lwt thread to finish before exiting ...
*)
         ignore
           ((match !on_unload_script with
             | None -> Lwt.return ()
             | Some script -> Js.Unsafe.variable script) >>= fun () ->
            (match !at_exit_script with
              | None -> Lwt.return ()
              | Some script -> Js.Unsafe.variable script));
         Js.bool true))

(* Also:
  May be we can add automatically a special "close_process" coservice for each
  Eliom site, that will be called by the process when exiting.
*)
*)

let load_eliom_data_
    ((tree, (((timeofday, _), _) as page_data), cookies, onload, onunload, si) :
        Eliom_client_types.eliom_data_type)
    node : unit Lwt.t =
  (match tree with
    | Ocsigen_lib.Left ref_tree ->
      Eliommod_cli.relink_dom timeofday node ref_tree;
    | Ocsigen_lib.Right ref_tree_list ->
      Eliommod_cli.relink_dom_list timeofday (node##childNodes) ref_tree_list);
  Eliommod_cli.fill_page_data_table page_data;
  Eliommod_client_cookies.update_cookie_table cookies;
  Eliom_request_info.set_session_info si;
  on_unload_scripts := [fun () -> List.iter Js.Unsafe.variable onunload; Lwt.return ()];
  List.iter Js.Unsafe.variable onload;
  Lwt.return ()
(* originaly onload was supposed to return unit Lwt.t, but it is not
   type checked: there are execution error if the returned value is
   not effectively an Lwt.t. By assuming it to return unit, the
   effectively returned value is ignored and no runtime error can
   occur this way.
   This is the same problem for on_unload below. *)

let set_inner_html (ed, content) =
  ignore (Lwt_list.iter_p (fun f -> f ()) !on_unload_scripts);
  on_unload_scripts := [];
  let container_node = Lazy.force container_node in
  container_node##innerHTML <- Js.string content;
  load_eliom_data_ ed container_node

let on_unload f =
  on_unload_scripts := f::(!on_unload_scripts)

let set_content = function
  | Eliom_client_types.EAContent c -> set_inner_html c
(* For now only one case *)

let set_content_and_url (c, u) =
  change_url_string u;
  set_content c


let change_page
    ?absolute ?absolute_path ?https
    ~(service : ('get, 'post, 
                 [< `Attached of (Eliom_services.attached_service_kind, [< Eliom_services.getpost]) Eliom_services.a_s
                 | `Nonattached of [< Eliom_services.getpost] Eliom_services.na_s ],
                 [< Eliom_services.suff ], 'h, 'i,
                 [< Eliom_services.registrable ], 'j)
        Eliom_services.service)
    ?hostname ?port ?fragment ?keep_nl_params
    ?(nl_params=Eliom_parameters.empty_nl_params_set) ?keep_get_na_params
    (g : 'get) (p : 'post) =
  if not (Eliom_services.do_appl_xhr 
            (Eliom_process.get_application_name ()) service)
  then
    Lwt.return (exit_to
                  ?absolute ?absolute_path ?https
                  ~service
                  ?hostname ?port ?fragment ?keep_nl_params
                  ~nl_params ?keep_get_na_params
                  g p)
  else begin
    (match
        create_request_
          ?absolute ?absolute_path ?https
          ~service
          ?hostname ?port ?fragment ?keep_nl_params
          ?keep_get_na_params
          g p
     with
       | Ocsigen_lib.Left uri -> 
         Eliom_request.http_get ~cookies_info:(https, service, g) uri []
       | Ocsigen_lib.Right (uri, p) -> 
         Eliom_request.http_post ~cookies_info:(https, service, g) uri p)
    >>= fun r -> set_content (Eliom_request.get_eliom_appl_result r)
    >>= fun () ->
    (*VVV The URL is created twice ... 
      Once with tab cookies nlp (for the request), 
      and once without it (we do not want it to appear in the URL).
      How to avoid this?
    *)
    change_url
      ?absolute ?absolute_path ?https
      ~service
      ?hostname ?port ?fragment ?keep_nl_params ~nl_params ?keep_get_na_params
      g p;
  (*VVV change the URL only if it is different? *)
    Lwt.return ()
  end





let call_service
    ?absolute ?absolute_path ?https
    ~service
    ?hostname ?port ?fragment ?keep_nl_params ?nl_params ?keep_get_na_params
    g p =
  (match create_request_
     ?absolute ?absolute_path ?https
     ~service
     ?hostname ?port ?fragment ?keep_nl_params ?nl_params ?keep_get_na_params
     g p
   with
     | Ocsigen_lib.Left uri ->
       Eliom_request.http_get ~cookies_info:(https, service, g) uri []
     | Ocsigen_lib.Right (uri, p) ->
       Eliom_request.http_post ~cookies_info:(https, service, g) uri p)


let call_caml_service
    ?absolute ?absolute_path ?https ~service
    ?hostname ?port ?fragment ?keep_nl_params ?nl_params ?keep_get_na_params
    g p =
  call_service
    ?absolute ?absolute_path ?https
    ~service
    ?hostname ?port ?fragment ?keep_nl_params ?nl_params ?keep_get_na_params
    g p
  >>= fun s ->
  Lwt.return (Marshal.from_string (Ocsigen_lib.urldecode_string s) 0)


let fake_page = Dom_html.createBody Dom_html.document
(*FIX: is that correct?
  XHTML5.M.toelt (XHTML5.M.body [])
*)

let get_subpage
    ?absolute ?absolute_path ?https ~service
    ?hostname ?port ?fragment ?keep_nl_params
    ?(nl_params=Eliom_parameters.empty_nl_params_set) ?keep_get_na_params
    g p =
(*VVV Should we fail if the service does not belong to the same application? *)
  (match create_request_
     ?absolute ?absolute_path ?https ~service
     ?hostname ?port ?fragment ?keep_nl_params
     ?keep_get_na_params
     g p
   with
     | Ocsigen_lib.Left uri ->
       Eliom_request.http_get ~cookies_info:(https, service, g) uri []
     | Ocsigen_lib.Right (uri, p) ->
       Eliom_request.http_post ~cookies_info:(https, service, g) uri p)
  >>= fun r -> match Eliom_request.get_eliom_appl_result r with
    | Eliom_client_types.EAContent (ed, content) ->
    (* Hack to make the result considered as XHTML: *)
      fake_page##innerHTML <- Js.string content;
      let nodes = fake_page##childNodes in
      let node_list = ref [] in
      for i = nodes##length - 1 downto 0 do
        node_list := Js.Optdef.get (nodes##item (i)) (fun () -> assert false)
                     :: !node_list
      done;
  
      load_eliom_data_ ed fake_page >>= fun () ->
      fake_page##innerHTML <- Js.string "";
      Lwt.return (XHTML5.M.totl !node_list)





(*****************************************************************************)
(* Make the back button work when only the fragment has changed ... *)
(*VVV We check the fragment every t second ... :-( *)

let write_fragment s = Dom_html.window##location##hash <- Js.string s

let read_fragment () = Js.to_string Dom_html.window##location##hash


let (fragment, set_fragment_signal) = React.S.create (read_fragment ())

let rec fragment_polling () =
  Lwt_js.sleep 0.2 >>= fun () ->
  let new_fragment = read_fragment () in
  set_fragment_signal new_fragment;
  fragment_polling ()

let _ = fragment_polling ()

let auto_change_page fragment =
  ignore
    (let l = String.length fragment in
     if (l = 0) || ((l > 1) && (fragment.[1] = '!'))
     then 
       if fragment <> !current_fragment
       then
         (
         current_fragment := fragment; 
         let uri =
           match l with
             | 2 -> "./" (* fix for firefox *)
             | 0 | 1 -> Eliom_request_info.full_uri
             | _ -> String.sub fragment 2 ((String.length fragment) - 2) 
         in
         Eliom_request.http_get uri [] >>= fun r ->
         set_content (Eliom_request.get_eliom_appl_result r))
       else Lwt.return ()
     else Lwt.return ())

let _ = React.E.map auto_change_page (React.S.changes fragment)

(* ==A closure that is registered by default to simulate <a> *)
let _ =
  Eliommod_cli.register_closure
    Eliom_client_types.a_closure_id
    (fun
       (absolute, absolute_path, https, service, hostname, port,
        fragment, keep_nl_params, nl_params, getparams)
       ->
         let absolute = Eliommod_cli.unwrap absolute in
         let https = Eliommod_cli.unwrap https in
         let service = Eliommod_cli.unwrap service in
         let hostname = Eliommod_cli.unwrap hostname in
         let port = Eliommod_cli.unwrap port in
         let fragment = Eliommod_cli.unwrap fragment in
         let keep_nl_params = Eliommod_cli.unwrap keep_nl_params in
         let nl_params = Eliommod_cli.unwrap nl_params in
         let getparams = Eliommod_cli.unwrap getparams in
         let absolute_path = Eliommod_cli.unwrap absolute_path in
         ignore
           (change_page
              ?absolute ?absolute_path ?https
              ~service ?hostname ?port ?fragment ?keep_nl_params ?nl_params
              getparams ()))



let make_a_with_onclick
    make_a
    register_event
    ?absolute
    ?absolute_path
    ?https
    ?a
    ~service
    ?hostname
    ?port
    ?fragment
    ?keep_nl_params
    ?nl_params
    content
    getparams =
  let node = make_a ?a ?onclick:None content in
  register_event node "onclick"
    (fun () -> change_page
       ?absolute
       ?absolute_path
       ?https
       ~service
       ?hostname
       ?port
       ?fragment
       ?keep_nl_params
       ?nl_params
       getparams ())
    ();
  node


