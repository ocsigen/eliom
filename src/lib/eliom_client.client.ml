(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2010 Vincent Balat
 * Copyright (C) 2011 Jérôme Vouillon, Grégoire Henry, Pierre Chambart
 * Copyright (C) 2012 Benedikt Becker
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

include Eliom_client_core
(* TODO: Implement better separation between Eliom_client_core and
   Eliom_client. Eliom_client_core is supposed to be linked early,
   before Eliom_service, in order to make possible to use the
   client-server syntax in Eliom itself. *)

open Js_of_ocaml
open Eliom_lib
module Opt = Eliom_lib.Option

let lock_request_handling = Eliom_request.lock
let unlock_request_handling = Eliom_request.unlock

type ('a, +'b) server_function = 'a -> 'b Lwt.t

let only_replace_body = ref false
let persist_document_head () = only_replace_body := true
(*
Cordova does not allow to read from a file when using the WkWebview.
So, CSS preloading does not work. This provide a work-around.
Also, with Chrome, the corresponding XHRs will block if other requests
have been scheduled before, even when the CSS is cached. This can slow
down page changes.
*)

let insert_base page =
  let b = Dom_html.createBase Dom_html.document in
  b##.href := Js.string (Eliom_process.get_base_url ());
  b##.id := Js.string Eliom_common_base.base_elt_id;
  Js.Opt.case
    page##(querySelector (Js.string "head"))
    (fun () -> Lwt_log.ign_debug_f "No <head> found in document")
    (fun head -> Dom.appendChild head b)

let get_global_data () =
  let def () = None
  and id = Js.string "__global_data" in
  Js.Optdef.case (Dom_html.window##.localStorage) def @@ fun storage ->
  Js.Opt.case (storage##(getItem id)) def @@ fun v ->
  Lwt_log.ign_debug_f "Unwrap __global_data";
  match
    Eliom_unwrap.unwrap (Url.decode (Js.to_string v)) 0
  with
  | {Eliom_runtime.ecs_data = `Success v} ->
    Lwt_log.ign_debug_f "Unwrap __global_data success";
    Some v
  | _ ->
    None

let normalize_app_path p =
  (* remove "" from beginning and end of path *)
  let p = Eliom_lib.Url.split_path p in
  let p = match  p with "" :: p -> p          | _ -> p in
  match List.rev p with "" :: p -> List.rev p | _ -> p

let init_client_app
    ~app_name ?(ssl = false) ~hostname ?(port = 80) ~site_dir () =
  Lwt_log.ign_debug_f "Eliom_client.init_client_app called.";
  Eliom_process.appl_name_r := Some app_name;
  Eliom_request_info.client_app_initialised := true;
  (* For site_dir, we want no trailing slash. We tend to concatenate
     it with relative paths, or treat it as a prefix to be removed
     from other paths. The trailing slash would be burdensome.

     In contrast, we do need the trailing slash in
     cpi_original_full_path, because we do have the trailing slash in
     page URLs., Hence the site_dir @ [""] below. *)
  Eliom_process.set_sitedata
    {Eliom_types.site_dir = site_dir;
     site_dir_string = String.concat "/" site_dir};
  Eliom_process.set_info {Eliom_common.cpi_ssl = ssl ;
                          cpi_hostname = hostname;
                          cpi_server_port = port;
                          cpi_original_full_path = site_dir @ [""]
                         };
  Eliom_process.set_request_template None;
  (* We set the tab cookie table, with the app name inside: *)
  Eliom_process.set_request_cookies
    (Ocsigen_cookie_map.add ~path:[]
       Eliom_common.appl_name_cookie_name
       (Ocsigen_cookie_map.OSet (None, app_name, false))
       Ocsigen_cookie_map.empty);
  ignore (get_global_data ())

let is_client_app () = !Eliom_common.is_client_app

let _ =
  Eliom_common.is_client_app :=
    (* Testing if variable __eliom_appl_process_info exists: *)
    not (Js.Optdef.test Js.Unsafe.global##.___eliom_appl_process_info_foo)

let onunload_fun _ =
  update_state ();
  run_callbacks (flush_onunload ());
  Js._true

let onbeforeunload_fun _ = run_onbeforeunload ()

let set_base_url () =
  Eliom_process.set_base_url
    (String.concat
       ""
       [ Js.to_string (Dom_html.window##.location##.protocol)
       ; "//"
       ; Js.to_string (Dom_html.window##.location##.host)
       ; Js.to_string (Dom_html.window##.location##.pathname) ])

let dom_history_ready = ref false

(* Function called (in Eliom_client_main), once when starting the app.
   Either when sent by a server or initiated on client side.

   For client apps, we read __eliom_server, __eliom_app_name,
   __eliom_app_path JS variables set by the client app (via the HTML
   file loading us).

   - __eliom_server   : remote Eliom server to contact
   - __eliom_app_name : application name
   - __eliom_app_path : path app is under. We use this path for calls to
                        server functions (see Eliom_uri). *)
let init () =
  (* Initialize client app if the __eliom_server variable is defined *)
  if is_client_app ()
  && Js.Optdef.test Js.Unsafe.global##.___eliom_server_
  && Js.Optdef.test Js.Unsafe.global##.___eliom_app_name_
  then begin
    let app_name = Js.to_string (Js.Unsafe.global##.___eliom_app_name_)
    and site_dir =
      Js.Optdef.case
        Js.Unsafe.global##.___eliom_path_
        (fun () -> [])
        (fun p -> normalize_app_path (Js.to_string p))
    in
    match
      Url.url_of_string (Js.to_string (Js.Unsafe.global##.___eliom_server_))
    with
    | Some (Http { hu_host; hu_port; hu_path; _ }) ->
      init_client_app
        ~app_name
        ~ssl:false ~hostname:hu_host ~port:hu_port ~site_dir
        ()
    | Some (Https { hu_host; hu_port; hu_path; _ }) ->
      init_client_app
        ~app_name
        ~ssl:true ~hostname:hu_host ~port:hu_port ~site_dir ()
    | _ -> ()
  end;

  let js_data = lazy (Eliom_request_info.get_request_data ()) in
  Js.Optdef.case
    (Js.Unsafe.global##.___eliom_global_data_)
    (fun () ->
       (* Global data are in [js_data], so we unmarshal it right away. *)
       ignore (Lazy.force js_data))
    (fun global_data ->
       (* Global data are in a separate file. We should not unmarshal
          [js_data] right away but only once the client program has
          been initialized. *)
       ignore (Eliom_unwrap.unwrap_js global_data);
       Js.Unsafe.delete (Js.Unsafe.global) "__eliom_global_data");

  (* <base> *)
  (* The first time we load the page, we record the initial URL in a client
     side ref, in order to set <base> (on client-side) in header for each
     pages. *)
  set_base_url ();
  insert_base Dom_html.document;
  (* </base> *)

  (* Decoding tab cookies.
     2016-03 This was done at the beginning of onload below
     but this makes it impossible to use cookies
     during initialisation phase. I move this here. -- Vincent *)
  Eliommod_cookies.update_cookie_table
    (Some (Eliom_process.get_info ()).cpi_hostname)
    (Eliom_request_info.get_request_cookies ());

  let onload_handler = ref None in

  let onload ev =
    let js_data = Lazy.force js_data in
    Lwt_log.ign_debug ~section "onload (client main)";
    begin match !onload_handler with
      Some h -> Dom.removeEventListener h; onload_handler := None
    | None   -> ()
    end;
    set_initial_load ();
    Lwt.async
      (fun () ->
         if !Eliom_config.debug_timings
         then Firebug.console##(time (Js.string "onload"));
         let%lwt () =
           Eliom_request_info.set_session_info
             ~uri:(String.concat "/" (Eliom_request_info.get_csp_original_full_path ()))
             js_data.Eliom_common.ejs_sess_info @@ fun () ->
           Lwt.return_unit
         in
         (* Give the browser the chance to actually display the page NOW *)
         let%lwt () = Js_of_ocaml_lwt.Lwt_js.sleep 0.001 in
         (* Ordering matters. See [Eliom_client.set_content] for explanations *)
         relink_request_nodes (Dom_html.document##.documentElement);
         let root = Dom_html.document##.documentElement in
         let closure_nodeList,attrib_nodeList =
           relink_page_but_client_values root
         in
         do_request_data js_data.Eliom_common.ejs_request_data;
         (* XXX One should check that all values have been unwrapped.
            In fact, client values should be special and all other values
            should be eagerly unwrapped. *)
         let () =
           relink_attribs root
             js_data.Eliom_common.ejs_client_attrib_table attrib_nodeList in

         let onload_closure_nodes =
           relink_closure_nodes
             root js_data.Eliom_common.ejs_event_handler_table
             closure_nodeList
         in
         reset_request_nodes ();
         Eliommod_dom.add_formdata_hack_onclick_handler ();
         if not (is_client_app ()) then
           dom_history_ready := true;
         let load_callbacks =
           flush_onload () @ [ onload_closure_nodes; broadcast_load_end ] in
         Lwt_mutex.unlock load_mutex;
         run_callbacks load_callbacks;
         if !Eliom_config.debug_timings
         then Firebug.console##(timeEnd (Js.string "onload"));
         Lwt.return_unit);
    Js._false
  in

  Lwt_log.ign_debug ~section "Set load/onload events";

  onload_handler :=
    Some (Dom.addEventListener Dom_html.window (Dom.Event.make "load")
            (Dom.handler onload) Js._true);

  add_string_event_listener Dom_html.window "beforeunload"
    onbeforeunload_fun false;

  ignore
    (Dom.addEventListener Dom_html.window (Dom.Event.make "unload")
       (Dom_html.handler onunload_fun) Js._false)


(* == Low-level: call service. *)

let create_request__
    ?absolute ?absolute_path ?https
    (type m)
    ~(service : (_, _, m, _, _, _, _, _, _, _, _) Eliom_service.t)
    ?hostname ?port ?fragment
    ?keep_nl_params ?nl_params ?keep_get_na_params
    get_params post_params =
  let path, get_params, fragment, post_params =
    Eliom_uri.make_post_uri_components__
      ?absolute ?absolute_path ?https
      ~service
      ?hostname ?port ?fragment ?keep_nl_params ?nl_params
      ?keep_get_na_params get_params post_params
  in
  let uri =
    Eliom_uri.make_string_uri_from_components (path, get_params, fragment)
  in
  uri, get_params, post_params

let create_request_ (type m)
    ?absolute ?absolute_path ?https
    ~(service : (_, _, m, _, _, _, _, _, _, _, _) Eliom_service.t)
    ?hostname ?port ?fragment
    ?keep_nl_params ?nl_params ?keep_get_na_params
    get_params post_params =

  (* TODO: allow get_get_or_post service to return also the service
     with the correct subtype. Then do use Eliom_uri.make_string_uri
     and Eliom_uri.make_post_uri_components instead of
     Eliom_uri.make_string_uri_ and
     Eliom_uri.make_post_uri_components__ *)

  match Eliom_service.which_meth service with
  | Eliom_service.Get' ->
    let ((_, get_params, _) as components) =
      Eliom_uri.make_uri_components
        ?absolute ?absolute_path ?https ~service
        ?hostname ?port ?fragment ?keep_nl_params ?nl_params get_params
    in
    let uri = Eliom_uri.make_string_uri_from_components components in
    `Get (uri, get_params)
  | Eliom_service.Post' ->
    `Post
      (create_request__
         ?absolute ?absolute_path ?https ~service ?hostname ?port ?fragment
         ?keep_nl_params ?nl_params ?keep_get_na_params
         get_params post_params)
  | Eliom_service.Put' ->
    `Put
      (create_request__
         ?absolute ?absolute_path ?https ~service ?hostname ?port ?fragment
         ?keep_nl_params ?nl_params ?keep_get_na_params
         get_params post_params)
  | Eliom_service.Delete' ->
    `Delete
      (create_request__
         ?absolute ?absolute_path ?https ~service ?hostname ?port ?fragment
         ?keep_nl_params ?nl_params ?keep_get_na_params
         get_params post_params)

let raw_call_service
    ?absolute ?absolute_path ?https ~service ?hostname ?port ?fragment
    ?keep_nl_params ?nl_params ?keep_get_na_params
    ?progress ?upload_progress ?override_mime_type
    get_params post_params =
  (* with_credentials = true is necessary for client side apps when
     we want the Eliom server to be different from the server for
     static files (if any). For example when testing a mobile app
     in a browser, with Cordova's Web server.
     Also set with_credentials to true in CORS configuration.
  *)
  let with_credentials = not (Eliom_service.is_external service) in
  let%lwt uri, content =
    match create_request_
            ?absolute ?absolute_path ?https ~service ?hostname ?port ?fragment
            ?keep_nl_params ?nl_params ?keep_get_na_params
            get_params post_params
    with
    | `Get (uri, _) ->
      Eliom_request.http_get
        ~with_credentials
        ?cookies_info:(Eliom_uri.make_cookies_info (https, service)) uri []
        ?progress ?upload_progress ?override_mime_type
        Eliom_request.string_result
    | `Post (uri, _, post_params) ->
      Eliom_request.http_post
        ~with_credentials
        ?cookies_info:(Eliom_uri.make_cookies_info (https, service))
        ?progress ?upload_progress ?override_mime_type
        uri post_params Eliom_request.string_result
    | `Put (uri, _, post_params) ->
      Eliom_request.http_put
        ~with_credentials
        ?cookies_info:(Eliom_uri.make_cookies_info (https, service))
        ?progress ?upload_progress ?override_mime_type
        uri post_params Eliom_request.string_result
    | `Delete (uri, _, post_params) ->
      Eliom_request.http_delete
        ~with_credentials
        ?cookies_info:(Eliom_uri.make_cookies_info (https, service))
        ?progress ?upload_progress ?override_mime_type
        uri post_params Eliom_request.string_result in
  match content with
  | None -> Lwt.fail (Eliom_request.Failed_request 204)
  | Some content -> Lwt.return (uri, content)

let call_service
    ?absolute ?absolute_path ?https ~service ?hostname ?port ?fragment
    ?keep_nl_params ?nl_params ?keep_get_na_params
    ?progress ?upload_progress ?override_mime_type
    get_params post_params =
  let%lwt _, content =
    raw_call_service
      ?absolute ?absolute_path ?https ~service ?hostname ?port ?fragment
      ?keep_nl_params ?nl_params ?keep_get_na_params
      ?progress ?upload_progress ?override_mime_type
      get_params post_params in
  Lwt.return content

(* == Leave an application. *)

let exit_to
    ?absolute ?absolute_path ?https ~service ?hostname ?port ?fragment
    ?keep_nl_params ?nl_params ?keep_get_na_params
    get_params post_params =
  (match create_request_
           ?absolute ?absolute_path ?https ~service ?hostname ?port ?fragment
           ?keep_nl_params ?nl_params ?keep_get_na_params
           get_params post_params
   with
   | `Get (uri, _) -> Eliom_request.redirect_get uri
   | `Post (uri, _, post_params) -> Eliom_request.redirect_post uri post_params
   | `Put (uri, _, post_params) -> Eliom_request.redirect_put uri post_params
   | `Delete (uri, _, post_params) ->
     Eliom_request.redirect_delete uri post_params)

let window_open ~window_name ?window_features
    ?absolute ?absolute_path ?https ~service ?hostname ?port ?fragment
    ?keep_nl_params ?nl_params ?keep_get_na_params
    get_params =
  match create_request_
          ?absolute ?absolute_path ?https ~service ?hostname ?port ?fragment
          ?keep_nl_params ?nl_params ?keep_get_na_params
          get_params ()
  with
  | `Get (uri, _) ->
    (* open_ signature changed in JSOO master (after 2.8.3). Obj.magic
       will allow us to keep compatibility with both 2.8.x and future
       versions *)
    Obj.magic @@
    Dom_html.window##(
      open_ (Js.string uri) window_name
        (Js.Opt.option window_features)
    )
  | `Post (_, _, _) -> assert false
  | `Put (_, _, _) -> assert false
  | `Delete (_, _, _) -> assert false


(* == Call caml service.

   Unwrap the data and execute the associated onload event
   handlers.
*)

let unwrap_caml_content content =
  let r : 'a Eliom_runtime.eliom_caml_service_data =
    Eliom_unwrap.unwrap (Url.decode content) 0
  in
  Lwt.return (r.Eliom_runtime.ecs_data,
              r.Eliom_runtime.ecs_request_data)

let call_ocaml_service
    ?absolute ?absolute_path ?https ~service ?hostname ?port ?fragment
    ?keep_nl_params ?nl_params ?keep_get_na_params
    ?progress ?upload_progress ?override_mime_type
    get_params post_params =
  Lwt_log.ign_debug ~section "Call OCaml service";
  let%lwt _, content =
    raw_call_service
      ?absolute ?absolute_path ?https ~service ?hostname ?port ?fragment
      ?keep_nl_params ?nl_params ?keep_get_na_params
      ?progress ?upload_progress ?override_mime_type
      get_params post_params in
  let%lwt () = Lwt_mutex.lock load_mutex in
  set_loading_phase ();
  let%lwt content, request_data = unwrap_caml_content content in
  do_request_data request_data;
  reset_request_nodes ();
  let load_callbacks = [broadcast_load_end] in
  Lwt_mutex.unlock load_mutex;
  run_callbacks load_callbacks;
  match content with
  | `Success result -> Lwt.return result
  | `Failure msg -> Lwt.fail (Eliom_client_value.Exception_on_server msg)

(* == Current uri.

   This reference is used in [change_page_uri] and popstate event
   handler to mimic browser's behaviour with fragment: we do not make
   any request to the server, if only the fragment part of url
   changes.

*)

let path_and_args_of_uri uri =
  let path_of_string s =
    match Url.path_of_path_string s with
    | "." :: path ->
      path
    | path ->
      path
  in
  match Url.url_of_string uri with
  | Some (Url.Http url | Url.Https url) ->
    url.Url.hu_path, url.Url.hu_arguments
  | _ ->
    match
      try
        Some (String.index uri '?')
      with Not_found ->
        None
    with
    | Some n ->
      path_of_string String.(sub uri 0 n),
      Url.decode_arguments String.(sub uri (n + 1) (length uri - n - 1))
    | None ->
      path_of_string uri, []

let set_current_uri, get_current_uri =
  let set_current_uri uri =
    let current_uri = fst (Url.split_fragment uri) in
    (get_this_page ()).url <- current_uri;
    let (path, all_get_params) = path_and_args_of_uri current_uri in
    Lwt.async @@ fun () ->
    Eliom_request_info.update_session_info ~path
      ~all_get_params ~all_post_params:None (fun () -> Lwt.return_unit)
  in
  let get_current_uri () = (get_this_page ()).url in
  set_current_uri, get_current_uri

(* == Function [change_url_string] changes the URL, without doing a request.

   It uses the History API if present, otherwise we write the new URL
   in the fragment part of the URL (see 'redirection_script' in
   'server/eliom_registration.ml'). *)

let current_pseudo_fragment = ref ""
let url_fragment_prefix = "!"
let url_fragment_prefix_with_sharp = "#!"

let reload_function = ref None
let reload_functions = ref []
let set_reload_function f = reload_function := Some f

(* TODO: unify History and reload_functions *)

let set_max_dist_history_doms limit =
  History.max_num_doms := limit;
  History.garbage_collect_doms ()

let push_history_dom () =
  if !dom_history_ready then begin
    let page = !active_page in
    let dom =
      if !only_replace_body
      then Dom_html.document##.body
      else Dom_html.document##.documentElement in
    page.dom <- Some dom;
    History.garbage_collect_doms ()
  end

module Page_status = struct
  include Page_status_t

  let signal () = let p = get_this_page () in p.page_status

  module Events = struct
    let changes () = React.S.changes (signal ())
    let active () =
      changes () |> React.E.fmap @@ function Active -> Some () | _ -> None
    let cached () =
      changes () |> React.E.fmap @@ function Cached -> Some () | _ -> None
    let dead () =
      changes () |> React.E.fmap @@ function Dead -> Some () | _ -> None
    let inactive () = React.E.select [cached (); dead ()]
  end

  let maybe_just_once ~once e = if once then React.E.once e else e
  let stop_event ?(stop = React.E.never) e =
    ignore @@ React.E.map (fun () -> React.E.stop ~strong:true e) stop

  let onactive ?(now = true) ?(once = false) ?stop action =
    let on_event () =
      stop_event ?stop @@
        React.E.map action @@ maybe_just_once ~once @@ Events.active ()
    in
    if now && React.S.value (signal ()) = Active
      then (action (); if not once then on_event ())
      else on_event ()

  let oncached ?(once = false) ?stop action =
    stop_event ?stop @@
      React.E.map action @@ maybe_just_once ~once @@ Events.cached ()

  let ondead ?stop action =
    stop_event ?stop @@
      React.E.map action (Events.dead ())

  let oninactive ?(once = false) ?stop action =
    stop_event ?stop @@
      React.E.map action @@ maybe_just_once ~once @@ Events.inactive ()

  let while_active ?now ?(stop = React.E.never) action =
    let thread = ref Lwt.return_unit in
    onactive ?now ~stop (fun () -> thread := action ());
    oninactive ~stop (fun () -> Lwt.cancel !thread);
    ignore @@ React.E.map (fun () -> Lwt.cancel !thread) stop
end

let is_in_cache state_id =
  match History.find_by_state_index state_id.state_index with
  | Some ({dom = Some _}) -> true
  | _ -> false

let stash_reload_function f =
  let page = get_this_page () in
  let state_id = page.page_id in
  let id = state_id.state_index in
  Lwt_log.ign_debug_f ~section:section_page
    "Update reload function for page %d" id;
  reload_functions :=
    (id, f) ::
    (List.filter (fun (id', _) -> id <> id') !reload_functions)

let change_url_string ~replace uri =
  Lwt_log.ign_debug_f ~section:section_page "Change url string: %s" uri;
  let full_uri =
    if !Eliom_common.is_client_app then uri else Url.resolve uri in
  set_current_uri full_uri;
  if Eliom_process.history_api then begin
    let this_page = get_this_page () in
    if replace then begin
      Opt.iter stash_reload_function !reload_function;
      Dom_html.window##.history##replaceState
        (Js.Opt.return (this_page.page_id,
                        Js.string full_uri))
        (Js.string "")
        (if !Eliom_common.is_client_app then Js.null else
         Js.Opt.return (Js.string uri))
    end
    else begin
      update_state();
      let state_id = this_page.page_id in
      let erase_future () =
        reload_functions :=
          List.filter (fun (id, _) -> id <= state_id.state_index)
            !reload_functions;
      in erase_future ();
      Opt.iter stash_reload_function !reload_function;
      Dom_html.window##.history##pushState
        (Js.Opt.return (this_page.page_id, Js.string full_uri))
        (Js.string "")
        (if !Eliom_common.is_client_app then Js.null else
         Js.Opt.return (Js.string uri))
    end;
    Eliommod_dom.touch_base ();
  end else begin
    current_pseudo_fragment := url_fragment_prefix_with_sharp^uri;
    if uri <> fst (Url.split_fragment Url.Current.as_string)
    then Dom_html.window##.location##.hash := Js.string (url_fragment_prefix^uri)
  end

(* == Function [change_url] changes the URL, without doing a request.
   It takes a GET (co-)service as parameter and its parameters.
 *)

let change_url
    ?(replace = false)
    ?absolute
    ?absolute_path
    ?https
    ~service
    ?hostname
    ?port
    ?fragment
    ?keep_nl_params
    ?nl_params
    params =
  Lwt_log.ign_debug ~section:section_page "Change url";
  reload_function :=
    (match Eliom_service.xhr_with_cookies service with
     | None when
           (https = Some true && not Eliom_request_info.ssl_)
           || (https = Some false && Eliom_request_info.ssl_) ->
         None
     | Some (Some _ as t) when t = Eliom_request_info.get_request_template () ->
         None
     | _ ->
       match Eliom_service.reload_fun service with
       | Some rf -> Some (fun () () -> rf params ())
       | None    -> None);
  change_url_string ~replace
    (Eliom_uri.make_string_uri
       ?absolute
       ?absolute_path
       ?https
       ~service
       ?hostname
       ?port
       ?fragment
       ?keep_nl_params
       ?nl_params params)

let set_template_content ~replace ~uri ?fragment =
  let really_set content () =
    reload_function := None;
    (match fragment with
     | None -> change_url_string ~replace uri
     | Some fragment ->
       change_url_string ~replace (uri ^ "#" ^ fragment));
    let%lwt () = Lwt_mutex.lock load_mutex in
    let%lwt (), request_data = unwrap_caml_content content in
    do_request_data request_data;
    reset_request_nodes ();
    let load_callbacks = flush_onload () in
    Lwt_mutex.unlock load_mutex;
    run_callbacks load_callbacks;
    Lwt.return_unit
  and cancel () = Lwt.return_unit in
  function
  | None ->
    Lwt.return_unit
  | Some content ->
    run_onunload_wrapper (really_set content) cancel

let set_uri ~replace ?fragment uri =
  (* Changing url: *)
  match fragment with
  | None -> change_url_string ~replace uri
  | Some fragment -> change_url_string ~replace (uri ^ "#" ^ fragment)

let replace_page ~do_insert_base new_page =
  if !Eliom_config.debug_timings
  then Firebug.console##(time (Js.string "replace_page"));
  if !only_replace_body then begin
    let new_body = new_page##.childNodes##(item (1)) in
    Js.Opt.iter new_body (fun new_body ->
    Dom.replaceChild Dom_html.document##.documentElement
      new_body
      Dom_html.document##.body)
  end else begin
    (* We insert <base> in the page.
       The URLs of all other pages will be computed w.r.t.
       the base URL. *)
    if do_insert_base then insert_base new_page;
    Dom.replaceChild Dom_html.document
      new_page
      Dom_html.document##.documentElement
  end;
  if !Eliom_config.debug_timings
  then Firebug.console##(timeEnd (Js.string "replace_page"))

(* Function to be called for client side services: *)
let set_content_local ?offset ?fragment new_page =
  Lwt_log.ign_debug ~section:section_page "Set content local";
  let locked = ref true in
  let recover () =
    if !locked then Lwt_mutex.unlock load_mutex;
    if !Eliom_config.debug_timings then
      Firebug.console##(timeEnd (Js.string "set_content_local"))
  and really_set () =
    (* Inline CSS in the header to avoid the "flashing effect".
       Otherwise, the browser start to display the page before
       loading the CSS. *)
    let preloaded_css =
      if !only_replace_body then Lwt.return_unit else
      Eliommod_dom.preload_css new_page in
    (* Wait for CSS to be inlined before substituting global nodes: *)
    let%lwt () = preloaded_css in
    (* Really change page contents *)
    replace_page ~do_insert_base:true new_page;
    Eliommod_dom.add_formdata_hack_onclick_handler ();
    dom_history_ready := true;
    let load_callbacks = flush_onload () @ [broadcast_load_end] in
    locked := false;
    Lwt_mutex.unlock load_mutex;
    (* run callbacks upon page activation (or now), but just once *)
    Page_status.onactive ~once:true (fun () -> run_callbacks load_callbacks);
    scroll_to_fragment ?offset fragment;
    advance_page ();
    if !Eliom_config.debug_timings
    then Firebug.console##(timeEnd (Js.string "set_content_local"));
    Lwt.return_unit
  in
  let cancel () = recover (); Lwt.return_unit in
  try%lwt
    let%lwt () = Lwt_mutex.lock load_mutex in
    set_loading_phase ();
    if !Eliom_config.debug_timings
    then Firebug.console##(time (Js.string "set_content_local"));
    run_onunload_wrapper really_set cancel
  with exn ->
    recover ();
    Lwt_log.ign_debug ~section ~exn "set_content_local";
    Lwt.fail exn

(* Function to be called for server side services: *)
let set_content ~replace ~uri ?offset ?fragment content =
  Lwt_log.ign_debug ~section:section_page "Set content";
  (* TODO: too early? *)
  let target_uri = uri in
  let%lwt () =
    run_lwt_callbacks
      { in_cache = is_in_cache !active_page.page_id;
        origin_uri = get_current_uri ();
        target_uri;
        origin_id = !active_page.page_id.state_index;
        target_id = None}
      (flush_onchangepage ())
  in
  match content with
  | None -> Lwt.return_unit
  | Some content ->
    let locked = ref true in
    let really_set () =
      reload_function := None;
      set_uri ~replace ?fragment uri;
      (* Convert the DOM nodes from XML elements to HTML elements. *)
      let fake_page =
        Eliommod_dom.html_document content registered_process_node
      in
      (* insert_base fake_page; Now done server side *)
      (* Inline CSS in the header to avoid the "flashing effect".
         Otherwise, the browser start to display the page before
         loading the CSS. *)
      let preloaded_css =
        if !only_replace_body then Lwt.return_unit else
        Eliommod_dom.preload_css fake_page in
      (* Unique nodes of scope request must be bound before the
         unmarshalling/unwrapping of page data. *)

      relink_request_nodes fake_page;
      (* Put the loaded data script in action *)
      load_data_script fake_page;
      (* Unmarshall page data. *)
      let cookies = Eliom_request_info.get_request_cookies () in
      let js_data = Eliom_request_info.get_request_data () in
      (* Update tab-cookies: *)
      let host =
          match Url.url_of_string uri with
          | Some (Url.Http url)
          | Some (Url.Https url) -> Some url.Url.hu_host
          | _ -> None in
      Eliommod_cookies.update_cookie_table host cookies;
      (* Wait for CSS to be inlined before substituting global nodes: *)
      let%lwt () = preloaded_css in
      (* Bind unique node (request and global) and register event
         handler.  Relinking closure nodes must take place after
         initializing the client values *)
      let closure_nodeList, attrib_nodeList =
        relink_page_but_client_values fake_page
      in
      Eliom_request_info.set_session_info ~uri
        js_data.Eliom_common.ejs_sess_info @@ fun () ->
      (* Really change page contents *)
      replace_page ~do_insert_base:false fake_page;
      (* Initialize and provide client values. May need to access to
         new DOM. Necessary for relinking closure nodes *)
      do_request_data js_data.Eliom_common.ejs_request_data;
      (* Replace closure ids in document with event handlers
         (from client values) *)
      let () = relink_attribs
          Dom_html.document##.documentElement
          js_data.Eliom_common.ejs_client_attrib_table attrib_nodeList in
      let onload_closure_nodes =
        relink_closure_nodes
          Dom_html.document##.documentElement
          js_data.Eliom_common.ejs_event_handler_table closure_nodeList
      in
      (* The request node table must be empty when nodes received via
         call_ocaml_service are unwrapped. *)
      reset_request_nodes ();
      Eliommod_dom.add_formdata_hack_onclick_handler ();
      dom_history_ready := true;
      locked := false;
      let load_callbacks =
        flush_onload () @ [onload_closure_nodes; broadcast_load_end] in
      Lwt_mutex.unlock load_mutex;
      run_callbacks load_callbacks;
      scroll_to_fragment ?offset fragment;
      advance_page ();
      if !Eliom_config.debug_timings then
        Firebug.console##(timeEnd (Js.string "set_content"));
      Lwt.return_unit
    and recover () =
      if !locked then Lwt_mutex.unlock load_mutex;
      if !Eliom_config.debug_timings
      then Firebug.console##(timeEnd (Js.string "set_content"))
    in
    try%lwt
      let%lwt () = Lwt_mutex.lock load_mutex in
      set_loading_phase ();
      if !Eliom_config.debug_timings
      then Firebug.console##(time (Js.string "set_content"));
      let g () = recover (); Lwt.return_unit in
      run_onunload_wrapper really_set g
    with exn ->
      recover ();
      Lwt_log.ign_debug ~section ~exn "set_content";
      Lwt.fail exn

let ocamlify_params =
  List.map
    (function
      | v, `String s -> v, Js.to_string s
      | _, _ -> assert false)

let make_uri subpath params =
  let base =
    if is_client_app () then
      match subpath with
      | _ :: _ ->
        String.concat "/" subpath
      | [] ->
        "/"
    else
      let path =
        match subpath with
        | _ :: _ ->
          String.concat "/" subpath
        | [] ->
          ""
      and port =
        match Url.Current.port with
        | Some port ->
          Printf.sprintf ":%d" port
        | None ->
          ""
      in
      Printf.sprintf "%s//%s%s/%s"
        Url.Current.protocol Url.Current.host
        port path
  and params = List.map (fun (s, s') -> s, `String (Js.string s')) params in
  Eliom_uri.make_string_uri_from_components (base, params, None)

let route ({ Eliom_route.i_subpath ; i_get_params ; i_post_params } as info) =
  Lwt_log.ign_debug ~section:section_page "Route";
  let info, i_subpath =
    match i_subpath with
    | ["."; ""] ->
      {info with i_subpath = []}, []
    | i_subpath ->
      info, i_subpath
  in
  let uri = make_uri i_subpath i_get_params in
  Eliom_request_info.update_session_info ~path:i_subpath
    ~all_get_params:i_get_params ~all_post_params:(Some i_post_params)
  @@ fun () ->
  let%lwt result =
    Eliom_route.call_service
      { info with
        Eliom_route.i_get_params =
          Eliom_common.(remove_prefixed_param nl_param_prefix)
            i_get_params }
  in
  Lwt.return (uri, result)

let switch_to_https () =
  let info = Eliom_process.get_info () in
  Eliom_process.set_info {info with Eliom_common.cpi_ssl = true }

let rec handle_result ~replace ~uri result =
  match%lwt result with
  | Eliom_service.No_contents ->
     Lwt.return_unit
  | Dom d ->
     change_url_string ~replace uri;
     set_content_local d
  | Redirect service ->
     change_page ~replace ~service () ()
  | Reload_action {hidden; https} ->
     match hidden, https with
     | false, false ->
        reload_without_na_params
          ~replace ~uri ~fallback:Eliom_service.reload_action
     | false, true ->
        switch_to_https ();
        reload_without_na_params
          ~replace ~uri ~fallback:Eliom_service.reload_action_https
     | true, false ->
        reload ~replace ~uri ~fallback:Eliom_service.reload_action_hidden
     | true, true ->
        switch_to_https ();
        reload ~replace ~uri ~fallback:Eliom_service.reload_action_https_hidden

(* == Main (exported) function: change the content of the page without
   leaving the javascript application. See [change_page_uri] for the
   function used to change page when clicking a link and
   [change_page_{get,post}_form] when submiting a form. *)
and change_page :
      'get 'post 'meth 'attached 'co 'ext 'reg 'tipo 'gn 'pn.
      ?ignore_client_fun:bool ->
      ?replace:bool ->
      ?absolute:bool ->
      ?absolute_path:bool ->
      ?https:bool ->
      service:
        ('get, 'post, 'meth, 'attached, 'co, 'ext, 'reg, 'tipo, 'gn, 'pn,
         Eliom_service.non_ocaml)
        Eliom_service.t ->
      ?hostname:string ->
      ?port:int ->
      ?fragment:string ->
      ?keep_nl_params:[ `All | `None | `Persistent ] ->
      ?nl_params:Eliom_parameter.nl_params_set ->
      ?keep_get_na_params:bool ->
      ?progress:(int -> int -> unit) ->
      ?upload_progress:(int -> int -> unit) ->
      ?override_mime_type:string ->
      'get -> 'post -> unit Lwt.t
  =
  fun
  (type m)
    ?(ignore_client_fun = false)
    ?(replace = false)
    ?absolute ?absolute_path ?https
    ~(service : (_, _, m, _, _, _, _, _, _, _, _) Eliom_service.t)
    ?hostname ?port ?fragment
    ?keep_nl_params ?(nl_params = Eliom_parameter.empty_nl_params_set)
    ?keep_get_na_params
    ?progress ?upload_progress ?override_mime_type
    get_params post_params ->
  Lwt_log.ign_debug ~section:section_page "Change page";
  let xhr = Eliom_service.xhr_with_cookies service in
  if xhr = None
  || (https = Some true && not Eliom_request_info.ssl_)
  || (https = Some false && Eliom_request_info.ssl_)
  then
    let () = Lwt_log.ign_debug ~section:section_page "change page: xhr is None" in
    Lwt.return
      (exit_to
         ?absolute ?absolute_path ?https ~service ?hostname ?port ?fragment
         ?keep_nl_params ~nl_params ?keep_get_na_params
         get_params post_params)
  else
    with_progress_cursor
      (match xhr with
       | Some (Some tmpl as t) when t = Eliom_request_info.get_request_template () ->
         Lwt_log.ign_debug ~section:section_page
           "change page: xhr is Some of get request template";
         let nl_params =
           Eliom_parameter.add_nl_parameter
             nl_params Eliom_request.nl_template tmpl
         in
         let%lwt uri, content =
           raw_call_service
             ?absolute ?absolute_path ?https ~service ?hostname ?port ?fragment
             ?keep_nl_params ~nl_params ?keep_get_na_params
             ?progress ?upload_progress ?override_mime_type
             get_params post_params in
         set_template_content ~replace ~uri ?fragment (Some content)
       | _ ->
         match Eliom_service.client_fun service with
         | Some f when (not ignore_client_fun) ->
           Lwt_log.ign_debug ~section:section_page
             "change page: client_fun service is Some and (not ignore_client_fun)";
           (* The service has a client side implementation.
              We do not make the request *)
           (* I record the function to be used for void coservices: *)
           Eliom_lib.Option.iter
             (fun rf -> reload_function := Some (fun () -> rf get_params))
             (Eliom_service.reload_fun service);
           let uri, l, l' =
             match
               create_request_
                 ~absolute:true
                 ?absolute_path ?https ~service ?hostname ?port
                 ?fragment ?keep_nl_params ~nl_params ?keep_get_na_params
                 get_params post_params
             with
             | `Get (uri, l) ->
               uri, l, None
             | `Post (uri, l, l')
             | `Put (uri, l, l')
             | `Delete (uri, l, l') ->
               uri, l, Some (ocamlify_params l')
           in
           let l = ocamlify_params l in
           Eliom_request_info.update_session_info
             ~path:(Url.path_of_url_string uri)
             ~all_get_params:l ~all_post_params:l'
           @@ fun () ->
           let%lwt () =
             run_lwt_callbacks
               {in_cache = is_in_cache !active_page.page_id;
                origin_uri = get_current_uri ();
                target_uri = uri;
                origin_id = !active_page.page_id.state_index;
                target_id = None}
               (flush_onchangepage ())
           in
           with_new_page ~replace () @@ fun () ->
           handle_result ~replace ~uri (f get_params post_params)
         | None when is_client_app () ->
           Lwt_log.ign_debug ~section:section_page
             "change page: client_fun service is None and is_client_app";
           Lwt.return @@ exit_to
             ?absolute ?absolute_path ?https ~service ?hostname ?port
             ?fragment ?keep_nl_params ~nl_params ?keep_get_na_params
             get_params post_params
         | _ ->
           Lwt_log.ign_debug ~section:section_page
             "change page: client_fun service is anything else";
           if is_client_app () then
             failwith
               (Printf.sprintf "change page: no client-side service (%b)"
                  ignore_client_fun);
           (* No client-side implementation *)
           with_new_page ~replace () @@ fun () ->
           reload_function := None;
           let cookies_info = Eliom_uri.make_cookies_info (https, service) in
           let%lwt (uri, content) =
             match
               create_request_
                 ?absolute ?absolute_path ?https ~service ?hostname ?port
                 ?fragment ?keep_nl_params ~nl_params ?keep_get_na_params
                 get_params post_params
             with
             | `Get (uri, _) ->
               Eliom_request.http_get
                 ~expecting_process_page:true ?cookies_info uri []
                 Eliom_request.xml_result
             | `Post (uri, _, p) ->
               Eliom_request.http_post
                 ~expecting_process_page:true ?cookies_info uri p
                 Eliom_request.xml_result
             | `Put (uri, _, p) ->
               Eliom_request.http_put
                 ~expecting_process_page:true ?cookies_info uri p
                 Eliom_request.xml_result
             | `Delete (uri, _, p) ->
               Eliom_request.http_delete
                 ~expecting_process_page:true ?cookies_info uri p
                 Eliom_request.xml_result
           in
           let uri, fragment = Url.split_fragment uri in
           set_content ~replace ~uri ?fragment content)

and change_page_unknown
    ?meth ?hostname ?(replace = false) i_subpath i_get_params i_post_params =
  Lwt_log.ign_debug ~section:section_page "Change page unknown";
  let i_sess_info = Eliom_request_info.get_sess_info ()
  and i_meth =
    match meth, i_post_params with
    | Some meth, _ ->
      (meth : [`Get | `Post | `Put | `Delete] :> Eliom_common.meth)
    | None, [] ->
      `Get
    | _, _ ->
      `Post
  in
  with_new_page ~replace () @@ fun () ->
  let%lwt (uri, result) =
    route {
        Eliom_route.i_sess_info ;
        i_subpath ;
        i_meth ;
        i_get_params ;
        i_post_params
      }
  in
  handle_result ~replace ~uri (Lwt.return result)

and reload ~replace ~uri ~fallback =
  let path, args = path_and_args_of_uri uri in
  try%lwt
    change_page_unknown ~replace path args []
  with _ ->
    change_page
      ~replace
      ~ignore_client_fun:true
      ~service:fallback
      () ()

and reload_without_na_params ~replace ~uri ~fallback =
  let path, args = path_and_args_of_uri uri in
  let args = Eliom_common.remove_na_prefix_params args in
  try%lwt
    change_page_unknown ~replace path args []
  with _ ->
    change_page
      ~replace
      ~ignore_client_fun:true
      ~service:fallback
      () ()

(* Function used in "onclick" event handler of <a>.  *)
let change_page_uri_a ?cookies_info ?tmpl ?(get_params = []) full_uri =
  Lwt_log.ign_debug ~section:section_page "Change page uri";
  with_progress_cursor
    (let uri, fragment = Url.split_fragment full_uri in
     if uri <> get_current_uri () || fragment = None
     then begin
       if is_client_app () then
         failwith "Change_page_uri_a called on client app";
       match tmpl with
       | Some t when tmpl = Eliom_request_info.get_request_template () ->
         let%lwt (uri, content) = Eliom_request.http_get
             ?cookies_info uri
             ((Eliom_request.nl_template_string, t) :: get_params)
             Eliom_request.string_result
         in
         set_template_content ~replace:false ~uri ?fragment content
       | _ ->
         let%lwt (uri, content) = Eliom_request.http_get
             ~expecting_process_page:true ?cookies_info uri get_params
             Eliom_request.xml_result
         in
         set_content ~replace:false ~uri ?fragment content
     end else begin
       change_url_string ~replace:true full_uri;
       scroll_to_fragment fragment;
       Lwt.return_unit
     end)

let change_page_uri ?replace full_uri =
  Lwt_log.ign_debug ~section:section_page "Change page uri";
  try%lwt
    match Url.url_of_string full_uri with
    | Some (Url.Http url | Url.Https url) ->
      change_page_unknown ?replace url.Url.hu_path url.Url.hu_arguments []
    | _ ->
      failwith "invalid url"
  with _ ->
    if is_client_app () then
      failwith (Printf.sprintf
                  "Change page uri: can't find service for %s" full_uri)
    else
      (Lwt_log.ign_debug ~section "Change page uri: resort to server";
       change_page_uri_a full_uri)

(* Functions used in "onsubmit" event handler of <form>.  *)

let change_page_get_form ?cookies_info ?tmpl form full_uri =
  with_progress_cursor
    (let form = Js.Unsafe.coerce form in
     let uri, fragment = Url.split_fragment full_uri in
     match tmpl with
     | Some t when tmpl = Eliom_request_info.get_request_template () ->
       let%lwt uri, content = Eliom_request.send_get_form
           ~get_args:[Eliom_request.nl_template_string, t]
           ?cookies_info form uri
           Eliom_request.string_result
       in
       set_template_content ~replace:false ~uri ?fragment content
     | _ ->
       let%lwt uri, content = Eliom_request.send_get_form
           ~expecting_process_page:true ?cookies_info form uri
           Eliom_request.xml_result
       in
       set_content ~replace:false ~uri ?fragment content )

let change_page_post_form ?cookies_info ?tmpl form full_uri =
  with_progress_cursor
    (let form = Js.Unsafe.coerce form in
     let uri, fragment = Url.split_fragment full_uri in
     match tmpl with
     | Some t when tmpl = Eliom_request_info.get_request_template () ->
       let%lwt uri, content = Eliom_request.send_post_form
           ~get_args:[Eliom_request.nl_template_string, t]
           ?cookies_info form uri
           Eliom_request.string_result
       in
       set_template_content ~replace:false ~uri ?fragment content
     | _ ->
       let%lwt uri, content = Eliom_request.send_post_form
           ~expecting_process_page:true ?cookies_info form uri
           Eliom_request.xml_result
       in
       set_content ~replace:false ~uri ?fragment content )

let _ =
  change_page_uri_ :=
    (fun ?cookies_info ?tmpl href ->
       Lwt.ignore_result (change_page_uri_a ?cookies_info ?tmpl href));
  change_page_get_form_ :=
    (fun ?cookies_info ?tmpl form href ->
       Lwt.ignore_result (change_page_get_form ?cookies_info ?tmpl form href));
  change_page_post_form_ :=
    (fun ?cookies_info ?tmpl form href ->
       Lwt.ignore_result (change_page_post_form ?cookies_info ?tmpl form href))

(* == Main (internal) function: change the content of the page without leaving
      the javascript application. *)




(* == Navigating through the history... *)

(* Given a state_id, [replace_page_in_history] replaces the current DOM with a
   DOM from the DOM cache. *)
let restore_history_dom id =
  match History.find_by_state_index id with
  | Some page ->
    begin
      match page.dom with
      | Some dom ->
        if !only_replace_body
          then Dom.replaceChild
            Dom_html.document##.documentElement dom Dom_html.document##.body
          else Dom.replaceChild
            Dom_html.document dom Dom_html.document##.documentElement
      | None -> Lwt_log.ign_error ~section "DOM not actually cached"
    end;
    set_active_page page
  | _ -> Lwt_log.ign_error ~section "cannot find DOM in history"

let () =

  if Eliom_process.history_api
  then

    let revisit full_uri state_id =
      let state =
        try
          get_state state_id
        with Not_found ->
          failwith
            (Printf.sprintf
               "revisit: state id %x/%x not found in sessionStorage (%s)"
               state_id.session_id state_id.state_index full_uri)
      in
      let target_id = state_id.state_index in
      let ev =
        {in_cache = is_in_cache state_id;
         origin_uri = get_current_uri ();
         target_uri = full_uri;
         origin_id = !active_page.page_id.state_index;
         target_id = Some target_id } in
      let tmpl = state.template in
      Lwt.ignore_result @@
        with_progress_cursor @@
          let uri, fragment = Url.split_fragment full_uri in
          if uri = get_current_uri ()
          then begin
              !active_page.page_id <- state_id;
              scroll_to_fragment ~offset:state.position fragment;
              Lwt.return_unit
            end
            else begin
              try (* serve cached page from the from history_doms *)
                if not (is_in_cache state_id) then raise Not_found;
                let%lwt () = run_lwt_callbacks ev (flush_onchangepage ()) in
                restore_history_dom target_id;
                set_current_uri uri;
                let%lwt () =
                  Js_of_ocaml_lwt.Lwt_js_events.request_animation_frame () in
                scroll_to_fragment ~offset:state.position fragment;
                (* Wait for the dom to be repainted before scrolling *)
                let%lwt () =
                  Js_of_ocaml_lwt.Lwt_js_events.request_animation_frame () in
                scroll_to_fragment ~offset:state.position fragment;
                (* When we use iPhone, we need to wait for one more
                   [request_animation_frame] before scrolling.The
                   function [scroll_to_fragment] is called twice. In
                   other words, we want to call [scroll_to_fragment]
                   as early as possible so that the scroll position
                   will not jump after the second [request_animation_frame]
                   if the dom has already be painted after the first one. *)
                Lwt.return_unit
              with Not_found ->
              let session_changed = state_id.session_id <> session_id in
              if session_changed && is_client_app () then
                failwith (Printf.sprintf
                            "revisit: session changed on client: %d => %d (%s)"
                            state_id.session_id session_id full_uri);
              try (* same session *)
                if session_changed then raise Not_found;
                let rf = List.assq state_id.state_index !reload_functions in
                reload_function := Some rf;
                let%lwt () = run_lwt_callbacks ev (flush_onchangepage ()) in
                let old_page = History.find_by_state_index state_id.state_index in
                with_new_page ~state_id ?old_page ~replace:false () @@ fun () ->
                set_current_uri uri;
                History.replace (get_this_page ());
                let%lwt () =
                  match%lwt rf () () with
                  | Eliom_service.Dom d -> set_content_local d
                  | r -> handle_result ~uri:(get_current_uri ()) ~replace:true (Lwt.return r)
                in
                scroll_to_fragment ~offset:state.position fragment;
                Lwt.return_unit
              with Not_found -> (* different session ID *)
              set_current_uri uri;
              match tmpl with
              | Some t when tmpl = Eliom_request_info.get_request_template () ->
                let%lwt (uri, content) = Eliom_request.http_get
                    uri [(Eliom_request.nl_template_string, t)]
                    Eliom_request.string_result
                in
                let%lwt () =
                  set_template_content content
                    ~replace:true ~uri
                in
                scroll_to_fragment ~offset:state.position fragment;
                Lwt.return_unit
              | _ ->
                 if is_client_app () then
                   failwith (Printf.sprintf "revisit: could not generate page client-side (%s)" full_uri);
                with_new_page
                  ?state_id:(if session_changed then None else Some state_id)
                  ~replace:false () @@ fun () ->
                let%lwt uri, content =
                  Eliom_request.http_get ~expecting_process_page:true uri []
                    Eliom_request.xml_result in
                let%lwt () =
                  set_content
                    ~uri
                    ~replace:true
                    ~offset:state.position
                    ?fragment content in
                Lwt.return_unit
            end
    in

    let revisit full_uri state_id =
      (* CHECKME: is it OK that set_state happens after the unload
         callbacks are executed? *)
      let f () = update_state (); revisit full_uri state_id
      and cancel () = () in
      run_onunload_wrapper f cancel
    in

    Lwt.ignore_result
      (let%lwt () = wait_load_end () in
       Dom_html.window##.history##(replaceState
         (Js.Opt.return (!active_page.page_id, Dom_html.window##.location##.href))
         (Js.string "")
         (Js.null) );
       Lwt.return_unit);

    Dom_html.window##.onpopstate :=
      Dom_html.handler (fun event ->
        Eliommod_dom.touch_base ();
        Js.Opt.case ((Js.Unsafe.coerce event)##.state :
                       (state_id * Js.js_string Js.t) Js.opt)
          (fun () -> () (* Ignore dummy popstate event fired by chromium. *))
          (fun (state, full_uri) -> revisit (Js.to_string full_uri) state);
        Js._false)

  else (* Without history API *)

    (* FIXME: This should be adapted to work with template...
       Solution: add the "state_id" in the fragment ??
    *)

    let read_fragment () = Js.to_string Dom_html.window##.location##.hash in
    let auto_change_page fragment =
      Lwt.ignore_result
        (let l = String.length fragment in
         if (l = 0) || ((l > 1) && (fragment.[1] = '!'))
         then if fragment <> !current_pseudo_fragment then
             (current_pseudo_fragment := fragment;
              let uri =
                match l with
                | 2 -> "./" (* fix for firefox *)
                | 0 | 1 -> fst (Url.split_fragment Url.Current.as_string)
                | _ -> String.sub fragment 2 ((String.length fragment) - 2)
              in
              (* CCC TODO handle templates *)
              change_page_uri uri)
           else Lwt.return_unit
         else Lwt.return_unit)
    in

    Eliommod_dom.onhashchange (fun s -> auto_change_page (Js.to_string s));
    let first_fragment = read_fragment () in
    if first_fragment <> !current_pseudo_fragment
    then
      Lwt.ignore_result (
        let%lwt () = wait_load_end () in
        auto_change_page first_fragment;
        Lwt.return_unit)

let () =
  Eliom_unwrap.register_unwrapper
    (Eliom_unwrap.id_of_int Eliom_common_base.server_function_unwrap_id_int)
    (fun (service, _) ->
       (* 2013-07-31 I make all RPC's absolute because otherwise
          it does not work with mobile apps.
          Is it a problem?
          -- Vincent *)
       call_ocaml_service ~absolute:true ~service ())

let get_application_name = Eliom_process.get_application_name

let set_client_html_file = Eliom_common.set_client_html_file

module Additional_headers = Eliom_request.Additional_headers
