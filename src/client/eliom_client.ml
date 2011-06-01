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

open Eliom_pervasives

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
        Left uri
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
        Right (uri, p)



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
     | Left uri -> Eliom_request.redirect_get uri
     | Right (uri, p) -> Eliom_request.redirect_post uri p)


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
       | Left uri -> uri
       | Right (uri, p) -> uri)
  in
  change_url_string uri


(* lazy because we want the page to be loaded *)
let container_node =
  lazy (HTML5.Dom.retrieve_node (unmarshal_js_var "container_id"))

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


(* let change_page_uri_ = *)
  (* ref (fun ?cookies_info ?get_params a -> failwith "not initialised") *)
(* let change_page_get_form_ = *)
  (* ref (fun ?cookies_info a b -> failwith "not initialised") *)
(* let change_page_post_form_ = *)
  (* ref (fun ?cookies_info a b -> failwith "not initialised") *)

(* let bind_form_or_link = function *)
  (* | Eliom_types.OFA (node, href, cookies_info) -> *)
    (* let node = Js.Unsafe.coerce (node) in *)
    (* XML.register_event ?keep_default:(Some false) node "onclick" *)
      (* (fun () -> !change_page_uri_ ?cookies_info href) *)
      (* () *)
  (* | Eliom_types.OFForm_get (node, uri, cookies_info) -> *)
    (* let node = Js.Unsafe.coerce (node) in *)
    (* XML.register_event ?keep_default:(Some false) node "onsubmit" *)
      (* (fun () -> !change_page_get_form_ ?cookies_info node uri) *)
      (* (); *)
  (* | Eliom_types.OFForm_post (node, uri, cookies_info) -> *)
    (* let node = Js.Unsafe.coerce (node) in *)
    (* XML.register_event ?keep_default:(Some false) node "onsubmit" *)
      (* (fun () -> !change_page_post_form_ ?cookies_info node uri) *)
      (* () *)

(* BEGIN FORMDATA HACK: This is only needed if FormData is not available in the browser.
   When it will be commonly available, remove all sections marked by "FORMDATA HACK" !
   Notice: this hack is used to circumvent a limitation in FF4 implementation of formdata:
     if the user click on a button in a form, formdatas created in the onsubmit callback normaly contains the value of the button. ( it is the behaviour of chromium )
     in FF4, it is not the case: we must do this hack to find wich button was clicked.

   This is implemented in:
   * this file -> here and called in load_eliom_data
   * Eliom_request: in send_post_form
   * in js_of_ocaml, module Form: the code to emulate FormData *)

(* let onclick_on_body_handler event = *)
  (* (match Dom_html.tagged (Dom_html.eventTarget event) with *)
    (* | Dom_html.Button button -> *)
      (* (Js.Unsafe.variable "window")##eliomLastButton <- Some button; *)
    (* | Dom_html.Input input when input##_type = Js.string "submit" -> *)
      (* (Js.Unsafe.variable "window")##eliomLastButton <- Some input; *)
    (* | _ -> (Js.Unsafe.variable "window")##eliomLastButton <- None); *)
  (* Js._true *)

(* let add_onclick_events = *)
  (* let registered = ref false in *)
  (* fun () -> *)
    (* if !registered *)
    (* then () *)
    (* else *)
      (* begin *)
	(* ignore (Dom_html.addEventListener ( Dom_html.window##document##body ) *)
		  (* Dom_html.Event.click ( Dom_html.handler onclick_on_body_handler ) *)
		  (* Js._true *)
		  (* :Dom_html.event_listener_id); *)
	(* registered := true; *)
      (* end *)

(* END FORMDATA HACK *)

let load_eliom_data js_data contents : unit Lwt.t =
  (* Now we bind the XHR forms and links sent by the server: *)
  (* List.iter bind_form_or_link (Eliommod_cli.unwrap js_data.Eliom_types.ejs_xhr); *)
  (* FIXME GRGR fix and uncomment previous line *)
  Eliommod_cli.register_nodes js_data.Eliom_types.ejs_ref_tree contents;
  Eliommod_cookies.update_cookie_table js_data.Eliom_types.ejs_tab_cookies;
  Eliom_request_info.set_session_info js_data.Eliom_types.ejs_sess_info;
  let on_load =
    List.map XML.reify_event js_data.Eliom_types.ejs_onload in
  let on_unload =
    List.map XML.reify_event js_data.Eliom_types.ejs_onunload in
  on_unload_scripts := [fun () -> Lwt_list.iter_s (fun f -> f()) on_unload];
  Lwt_list.iter_p (fun f -> f()) on_load

let set_inner_html (ed, content) =
  ignore (Lwt_list.iter_p (fun f -> f ()) !on_unload_scripts);
  on_unload_scripts := [];
  let container_node = Lazy.force container_node in
  container_node##innerHTML <- Js.string content;
  load_eliom_data ed container_node

let on_unload f =
  on_unload_scripts := f::(!on_unload_scripts)

let rec change_page_set_content :
    'get 'post 'd 'e 'm 'n 'o 'p 'q 'return.
    ((int ->
        ?absolute:bool ->
        ?absolute_path:bool ->
        ?https:bool ->
        service:('get, 'post,
                 ([< `Attached of
                     (Eliom_services.attached_service_kind,
                      ([< Eliom_services.getpost ] as 'q))
                       Eliom_services.a_s
                  | `Nonattached of ([< Eliom_services.getpost ] as 'p) Eliom_services.na_s ] as 'm),
                 ([< `WithSuffix | `WithoutSuffix ] as 'n), 'd, 'e,
                 ([< Eliom_services.registrable ] as 'o),
                 'return)
          Eliom_services.service ->
        ?hostname:string ->
        ?port:int ->
        ?fragment:string ->
        ?keep_nl_params:[ `All | `None | `Persistent ] ->
        ?nl_params:(string * string) list String.Table.t ->
        ?keep_get_na_params:bool -> 'get -> 'post -> unit Lwt.t) *
        (int -> Eliom_services.eliom_appl_answer -> unit Lwt.t))
  = (
  (* change_page *)
  (fun
    i
    ?absolute
    ?absolute_path
    ?https
    ~service
    ?hostname
    ?port
    ?fragment
    ?keep_nl_params
    ?(nl_params=Eliom_parameters.empty_nl_params_set)
    ?keep_get_na_params
    g
    p ->
  if not (Eliom_services.xhr_with_cookies service)
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
       | Left uri ->
         Eliom_request.http_get
           ?cookies_info:(Eliom_uri.make_cookies_info (https, service)) uri []
       | Right (uri, p) ->
         Eliom_request.http_post
           ?cookies_info:(Eliom_uri.make_cookies_info (https, service)) uri p)
    >>= fun r ->
    (snd change_page_set_content) i (Eliom_request.get_eliom_appl_result r)
  end),

(* set_content *) (fun i -> function
  | Eliom_services.EAContent (c, u) ->
    change_url_string u;
    set_inner_html c
  | Eliom_services.EAHalfRedir u ->
    Eliom_request.redirect_get u;
    Lwt.fail Eliom_request.Program_terminated
  | Eliom_services.EAFullRedir service ->
    if i < Eliom_request.max_redirection_level
    then (fst change_page_set_content) (i+1)
      ?absolute:None
      ?absolute_path:None
      ?https:None
      ~service
      ?hostname:None
      ?port:None
      ?fragment:None
      ?keep_nl_params:None
      ?nl_params:None
      ?keep_get_na_params:None
      () ()
    else Lwt.fail Eliom_request.Looping_redirection)
)


let set_content = (snd change_page_set_content) 0

let change_page
    ?absolute
    ?absolute_path
    ?https
    ~service
    ?hostname
    ?port
    ?fragment
    ?keep_nl_params
    ?nl_params
    ?keep_get_na_params
    g
    p =
  (fst change_page_set_content) 0
    ?absolute
    ?absolute_path
    ?https
    ~service
    ?hostname
    ?port
    ?fragment
    ?keep_nl_params
    ?nl_params
    ?keep_get_na_params
    g
    p


let change_page_uri ?cookies_info ?(get_params = []) uri =
  Eliom_request.http_get ?cookies_info uri get_params >>= fun r ->
  set_content (Eliom_request.get_eliom_appl_result r)

let change_page_get_form ?cookies_info form uri =
  let form = Js.Unsafe.coerce form in
  Eliom_request.send_get_form ?cookies_info form uri >>= fun r ->
  set_content (Eliom_request.get_eliom_appl_result r)

let change_page_post_form ?cookies_info form uri =
  let form = Js.Unsafe.coerce form in
  Eliom_request.send_post_form ?cookies_info form uri >>= fun r ->
  set_content (Eliom_request.get_eliom_appl_result r)

(* FIXME GRGR *)
(* let _ = *)
  (* change_page_uri_ := change_page_uri; *)
  (* change_page_get_form_ := change_page_get_form; *)
  (* change_page_post_form_ := change_page_post_form *)


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
     | Left uri ->
       Eliom_request.http_get
         ?cookies_info:(Eliom_uri.make_cookies_info (https, service)) uri []
     | Right (uri, p) ->
       Eliom_request.http_post
         ?cookies_info:(Eliom_uri.make_cookies_info (https, service)) uri p)


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
  let unmarshal s : 'a =
    let (value,sent_nodes) =
      (Marshal.from_string (Url.decode s) 0:'a Eliom_types.eliom_comet_data_type)
    in
    ignore (List.map (Eliommod_cli.rebuild_xml 0L) sent_nodes);
    Eliom_unwrap.unwrap value
  in
  Lwt.return (unmarshal s)


let fake_page = Dom_html.createBody Dom_html.document
(*FIX: is that correct?
  XHTML5.M.toelt (XHTML5.M.body [])
*)

let rec get_subpage_ :
    'get 'post 'd 'e 'm 'n 'o 'p 'q 'return.
    int ->
  ?absolute:bool ->
  ?absolute_path:bool ->
  ?https:bool ->
  service:('get, 'post,
           ([< `Attached of
               (Eliom_services.attached_service_kind,
                ([< Eliom_services.getpost ] as 'q))
                 Eliom_services.a_s
            | `Nonattached of ([< Eliom_services.getpost ] as 'p) Eliom_services.na_s ] as 'm),
           ([< `WithSuffix | `WithoutSuffix ] as 'n), 'd, 'e,
           ([< Eliom_services.registrable ] as 'o),
           'return)
    Eliom_services.service ->
  ?hostname:string ->
  ?port:int ->
  ?fragment:string ->
  ?keep_nl_params:[ `All | `None | `Persistent ] ->
  ?nl_params:(string * string) list String.Table.t ->
  ?keep_get_na_params:bool -> 'get -> 'post ->
  [< `PCDATA | XHTML_types.flow ] HTML5.M.elt list Lwt.t
    = fun i
    ?absolute ?absolute_path ?https ~service
    ?hostname ?port ?fragment ?keep_nl_params
    ?(nl_params=Eliom_parameters.empty_nl_params_set) ?keep_get_na_params
    g p ->
(*VVV Should we fail if the service does not belong to the same application? *)
  (match create_request_
     ?absolute ?absolute_path ?https ~service
     ?hostname ?port ?fragment ?keep_nl_params
     ?keep_get_na_params
     g p
   with
     | Left uri ->
       Eliom_request.http_get
         ?cookies_info:(Eliom_uri.make_cookies_info (https, service)) uri []
     | Right (uri, p) ->
       Eliom_request.http_post
         ?cookies_info:(Eliom_uri.make_cookies_info (https, service)) uri p)
  >>= fun r -> match Eliom_request.get_eliom_appl_result r with
    | Eliom_services.EAContent ((ed, content), _) -> begin
      (* Hack to make the result considered as XHTML: *)
      fake_page##innerHTML <- Js.string content;
      let nodes = fake_page##childNodes in
      let node_list = ref [] in
      for i = nodes##length - 1 downto 0 do
        node_list := Js.Optdef.get (nodes##item (i)) (fun () -> assert false)
        :: !node_list
      done;

      load_eliom_data ed fake_page >>= fun () ->
      fake_page##innerHTML <- Js.string "";
      assert false (* GRGR FIXME *)
      (* Lwt.return !node_list *)
    end
    | Eliom_services.EAHalfRedir u ->
      (* strange ... *)
      Eliom_request.redirect_get u;
      Lwt.fail Eliom_request.Program_terminated
    | Eliom_services.EAFullRedir service ->
      if i < Eliom_request.max_redirection_level
      then get_subpage_ (i+1) ~service () ()
      else Lwt.fail Eliom_request.Looping_redirection

let get_subpage
    ?absolute ?absolute_path ?https ~service
    ?hostname ?port ?fragment ?keep_nl_params
    ?nl_params ?keep_get_na_params
    g p =
  get_subpage_ 0
    ?absolute ?absolute_path ?https ~service
    ?hostname ?port ?fragment ?keep_nl_params
    ?nl_params ?keep_get_na_params
    g p




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

(*SGO* Server generated onclicks/onsubmits

(* A closure that is registered by default to simulate <a>.
   For use with server side generated links.
 *)
let _ =
  Eliommod_cli.register_closure
    Eliom_types.a_closure_id
    (fun (cookies_info, uri) ->
      let uri = Eliommod_cli.unwrap uri in
      let cookies_info = Eliommod_cli.unwrap cookies_info in
      ignore (change_page_uri ?cookies_info uri);
      Js._false);
  Eliommod_cli.register_closure
    Eliom_client_types.get_closure_id
    (fun (cookies_info, uri) ->
      let uri = Eliommod_cli.unwrap uri in
      let node = Js.Unsafe.variable Eliom_client_types.eliom_temporary_form_node_name in
      let cookies_info = Eliommod_cli.unwrap cookies_info in
      ignore (change_page_get_form ?cookies_info node uri);
      Js._false);
  Eliommod_cli.register_closure
    Eliom_client_types.post_closure_id
    (fun (cookies_info, uri) ->
      let uri = Eliommod_cli.unwrap uri in
      let node = Js.Unsafe.variable Eliom_client_types.eliom_temporary_form_node_name in
      let cookies_info = Eliommod_cli.unwrap cookies_info in
      ignore (change_page_post_form ?cookies_info node uri);
      Js._false)

*)
