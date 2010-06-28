(* Ocsigen
 * http://www.ocsigen.org
 * Module eliom_client.ml
 * Copyright (C) 2010 Vincent Balat
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


exception Failed_service of int

let (>>=) = Lwt.bind
let (>>>) x f = f x
let current_fragment = ref ""
let url_fragment_prefix = "!"
let url_fragment_prefix_with_sharp = "#!"
let appl_name = Eliom_sessions.appl_name


external string_of_byte_string : int Js.js_array Js.t -> string =
  "caml_string_of_byte_string"
let unmarshal v =
  Marshal.from_string (string_of_byte_string (Js.Unsafe.variable v)) 0

let appl_instance_id = Js.to_string (Js.Unsafe.variable "appl_instance_id")


let create_request_
    ?absolute ?absolute_path ?https
    ~(service : ('get, 'post, 
                 [< `Attached of (Eliom_services.attached_service_kind, [< Eliom_services.getpost]) Eliom_services.a_s
                 | `Nonattached of [< Eliom_services.getpost] Eliom_services.na_s ],
                 [< Eliom_services.suff ], 'h, 'i,
                 [< Eliom_services.registrable ], 'j)
        Eliom_services.service)
    ~sp
    ?hostname ?port ?fragment ?keep_nl_params ?nl_params ?keep_get_na_params
    g p =
  match Eliom_services.get_get_or_post service with
    | `Get ->
        let uri =
          Eliom_uri.make_string_uri
            ?absolute ?absolute_path ?https
            ~service ~sp
            ?hostname ?port ?fragment ?keep_nl_params ?nl_params g
        in
        Ocsigen_lib.Left uri
    | `Post ->
        let path, g, fragment, p =
          Eliom_uri.make_post_uri_components
            ?absolute ?absolute_path ?https
            ~service ~sp
            ?hostname ?port ?fragment ?keep_nl_params ?nl_params
            ?keep_get_na_params g p
        in
        let uri = 
          Eliom_uri.make_string_uri_from_components (path, g, fragment) 
        in
        Ocsigen_lib.Right (uri, p)


let redirect_get url = Dom_html.window##location##href <- Js.string url

let redirect_post url params =
  let f = Dom_html.createForm Dom_html.document in
  f##action <- Js.string url;
  f##_method <- Js.string "post";
  List.iter
    (fun (n, v) ->
       let i =
         Dom_html.createInput
           ~_type:(Js.string "text") ~name:(Js.string n) Dom_html.document in
       i##value <- Js.string v;
       Dom.appendChild f i)
    params;
  f##submit ()


let exit_to
    ?absolute ?absolute_path ?https
    ~(service : ('get, 'post, 
                 [< `Attached of (Eliom_services.attached_service_kind, [< Eliom_services.getpost]) Eliom_services.a_s
                 | `Nonattached of [< Eliom_services.getpost] Eliom_services.na_s ],
                 [< Eliom_services.suff ], 'h, 'i,
                 [< Eliom_services.registrable ], 'j)
        Eliom_services.service)
    ~sp
    ?hostname ?port ?fragment ?keep_nl_params ?nl_params ?keep_get_na_params
    g p =
  (match create_request_
     ?absolute ?absolute_path ?https
     ~service ~sp
     ?hostname ?port ?fragment ?keep_nl_params ?nl_params ?keep_get_na_params
     g p
   with
     | Ocsigen_lib.Left uri -> redirect_get uri
     | Ocsigen_lib.Right (uri, p) -> redirect_post uri p)


(** This will change the URL, without doing a request.
    As browsers do not not allow to change the URL,
    we write the new URL in the fragment part of the URL.
    A script must do the redirection if there is something in the fragment.
    Usually this function is only for internal use.
*)
let change_url
(*VVV is it safe to have absolute URLs? do we accept non absolute paths? *)
    ?absolute ?absolute_path ?https
    ~(service : ('get, 'post, 
                 [< `Attached of (Eliom_services.attached_service_kind, [< Eliom_services.getpost]) Eliom_services.a_s
                 | `Nonattached of [< Eliom_services.getpost] Eliom_services.na_s ],
                 [< Eliom_services.suff ], 'h, 'i,
                 [< Eliom_services.registrable ], 'j)
        Eliom_services.service)
    ~sp
    ?hostname ?port ?fragment ?keep_nl_params ?nl_params ?keep_get_na_params
    g p =
(*VVV only for GET services? *)
  let uri =
    (match create_request_
       ?absolute ?absolute_path ?https
       ~service ~sp
       ?hostname ?port ?fragment ?keep_nl_params ?nl_params ?keep_get_na_params
       g p
     with
       | Ocsigen_lib.Left uri -> uri
       | Ocsigen_lib.Right (uri, p) -> uri)
  in
  current_fragment := url_fragment_prefix_with_sharp^uri;
  Dom_html.window##location##hash <- Js.string (url_fragment_prefix^uri)


let container_node = lazy (Js.Unsafe.coerce (Eliommod_client.unwrap_node (unmarshal "container_node")): Dom_html.element Js.t)



let set_inner_html code s =
  if code <> 200
  then Lwt.fail (Failed_service code)
  else begin
    let (ref_tree_list, (((timeofday, _), _) as global_data), content) = 
      Marshal.from_string (Ocsigen_lib.urldecode_string s) 0 
    in
    let container_node = Lazy.force container_node in
    container_node##innerHTML <- Js.string content;
    Eliommod_client.relink_dom_list 
      timeofday (container_node##childNodes) ref_tree_list;
    Eliommod_client.fill_global_data_table global_data;
    Lwt.return ()
  end



let change_page
    ?absolute ?absolute_path ?https
    ~(service : ('get, 'post, 
                 [< `Attached of (Eliom_services.attached_service_kind, [< Eliom_services.getpost]) Eliom_services.a_s
                 | `Nonattached of [< Eliom_services.getpost] Eliom_services.na_s ],
                 [< Eliom_services.suff ], 'h, 'i,
                 [< Eliom_services.registrable ], 'j)
        Eliom_services.service)
    ~sp
    ?hostname ?port ?fragment ?keep_nl_params
    ?(nl_params=Eliom_parameters.empty_nl_params_set) ?keep_get_na_params
    (g : 'get) (p : 'post) =
  if Eliom_services.get_application_name service <> (Some appl_name)
  then
    Lwt.return (exit_to
                  ?absolute ?absolute_path ?https
                  ~service ~sp
                  ?hostname ?port ?fragment ?keep_nl_params
                  ~nl_params ?keep_get_na_params
                  g p)
  else
    (match
        create_request_
          ?absolute ?absolute_path ?https
          ~service ~sp
          ?hostname ?port ?fragment ?keep_nl_params
          ~nl_params:(Eliom_parameters.add_nl_parameter
                        nl_params
                        Eliom_parameters.eliom_appl_nlp
                        (appl_name, appl_instance_id))
          ?keep_get_na_params
          g p
     with
       | Ocsigen_lib.Left uri -> 
           Lwt_obrowser.http_get uri [] >>= fun r ->
           Lwt.return (r, uri)
       | Ocsigen_lib.Right (uri, p) -> 
           Lwt_obrowser.http_post uri p >>= fun r ->
           Lwt.return (r, uri))
    >>= fun ((code, s), uri) ->
    set_inner_html code s >>= fun () ->
(*VVV The URL is created twice ... 
  Once with eliom_appl_instance_id (for the request), 
  and once without it (we do not want it to appear in the URL).
  How to avoid this?
*)
    change_url
      ?absolute ?absolute_path ?https
      ~service ~sp
      ?hostname ?port ?fragment ?keep_nl_params ~nl_params ?keep_get_na_params
      g p;
(*VVV change the URL only if it is different? *)
    Lwt.return ()





let call_service
    ?absolute ?absolute_path ?https
    ~service ~sp
    ?hostname ?port ?fragment ?keep_nl_params ?nl_params ?keep_get_na_params
    g p =
  (match create_request_
     ?absolute ?absolute_path ?https
     ~service ~sp
     ?hostname ?port ?fragment ?keep_nl_params ?nl_params ?keep_get_na_params
     g p
   with
     | Ocsigen_lib.Left uri -> Lwt_obrowser.http_get uri []
     | Ocsigen_lib.Right (uri, p) -> Lwt_obrowser.http_post uri p)
  >>= fun (code, s) ->
  if code = 200
  then Lwt.return s
  else Lwt.fail (Failed_service code)



let call_caml_service
    ?absolute ?absolute_path ?https
    ~service ~sp
    ?hostname ?port ?fragment ?keep_nl_params ?nl_params ?keep_get_na_params
    g p =
  call_service
    ?absolute ?absolute_path ?https
    ~service ~sp
    ?hostname ?port ?fragment ?keep_nl_params ?nl_params ?keep_get_na_params
    g p
  >>= fun s ->
(* the string is urlencoded because otherwise js does strange things
   with strings ... *)
  Lwt.return (Marshal.from_string (Ocsigen_lib.urldecode_string s) 0)


       

let fake_page = Dom_html.createBody Dom_html.document
(*FIX: is that correct?
  XHTML.M.toelt (XHTML.M.body [])
*)

let get_subpage
    ?absolute ?absolute_path ?https
    ~service ~sp
    ?hostname ?port ?fragment ?keep_nl_params
    ?(nl_params=Eliom_parameters.empty_nl_params_set) ?keep_get_na_params
    g p =
(*VVV Should we fail if the service does not belong to the same application? *)
  (match create_request_
     ?absolute ?absolute_path ?https
     ~service ~sp
     ?hostname ?port ?fragment ?keep_nl_params
     ~nl_params:(Eliom_parameters.add_nl_parameter
                   nl_params
                   Eliom_parameters.eliom_appl_nlp
                   (appl_name, appl_instance_id))
     ?keep_get_na_params
     g p
   with
     | Ocsigen_lib.Left uri -> 
         Lwt_obrowser.http_get uri []
     | Ocsigen_lib.Right (uri, p) -> Lwt_obrowser.http_post uri p)
  >>= fun (code, s) ->
  if code <> 200
  then Lwt.fail (Failed_service code)
  else begin
    let (ref_tree_list, (((timeofday, _), _) as global_data), content) = 
      Marshal.from_string (Ocsigen_lib.urldecode_string s) 0 
    in
    (* Hack to make the result considered as XHTML: *)
    fake_page##innerHTML <- Js.string content;
    let nodes = fake_page##childNodes in
    let node_list = ref [] in
    for i = nodes##length - 1 downto 0 do
      node_list := nodes##item (i) :: !node_list
    done;
    Eliommod_client.relink_dom_list timeofday nodes ref_tree_list;
    fake_page##innerHTML <- Js.string "";
    Eliommod_client.fill_global_data_table global_data;
    Lwt.return (XHTML.M.totl !node_list)
  end





(*****************************************************************************)
(* Make the back button work when only the fragment has changed ... *)
(*VVV We check the fragment every t second ... :-( *)

let write_fragment s = Ocsigen_lib.window##location##hash <- Js.string s

let read_fragment () = Js.to_string Ocsigen_lib.window##location##hash


let (fragment, set_fragment_signal) = React.S.create (read_fragment ())

let rec fragment_polling () =
  Lwt_js.sleep 0.2 >>= fun () ->
  let new_fragment = read_fragment () in
  if new_fragment <> (React.S.value fragment)
  then set_fragment_signal new_fragment;
  fragment_polling ()

let _ = fragment_polling ()


let eliom_appl_nlp =
  Eliom_parameters.string_of_nl_params_set
    (Eliom_parameters.add_nl_parameter
       Eliom_parameters.empty_nl_params_set
       Eliom_parameters.eliom_appl_nlp
       (appl_name, appl_instance_id))

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
             | 0 | 1 -> Eliom_sessions.full_uri
             | _ ->
                 String.sub fragment 2 ((String.length fragment) - 2) 
         in
         let uri =
           if String.contains uri '?'
           then String.concat "&" [uri; eliom_appl_nlp]
           else String.concat "?" [uri; eliom_appl_nlp]
         in
         Lwt_obrowser.http_get uri [] >>= fun (code, s) ->
         set_inner_html code s
         )
       else Lwt.return ()
     else Lwt.return ())

let _ = React.E.map auto_change_page (React.S.changes fragment)

(* ==A closure that is registered by default to simulate <a> *)
let _ =
  Eliommod_client.register_closure
    Eliom_client_types.a_closure_id
    (fun
       (absolute, absolute_path, https, service, sp, hostname, port,
        fragment, keep_nl_params, nl_params, getparams)
       ->
         let absolute = Eliommod_client.unwrap absolute in
         let https = Eliommod_client.unwrap https in
         let service = Eliommod_client.unwrap service in
         let sp = Eliommod_client.unwrap_sp sp in
         let hostname = Eliommod_client.unwrap hostname in
         let port = Eliommod_client.unwrap port in
         let fragment = Eliommod_client.unwrap fragment in
         let keep_nl_params = Eliommod_client.unwrap keep_nl_params in
         let nl_params = Eliommod_client.unwrap nl_params in
         let getparams = Eliommod_client.unwrap getparams in
         let absolute_path = Eliommod_client.unwrap absolute_path in
         ignore
           (change_page
              ?absolute ?absolute_path ?https
              ~service ~sp ?hostname ?port ?fragment ?keep_nl_params ?nl_params
              getparams ()))


let make_a_with_onclick 
    make_a
    register_event
    ?absolute
    ?absolute_path
    ?https
    ?a
    ~service
    ~sp
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
       ~sp
       ?hostname
       ?port
       ?fragment
       ?keep_nl_params
       ?nl_params
       getparams ())
    ();
  node
