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
  
let create_request_
    ?absolute ?absolute_path ?https
    ~sp ~service
    ?hostname ?port ?fragment ?keep_nl_params ?nl_params ?keep_get_na_params
    g p =
  match Eliom_services.get_get_or_post service with
    | `Get ->
        let uri =
          Eliom_mkforms.make_string_uri
            ?absolute ?absolute_path ?https
            ~sp ~service
            ?hostname ?port ?fragment ?keep_nl_params ?nl_params g
        in
        Ocsigen_lib.Left uri
    | `Post ->
        let path, g, fragment, p =
          Eliom_mkforms.make_post_uri_components
            ?absolute ?absolute_path ?https
            ~sp ~service
            ?hostname ?port ?fragment ?keep_nl_params ?nl_params
            ?keep_get_na_params g p
        in
        let uri = 
          Eliom_mkforms.make_string_uri_from_components (path, g, fragment) 
        in
        Ocsigen_lib.Right (uri, p)


let call_service
    ?absolute ?absolute_path ?https
    ~sp ~service
    ?hostname ?port ?fragment ?keep_nl_params ?nl_params ?keep_get_na_params
    g p =
  (match create_request_
     ?absolute ?absolute_path ?https
     ~sp ~service
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
    ~sp ~service
    ?hostname ?port ?fragment ?keep_nl_params ?nl_params ?keep_get_na_params
    g p =
  call_service
    ?absolute ?absolute_path ?https
    ~sp ~service
    ?hostname ?port ?fragment ?keep_nl_params ?nl_params ?keep_get_na_params
    g p
  >>= fun s ->
(* the string is urlencoded because otherwise js does strange things
   with strings ... *)
  Lwt.return (Marshal.from_string (Ocsigen_lib.urldecode_string s) 0)


let exit_to
    ?absolute ?absolute_path ?https
    ~sp ~service
    ?hostname ?port ?fragment ?keep_nl_params ?nl_params ?keep_get_na_params
    g p =
  (match create_request_
     ?absolute ?absolute_path ?https
     ~sp ~service
     ?hostname ?port ?fragment ?keep_nl_params ?nl_params ?keep_get_na_params
     g p
   with
     | Ocsigen_lib.Left uri -> Js.redirect_get uri
     | Ocsigen_lib.Right (uri, p) -> Js.redirect_post uri p)



let url_fragment_prefix = "!"
let url_fragment_prefix_with_sharp = "#!"

(** This will change the URL, without doing a request.
    As browsers do not not allow to change the URL,
    we write the new URL in the fragment part of the URL.
    A script must do the redirection if there is something in the fragment.
    Usually this function is only for internal use.
*)
let change_url
(*VVV is it safe to have absolute URLs? do we accept non absolute paths? *)
    ?absolute ?absolute_path ?https
    ~sp ~service
    ?hostname ?port ?fragment ?keep_nl_params ?nl_params ?keep_get_na_params
    g p =
(*VVV only for GET services? *)
  let uri =
    (match create_request_
       ?absolute ?absolute_path ?https
       ~sp ~service
       ?hostname ?port ?fragment ?keep_nl_params ?nl_params ?keep_get_na_params
       g p
     with
       | Ocsigen_lib.Left uri -> uri
       | Ocsigen_lib.Right (uri, p) -> uri)
  in
  current_fragment := url_fragment_prefix_with_sharp^uri; 
  JSOO.eval "window.location" >>> 
  JSOO.set "hash" (JSOO.inject (JSOO.String (url_fragment_prefix^uri)))


let container_node = 
  Eliom_obrowser_client.unwrap_node 
    (Obj.obj (JSOO.eval "container_node" >>> JSOO.as_block))

let set_inner_html code s =
  if code <> 200
  then Lwt.fail (Failed_service code)
  else begin
    let (ref_tree_list, (((timeofday, _), _) as global_data), content) = 
      Marshal.from_string (Ocsigen_lib.urldecode_string s) 0 
    in
(*VVV change only the content of the container, not the full body! *)
    let container = container_node in
    container >>> JSOO.set "innerHTML" (JSOO.string content);
    Eliom_obrowser_client.relink_dom_list 
      timeofday (Js.Node.children container) ref_tree_list;
    Eliom_obrowser_client.fill_global_data_table global_data;
    Lwt.return ()
  end


let change_page
    ?absolute ?absolute_path ?https
    ~sp ~service
    ?hostname ?port ?fragment ?keep_nl_params
    ?(nl_params=Eliom_parameters.empty_nl_params_set) ?keep_get_na_params
    g p =
  (match create_request_
     ?absolute ?absolute_path ?https
     ~sp ~service
     ?hostname ?port ?fragment ?keep_nl_params
     ~nl_params:(Eliom_parameters.add_nl_parameter
                   nl_params
                   Eliom_parameters.eliom_appl_flag
                   true)
     ?keep_get_na_params
     g p
   with
     | Ocsigen_lib.Left uri -> 
         Lwt_obrowser.http_get uri [] >>= fun r ->
         Lwt.return (r, uri)
     | Ocsigen_lib.Right (uri, p) -> Lwt_obrowser.http_post uri p >>= fun r ->
         Lwt.return (r, uri))
  >>= fun ((code, s), uri) ->
  set_inner_html code s >>= fun () ->
(*VVV The URL is created twice ... 
  Once with eliom_appl_flag (for the request), 
  and once without it (we do not want it to appear in the URL).
  How to avoid this?
*)
  change_url
    ?absolute ?absolute_path ?https
    ~sp ~service
    ?hostname ?port ?fragment ?keep_nl_params ~nl_params ?keep_get_na_params
    g p;
(*VVV change the URL only if it is different? *)
  Lwt.return ()




(*****************************************************************************)
(* Make the back button work when only the fragment has changed ... *)
(*VVV We check the fragment every t second ... :-( *)

let write_fragment s =
  Ocsigen_lib.window >>> JSOO.get "location" >>> JSOO.set "hash" s

let read_fragment () =
  Ocsigen_lib.window >>> JSOO.get "location"
                     >>> JSOO.get "hash"
                     >>> JSOO.as_string


let (fragment, set_fragment_signal) = React.S.create (read_fragment ())

let rec fragment_polling () =
  Lwt_obrowser.sleep 0.2 >>= fun () ->
  let new_fragment = read_fragment () in
  if new_fragment <> (React.S.value fragment)
  then set_fragment_signal new_fragment;
  fragment_polling ()

let _ = fragment_polling ()


let eliom_appl_flag =
  Eliom_parameters.string_of_nl_params_set
    (Eliom_parameters.add_nl_parameter
       Eliom_parameters.empty_nl_params_set
       Eliom_parameters.eliom_appl_flag
       true)

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
           then String.concat "&" [uri; eliom_appl_flag]
           else String.concat "?" [uri; eliom_appl_flag]
         in
         Lwt_obrowser.http_get uri [] >>= fun (code, s) ->
         set_inner_html code s
         )
       else Lwt.return ()
     else Lwt.return ())

let _ = React.E.map auto_change_page (React.S.changes fragment)

