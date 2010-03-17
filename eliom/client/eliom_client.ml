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

let call_service
    ?absolute ?absolute_path ?https
    ~sp ~service
    ?hostname ?port ?fragment ?keep_nl_params ?nl_params ?keep_get_na_params
    g p =
  (match Eliom_services.get_get_or_post service with
     | `Get ->
         let uri =
           Eliom_mkforms.make_string_uri
             ?absolute ?absolute_path ?https
             ~sp ~service
             ?hostname ?port ?fragment ?keep_nl_params ?nl_params g
         in
         Lwt_obrowser.http_get uri []
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
         Lwt_obrowser.http_post uri p)
  >>= fun (code, s) ->
  if code = 200
  then Lwt.return s
  else Lwt.fail (Failed_service code)




let exit_to
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
        Js.redirect_get uri
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
        Js.redirect_post uri p





let url_fragment_prefix = "!"

(** This will change the URL, without doing a request.
    As browsers do not not allow to change the URL,
    we write the new URL in the fragment part of the URL,
    a script must do the redirection if there is something in the fragment.
    Usually this function is only fior internal use.
*)
let change_url
(*VVV is it safe to have absolute URLs? do we accept non absolute paths? *)
    ?absolute ?absolute_path ?https
    ~sp ~service
    ?hostname ?port ?fragment ?keep_nl_params ?nl_params g =
(*VVV only for GET services?   ?keep_get_na_params g p = *)
  let uri =
    Eliom_mkforms.make_string_uri
      ?absolute ?absolute_path ?https
      ~sp ~service
      ?hostname ?port ?fragment ?keep_nl_params ?nl_params g
  in
  JSOO.eval "window.location" >>> 
  JSOO.set "hash" (JSOO.inject (JSOO.String (url_fragment_prefix^uri)))
