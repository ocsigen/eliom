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

let call_service ?https ~sp ~service ?fragment ?keep_nl_params ?nl_params g p =
  (match Eliom_services.get_get_or_post service with
     | `Get ->
         let uri =
           Eliom_mkforms.make_string_uri
             ?https ~sp ~service ?fragment ?keep_nl_params ?nl_params g
         in
         Lwt_obrowser.http_get uri []
     | `Post ->
         let path, g, fragment, p =
           Eliom_mkforms.make_post_uri_components ~sp ~service g p
         in
         let uri = 
           Eliom_mkforms.make_string_uri_from_components (path, g, fragment) 
         in
         Lwt_obrowser.http_post uri p)
  >>= fun (code, s) ->
  if code = 200
  then Lwt.return s
  else Lwt.fail (Failed_service code)
