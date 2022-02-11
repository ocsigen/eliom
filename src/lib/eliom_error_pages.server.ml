(* Ocsigen
 * Copyright (C) 2005 Vincent Balat
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

open Eliom_lib
open Eliom_content_core
open Html.F

let page_error_param_type l =
  let s =
    match l with
    | [] -> [txt "Wrong type for parameter"]
    | [(n, _)] -> [txt "Wrong type for parameter "; em [txt n]; txt "."]
    | (n, _) :: ll ->
        txt "Wrong type for parameters "
        :: List.fold_left
             (fun deb (n, _) -> em [txt n] :: txt ", " :: deb)
             [em [txt n]; txt "."]
             ll
  in
  html (head (title (txt "")) []) (body [h1 s])

let page_bad_param after_action gl pl =
  let s = "Wrong parameters" in
  html
    (head (title (txt s)) [])
    (body
       (h1 [txt s]
       ::
       (if Ocsigen_config.get_debugmode ()
       then
         [ h2 [txt "Debugging information:"]
         ; (if after_action
           then
             p
               [ txt
                   "An action occurred successfully. But Eliom was unable to find the service for displaying the page."
               ]
           else
             p
               [ txt
                   "Eliom was unable to find a service matching these parameters."
               ])
         ; (match gl with
           | [] -> p [txt "No GET parameters have been given to services."]
           | (n, a) :: l ->
               p
                 [ txt "GET parameters given to services: "
                 ; em
                     (txt n :: txt "=" :: txt a
                     :: List.fold_right
                          (fun (n, a) b ->
                            txt "&" :: txt n :: txt "=" :: txt a :: b)
                          l [txt "."]) ])
         ; (match pl with
           | [] -> p [txt "No POST parameters have been given to services."]
           | a :: l ->
               p
                 (txt "Names of POST parameters given to services: "
                 :: em [txt a]
                 :: List.fold_right
                      (fun n b -> txt ", " :: em [txt n] :: b)
                      l [txt "."])) ]
       else [])))

let page_session_expired =
  let s = "Session expired" in
  html (head (title (txt s)) []) (body [h1 [txt s]])
