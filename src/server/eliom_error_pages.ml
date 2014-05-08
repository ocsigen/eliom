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
open Html5.F

let page_error_param_type l =
  let s = match l with
    [] -> [pcdata "Wrong type for parameter"]
  | [(n,_)] -> [pcdata "Wrong type for parameter ";em [pcdata n];pcdata "."]
  | (n,_)::ll ->
      (pcdata "Wrong type for parameters ")::
      (List.fold_left (fun deb (n,_) -> (em [pcdata n])::(pcdata ", ")::deb)
         [em [pcdata n];pcdata "."] ll)
  in
  html
    (head (title (pcdata "")) [])
    (body
       [h1 s]
    )

let page_bad_param after_action gl pl =
  let s = "Wrong parameters" in
  html
    (head (title (pcdata s)) [])
    (body
       ((h1 [pcdata s])::
          (if Ocsigen_config.get_debugmode ()
           then
             [h2 [pcdata "Debugging information:"];
              (if after_action
               then
                 (p [pcdata "An action occurred successfully. But Eliom was unable to find the service for displaying the page."])
               else
                 (p [pcdata "Eliom was unable to find a service matching these parameters."]));
              (match gl with
                 | [] -> p [pcdata "No GET parameters have been given to services."]
                 | (n, a)::l ->
                     p ((pcdata "GET parameters given to services: ")::
                          [em
                             ((pcdata n)::(pcdata "=")::(pcdata a)::
                                (List.fold_right
                                   (fun (n, a) b ->
                                      (pcdata "&")::
                                        (pcdata n)::(pcdata "=")::(pcdata a)::b)
                                   l [pcdata "."]))]));
              (match pl with
                 | [] -> p [pcdata "No POST parameters have been given to services."]
                 | a::l ->
                     p ((pcdata "Names of POST parameters given to services: ")::
                          (em [pcdata a])::
                          (List.fold_right
                             (fun n b -> (pcdata ", ")::(em [pcdata n])::b)
                             l [pcdata "."])))]
           else [])
       )
    )

let page_session_expired  =
  let s = "Session expired" in
  html
    (head (title (pcdata s)) [])
    (body
       [h1 [pcdata s]]
    )
