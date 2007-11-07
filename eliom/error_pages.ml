(* Ocsigen
 * Copyright (C) 2005 Vincent Balat
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

open XHTML.M

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

let page_bad_param l = 
  let s = "Wrong parameters" in
  html
    (head (title (pcdata s)) [])
    (body
       [h1 [pcdata s];
        if l = [] 
        then
          p [pcdata "(no POST parameters)."]
        else
          p ((pcdata "(Post parameters are: ")::
             (List.fold_right (fun n b -> (em [pcdata n])::(pcdata ", ")::b) 
                l [pcdata ")."]))]
    )

let page_session_expired  = 
  let s = "Session expired" in
  html
    (head (title (pcdata s)) [])
    (body
       [h1 [pcdata s]]
    )

