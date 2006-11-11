(* Ocsigen
 * Copyright (C) 2005 Vincent Balat
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
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

let page_bad_param        = << <html><body><h1>Wrong parameters</h1></body></html> >>

let page_session_expired  = << <html><body><h1>Session expired</h1></body></html> >>

let error_page s =
  << <html>
       <body>
          <h1> Error </h1>
          <p>$str:s$</p>
        </body>
      </html>
  >>

