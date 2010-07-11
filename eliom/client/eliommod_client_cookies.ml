(* Ocsigen
 * http://www.ocsigen.org
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


let cookie_table = ref Ocsigen_cookies.Cookies.empty

let update_cookie_table cookieset =
  let now = Js.to_float ((jsnew Js.date_now ())##valueOf ()) in
  Firebug.console##log (3.14);
  Firebug.console##log (now);
  Ocsigen_cookies.Cookies.iter
    (fun path table ->
      Ocsigen_lib.String_Table.iter
        (fun name -> function 
          | Ocsigen_cookies.OSet (Some exp, _, _) when exp <= now ->
            cookie_table := Ocsigen_cookies.remove_cookie path name !cookie_table
          | Ocsigen_cookies.OUnset -> 
            cookie_table := Ocsigen_cookies.remove_cookie path name !cookie_table
          | Ocsigen_cookies.OSet (exp, value, secure) -> 
            cookie_table := 
              Ocsigen_cookies.add_cookie
              path name (exp, value, secure)
              !cookie_table)
        table
    )
    cookieset
