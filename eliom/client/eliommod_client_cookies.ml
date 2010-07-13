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

let now () = Js.to_float (Js.date##now ())

let update_cookie_table cookieset =
  let now = now () in
  Ocsigen_cookies.Cookies.iter
    (fun path table ->
      Firebug.console##log (Js.string (String.concat "/" path));
      Ocsigen_lib.String_Table.iter
        (fun name -> function 
          | Ocsigen_cookies.OSet (Some exp, _, _) when exp <= now ->
            Firebug.console##log (Js.string ("unset"));
            Firebug.console##log (Js.string (name));
            cookie_table := Ocsigen_cookies.remove_cookie path name !cookie_table
          | Ocsigen_cookies.OUnset -> 
            Firebug.console##log (Js.string ("unset"));
            Firebug.console##log (Js.string (name));
            cookie_table := Ocsigen_cookies.remove_cookie path name !cookie_table
          | Ocsigen_cookies.OSet (exp, value, secure) ->
            Firebug.console##log (Js.string ("set"));
            Firebug.console##log (Js.string (name));
            Firebug.console##log (Js.string (value));
            cookie_table := 
              Ocsigen_cookies.add_cookie
              path name (exp, value, secure)
              !cookie_table)
        table
    )
    cookieset


let get_cookies_to_send https path =
  let now = now () in
  Ocsigen_cookies.Cookies.fold
    (fun cpath t cookie_list ->
      if Ocsigen_lib.list_is_prefix_skip_end_slash cpath path
      then Ocsigen_lib.String_Table.fold
        (fun name (exp, value, secure) cookie_list ->
          match exp with
            | Some exp when exp <= now ->
              cookie_table := 
                Ocsigen_cookies.remove_cookie cpath name !cookie_table;
              cookie_list
            | _ ->
              if (not secure) || https
              then (name, value)::cookie_list
              else cookie_list
        )
        t
        cookie_list
      else cookie_list
    )
    !cookie_table
    []
