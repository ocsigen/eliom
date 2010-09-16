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


let appl_name = 
  lazy 
    (let (_, v, _) =
       (Ocsigen_lib.String_Table.find
          Eliom_common.appl_name_cookie_name
          (Ocsigen_cookies.Cookies.find
             []
             !(Eliommod_client_cookies.cookie_table)))
     in v)

let get_application_name ~sp =
  Ocsigen_lib.debug (Lazy.force appl_name); Some (Lazy.force appl_name)
