(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) CNRS Univ Paris Diderot
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

open Eliom_client_core

let register_client_closure ~closure_id closure =
  Client_closure.register ~closure_id ~closure

let open_client_section compilation_unit_id =
  do_next_client_section_data ~compilation_unit_id

let close_server_section compilation_unit_id =
  do_next_server_section_data ~compilation_unit_id

let get_injection ?ident ?pos name = Injection.get ?ident ?pos ~name

let get_escaped_value = Eliom_lib.from_poly
