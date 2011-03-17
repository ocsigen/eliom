(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2010 Vincent Balat
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

include Eliom_services_cli

let need_process_cookies s = not (is_external s)
(* If there is a client side process, we do an XHR with tab cookies *)

let appl_content_capable s =
  match s.send_appl_content with
    | XAlways -> true
    | XNever -> false
    | XSame_appl an -> Some an = Eliom_process.get_application_name ()

let xhr_with_cookies s =
  need_process_cookies s && appl_content_capable s
    
