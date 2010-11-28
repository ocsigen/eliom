(* Ocsigen
 * http://www.ocsigen.org
 * Module eliom_sessions.ml
 * Copyright (C) 2009 Vincent Balat
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


let ssl_ = match Url.Current.get () with
  | Some (Url.Https _) -> true
  | Some (Url.Http _) | Some (Url.File _) | None -> false

let get_csp_ssl () = ssl_
let get_csp_ssl_sp = get_csp_ssl

let host_ = Url.Current.host

let get_csp_hostname () = host_
let get_csp_hostname_sp = get_csp_hostname

let port_ = match Url.Current.port with
  | Some p -> p
  | None -> if ssl_ then 443 else 80

let get_csp_server_port () = port_
let get_csp_server_port_sp = get_csp_server_port

let full_path_ =
  match Url.Current.path with
    | ""::l -> l
    | l -> l
  

let get_csp_original_full_path () = full_path_
let get_csp_original_full_path_sp = get_csp_original_full_path
