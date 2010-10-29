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

let get_ssl ~sp = ssl_

let host_ = Url.Current.host

let get_hostname ~sp = host_

let port_ = match Url.Current.port with
  | Some p -> p
  | None -> if ssl_ then 443 else 80

let get_server_port ~sp = port_

let full_path_ = Url.Current.path

let get_original_full_path ~sp = full_path_
