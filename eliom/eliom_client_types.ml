(* Ocsigen
 * http://www.ocsigen.org
 * Module eliom_client_types.ml
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


(* Some types are different on client side: *)

type sitedata =
  {site_dir: Ocsigen_lib.url_path;
   site_dir_string: string;
  }


type server_params =
    {
     sp_si: Eliom_common.sess_info;
     sp_sitedata: sitedata (* data for the whole site *);
(*     sp_cookie_info: tables cookie_info; *)
     sp_suffix: Ocsigen_lib.url_path option (* suffix *);
     sp_fullsessname: string option (* the name of the session
                                       to which belong the service
                                       that answered
                                       (if it is a session service) *)}


type 'a data_key = int * int

let to_data_key_ v = v
let of_data_key_ v = v

let string_map f s =
  let r = ref [] in
  for i = String.length s - 1 downto 0 do
    r := f s.[i] :: !r;
  done;
  !r

let jsmarshal v =
  let s = Marshal.to_string v [] in
  let s' = string_map (fun c -> Printf.sprintf "0x%02X" (Char.code c)) s in
  Printf.sprintf "[%s]" (String.concat "," s')

(* For client side program, we sometimes simulate links and forms
   with client side functions.
   Here are there identifiers: *)
let a_closure_id = 0x0
let a_closure_id_string = Printf.sprintf "0x%X" a_closure_id
