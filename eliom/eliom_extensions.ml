(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2008 Vincent Balat
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
(*****************************************************************************)
(*****************************************************************************)
(** Run Ocsigen extensions that can access Eliom data                        *)
(*****************************************************************************)
(*****************************************************************************)

let module_action : 
    ('a -> 
       Ocsigen_extensions.answer Lwt.t) ref = 
  ref (fun _ -> failwith "Eliommod_extension")

let run_eliom_extension
    now
    (ri,
     si,
     cookies_to_set,
     all_cookie_info)
    sitedata
    =

  let sp =
    Eliom_common.make_server_params sitedata all_cookie_info ri [] si None
  in
  !module_action sp


let register_eliom_extension f =
  module_action := f

let get_eliom_extension () = !module_action
