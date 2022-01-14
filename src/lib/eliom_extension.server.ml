(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2008 Vincent Balat
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

type eliom_extension_sig =
  unit -> Ocsigen_extensions.answer Lwt.t

let module_action : eliom_extension_sig ref =
  ref (fun _ -> failwith "Eliommod_extension")


let register_eliom_extension f =
  module_action := f

let get_eliom_extension () = !module_action


let run_eliom_extension (fext : eliom_extension_sig) _now info sitedata  =
  let sp = Eliom_common.make_server_params sitedata info None None in
  Lwt.with_value Eliom_common.sp_key (Some sp) fext
