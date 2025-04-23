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

include Eliom_common_base

type server_params = unit

let get_sp () = ()
let get_sp_option () = Some ()

type 'a wrapper = unit

let make_wrapper _ : 'a wrapper = ()
let empty_wrapper () : 'a wrapper = ()

type unwrap_id = Eliom_unwrap.unwrap_id

let react_up_unwrap_id : unwrap_id =
  Eliom_unwrap.id_of_int react_up_unwrap_id_int

let react_down_unwrap_id : unwrap_id =
  Eliom_unwrap.id_of_int react_down_unwrap_id_int

let signal_down_unwrap_id : unwrap_id =
  Eliom_unwrap.id_of_int signal_down_unwrap_id_int

let comet_channel_unwrap_id : unwrap_id =
  Eliom_unwrap.id_of_int comet_channel_unwrap_id_int

let bus_unwrap_id : unwrap_id = Eliom_unwrap.id_of_int bus_unwrap_id_int

(* On client side, we have sitedata.
   Thus, we can define new services.
   That's why this function returns Some sitedata. *)
let sitedata : Eliom_types.sitedata option ref = ref None

let global_register_allowed () =
  match !sitedata with None -> None | Some s -> Some (fun () -> s)

let get_site_dir sitedata = sitedata.Eliom_types.site_dir
let get_site_dir_string sitedata = sitedata.Eliom_types.site_dir_string
let add_unregistered _ _ = ()

module To_and_of_shared = struct
  type 'a t = 'a to_and_of

  let of_string {of_string; _} = of_string
  let to_string {to_string; _} = to_string
  let to_and_of tao = tao
end

let client_html_file, set_client_html_file =
  let r = ref "eliom.html" in
  ( (fun () -> !r)
  , fun s ->
      assert !is_client_app;
      r := s )

let defer get f =
  let r = ref None in
  (match get () with
  | Some v -> r := Some (f v)
  | None -> raise (Eliom_site_information_not_available "defer"));
  r
