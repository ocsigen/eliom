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

let make_wrapper f :'a wrapper = ()
let empty_wrapper () :'a wrapper = ()

type toucher = unit

let make_toucher f : toucher = ()

type unwrap_id = Eliom_client_unwrap.unwrap_id
let react_up_unwrap_id : unwrap_id = Eliom_client_unwrap.id_of_int react_up_unwrap_id_int
let react_down_unwrap_id : unwrap_id = Eliom_client_unwrap.id_of_int react_down_unwrap_id_int
let comet_channel_unwrap_id : unwrap_id = Eliom_client_unwrap.id_of_int comet_channel_unwrap_id_int
let bus_unwrap_id : unwrap_id = Eliom_client_unwrap.id_of_int bus_unwrap_id_int
let node_unwrap_id : unwrap_id = Eliom_client_unwrap.id_of_int node_unwrap_id_int
