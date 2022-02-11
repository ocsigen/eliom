(* Ocsigen://www.ocsigen.org
 * Module eliom_client_types.ml
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

open Eliom_lib
include Eliom_types_base

(* let wrap_parameters (v : 'a) : client_expr_parameters = *)
(* let v = Obj.repr v in *)
(* if Obj.is_int v *)
(* then PImm (Obj.obj v) *)
(* else *)
(* let cri = Eliom_request_info.get_sp_client_request_info () in *)
(* let id = List.length cri.Eliom_common.cri_page_data in *)
(* cri.Eliom_common.cri_page_data <- *)
(* (Obj.obj v) :: cri.Eliom_common.cri_page_data; *)
(* PRef id *)

(* server to client encoding of eliom data *)
(* the string is urlencoded because otherwise js does strange things
   with strings ... *)
let encode_eliom_data r =
  Url.encode ~plus:false (Marshal.to_string (Eliom_wrap.wrap r) [])

(*
let string_map f s =
  let r = ref [] in
  for i = String.length s - 1 downto 0 do
    r := f s.[i] :: !r;
  done;
  !r
*)
