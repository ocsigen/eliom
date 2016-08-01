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

(******************************************************************************)

(* Global data *)

type compilation_unit_global_data2 =
  { mutable server_section :
      Eliom_serial.client_value_datum array list;
    mutable client_section :
      Eliom_serial.injection_datum array list }

let get_global_data, modify_global_data =
  (* We have to classify global data from ocsigen extensions (no site
     available) and eliommodules (site data available).
     Furthermore, the Eliom services must only send global data from
     ocsigen extensions and their own site.  *)
  let global_data = ref Eliom_lib.String_map.empty in
  let site_data = Eliom_reference.Volatile.eref ~scope:Eliom_common.site_scope
      Eliom_lib.String_map.empty
  in
  let is_site_available () =
    (* Matches valid states for Eliom_common.get_site_data *)
    Eliom_common.(get_sp_option () <> None || during_eliom_module_loading ())
  in
  let get () =
    if is_site_available () then
      Eliom_lib.String_map.merge
        (fun compilation_unit_id global site ->
           match global, site with
           | None, None -> assert false
           | Some data, None | None, Some data -> Some data
           | Some _, Some site_data ->
             Eliom_lib.Lwt_log.ign_error_f
               ~section:Eliom_lib.Lwt_log.eliom
               "Compilation unit %s linked globally AND as Eliom module"
               compilation_unit_id;
             Some site_data)
        !global_data
        (Eliom_reference.Volatile.get site_data)
    else
      !global_data
  in
  let modify f =
    if is_site_available () then
      Eliom_reference.Volatile.modify site_data f
    else
      global_data := f !global_data
  in
  get, modify

let current_server_section_data = ref []

let get_compilation_unit_global_data compilation_unit_id =
  if not (Eliom_lib.String_map.mem compilation_unit_id (get_global_data ()))
  then
    ( let data = { server_section = []; client_section = [] } in
      ignore (modify_global_data
                (Eliom_lib.String_map.add compilation_unit_id data)) );
  Eliom_lib.String_map.find compilation_unit_id (get_global_data ())

let close_server_section compilation_unit_id =
  let data = get_compilation_unit_global_data compilation_unit_id in
  data.server_section
  <- Array.of_list (List.rev !current_server_section_data)
     :: data.server_section;
  current_server_section_data := []

let close_client_section compilation_unit_id injection_data =
  let data = get_compilation_unit_global_data compilation_unit_id in
  let injection_datum (injection_id, injection_value, loc) =
    { Eliom_serial.injection_id;
      injection_value ; injection_dbg = Some loc }
  in
  let injection_data = Array.of_list injection_data in
  data.client_section <-
    Array.map injection_datum injection_data :: data.client_section

let get_global_data () =
  Eliom_lib.String_map.map
    (fun {server_section; client_section}->
       { Eliom_serial.server_sections_data
         = Array.of_list (List.rev server_section);
         client_sections_data = Array.of_list (List.rev client_section) })
    (get_global_data ())

(* Request data *)

let request_data
  : Eliom_serial.client_value_datum list
      Eliom_reference.Volatile.eref =
  Eliom_reference.Volatile.eref ~scope:Eliom_common.request_scope []

let get_request_data () =
  Array.of_list (List.rev (Eliom_reference.Volatile.get request_data))

(* Register data *)

let register_client_value_data ~closure_id ~args ~value =
  let client_value_datum = { Eliom_serial.closure_id; args; value }
  in
  if Eliom_common.get_sp_option () = None then
    current_server_section_data :=
      client_value_datum :: !current_server_section_data
  else
    Eliom_reference.Volatile.modify request_data
      (fun sofar -> client_value_datum :: sofar)


(*****************************************************************************)
(* Syntax helpers *)

let escaped_value = Eliom_client_value.escaped_value

let last_id = ref 0

let fragment ?pos closure_id args =
  let instance_id =
    if Eliom_common.get_sp_option () = None then begin
      incr last_id;
      !last_id
    end else
      0
  in
  let value = Eliom_client_value.create_client_value ?loc:pos ~instance_id in
  register_client_value_data
    ~closure_id ~args:(Eliom_lib.to_poly args) ~value;
  Eliom_client_value.client_value_from_server_repr value

let pos pos_fname (lnum1, bol1, cnum1) (lnum2, bol2, cnum2) =
  Lexing.(
    { pos_fname ;
      pos_lnum = lnum1 ;
      pos_bol = bol1 ;
      pos_cnum = cnum1 ;
    },
    { pos_fname;
      pos_lnum = lnum2 ;
      pos_bol = bol2 ;
      pos_cnum = cnum2 ;
    })
