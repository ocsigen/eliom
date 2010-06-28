(* Ocsigen
 * http://www.ocsigen.org
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


let (make_a_with_onclick :
       (?a:'a -> ?onclick:string -> 'c -> 'd) ->
      ('d -> string -> ('e -> unit Lwt.t) -> unit -> 'f) ->
      ?absolute:bool ->
      ?absolute_path:bool ->
      ?https:bool ->
      ?a:'a ->
      service:('g, unit, [< Eliom_services.get_service_kind ],
               [< Eliom_services.suff ], 'h, 'i,
               [< Eliom_services.registrable ], 'j)
        Eliom_services.service ->
      sp:Eliom_sessions.server_params ->
      ?hostname:string ->
      ?port:int ->
      ?fragment:string ->
      ?keep_nl_params:[ `All | `None | `Persistent ] ->
      ?nl_params:Eliom_parameters.nl_params_set ->
      'c -> 'g -> 'd) =
  fun
    make_a
    register_event
    ?absolute
    ?absolute_path
    ?https
    ?a
    ~service
    ~sp
    ?hostname
    ?port
    ?fragment
    ?keep_nl_params
    ?nl_params
    content
    getparams ->
  make_a
    ?a
    ?onclick:
    (Some ((fun arg1 arg2 arg3 arg4 arg5 arg6
              arg7 arg8 arg9 arg10 arg11 ->
                "caml_run_from_table ("^
                  Eliom_client_types.a_closure_id_string^","^
                  ((Eliom_client_types.jsmarshal
                      (arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8,
                       arg9, arg10, arg11))
                   ^ ")"))
              (Eliommod_client.wrap ~sp absolute)
              (Eliommod_client.wrap ~sp absolute_path)
              (Eliommod_client.wrap ~sp https)
              (Eliommod_client.wrap ~sp service)
              (Eliommod_client.wrap_sp sp)
              (Eliommod_client.wrap ~sp hostname)
              (Eliommod_client.wrap ~sp port)
              (Eliommod_client.wrap ~sp fragment)
              (Eliommod_client.wrap ~sp keep_nl_params)
              (Eliommod_client.wrap ~sp nl_params)
              (Eliommod_client.wrap ~sp getparams)
     ))
    content

