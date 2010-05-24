(* Ocsigen
 * http://www.ocsigen.org
 * Module eliom_client.ml
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

exception Failed_service of int

val call_service :
  ?absolute:bool ->
  ?absolute_path:bool ->
  ?https:bool ->
  service:('a, 'b,
           [< Eliom_services.service_kind ],
           [< `WithSuffix | `WithoutSuffix ], 'd, 'e, 
           [< Eliom_services.registrable ], 'return)
          Eliom_services.service ->
  sp:Eliom_client_types.server_params ->
  ?hostname:string ->
  ?port:int ->
  ?fragment:string ->
  ?keep_nl_params:[ `All | `None | `Persistent ] ->
  ?nl_params:(string * string) list Ocsigen_lib.String_Table.t ->
  ?keep_get_na_params:bool -> 'a -> 'b -> string Lwt.t

val call_caml_service :
  ?absolute:bool ->
  ?absolute_path:bool ->
  ?https:bool ->
  service:('a, 'b,
           [< Eliom_services.service_kind ],
           [< `WithSuffix | `WithoutSuffix ], 'd, 'e, 
           [< Eliom_services.registrable ], 'return Eliom_parameters.caml)
          Eliom_services.service ->
  sp:Eliom_client_types.server_params ->
  ?hostname:string ->
  ?port:int ->
  ?fragment:string ->
  ?keep_nl_params:[ `All | `None | `Persistent ] ->
  ?nl_params:(string * string) list Ocsigen_lib.String_Table.t ->
  ?keep_get_na_params:bool -> 'a -> 'b -> 'return Lwt.t

val exit_to :
  ?absolute:bool ->
  ?absolute_path:bool ->
  ?https:bool ->
  service:('a, 'b,
           [< Eliom_services.service_kind ],
           [< `WithSuffix | `WithoutSuffix ], 'd, 'e, 
           [< Eliom_services.registrable ], 'return)
          Eliom_services.service ->
  sp:Eliom_client_types.server_params ->
  ?hostname:string ->
  ?port:int ->
  ?fragment:string ->
  ?keep_nl_params:[ `All | `None | `Persistent ] ->
  ?nl_params:(string * string) list Ocsigen_lib.String_Table.t ->
  ?keep_get_na_params:bool -> 'a -> 'b -> unit

val change_url :
  ?absolute:bool ->
  ?absolute_path:bool ->
  ?https:bool ->
  service:('a, 'b,
           [< Eliom_services.service_kind ],
           [< `WithSuffix | `WithoutSuffix ], 'd, 'e, 
           [< Eliom_services.registrable ], 'return)
          Eliom_services.service ->
  sp:Eliom_client_types.server_params ->
  ?hostname:string ->
  ?port:int ->
  ?fragment:string ->
  ?keep_nl_params:[ `All | `None | `Persistent ] ->
  ?nl_params:(string * string) list Ocsigen_lib.String_Table.t ->
  ?keep_get_na_params:bool -> 'a -> 'b -> unit

val change_page :
  ?absolute:bool ->
  ?absolute_path:bool ->
  ?https:bool ->
  service:('a, 'b,
           [< Eliom_services.service_kind ],
           [< `WithSuffix | `WithoutSuffix ], 'd, 'e, 
           [< Eliom_services.registrable ], 'return)
          Eliom_services.service ->
  sp:Eliom_client_types.server_params ->
  ?hostname:string ->
  ?port:int ->
  ?fragment:string ->
  ?keep_nl_params:[ `All | `None | `Persistent ] ->
  ?nl_params:(string * string) list Ocsigen_lib.String_Table.t ->
  ?keep_get_na_params:bool -> 'a -> 'b -> unit Lwt.t

val get_subpage :
  ?absolute:bool ->
  ?absolute_path:bool ->
  ?https:bool ->
  service:('a, 'b,
           [< Eliom_services.service_kind ],
           [< `WithSuffix | `WithoutSuffix ], 'd, 'e, 
           [< Eliom_services.registrable ], 'return)
          Eliom_services.service ->
  sp:Eliom_client_types.server_params ->
  ?hostname:string ->
  ?port:int ->
  ?fragment:string ->
  ?keep_nl_params:[ `All | `None | `Persistent ] ->
  ?nl_params:(string * string) list Ocsigen_lib.String_Table.t ->
  ?keep_get_na_params:bool -> 'a -> 'b -> 
  [< `PCDATA | XHTML.M.flow ] XHTML.M.elt list Lwt.t


(**/**)

val make_a_with_onclick :
  (?a:'a -> ?onclick:XML.event -> 'c -> 'd) ->
  ('d -> string -> (unit -> unit Lwt.t) -> unit -> 'f) ->
  ?absolute:bool ->
  ?absolute_path:bool ->
  ?https:bool ->
  ?a:'a ->
  service:('get, unit, [< Eliom_services.get_service_kind ],
           [< Eliom_services.suff ], 'gn, 'pn,
           [< Eliom_services.registrable ], 'return)
    Eliom_services.service ->
  sp:Eliom_sessions.server_params ->
  ?hostname:string ->
  ?port:int ->
  ?fragment:string ->
  ?keep_nl_params:[ `All | `None | `Persistent ] ->
  ?nl_params:Eliom_parameters.nl_params_set ->
  'c -> 'get -> 'd
