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

val create_request_ :
  ?absolute:bool ->
  ?absolute_path:bool ->
  ?https:bool ->
  sp:Eliom_client_types.server_params ->
  service:('a, 'b,
           [ `Attached of
               ([> `External ], [ `Get | `Post ]) Eliom_services.a_s
           | `Nonattached of 'c Eliom_services.na_s ],
           [< `WithSuffix | `WithoutSuffix ], 'd, 'e, 'f, 'return)
          Eliom_services.service ->
  ?hostname:string ->
  ?port:int ->
  ?fragment:string ->
  ?keep_nl_params:[ `All | `None | `Persistent ] ->
  ?nl_params:(string * string) list Ocsigen_lib.String_Table.t ->
  ?keep_get_na_params:bool ->
  'a -> 'b -> (string, string * (string * string) list) Ocsigen_lib.leftright

val call_service :
  ?absolute:bool ->
  ?absolute_path:bool ->
  ?https:bool ->
  sp:Eliom_client_types.server_params ->
  service:('a, 'b,
           [ `Attached of
               ([> `External ], [ `Get | `Post ]) Eliom_services.a_s
           | `Nonattached of 'c Eliom_services.na_s ],
           [< `WithSuffix | `WithoutSuffix ], 'd, 'e, 'f, 'return)
          Eliom_services.service ->
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
  sp:Eliom_client_types.server_params ->
  service:('a, 'b,
           [ `Attached of
               ([> `External ], [ `Get | `Post ]) Eliom_services.a_s
           | `Nonattached of 'c Eliom_services.na_s ],
           [< `WithSuffix | `WithoutSuffix ], 'd, 'e, 'f, 'return Eliom_parameters.caml)
          Eliom_services.service ->
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
  sp:Eliom_client_types.server_params ->
  service:('a, 'b,
           [ `Attached of
               ([> `External ], [ `Get | `Post ]) Eliom_services.a_s
           | `Nonattached of 'c Eliom_services.na_s ],
           [< `WithSuffix | `WithoutSuffix ], 'd, 'e, 'f,
           Eliom_services.http)
          Eliom_services.service ->
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
  sp:Eliom_client_types.server_params ->
  service:('a, 'b,
           [ `Attached of
               ([> `External ], [ `Get | `Post ]) Eliom_services.a_s
           | `Nonattached of 'c Eliom_services.na_s ],
           [< `WithSuffix | `WithoutSuffix ], 'd, 'e, 'f, 'return)
          Eliom_services.service ->
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
  sp:Eliom_client_types.server_params ->
  service:('a, 'b,
           [ `Attached of
               ([> `External ], [ `Get | `Post ]) Eliom_services.a_s
           | `Nonattached of 'c Eliom_services.na_s ],
           [< `WithSuffix | `WithoutSuffix ], 'd, 'e, 'f,
           Eliom_services.appl_service)
          Eliom_services.service ->
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
  sp:Eliom_client_types.server_params ->
  service:('a, 'b,
           [ `Attached of
               ([> `External ], [ `Get | `Post ]) Eliom_services.a_s
           | `Nonattached of 'c Eliom_services.na_s ],
           [< `WithSuffix | `WithoutSuffix ], 'd, 'e, 'f,
           Eliom_services.appl_service)
          Eliom_services.service ->
  ?hostname:string ->
  ?port:int ->
  ?fragment:string ->
  ?keep_nl_params:[ `All | `None | `Persistent ] ->
  ?nl_params:(string * string) list Ocsigen_lib.String_Table.t ->
  ?keep_get_na_params:bool -> 'a -> 'b -> 
  [< `PCDATA | XHTML.M.flow ] XHTML.M.elt list Lwt.t
