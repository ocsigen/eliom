(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (c) CNRS Univ Paris Diderot
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


(**/**)


(** Server representation of client values.
    Developer-visible functions should always operate on
    {% <<a_api subproject="server" | type Eliom_client_value.t >> %}.
*)
module Client_value_server_repr : sig
  type +'a t
  (** instance_id is zero for local client values, unique for global
      client values *)
  val create:
    ?loc:Eliom_lib_base.pos ->
    instance_id:int ->
    unwrapper:Eliom_wrap.unwrapper -> _ t
  val instance_id: _ t -> int
  val loc : _ t -> Eliom_lib_base.pos option
  val clear_loc : _ t -> unit
  val to_poly : _ t -> Ocsigen_lib.poly t
end

(** The representation of escaped values (values injected into client
    values) is opaque. *)
type escaped_value = Ocsigen_lib.poly

module RawXML : sig

  type separator = Space | Comma

  val separator_to_string : separator -> string

  type cookie_info = (bool * string list) deriving (Json)

  type caml_event_handler =
    | CE_registered_closure of
        string * Ocsigen_lib.poly (* 'a Js.t -> unit) client_value *)
    | CE_client_closure of
        (Dom_html.event Js.t -> unit) (* Client side-only *)
    | CE_client_closure_mouse of
        (Dom_html.mouseEvent Js.t -> unit) (* Client side-only *)
    | CE_client_closure_keyboard of
        (Dom_html.keyboardEvent Js.t -> unit) (* Client side-only *)
    | CE_call_service of
        ( [ `A | `Form_get | `Form_post] *
          (cookie_info option) *
          string option *
          Ocsigen_lib.poly (* (unit -> bool) client_value *)
        ) option Eliom_lazy.request

  type internal_event_handler =
    | Raw of string
    | Caml of caml_event_handler

  type uri = string Eliom_lazy.request
  val string_of_uri : uri -> string
  val uri_of_string : string -> uri
  val uri_of_fun : (unit -> string) -> uri

  val internal_event_handler_of_service :
    (  [ `A | `Form_get | `Form_post]
       * cookie_info option
       * string option
       * Eliom_lib.poly
    )  option Eliom_lazy.request
    -> internal_event_handler

  val ce_registered_closure_class : string
  val ce_registered_attr_class : string
  val ce_call_service_class : string
  val process_node_class : string
  val request_node_class : string

  val ce_call_service_attrib : string
  val ce_template_attrib : string
  val node_id_attrib : string

  val closure_attr_prefix : string
  val closure_name_prefix : string

  val client_attr_prefix : string
  val client_name_prefix : string

  type aname = string
  type acontent =
    | AFloat of float
    | AInt of int
    | AStr of string
    | AStrL of separator * string list

  type racontent =
    | RA of acontent
    | RAReact of acontent option React.signal
    | RACamlEventHandler of caml_event_handler
    | RALazyStr of string Eliom_lazy.request
    | RALazyStrL of separator * string Eliom_lazy.request list
    | RAClient of string * attrib option * Ocsigen_lib.poly
    (* attrib Eliom_client_value.t *)
  and attrib = aname * racontent

  val aname : attrib -> aname
  val acontent : attrib -> acontent
  val racontent : attrib -> racontent

  val react_float_attrib : aname -> float React.signal-> attrib
  val react_int_attrib : aname -> int React.signal -> attrib
  val react_string_attrib : aname -> string React.signal-> attrib
  val react_space_sep_attrib : aname -> string list React.signal-> attrib
  val react_comma_sep_attrib : aname -> string list React.signal-> attrib
  val react_poly_attrib : aname -> string -> bool React.signal -> attrib

  val float_attrib : aname -> float -> attrib
  val int_attrib : aname -> int -> attrib
  val string_attrib : aname -> string -> attrib
  val space_sep_attrib : aname -> string list -> attrib
  val comma_sep_attrib : aname -> string list -> attrib
  val internal_event_handler_attrib : aname -> internal_event_handler -> attrib
  val uri_attrib : aname -> string Eliom_lazy.request -> attrib
  val uris_attrib : aname -> string Eliom_lazy.request list -> attrib

  type ename = string
  type node_id =
    | NoId
    | ProcessId of string
    | RequestId of string

  module ClosureMap : Map.S with type key = string (* crypto *)

  type event_handler_table =
    Ocsigen_lib.poly (* (biggest_event Js.t -> unit) client_value*) ClosureMap.t

  type client_attrib_table = Ocsigen_lib.poly (* attrib client_value *) ClosureMap.t

  val filter_class_attribs : node_id -> (string * racontent) list -> (string * racontent) list
end

val tyxml_unwrap_id_int : int
val client_value_unwrap_id_int : int

(** Data for initializing one client value *)
type client_value_datum = {
  closure_id : string;
  args : Ocsigen_lib.poly;
  value : Ocsigen_lib.poly Client_value_server_repr.t
}

(** Data for initializing one injection *)
type injection_datum = {
  injection_dbg : (Eliom_lib_base.pos * string option) option;
  injection_id : int;
  injection_value : Ocsigen_lib.poly;
}

(** Data for initializing client values and injections of one compilation unit *)
type compilation_unit_global_data = {
  server_sections_data : client_value_datum array array;
  client_sections_data : injection_datum array array;
}

(** Data for initializing client values and injection of the client
    program. Sent with the response to the initial request of a client
    process. *)
type global_data = compilation_unit_global_data Eliom_lib.String_map.t

(** Data for initializing client values sent with a request. Sent with
    the response to any follow-up request of a client process. *)
type request_data = client_value_datum array

val global_data_unwrap_id_int : int

type 'a eliom_caml_service_data = {
  ecs_request_data: request_data;
  ecs_data: 'a;
}

(* the data sent on channels *)
type 'a eliom_comet_data_type = 'a Eliom_wrap.wrapped_value
