(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2011 Grégoire Henry
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

open Ocsigen_lib_base

exception Eliom_Internal_Error of string

(** Module with Lwt operators: Open to use them without polluting your scope. *)
module Lwt_ops : sig
  val ( >>= ) : 'a Lwt.t -> ('a -> 'b Lwt.t) -> 'b Lwt.t
  val ( =<< ) : ('a -> 'b Lwt.t) -> 'a Lwt.t -> 'b Lwt.t
  val ( >|= ) : 'a Lwt.t -> ('a -> 'b) -> 'b Lwt.t
  val ( =|< ) : ('a -> 'b) -> 'a Lwt.t -> 'b Lwt.t
end

val escape_quotes : string -> string

(**/**)

val fresh_ix : unit -> int
val get_option : 'a option -> 'a

module RawXML : sig

  type separator = Space | Comma

  val separator_to_string : separator -> string

  type cookie_info = (bool * string list) deriving (Json)

  type -'a caml_event_handler =
    | CE_registered_closure of string * ((#Dom_html.event as 'a) Js.t -> unit) Eliom_server.Client_value.t
    | CE_client_closure of ('a Js.t -> unit) (* Client side-only *)
    | CE_call_service of
        ([ `A | `Form_get | `Form_post] * (cookie_info option) * string option) option Eliom_lazy.request

  type event_handler =
    | Raw of string
    | Caml of Dom_html.event caml_event_handler

  type uri = string Eliom_lazy.request
  val string_of_uri : uri -> string
  val uri_of_string : string -> uri
  val uri_of_fun : (unit -> string) -> uri

  val event_handler_of_string : string -> event_handler
  val string_of_event_handler : event_handler -> string
  val event_handler_of_service :
    ([ `A | `Form_get | `Form_post] * (cookie_info option) * string option) option Eliom_lazy.request ->
      event_handler

  (* Deprecated alias. *)
  val event_of_service :
           ([ `A | `Form_get | `Form_post ] * cookie_info option *
            string option)
           option Eliom_lazy.request -> event_handler
  val event_of_string : string -> event_handler
  val string_of_handler : event_handler -> string

  val ce_registered_closure_class : string
  val ce_call_service_class : string
  val process_node_class : string
  val request_node_class : string

  val ce_call_service_attrib : string
  val ce_template_attrib : string
  val node_id_attrib : string

  val closure_attr_prefix : string
  val closure_attr_prefix_len : int

  type aname = string
  type acontent =
    | AFloat of float
    | AInt of int
    | AStr of string
    | AStrL of separator * string list
  type racontent =
    | RA of acontent
    | RACamlEventHandler of Dom_html.event caml_event_handler
    | RALazyStr of string Eliom_lazy.request
    | RALazyStrL of separator * string Eliom_lazy.request list
  type attrib = aname * racontent
  val aname : attrib -> aname
  val acontent : attrib -> acontent
  val racontent : attrib -> racontent

  val float_attrib : aname -> float -> attrib
  val int_attrib : aname -> int -> attrib
  val string_attrib : aname -> string -> attrib
  val space_sep_attrib : aname -> string list -> attrib
  val comma_sep_attrib : aname -> string list -> attrib
  val event_handler_attrib : aname -> event_handler -> attrib
  val uri_attrib : aname -> string Eliom_lazy.request -> attrib
  val uris_attrib : aname -> string Eliom_lazy.request list -> attrib

  (* Deprecated alias. *)
  val event_attrib : aname -> event_handler -> attrib

  type ename = string
  type node_id =
    | NoId
    | ProcessId of string
    | RequestId of string

  module ClosureMap : Map.S with type key = string (* crypto *)

  type event_handler_table =
    ((Dom_html.event Js.t -> unit) Eliom_server.Client_value.t) ClosureMap.t

end

val tyxml_unwrap_id_int : int
val client_value_unwrap_id_int : int

module Int64_map : sig
  include Map.S with type key = int64
  val from_list : (key * 'a) list -> 'a t
end
module Int_map : sig
  include Map.S with type key = int
  val from_list : (key * 'a) list -> 'a t
end
module String_map : sig
  include Map.S with type key = string
  val from_list : (key * 'a) list -> 'a t
end

(**/**)

