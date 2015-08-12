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

include module type of Ocsigen_lib_base
  with type poly = Ocsigen_lib.poly
  and type yesnomaybe = Ocsigen_lib_base.yesnomaybe
  and type ('a, 'b) leftright = ('a, 'b) Ocsigen_lib_base.leftright
  and type 'a Clist.t = 'a Ocsigen_lib_base.Clist.t
  and type 'a Clist.node = 'a Ocsigen_lib_base.Clist.node

include module type of Eliom_lib_base
  with type 'a Int64_map.t = 'a Eliom_lib_base.Int64_map.t
  with type 'a String_map.t = 'a Eliom_lib_base.String_map.t
  with type 'a Int_map.t = 'a Eliom_lib_base.Int_map.t
  with type client_value_datum = Eliom_lib_base.client_value_datum
  with type 'a injection_datum := 'a Eliom_lib_base.injection_datum
  with type 'a compilation_unit_global_data = 'a Eliom_lib_base.compilation_unit_global_data
  with type 'a global_data := 'a Eliom_lib_base.global_data
  with type request_data = Eliom_lib_base.request_data

(** See {% <<a_api subproject="client"|type
    Eliom_pervasives.client_value>> %}. *)
type 'a client_value = 'a
type 'a shared_value = 'a

val create_shared_value : 'a -> 'a client_value -> 'a shared_value

module Shared : SHARED
  with type +'a t := 'a shared_value
   and type +'a c := 'a client_value

exception Eliom_Internal_Error of string

(** This exception is raised (in Lwt) on the client if a call to a
    server function {% <<a_api subproject="server"|val
    Eliom_pervasives.server_function>> %} fails (in Lwt) on the server
    side.

    The argument describes the original exception by
    {!Printexc.to_string}.
*)
exception Exception_on_server of string

type file_info = File.file Js.t

val to_json : ?typ:'a -> 'b -> string
val of_json : ?typ:'a -> string -> 'b

(** Event handlers like {% <<a_api | Eliom_content.Html5.F.a_onclick
    >> %} may raise [False] to cancel the event (as if the JavaScript
    function returned [false]).
*)
(* Cannot re-export exception Eliom_lib.False,
   cf. http://caml.inria.fr/mantis/view.php?id=5778 *)
(* (\** See {% <<a_api subproject="client"|exception Eliom_lib.False>> %}. *\) *)
exception False

module Url : sig
  include module type of Url_base (* From ocsigenserver *)
  include module type of Url (* From js_of_ocaml *)
  val decode : string -> string
  val encode : ?plus:bool -> string -> string
  val make_encoded_parameters : (string * string) list -> string
  val split_path : string -> string list
  val get_ssl : string -> bool option
end

module String : sig
  include module type of String_base
  val remove_eols : string -> string
end

module Lwt_log : sig
  include module type of Lwt_log_js
  with type level = Lwt_log_core.level
   and type logger = Lwt_log_core.logger
   and type section = Lwt_log_core.section
   and type template = Lwt_log_core.template
   and module Section = Lwt_log_core.Section
  val raise_error : ?inspect: 'v -> ?exn : exn -> ?section : section -> ?location : (string * int * int) -> ?logger : logger -> string -> 'a
  val raise_error_f : ?inspect: 'v -> ?exn : exn ->  ?section : section -> ?location : (string * int * int) -> ?logger : logger -> ('a, unit, string, 'any) format4 -> 'a
  val eliom : section
end

(** Deprecated. Use Lwt_log.ign_raise_error_f instead *)
val error : ('a, unit, string, 'b) format4 -> 'a
(** Deprecated. Use Lwt_log.ign_raise_error_f (with ~inspect argument) instead *)
val error_any : _ -> ('a, unit, string, 'b) format4 -> 'a
(** Deprecated. Use Lwt_log.ign_info_f instead *)
val debug : ('a, unit, string, unit) format4 -> 'a
(** Deprecated. Use Lwt_log.ign_info_f instead *)
val debug_exn : ('a, unit, string, unit) format4 -> exn -> 'a
(** Deprecated. Use Lwt_log.ign_info (with ~inspect argument) instead *)
val jsdebug : 'a -> unit

val alert : ('a, unit, string, unit) format4 -> 'a
val jsalert : Js.js_string Js.t -> unit
val debug_var : string -> 'a -> unit
val trace : ('a, unit, string, unit) format4 -> 'a
val lwt_ignore : ?message:string -> unit Lwt.t -> unit

val encode_form_value : 'a -> string
val unmarshal_js : Js.js_string Js.t -> 'a

val encode_header_value : 'a -> string

(**/**)

type injection_datum = poly Eliom_lib_base.injection_datum
type global_data = unit (* Global data only needed while unwrapping *)
