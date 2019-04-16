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

(** Eliom standard library *)

open Js_of_ocaml

(** See {% <<a_api project="ocsigenserver"| module Ocsigen_lib_base >> %}. *)
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

type file_info = File.file Js.t

val to_json : ?typ:'a -> 'b -> string
val of_json : ?typ:'a -> string -> 'b

module Url : sig

  (** URL manipulation *)

  (** See {% <<a_api project="ocsigenserver"| module
      Ocsigen_lib.Url_base >> %}. *)
  include module type of Url_base

  (** See {% <<a_api project="js_of_ocaml"| module Url >> %}. *)
  include module type of Url

  val decode : string -> string
  val encode : ?plus:bool -> string -> string
  val make_encoded_parameters : (string * string) list -> string
  val split_path : string -> string list
  val get_ssl : string -> bool option
  val resolve : string -> string
  val add_get_args : string -> (string * string) list -> string
  val string_of_url_path : encode:bool -> string list -> string
  val path_of_url : url -> string list

  (** Extracts path from a URL string. Works on a best-effort basis
      for relative URLs *)
  val path_of_url_string : string -> string list

end

(** Extension of {% <<a_api project="ocsigenserver"| module
    Ocsigen_lib_base.String_base >> %}. *)
module String : sig
  include module type of String_base
  val remove_eols : string -> string
end

(** Extension of {% <<a_api project="js_of_ocaml"| module Lwt_log_js >> %}. *)
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
val confirm : ('a, unit, string, bool) format4 -> 'a
val debug_var : string -> 'a -> unit
val trace : ('a, unit, string, unit) format4 -> 'a
val lwt_ignore : ?message:string -> unit Lwt.t -> unit

val encode_form_value : 'a -> string
val unmarshal_js : Js.js_string Js.t -> 'a

val encode_header_value : 'a -> string

(** Return a base-64 encoded cryptographic safe string of the given length.
    Not implemented client-side. *)
val make_cryptographic_safe_string : ?len:int -> unit -> string
