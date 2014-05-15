(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2012 Vincent Balat, Benedikt Becker
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


(** See {% <<a_api | module Eliom_content>> %} for complete module. *)

module Xml : sig
  include Xml_sigs.Iterable with type 'a wrap = 'a

  type -'a caml_event_handler constraint 'a = #Dom_html.event

  (**/**)

  val make_process_node : ?id:string -> elt -> elt
  val make_request_node : elt -> elt

  val uri_of_fun: (unit -> string) -> uri

  (* Building ref tree. *)
  type node_id
  val get_node_id : elt -> node_id
  val make_event_handler_table : elt -> Eliom_lib.RawXML.event_handler_table
  val make_client_attrib_table : elt -> Eliom_lib.RawXML.client_attrib_table

  val event_handler_of_string : string -> event_handler
  val string_of_event_handler : event_handler -> string
  val event_handler_of_service :
    ( [ `A | `Form_get | `Form_post ]
      * (bool * string list) option
      * string option) option Eliom_lazy.request -> event_handler

  val caml_event_handler : ((#Dom_html.event as 'a) Js.t -> unit) Eliom_lib.client_value -> 'a caml_event_handler
  val event_handler : (Dom_html.event Js.t -> unit) Eliom_lib.client_value -> event_handler

  type racontent =
    | RA of acontent
    | RAReact of acontent option React.signal
    | RACamlEventHandler of Dom_html.event caml_event_handler
    | RALazyStr of string Eliom_lazy.request
    | RALazyStrL of separator * string Eliom_lazy.request list
    | RAClient of string * attrib option * attrib Eliom_lib.Client_value_server_repr.t

  val racontent : attrib -> racontent

  val lazy_node : ?a:(attrib list) -> ename -> elt list Eliom_lazy.request -> elt

  (**/**)
  val wrap : elt -> 'a -> 'a Eliom_wrap.wrapped_value

end

(**/**)
module Eliom_xml : module type of Xml
    with type uri = Xml.uri
    and type separator = Xml.separator
    and type acontent = Xml.acontent
    and type attrib = Xml.attrib
    and type elt = Xml.elt
    and type 'a wrap = 'a
    and type -'a caml_event_handler = 'a Xml.caml_event_handler
(**/**)

module Svg : sig

  type +'a elt = Xml.elt (* ***!!! will be abstracted later! O.o DO NOT INSTALL eliom_content_core.cmi **)
  type 'a wrap = 'a
  type 'a attrib = Xml.attrib (* ***!!!! will be abstracted later! O.o DO NOT INSTALL eliom_content_core.cmi **)
  type uri = Xml.uri

  module F : sig

    module Raw : Svg_sigs.T with type Xml.uri = Xml.uri
                             and type Xml.event_handler = Xml.event_handler
                             and type Xml.attrib = Xml.attrib
                             and type Xml.elt = Xml.elt
			     and type +'a elt = 'a elt
                             and type 'a Xml.wrap = 'a
                             and type 'a wrap = 'a
                             and type +'a attrib = 'a attrib
		             and type uri = uri

    include module type of Raw

    include "sigs/eliom_svg_event_handler.mli"

  end

  module D : sig

    module Raw : Svg_sigs.T with type Xml.uri = Xml.uri
                             and type Xml.event_handler = Xml.event_handler
                             and type Xml.attrib = Xml.attrib
                             and type Xml.elt = Xml.elt
			     and type +'a elt = 'a elt
                             and type 'a Xml.wrap = 'a
                             and type 'a wrap = 'a
                             and type +'a attrib = 'a attrib
		             and type uri = uri

    include module type of Raw

    val client_attrib : ?init:'a attrib -> 'a attrib Eliom_lib.client_value -> 'a attrib

    include "sigs/eliom_svg_event_handler.mli"

  end

  module Id : sig

    type +'a id

    val new_elt_id: ?global:bool -> unit -> 'a id

    val create_named_elt: id:'a id -> 'a elt -> 'a elt

    val create_global_elt: 'a elt -> 'a elt
  end

  module Printer : Xml_sigs.Typed_simple_printer with type +'a elt := 'a F.elt
                                          and type doc := F.doc

end

module Html5 : sig

  (** See the Eliom manual for more information on {% <<a_manual
      chapter="clientserver-html" fragment="unique"| dom semantics vs. functional
      semantics>> %} for HTML5 tree manipulated by client/server
      application. *)

  type +'a elt = Xml.elt (* ***!!! will be abstracted later! O.o DO NOT INSTALL eliom_content_core.cmi **)
  type 'a wrap = 'a
  type 'a attrib = Xml.attrib (* ***!!! will be abstracted later! O.o DO NOT INSTALL eliom_content_core.cmi **)
  type uri = Xml.uri

  module F : sig

    module Raw : Html5_sigs.T
                   with type Xml.uri = Xml.uri
                   and type Xml.event_handler = Xml.event_handler
                   and type Xml.attrib = Xml.attrib
                   and type Xml.elt = Xml.elt
                   and type 'a Xml.wrap = 'a
                   with module Svg := Svg.F.Raw
                   with type +'a elt = 'a elt
                   and type 'a wrap = 'a
                   and type +'a attrib = 'a attrib
                   and type uri = uri

    include module type of Raw (*BB TODO Hide untyped [input]. *)

    include "sigs/eliom_html5_event_handler.mli"

    (**/**)
    type ('a, 'b, 'c) lazy_plus =
      ?a: (('a attrib) list) -> 'b elt Eliom_lazy.request -> ('b elt) list Eliom_lazy.request -> 'c elt

    val lazy_form:
      ([< Html5_types.form_attrib ], [< Html5_types.form_content_fun ], [> Html5_types.form ]) lazy_plus
  end

  module D : sig

    module Raw : Html5_sigs.T
                   with type Xml.uri = Xml.uri
                   and type Xml.event_handler = Xml.event_handler
                   and type Xml.attrib = Xml.attrib
                   and type Xml.elt = Xml.elt
                   and type 'a Xml.wrap = 'a
                   with module Svg := Svg.D.Raw
                   with type +'a elt = 'a elt
                   and type 'a wrap = 'a
                   and type +'a attrib = 'a attrib
                   and type uri = uri
    include module type of Raw (*BB TODO Hide untyped [input]. *)

    val client_attrib : ?init:'a attrib -> 'a attrib Eliom_lib.client_value -> 'a attrib

    include "sigs/eliom_html5_event_handler.mli"

    (**/**)
    type ('a, 'b, 'c) lazy_plus =
      ?a: (('a attrib) list) -> 'b elt Eliom_lazy.request -> ('b elt) list Eliom_lazy.request -> 'c elt

    val lazy_form:
      ([< Html5_types.form_attrib ], [< Html5_types.form_content_fun ], [> Html5_types.form ]) lazy_plus

  end

  module Id : sig
    type +'a id

    val new_elt_id: ?global:bool -> unit -> 'a id

    val create_named_elt: id:'a id -> 'a elt -> 'a elt

    val create_global_elt: 'a elt -> 'a elt

    (**/**)
    val have_id: 'a id -> 'b elt -> bool
  end

  module Custom_data : sig

    type 'a t

    val create : name:string -> ?default:'a -> to_string:('a -> string) -> of_string:(string -> 'a) -> unit -> 'a t

    val create_json : name:string -> ?default:'a -> 'a Deriving_Json.t -> 'a t

    val attrib : 'a t -> 'a -> [> | `User_data ] attrib

  end

  module Printer : Xml_sigs.Typed_simple_printer with type +'a elt := 'a F.elt
                                          and type doc := F.doc

end


module Xmld : sig
  include module type of Xml
  with type uri = Xml.uri
   and type separator = Xml.separator
   and type acontent = Xml.acontent
   and type attrib = Xml.attrib
   and type elt = Xml.elt
   and type 'a wrap = 'a
   and type -'a caml_event_handler = 'a Xml.caml_event_handler
   and type event_handler = Xml.event_handler
  val make_request_node : elt -> elt
  val make : econtent -> elt
  val make_lazy : econtent Eliom_lazy.request -> elt
end
