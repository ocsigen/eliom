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
  include Xml_sigs.Iterable
    with type 'a wrap = 'a
     and type 'a list_wrap = 'a list
     and type event_handler =
           (Dom_html.event Js.t -> unit) Eliom_client_value.t
     and type mouse_event_handler =
           (Dom_html.mouseEvent Js.t -> unit) Eliom_client_value.t
     and type keyboard_event_handler =
           (Dom_html.keyboardEvent Js.t -> unit) Eliom_client_value.t

  type caml_event_handler

  (**/**)

  val make_process_node : ?id:string -> elt -> elt
  val make_request_node : ?reset:bool -> elt -> elt

  val uri_of_fun: (unit -> string) -> uri

  (* Building ref tree. *)
  type node_id
  val get_node_id : elt -> node_id
  val make_event_handler_table : elt -> Eliom_runtime.RawXML.event_handler_table
  val make_client_attrib_table : elt -> Eliom_runtime.RawXML.client_attrib_table

  type internal_event_handler =
    | Raw of string
    | Caml of caml_event_handler

  val internal_event_handler_attrib : aname -> internal_event_handler -> attrib
  val internal_event_handler_of_service :
    ( [ `A | `Form_get | `Form_post ]
      * (bool * string list) option
      * string option
      * Eliom_lib.poly) option Eliom_lazy.request -> internal_event_handler

  val caml_event_handler :
    (Dom_html.event Js.t -> unit) Eliom_client_value.t ->
    caml_event_handler

  type racontent =
    | RA of acontent
    | RAReact of acontent option React.signal
    | RACamlEventHandler of caml_event_handler
    | RALazyStr of string Eliom_lazy.request
    | RALazyStrL of separator * string Eliom_lazy.request list
    | RAClient of string * attrib option * Eliom_lib.poly
    (* attrib client_value *)

  val racontent : attrib -> racontent

  val lazy_node : ?a:(attrib list) -> ename -> elt list Eliom_lazy.request -> elt

  (**/**)
  val wrap : elt -> 'a -> 'a Eliom_wrap.wrapped_value

  val client_attrib :
    ?init:attrib -> attrib Eliom_client_value.t -> attrib

end

module Svg : sig

  type 'a wrap = 'a
  type 'a list_wrap = 'a list
  type +'a elt
  type +'a attrib
  type uri = Xml.uri

  module F : sig

    module Raw : Svg_sigs.Make(Xml).T
      with type +'a elt = 'a elt
       and type +'a attrib = 'a attrib

    include module type of Raw

  end

  module D : sig

    module Raw : Svg_sigs.Make(Xml).T
      with type +'a elt = 'a elt
       and type +'a attrib = 'a attrib

    include module type of Raw

    val client_attrib :
      ?init:'a attrib -> 'a attrib Eliom_client_value.t -> 'a attrib

  end

  module Make
      (Xml : Xml_sigs.T
       with type elt = Xml.elt
        and type attrib = Xml.attrib)
      (C : Svg_sigs.Wrapped_functions with module Xml = Xml) :
    Svg_sigs.Make(Xml).T
    with type +'a elt = 'a elt
     and type +'a attrib = 'a attrib

  module Id : sig

    type +'a id

    val new_elt_id: ?global:bool -> unit -> 'a id

    val create_named_elt: id:'a id -> 'a elt -> 'a elt

    val create_global_elt: 'a elt -> 'a elt

    val create_request_elt: ?reset:bool -> 'a elt -> 'a elt

  end

  module Printer : Xml_sigs.Typed_simple_printer with type +'a elt := 'a F.elt
                                          and type doc := F.doc

end

module Html : sig

  (** See the Eliom manual for more information on {% <<a_manual
      chapter="clientserver-html" fragment="unique"| dom semantics vs. functional
      semantics>> %} for HTML5 tree manipulated by client/server
      application. *)

  type 'a wrap = 'a
  type 'a list_wrap = 'a list
  type +'a elt
  type +'a attrib
  type uri = Xml.uri

  module F : sig

    module Raw : Html_sigs.Make(Xml)(Svg.F.Raw).T
      with type +'a elt = 'a elt
       and type +'a attrib = 'a attrib

    include module type of Raw

    (**/**)
    type ('a, 'b, 'c) lazy_star =
      ?a: (('a attrib) list) -> ('b elt) list Eliom_lazy.request -> 'c elt

    val lazy_form:
      ([< Html_types.form_attrib ], [< Html_types.form_content_fun ], [> Html_types.form ]) lazy_star
  end

  module D : sig

    module Raw : Html_sigs.Make(Xml)(Svg.D.Raw).T
      with type +'a elt = 'a elt
       and type +'a attrib = 'a attrib

    include module type of Raw

    val client_attrib :
      ?init:'a attrib -> 'a attrib Eliom_client_value.t -> 'a attrib

    (**/**)
    type ('a, 'b, 'c) lazy_star =
      ?a: (('a attrib) list) -> ('b elt) list Eliom_lazy.request -> 'c elt

    val lazy_form:
      ([< Html_types.form_attrib ], [< Html_types.form_content_fun ], [> Html_types.form ]) lazy_star

  end

  module Make
      (Xml : Xml_sigs.T
       with type elt = Xml.elt
        and type attrib = Xml.attrib)
      (C : Html_sigs.Wrapped_functions with module Xml = Xml)
      (Svg : Svg_sigs.T with module Xml := Xml) :
    Html_sigs.Make(Xml)(Svg).T
    with type +'a elt = 'a elt
     and type +'a attrib = 'a attrib

  module Id : sig
    type +'a id

    val new_elt_id: ?global:bool -> unit -> 'a id

    val create_named_elt: id:'a id -> 'a elt -> 'a elt

    val create_global_elt: 'a elt -> 'a elt

    val create_request_elt: ?reset:bool -> 'a elt -> 'a elt

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
