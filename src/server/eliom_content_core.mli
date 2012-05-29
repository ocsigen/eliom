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

(** Low-level XML manipulation. *)
module Xml : sig

  (** {2 Base functions}
      Cf. {% <<a_api project="tyxml" | module Xml_sigs.Iterable >> %}. *)

  include Xml_sigs.Iterable

  (** {2 Unique nodes } *)

  (** Unique nodes are XML nodes that are manipulated 'by reference'
      when sent to the client part of an Eliom-application: the
      created element is allocated only one time in each instance of
      an application. See {% <<a_manual chapter="client"
      fragment="unique" |the eliom manual>>%} for more
      details. *)

  (** Event handlers *)

  (** Values of type ['a caml_event_handler] represents event handler
      build with the [{{ ... }}] syntax (see the Eliom manual for more
      information on {% <<a_manual chapter="client"
      fragment="syntax"|syntax extension>>%}). Such values are expected
      by functions like {!Eliom_service.on_load} or
      {!Eliom_content.Html5.a_onclick}. The type parameter is the
      type of the javascript event expected by the handler, for
      example {% <<a_api project="js_of_ocaml" | type
      Dom_html.mouseEvent>>%} or {% <<a_api project="js_of_ocaml" | type
      Dom_html.keyboardEvent >>%}. *)
  type -'a caml_event_handler constraint 'a = #Dom_html.event

  (**/**)

  val make_process_node : ?id:string -> elt -> elt
  val make_request_node : elt -> elt

  val uri_of_fun: (unit -> string) -> uri

  (* Building ref tree. *)
  type event_handler_table (* Concrete on client-side only. *)
  type node_id
  val get_node_id : elt -> node_id
  val make_event_handler_table : elt -> event_handler_table

  val event_handler_of_string : string -> event_handler
  val string_of_event_handler : event_handler -> string
  val event_handler_of_js : int64 -> Ocsigen_lib_base.poly -> #Dom_html.event caml_event_handler
  val event_handler_of_service :
    ( [ `A | `Form_get | `Form_post ]
      * (bool * string list) option
      * string option) option Eliom_lazy.request -> event_handler

  (* Deprecated alias. *)
  val event_of_string : string -> event_handler
  val string_of_handler : event_handler -> string
  val event_of_js : int64 -> Ocsigen_lib_base.poly -> #Dom_html.event caml_event_handler
  val event_of_service :
    ( [ `A | `Form_get | `Form_post ]
      * (bool * string list) option
      * string option ) option Eliom_lazy.request -> event_handler

  type racontent =
    | RA of acontent
    | RACamlEventHandler of Dom_html.event caml_event_handler
    | RALazyStr of string Eliom_lazy.request
    | RALazyStrL of separator * string Eliom_lazy.request list
  val racontent : attrib -> racontent

  val lazy_node : ?a:(attrib list) -> ename -> elt list Eliom_lazy.request -> elt

end

(**/**)
module Eliom_xml : module type of Xml
    with type uri = Xml.uri
    and type separator = Xml.separator
    and type acontent = Xml.acontent
    and type attrib = Xml.attrib
    and type elt = Xml.elt
    and type -'a caml_event_handler = 'a Xml.caml_event_handler
(**/**)

(** Building and pretty-printing valid SVG tree. *)
module Svg : sig

  (** See the Eliom manual for more information on{% <<a_manual
      chapter="client" fragment="unique"| dom semantics vs. functional
      semantics>> %} for SVG tree manipulated by client/server
      application. *)

  type +'a elt
  type 'a attrib
  type uri = Xml.uri

  (** Typed interface for building valid SVG tree (functional
      semantics). See {% <<a_api project="tyxml" | module type
      Svg_sigs.T >> %}. *)
  module F : Svg_sigs.T with type Xml.uri = Xml.uri
                        and type Xml.event_handler = Xml.event_handler
                        and type Xml.attrib = Xml.attrib
                        and type Xml.elt = Xml.elt
                        with type 'a elt = 'a elt
                        and type 'a attrib = 'a attrib
                        and type uri = uri

  (** Typed interface for building valid SVG tree (DOM semantics). See
      {% <<a_api project="tyxml" | module type Svg_sigs.T >> %}. *)
  module D : Svg_sigs.T with type Xml.uri = Xml.uri
                        and type Xml.event_handler = Xml.event_handler
                        and type Xml.attrib = Xml.attrib
                        and type Xml.elt = Xml.elt
                        with type 'a elt = 'a elt
                        and type 'a attrib = 'a attrib
                        and type uri = uri


  (** Node identifiers. *)
  module Id : sig
    (** The type of global SVG element identifier. *)
    type +'a id

    (** The function [new_elt_id ()] creates a new HTML5 element
        identifier. (see the Eliom manual for more information on {%
        <<a_manual project="eliom" chapter="client"
        fragment="global"|global element>>%}).*)
    val new_elt_id: ?global:bool -> unit -> 'a id

    (** The function [create_named_elt ~id elt] create a copy of the
        element [elt] that will be accessible through the name [id]. *)
    val create_named_elt: id:'a id -> 'a elt -> 'a elt

    (** The function [create_named_elt elt] is equivalent to
        [create_named_elt ~id:(new_elt_id ()) elt]. *)
    val create_global_elt: 'a elt -> 'a elt
  end

  (** SVG printer.
      See {% <<a_api project="tyxml" | module type Xml_sigs.Typed_simple_printer >> %}. *)
  module Printer : Xml_sigs.Typed_simple_printer with type 'a elt := 'a F.elt
                                          and type doc := F.doc

end

(** Building and printing valid (X)HTML5 tree. *)
module Html5 : sig

  (** See the Eliom manual for more information on {% <<a_manual
      chapter="client" fragment="unique"| dom semantics vs. functional
      semantics>> %} for HTML5 tree manipulated by client/server
      application. *)

  type +'a elt
  type +'a attrib
  type uri = Xml.uri

  (** Typed interface for building valid HTML5 tree (functional
      semantics). *)
  module F : sig

    (** Cf. {% <<a_api project="tyxml" | module type Html5_sigs.T >> %}. *)
    module Raw : Html5_sigs.T
                   with type Xml.uri = Xml.uri
                   and type Xml.event_handler = Xml.event_handler
                   and type Xml.attrib = Xml.attrib
                   and type Xml.elt = Xml.elt
                   with module Svg := Svg.F
                   with type +'a elt = 'a elt
                   and type 'a attrib = 'a attrib
                   and type uri = uri

    include module type of Raw (*BB TODO Hide untyped [input]. *)

    (** {2 Event handlers} *)

    (** Redefine event handler attributes to simplify their usage. *)
    include "sigs/eliom_html5_event_handler.mli"

    (**/**)
    type ('a, 'b, 'c) lazy_plus =
      ?a: (('a attrib) list) -> 'b elt Eliom_lazy.request -> ('b elt) list Eliom_lazy.request -> 'c elt

    val lazy_form:
      ([< Html5_types.form_attrib ], [< Html5_types.form_content_fun ], [> Html5_types.form ]) lazy_plus
  end

  (** Typed interface for building valid HTML5 tree (DOM semantics). *)
  module D : sig

    (** Cf. {% <<a_api project="tyxml" | module type Html5_sigs.T >> %}. *)
    module Raw : Html5_sigs.T
                   with type Xml.uri = Xml.uri
                   and type Xml.event_handler = Xml.event_handler
                   and type Xml.attrib = Xml.attrib
                   and type Xml.elt = Xml.elt
                   with module Svg := Svg.D
                   with type +'a elt = 'a elt
                   and type 'a attrib = 'a attrib
                   and type uri = uri
    include module type of Raw (*BB TODO Hide untyped [input]. *)

    (** {2 Event handlers} *)

    (** Redefine event handler attributes to simplify their usage. *)
    include "sigs/eliom_html5_event_handler.mli"

    (**/**)
    type ('a, 'b, 'c) lazy_plus =
      ?a: (('a attrib) list) -> 'b elt Eliom_lazy.request -> ('b elt) list Eliom_lazy.request -> 'c elt

    val lazy_form:
      ([< Html5_types.form_attrib ], [< Html5_types.form_content_fun ], [> Html5_types.form ]) lazy_plus

  end

  (** Node identifiers *)
  module Id : sig
    (** The type of global HTML5 element identifier. *)
    type +'a id

    (** The function [new_elt_id ()] creates a new global HTML5 element
        identifier (see the Eliom manual for more information on {%
        <<a_manual project="eliom" chapter="client"
        fragment="global"|global element>>%}).*)
    val new_elt_id: ?global:bool -> unit -> 'a id

    (** The function [create_named_elt ~id elt] create a copy of the
        element [elt] that will be sent to client with the reference
        [id]. *)
    val create_named_elt: id:'a id -> 'a elt -> 'a elt

    (** The function [create_named_elt elt] is equivalent to
        [create_named_elt ~id:(new_elt_id ()) elt]. *)
    val create_global_elt: 'a elt -> 'a elt

    (**/**)
    val have_id: 'a id -> 'b elt -> bool
  end

  (** {{:http://dev.w3.org/html5/html-xhtml-author-guide/}"Polyglot"} HTML5 printer.
     See {% <<a_api project="tyxml" | module type Xml_sigs.Typed_simple_printer >> %}. *)
  module Printer : Xml_sigs.Typed_simple_printer with type 'a elt := 'a F.elt
                                          and type doc := F.doc

end

(** Building and printing valid XHTML tree. *)
module Xhtml : sig

  (** Typed interface for building valid XHTML (Strict) tree. *)
  module F : sig

    module Raw : Xhtml_sigs.T
                   with type Xml.uri = Xml.uri
                   and type Xml.event_handler = Xml.event_handler
                   and type Xml.attrib = Xml.attrib
                   and type Xml.elt = Xml.elt
    include module type of Raw (*BB TODO Hide untyped [input]. *)

    (**/**)
    type ('a, 'b, 'c) lazy_plus =
        ?a: (('a attrib) list) -> 'b elt Eliom_lazy.request -> ('b elt) list Eliom_lazy.request -> 'c elt

    val lazy_form:
      action:Xml.uri ->
      ([< Xhtml_types.form_attrib ], [< Xhtml_types.form_content ], [> Xhtml_types.form ]) lazy_plus

  end


  (** Typed interface for building valid XHTML (1.0 Strict) tree. *)
  module F_01_00 : sig
(* TODO replicate (similar) for other F/D modules
(** Eliom service registration for XHTML page. This
    an instance the {!Registration} abstract signatures. *)
 *)

    (** See {% <<a_api project="tyxml" | module type Xhtml_sigs.T >> %}. *)
    module Raw : Xhtml_sigs.T
                   with type Xml.uri = Xml.uri
                   and type Xml.event_handler = Xml.event_handler
                   and type Xml.attrib = Xml.attrib
                   and type Xml.elt = Xml.elt
    include module type of Raw (*BB TODO Hide untyped link functions. *)

    (**/**)
    type ('a, 'b, 'c) lazy_plus =
      ?a: (('a attrib) list) -> 'b elt Eliom_lazy.request -> ('b elt) list Eliom_lazy.request -> 'c elt

    val lazy_form:
      action:Xml.uri ->
      ([< Xhtml_types.form_attrib ], [< Xhtml_types.form_content ], [> Xhtml_types.form ]) lazy_plus

  end

  (** Typed interface for building valid XHTML (1.1 Strict) tree. *)
  module F_01_01 : sig

    (** See {% <<a_api project="tyxml" | module type Xhtml_sigs.T >> %}. *)
    module Raw : Xhtml_sigs.T
                   with type Xml.uri = Xml.uri
                   and type Xml.event_handler = Xml.event_handler
                   and type Xml.attrib = Xml.attrib
                   and type Xml.elt = Xml.elt
    include module type of Raw  (*BB TODO Hide untyped link functions. *)

    (**/**)
    type ('a, 'b, 'c) lazy_plus =
      ?a: (('a attrib) list) -> 'b elt Eliom_lazy.request -> ('b elt) list Eliom_lazy.request -> 'c elt

    val lazy_form:
      action:Xml.uri ->
      ([< Xhtml_types.form_attrib ], [< Xhtml_types.form_content ], [> Xhtml_types.form ]) lazy_plus

  end

  (** XHTML (latest Strict) printer. See {% <<a_api project="tyxml" |
      module type Xml_sigs.Typed_simple_printer >> %}. This printer try
      to follow the {{:http://www.w3.org/TR/xhtml1/#guidelines}W3C
      guidelines} for HTML compatibility. Hence the resulting string
      could be serve as well as [application/xhtml+xml] or
      [text/html]. This however has some
      {{:http://hixie.ch/advocacy/xhtml}limitations}. *)
  module Printer : Xml_sigs.Typed_simple_printer
    with type 'a elt := 'a F.elt
    and type doc := F.doc

  (** XHTML (1.0 Strict) printer. See {% <<a_api project="tyxml" |
      module type Xml_sigs.Typed_simple_printer >> %}. This printer try
      to follow the {{:http://www.w3.org/TR/xhtml1/#guidelines}W3C
      guidelines} for HTML compatibility. Hence the resulting string
      could be serve as well as [application/xhtml+xml] or
      [text/html]. This however has some
      {{:http://hixie.ch/advocacy/xhtml}limitations}. *)
  module Printer_01_00 : Xml_sigs.Typed_simple_printer
                   with type 'a elt := 'a F_01_00.elt
                   and type doc := F_01_00.doc

  (** XHTML (1.1 Strict) printer. See {% <<a_api project="tyxml" |
      module type Xml_sigs.Typed_simple_printer >> %}. This printer try
      to follow the {{:http://www.w3.org/TR/xhtml1/#guidelines}W3C
      guidelines} for HTML compatibility. Hence the resulting string
      could be serve as well as [application/xhtml+xml] or
      [text/html]. This however has some
      {{:http://hixie.ch/advocacy/xhtml}limitations}. *)
  module Printer_01_01 : Xml_sigs.Typed_simple_printer
                   with type 'a elt := 'a F_01_01.elt
                   and type doc := F_01_01.doc

end
