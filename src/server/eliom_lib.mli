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

(** Pervasives module for Eliom-server: it extends the OCaml stdlib and should always be opened. *)

include module type of Ocsigen_lib
  with type poly = Ocsigen_lib.poly
  and type yesnomaybe = Ocsigen_lib.yesnomaybe
  and type ('a, 'b) leftright = ('a, 'b) Ocsigen_lib.leftright
  and type 'a Clist.t = 'a Ocsigen_lib.Clist.t
  and type 'a Clist.node = 'a Ocsigen_lib.Clist.node
  and type Ip_address.t = Ocsigen_lib.Ip_address.t

include module type of Eliom_lib_base
  with type 'a client_expr = 'a Eliom_lib_base.client_expr

(** {2 Pervasives} *)

exception Eliom_Internal_Error of string

type file_info = Ocsigen_extensions.file_info

val to_json : ?typ:'a Deriving_Json.t -> 'a -> string
val of_json : ?typ:'a Deriving_Json.t -> string -> 'a

(** {2 TyXML}

    XML tree manipulation within Eliom is based on the TyXML library
    but use a custom representation for XML values (see
    {!XML}). Then, [Eliom_lib] redefines the three high level
    interfaces ({!SVG}, {!HTML5} and {!XHTML}) that are provided by
    TyXML for valid XML tree creation and printing. *)

(** Low-level XML manipulation. *)
module XML : sig

  (** {2 Base functions } *)

  include XML_sigs.Iterable

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
      by functions like {!Eliom_services.on_load} or
      {!Eliom_lib.HTML5.a_onclick}. The type parameter is the
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

(** Building and pretty-printing valid SVG tree. *)
module SVG : sig

  (** See the Eliom manual for more information on{% <<a_manual
      chapter="client" fragment="unique"| dom semantics vs. functional
      semantics>> %} for SVG tree manipulated by client/server
      application. *)

  type +'a elt
  type 'a attrib
  type uri = XML.uri

  (** {2 Dom semantics} *)

  (** Typed interface for building valid SVG tree (DOM semantics). See
      {% <<a_api project="tyxml" | module type SVG_sigs.T >> %}. *)
  module D : SVG_sigs.T with module XML := XML
		        and type 'a elt = 'a elt
		        and type 'a attrib = 'a attrib
		        and type uri = uri

  (** {2 Functional semantics} *)

  (** Typed interface for building valid SVG tree (functional
      semantics). See {% <<a_api project="tyxml" | module type
      SVG_sigs.T >> %}. *)
  module F : SVG_sigs.T with module XML := XML
		        and type 'a elt = 'a elt
		        and type 'a attrib = 'a attrib
		        and type uri = uri

  (** {2 Global node} *)

  (** The type of global SVG element identifier. *)
  type +'a id

  (** See {!Eliom_lib.HTML5.new_elt_id} *)
  val new_elt_id: ?global:bool -> unit -> 'a id
  (** See {!Eliom_lib.HTML5.create_named_elt} *)
  val create_named_elt: id:'a id -> 'a elt -> 'a elt
  (** See {!Eliom_lib.HTML5.create_global_elt} *)
  val create_global_elt: 'a elt -> 'a elt

  (** {2 Printer} *)

  (** SVG printer.
      See {% <<a_api project="tyxml" | module type XML_sigs.TypedSimplePrinter >> %}. *)
  module P : XML_sigs.TypedSimplePrinter with type 'a elt := 'a F.elt
					  and type doc := F.doc

end

(** Building and printing valid (X)HTML5 tree. *)
module HTML5 : sig

  (** See the Eliom manual for more information on{% <<a_manual
      chapter="client" fragment="unique"| dom semantics vs. functional
      semantics>> %} for HTML5 tree manipulated by client/server
      application. *)

  type +'a elt
  type +'a attrib
  type uri = XML.uri

  (** {2 Dom semantics} *)

  (** Typed interface for building valid HTML5 tree (DOM semantics). See
      {% <<a_api project="tyxml" | module type HTML5_sigs.T >> %}. *)
  module D : sig

    (** See {% <<a_api project="tyxml" | module type HTML5_sigs.T >> %}. *)
    include HTML5_sigs.T with module XML := XML and module SVG := SVG.D
		         and type 'a elt = 'a elt
		         and type 'a attrib = 'a attrib
		         and type uri = uri

    (** {2 Event handlers} *)

    (**/**)
    include "sigs/eliom_html5_event_handler_raw.mli"
    (**/**)

    (** Redefine event handler attributes to simplify their usage. *)
    include "sigs/eliom_html5_event_handler.mli"

    (**/**)
    type ('a, 'b, 'c) lazy_plus =
      ?a: (('a attrib) list) -> 'b elt Eliom_lazy.request -> ('b elt) list Eliom_lazy.request -> 'c elt

    val lazy_form:
      ([< HTML5_types.form_attrib ], [< HTML5_types.form_content_fun ], [> HTML5_types.form ]) lazy_plus
    (**/**)

  end

  (** {2 Functional semantics} *)

  (** Typed interface for building valid HTML5 tree (functional
      semantics). See {% <<a_api project="tyxml" | module type
      HTML5_sigs.T >> %}. *)
  module F : sig

    (** See {% <<a_api project="tyxml" | module type HTML5_sigs.T >> %}. *)
    include HTML5_sigs.T with module XML := XML and module SVG := SVG.F
		         and type 'a elt = 'a elt
		         and type 'a attrib = 'a attrib
		         and type uri = uri

    (** {2 Event handlers} *)

    (**/**)
    include "sigs/eliom_html5_event_handler_raw.mli"
    (**/**)

    (** Redefine event handler attributes to simplify their usage. *)
    include "sigs/eliom_html5_event_handler.mli"

    (**/**)
    type ('a, 'b, 'c) lazy_plus =
      ?a: (('a attrib) list) -> 'b elt Eliom_lazy.request -> ('b elt) list Eliom_lazy.request -> 'c elt

    val lazy_form:
      ([< HTML5_types.form_attrib ], [< HTML5_types.form_content_fun ], [> HTML5_types.form ]) lazy_plus
    (**/**)

  end

  (** {2 Global node} *)
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
  (**/**)

  (** {2 Printer} *)

  (** {{:http://dev.w3.org/html5/html-xhtml-author-guide/}"Polyglot"} HTML5 printer.
     See {% <<a_api project="tyxml" | module type XML_sigs.TypedSimplePrinter >> %}. *)
  module P : XML_sigs.TypedSimplePrinter with type 'a elt := 'a F.elt
					  and type doc := F.doc

end

(** Building and printing valid XHTML tree. *)
module XHTML : sig

  (** Typed interface for building valid XHTML (Strict) tree. *)
  module F : sig

    include XHTML_sigs.T with module XML := XML

    (**/**)
    type ('a, 'b, 'c) lazy_plus =
        ?a: (('a attrib) list) -> 'b elt Eliom_lazy.request -> ('b elt) list Eliom_lazy.request -> 'c elt

    val lazy_form:
      action:XML.uri ->
      ([< XHTML_types.form_attrib ], [< XHTML_types.form_content ], [> XHTML_types.form ]) lazy_plus
    (**/**)

  end


  (** XHTML (latest Strict) printer. See {% <<a_api project="tyxml" |
      module type XML_sigs.TypedSimplePrinter >> %}. This printer try
      to follow the {{:http://www.w3.org/TR/xhtml1/#guidelines}W3C
      guidelines} for HTML compatibility. Hence the resulting string
      could be serve as well as [application/xhtml+xml] or
      [text/html]. This however has some
      {{:http://hixie.ch/advocacy/xhtml}limitations}. *)
  module P : XML_sigs.TypedSimplePrinter
    with type 'a elt := 'a F.elt
    and type doc := F.doc

  (** Typed interface for building valid XHTML (1.0 Strict) tree. *)
  module M_01_00 : sig

    (** See {% <<a_api project="tyxml" | module type XHTML_sigs.T >> %}. *)
    include XHTML_sigs.T with module XML := XML

    (**/**)
    type ('a, 'b, 'c) lazy_plus =
      ?a: (('a attrib) list) -> 'b elt Eliom_lazy.request -> ('b elt) list Eliom_lazy.request -> 'c elt

    val lazy_form:
      action:XML.uri ->
      ([< XHTML_types.form_attrib ], [< XHTML_types.form_content ], [> XHTML_types.form ]) lazy_plus
    (**/**)

  end

  (** XHTML (1.0 Strict) printer. See {% <<a_api project="tyxml" |
      module type XML_sigs.TypedSimplePrinter >> %}. This printer try
      to follow the {{:http://www.w3.org/TR/xhtml1/#guidelines}W3C
      guidelines} for HTML compatibility. Hence the resulting string
      could be serve as well as [application/xhtml+xml] or
      [text/html]. This however has some
      {{:http://hixie.ch/advocacy/xhtml}limitations}. *)
  module P_01_00 : XML_sigs.TypedSimplePrinter
                   with type 'a elt := 'a M_01_00.elt
		    and type doc := M_01_00.doc

  (** Typed interface for building valid XHTML (1.1 Strict) tree. *)
  module M_01_01 : sig

    (** See {% <<a_api project="tyxml" | module type XHTML_sigs.T >> %}. *)
    include XHTML_sigs.T with module XML := XML

    (**/**)
    type ('a, 'b, 'c) lazy_plus =
      ?a: (('a attrib) list) -> 'b elt Eliom_lazy.request -> ('b elt) list Eliom_lazy.request -> 'c elt

    val lazy_form:
      action:XML.uri ->
      ([< XHTML_types.form_attrib ], [< XHTML_types.form_content ], [> XHTML_types.form ]) lazy_plus
    (**/**)

  end

  (** XHTML (1.1 Strict) printer. See {% <<a_api project="tyxml" |
      module type XML_sigs.TypedSimplePrinter >> %}. This printer try
      to follow the {{:http://www.w3.org/TR/xhtml1/#guidelines}W3C
      guidelines} for HTML compatibility. Hence the resulting string
      could be serve as well as [application/xhtml+xml] or
      [text/html]. This however has some
      {{:http://hixie.ch/advocacy/xhtml}limitations}. *)
  module P_01_01 : XML_sigs.TypedSimplePrinter
                   with type 'a elt := 'a M_01_01.elt
		    and type doc := M_01_01.doc

end

