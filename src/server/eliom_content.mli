
open Eliom_content_core

(** Abstract signature for links and forms creation functions. For
    concrete instance see {!Html5}, {!Xhtml} or {!Html_text}. *)
module type Forms = "sigs/eliom_forms.mli"

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
    with type uri = XML.uri
    and type separator = XML.separator
    and type acontent = XML.acontent
    and type attrib = XML.attrib
    and type elt = XML.elt

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
  type -'a caml_event_handler = 'a XML.caml_event_handler constraint 'a = #Dom_html.event

  (**/**)

  val make_process_node : ?id:string -> elt -> elt
  val make_request_node : elt -> elt

  val uri_of_fun: (unit -> string) -> uri

  (* Building ref tree. *)
  type event_handler_table = XML.event_handler_table (* Concrete on client-side only. *)
  type node_id = XML.node_id
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

  (** {2 Functional semantics} *)

  (** Typed interface for building valid SVG tree (functional
      semantics). See {% <<a_api project="tyxml" | module type
      SVG_sigs.T >> %}. *)
  module F : SVG_sigs.T with module XML := XML
		        and type 'a elt = 'a SVG.elt
		        and type 'a attrib = 'a SVG.attrib
		        and type uri = SVG.uri

  (** {2 Dom semantics} *)

  (** Typed interface for building valid SVG tree (DOM semantics). See
      {% <<a_api project="tyxml" | module type SVG_sigs.T >> %}. *)
  module D : SVG_sigs.T with module XML := XML
		        and type 'a elt = 'a F.elt
		        and type 'a attrib = 'a F.attrib
		        and type uri = F.uri


  type +'a elt = 'a F.elt
  type 'a attrib = 'a F.attrib
  type uri = F.uri

  (** {2 Global node} *)
  module Id : sig
    (** The type of global SVG element identifier. *)
    type +'a id

    (** See {!Eliom_lib.HTML5.new_elt_id} *)
    val new_elt_id: ?global:bool -> unit -> 'a id
    (** See {!Eliom_lib.HTML5.create_named_elt} *)
    val create_named_elt: id:'a id -> 'a elt -> 'a elt
    (** See {!Eliom_lib.HTML5.create_global_elt} *)
    val create_global_elt: 'a elt -> 'a elt
  end

  (** {2 Printer} *)

  (** SVG printer.
      See {% <<a_api project="tyxml" | module type XML_sigs.TypedSimplePrinter >> %}. *)
  module Printer : XML_sigs.TypedSimplePrinter with type 'a elt := 'a F.elt
					  and type doc := F.doc

end

(** Building and printing valid (X)HTML5 tree. *)
module HTML5 : sig

  (** See the Eliom manual for more information on{% <<a_manual
      chapter="client" fragment="unique"| dom semantics vs. functional
      semantics>> %} for HTML5 tree manipulated by client/server
      application. *)

  (** {2 Functional semantics} *)

  (** Typed interface for building valid HTML5 tree (functional
      semantics). See {% <<a_api project="tyxml" | module type
      HTML5_sigs.T >> %}. *)
  module F : sig

    (** See {% <<a_api project="tyxml" | module type HTML5_sigs.T >> %}. *)
    include HTML5_sigs.T with module XML := XML and module SVG := SVG.F
		         and type 'a elt = 'a HTML5.elt
		         and type 'a attrib = 'a HTML5.attrib
		         and type uri = HTML5.uri
    (* TODO Hide untyped [a], [input]. *)
    val raw_a : ([< HTML5_types.a_attrib ], 'a, [> `A of 'a ]) star
    val raw_input : ([< HTML5_types.input_attrib ], [> HTML5_types.input ]) nullary

    (** {2 Event handlers} *)

    (** Redefine event handler attributes to simplify their usage. *)
    include "sigs/eliom_html5_event_handler.mli"
    include "sigs/eliom_html5_forms.mli"

    (**/**)
    include "sigs/eliom_html5_event_handler_raw.mli"
    (**/**)

    (**/**)
    type ('a, 'b, 'c) lazy_plus =
      ?a: (('a attrib) list) -> 'b elt Eliom_lazy.request -> ('b elt) list Eliom_lazy.request -> 'c elt

    val lazy_form:
      ([< HTML5_types.form_attrib ], [< HTML5_types.form_content_fun ], [> HTML5_types.form ]) lazy_plus
    (**/**)

  end

  (** {2 Dom semantics} *)

  (** Typed interface for building valid HTML5 tree (DOM semantics). See
      {% <<a_api project="tyxml" | module type HTML5_sigs.T >> %}. *)
  module D : sig

    (** See {% <<a_api project="tyxml" | module type HTML5_sigs.T >> %}. *)
    include HTML5_sigs.T with module XML := XML and module SVG := SVG.D
		         and type 'a elt = 'a F.elt
		         and type 'a attrib = 'a F.attrib
		         and type uri = F.uri
    (* TODO Hide untyped [a], [input]. *)
    val raw_a : ([< HTML5_types.a_attrib ], 'a, [> `A of 'a ]) star
    val raw_input : ([< HTML5_types.input_attrib ], [> HTML5_types.input ]) nullary

    (** {2 Event handlers} *)

    (** Redefine event handler attributes to simplify their usage. *)
    include "sigs/eliom_html5_event_handler.mli"
    include "sigs/eliom_html5_forms.mli"

    (**/**)
    include "sigs/eliom_html5_event_handler_raw.mli"
    (**/**)

    (**/**)
    type ('a, 'b, 'c) lazy_plus =
      ?a: (('a attrib) list) -> 'b elt Eliom_lazy.request -> ('b elt) list Eliom_lazy.request -> 'c elt

    val lazy_form:
      ([< HTML5_types.form_attrib ], [< HTML5_types.form_content_fun ], [> HTML5_types.form ]) lazy_plus
    (**/**)

  end

  type +'a elt = 'a F.elt
  type +'a attrib = 'a F.attrib
  type uri = F.uri

  (** {2 Global node} *)
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
    (**/**)
  end

  (** {2 Printer} *)

  (** {{:http://dev.w3.org/html5/html-xhtml-author-guide/}"Polyglot"} HTML5 printer.
     See {% <<a_api project="tyxml" | module type XML_sigs.TypedSimplePrinter >> %}. *)
  module Printer : XML_sigs.TypedSimplePrinter with type 'a elt := 'a F.elt
					  and type doc := F.doc

end

(** Building and printing valid XHTML tree. *)
module XHTML : sig

  (** Typed interface for building valid XHTML (Strict) tree. *)
  module F : sig

    include XHTML_sigs.T with module XML := XML
      with type +'a elt = 'a XHTML.F.elt
      and type 'a attrib = 'a XHTML.F.attrib
      and type uri = XHTML.F.uri

    (* TODO Hide untyped [a], [input]. *)
    val raw_a : ([< XHTML_types.a_attrib ], [< XHTML_types.a_content ], [> XHTML_types.a ]) star
    val raw_input : ([< XHTML_types.input_attrib ], [> XHTML_types.input ]) nullary

    include "sigs/eliom_xhtml_forms.mli"

    (**/**)
    type ('a, 'b, 'c) lazy_plus =
        ?a: (('a attrib) list) -> 'b elt Eliom_lazy.request -> ('b elt) list Eliom_lazy.request -> 'c elt

    val lazy_form:
      action:XML.uri ->
      ([< XHTML_types.form_attrib ], [< XHTML_types.form_content ], [> XHTML_types.form ]) lazy_plus
    (**/**)

  end


  (** Typed interface for building valid XHTML (1.0 Strict) tree. *)
  module F_01_00 : sig
(* TODO replicate (similar) for other F/D modules
(** Eliom service registration for XHTML page. This
    an instance the {!Registration} abstract signatures. *)
 *)

    (** See {% <<a_api project="tyxml" | module type XHTML_sigs.T >> %}. *)
    include XHTML_sigs.T with module XML := XML
      with type +'a elt = 'a XHTML.F_01_00.elt
      and type 'a attrib = 'a XHTML.F_01_00.attrib
      and type uri = XHTML.F_01_00.uri

    (* TODO Hide untyped [a], [input]. *)
    val raw_a : ([< XHTML_types.a_attrib ], [< XHTML_types.a_content ], [> XHTML_types.a ]) star
    val raw_input : ([< XHTML_types.input_attrib ], [> XHTML_types.input ]) nullary

    include "sigs/eliom_xhtml_forms.mli"

    (**/**)
    type ('a, 'b, 'c) lazy_plus =
      ?a: (('a attrib) list) -> 'b elt Eliom_lazy.request -> ('b elt) list Eliom_lazy.request -> 'c elt

    val lazy_form:
      action:XML.uri ->
      ([< XHTML_types.form_attrib ], [< XHTML_types.form_content ], [> XHTML_types.form ]) lazy_plus
    (**/**)

  end

  (** Typed interface for building valid XHTML (1.1 Strict) tree. *)
  module F_01_01 : sig

    (** See {% <<a_api project="tyxml" | module type XHTML_sigs.T >> %}. *)
    include XHTML_sigs.T with module XML := XML
      with type +'a elt = 'a XHTML.F_01_01.elt
      and type 'a attrib = 'a XHTML.F_01_01.attrib
      and type uri = XHTML.F_01_01.uri

    (* TODO Hide untyped [a], [input]. *)
    val raw_a : ([< XHTML_types.a_attrib ], [< XHTML_types.a_content ], [> XHTML_types.a ]) star
    val raw_input : ([< XHTML_types.input_attrib ], [> XHTML_types.input ]) nullary

    include "sigs/eliom_xhtml_forms.mli"

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
  module Printer : XML_sigs.TypedSimplePrinter
    with type 'a elt := 'a F.elt
    and type doc := F.doc

  (** XHTML (1.0 Strict) printer. See {% <<a_api project="tyxml" |
      module type XML_sigs.TypedSimplePrinter >> %}. This printer try
      to follow the {{:http://www.w3.org/TR/xhtml1/#guidelines}W3C
      guidelines} for HTML compatibility. Hence the resulting string
      could be serve as well as [application/xhtml+xml] or
      [text/html]. This however has some
      {{:http://hixie.ch/advocacy/xhtml}limitations}. *)
  module Printer_01_00 : XML_sigs.TypedSimplePrinter
                   with type 'a elt := 'a F_01_00.elt
		   and type doc := F_01_00.doc

  (** XHTML (1.1 Strict) printer. See {% <<a_api project="tyxml" |
      module type XML_sigs.TypedSimplePrinter >> %}. This printer try
      to follow the {{:http://www.w3.org/TR/xhtml1/#guidelines}W3C
      guidelines} for HTML compatibility. Hence the resulting string
      could be serve as well as [application/xhtml+xml] or
      [text/html]. This however has some
      {{:http://hixie.ch/advocacy/xhtml}limitations}. *)
  module Printer_01_01 : XML_sigs.TypedSimplePrinter
                   with type 'a elt := 'a F_01_01.elt
		   and type doc := F_01_01.doc

end


module Html_text : sig
  include "sigs/eliom_forms.mli"
    subst type uri := string
    and type pcdata_elt := string

    and type form_elt := string
    and type form_content_elt := string
    and type form_content_elt_list := string
    and type form_attrib_t := string

    and type 'a a_elt := string
    and type 'a a_content_elt := string
    and type 'a a_content_elt_list := string
    and type a_attrib_t := string

    and type link_elt := string
    and type link_attrib_t := string

    and type script_elt := string
    and type script_attrib_t := string

    and type textarea_elt := string
    and type textarea_attrib_t := string

    and type input_elt := string
    and type input_attrib_t := string

    and type select_elt := string
    and type select_attrib_t := string

    and type button_elt := string
    and type button_content_elt := string
    and type button_content_elt_list := string
    and type button_attrib_t := string

    and type optgroup_attrib_t := string
    and type option_attrib_t := string

    and type input_type_t := string
    and type raw_input_type_t := string
    and type button_type_t := string
    and type for_attrib := string
end
