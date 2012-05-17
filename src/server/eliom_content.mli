
open Eliom_content_core

(** Abstract signature for links and forms creation functions. For
    concrete instance see {!Html5}, {!Xhtml} or {!Html_text}. *)
module type Forms = "sigs/eliom_forms.mli"

(** {2 TyXML}

    XML tree manipulation within Eliom is based on the TyXML library
    but use a custom representation for XML values (see
    {!Xml}). Then, [Eliom_content] redefines the three high level
    interfaces ({!Svg}, {!Html5} and {!Xhtml}) that are provided by
    TyXML for valid XML tree creation and printing. *)

(** Low-level XML manipulation. *)
module Xml : sig

  (** {2 Base functions } *)

  include Xml_sigs.Iterable
    with type uri = Xml.uri
    and type separator = Xml.separator
    and type acontent = Xml.acontent
    and type attrib = Xml.attrib
    and type elt = Xml.elt

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
  type -'a caml_event_handler = 'a Xml.caml_event_handler constraint 'a = #Dom_html.event

  (**/**)

  val make_process_node : ?id:string -> elt -> elt
  val make_request_node : elt -> elt

  val uri_of_fun: (unit -> string) -> uri

  (* Building ref tree. *)
  type event_handler_table = Xml.event_handler_table (* Concrete on client-side only. *)
  type node_id = Xml.node_id
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
module Svg : sig

  (** See the Eliom manual for more information on{% <<a_manual
      chapter="client" fragment="unique"| dom semantics vs. functional
      semantics>> %} for SVG tree manipulated by client/server
      application. *)

  type +'a elt = 'a Eliom_content_core.Svg.elt
  type 'a attrib = 'a Eliom_content_core.Svg.attrib
  type uri = Eliom_content_core.Svg.uri

  (** {2 Functional semantics} *)

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

  (** {2 Dom semantics} *)

  (** Typed interface for building valid SVG tree (DOM semantics). See
      {% <<a_api project="tyxml" | module type Svg_sigs.T >> %}. *)
  module D : Svg_sigs.T with type Xml.uri = Xml.uri
                        and type Xml.event_handler = Xml.event_handler
                        and type Xml.attrib = Xml.attrib
                        and type Xml.elt = Xml.elt
                        with type 'a elt = 'a elt
		        and type 'a attrib = 'a attrib
		        and type uri = uri


  (** {2 Global node} *)
  module Id : sig
    (** The type of global SVG element identifier. *)
    type +'a id

    (** See {!Eliom_content.Html5.Id.new_elt_id} *)
    val new_elt_id: ?global:bool -> unit -> 'a id
    (** See {!Eliom_content.Html5.Id.create_named_elt} *)
    val create_named_elt: id:'a id -> 'a elt -> 'a elt
    (** See {!Eliom_content.Html5.Id.create_global_elt} *)
    val create_global_elt: 'a elt -> 'a elt
  end

  (** {2 Printer} *)

  (** SVG printer.
      See {% <<a_api project="tyxml" | module type Xml_sigs.Typed_simple_printer >> %}. *)
  module Printer : Xml_sigs.Typed_simple_printer with type 'a elt := 'a F.elt
					  and type doc := F.doc

end

(** Building and printing valid (X)HTML5 tree. *)
module Html5 : sig

  (** See the Eliom manual for more information on{% <<a_manual
      chapter="client" fragment="unique"| dom semantics vs. functional
      semantics>> %} for HTML5 tree manipulated by client/server
      application. *)

  type +'a elt = 'a Eliom_content_core.Html5.elt
  type +'a attrib = 'a Eliom_content_core.Html5.attrib
  type uri = Eliom_content_core.Html5.uri

  (** {2 Functional semantics} *)

  (** Typed interface for building valid HTML5 tree (functional
      semantics). See {% <<a_api project="tyxml" | module type
      Html5_sigs.T >> %}. *)
  module F : sig

    (** See {% <<a_api project="tyxml" | module type Html5_sigs.T >> %}. *)
    include Html5_sigs.T with type Xml.uri = Xml.uri
                         and type Xml.event_handler = Xml.event_handler
                         and type Xml.attrib = Xml.attrib
                         and type Xml.elt = Xml.elt
                         with type 'a elt = 'a elt
		         and type 'a attrib = 'a attrib
		         and type uri = uri
                         with module Svg := Svg.F

    val raw_a : ([< Html5_types.a_attrib ], 'a, [> `A of 'a ]) star
    val raw_input : ([< Html5_types.input_attrib ], [> Html5_types.input ]) nullary
    (*BB TODO Hide untyped [input]. *)

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
      ([< Html5_types.form_attrib ], [< Html5_types.form_content_fun ], [> Html5_types.form ]) lazy_plus
    (**/**)

  end

  (** {2 Dom semantics} *)

  (** Typed interface for building valid HTML5 tree (DOM semantics). See
      {% <<a_api project="tyxml" | module type Html5_sigs.T >> %}. *)
  module D : sig

    (** See {% <<a_api project="tyxml" | module type Html5_sigs.T >> %}. *)
    include Html5_sigs.T with type Xml.uri = Xml.uri
                         and type Xml.event_handler = Xml.event_handler
                         and type Xml.attrib = Xml.attrib
                         and type Xml.elt = Xml.elt
                         with type 'a elt = 'a elt
		         and type 'a attrib = 'a attrib
		         and type uri = uri
                         with module Svg := Svg.D

    val raw_a : ([< Html5_types.a_attrib ], 'a, [> `A of 'a ]) star
    val raw_input : ([< Html5_types.input_attrib ], [> Html5_types.input ]) nullary
    (*BB TODO Hide untyped [input]. *)

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
      ([< Html5_types.form_attrib ], [< Html5_types.form_content_fun ], [> Html5_types.form ]) lazy_plus
    (**/**)

  end

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
     See {% <<a_api project="tyxml" | module type Xml_sigs.Typed_simple_printer >> %}. *)
  module Printer : Xml_sigs.Typed_simple_printer with type 'a elt := 'a F.elt
					  and type doc := F.doc

end

(** Building and printing valid XHTML tree. *)
module Xhtml : sig

  (** Typed interface for building valid XHTML (Strict) tree. *)
  module F : sig

    include Xhtml_sigs.T with type Xml.uri = Xml.uri
                         and type Xml.event_handler = Xml.event_handler
                         and type Xml.attrib = Xml.attrib
                         and type Xml.elt = Xml.elt
                         with type +'a elt = 'a Xhtml.F.elt
                         and type 'a attrib = 'a Xhtml.F.attrib
                         and type uri = Xhtml.F.uri

    val raw_a : ([< Xhtml_types.a_attrib ], [< Xhtml_types.a_content ], [> Xhtml_types.a ]) star
    val raw_input : ([< Xhtml_types.input_attrib ], [> Xhtml_types.input ]) nullary
    (*BB TODO Hide untyped [input]. *)

    include "sigs/eliom_xhtml_forms.mli"

    (**/**)
    type ('a, 'b, 'c) lazy_plus =
        ?a: (('a attrib) list) -> 'b elt Eliom_lazy.request -> ('b elt) list Eliom_lazy.request -> 'c elt

    val lazy_form:
      action:Xml.uri ->
      ([< Xhtml_types.form_attrib ], [< Xhtml_types.form_content ], [> Xhtml_types.form ]) lazy_plus
    (**/**)

  end


  (** Typed interface for building valid XHTML (1.0 Strict) tree. *)
  module F_01_00 : sig
(* TODO replicate (similar) for other F/D modules
(** Eliom service registration for XHTML page. This
    an instance the {!Registration} abstract signatures. *)
 *)

    (** See {% <<a_api project="tyxml" | module type Xhtml_sigs.T >> %}. *)
    include Xhtml_sigs.T with type Xml.uri = Xml.uri
                         and type Xml.event_handler = Xml.event_handler
                         and type Xml.attrib = Xml.attrib
                         and type Xml.elt = Xml.elt
                         with type +'a elt = 'a Xhtml.F_01_00.elt
                         and type 'a attrib = 'a Xhtml.F_01_00.attrib
                         and type uri = Xhtml.F_01_00.uri

    val raw_a : ([< Xhtml_types.a_attrib ], [< Xhtml_types.a_content ], [> Xhtml_types.a ]) star
    val raw_input : ([< Xhtml_types.input_attrib ], [> Xhtml_types.input ]) nullary
    (*BB TODO Hide untyped [input]. *)

    include "sigs/eliom_xhtml_forms.mli"

    (**/**)
    type ('a, 'b, 'c) lazy_plus =
      ?a: (('a attrib) list) -> 'b elt Eliom_lazy.request -> ('b elt) list Eliom_lazy.request -> 'c elt

    val lazy_form:
      action:Xml.uri ->
      ([< Xhtml_types.form_attrib ], [< Xhtml_types.form_content ], [> Xhtml_types.form ]) lazy_plus
    (**/**)

  end

  (** Typed interface for building valid XHTML (1.1 Strict) tree. *)
  module F_01_01 : sig

    (** See {% <<a_api project="tyxml" | module type Xhtml_sigs.T >> %}. *)
    include Xhtml_sigs.T with type Xml.uri = Xml.uri
                         and type Xml.event_handler = Xml.event_handler
                         and type Xml.attrib = Xml.attrib
                         and type Xml.elt = Xml.elt
                         with type +'a elt = 'a Xhtml.F_01_01.elt
                         and type 'a attrib = 'a Xhtml.F_01_01.attrib
                         and type uri = Xhtml.F_01_01.uri

    val raw_a : ([< Xhtml_types.a_attrib ], [< Xhtml_types.a_content ], [> Xhtml_types.a ]) star
    val raw_input : ([< Xhtml_types.input_attrib ], [> Xhtml_types.input ]) nullary
    (*BB TODO Hide untyped [input]. *)

    include "sigs/eliom_xhtml_forms.mli"

    (**/**)
    type ('a, 'b, 'c) lazy_plus =
      ?a: (('a attrib) list) -> 'b elt Eliom_lazy.request -> ('b elt) list Eliom_lazy.request -> 'c elt

    val lazy_form:
      action:Xml.uri ->
      ([< Xhtml_types.form_attrib ], [< Xhtml_types.form_content ], [> Xhtml_types.form ]) lazy_plus
    (**/**)

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
