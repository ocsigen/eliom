
open Eliom_lib

(** XML building and deconstructing. *)
module Xml : sig

  type uri = string
  val uri_of_string : uri -> string
  val string_of_uri : string -> uri
  val uri_of_fun : (unit -> string) -> uri

  type aname = string
  type attrib

  type -'a caml_event_handler =
    | CE_registered_closure of string * ((#Dom_html.event as 'a) Js.t -> unit) client_expr
    | CE_client_closure of ('a Js.t -> unit)
    | CE_call_service of
	([ `A | `Form_get | `Form_post] * (bool * string list) option * string option) option Eliom_lazy.request

  type event_handler =
    | Raw of string
    | Caml of Dom_html.event caml_event_handler

  type ename = string
  type elt
  type econtent = private
    | Empty
    | Comment of string
    | EncodedPCDATA of string
    | PCDATA of string
    | Entity of string
    | Leaf of ename * attrib list
    | Node of ename * attrib list * elt list

  (**/**)

  val event_handler_of_service :
    ( [ `A | `Form_get | `Form_post ]
      * (bool * string list) option
      * string option) option Eliom_lazy.request -> event_handler
  val event_handler_of_function : (#Dom_html.event Js.t -> unit) -> event_handler

  (* Deprecated alias. *)
  val event_of_service :
    ( [ `A | `Form_get | `Form_post ]
      * (bool * string list) option
      * string option ) option Eliom_lazy.request -> event_handler
  val event_of_function : ((#Dom_html.event Js.t as 'a) -> unit) -> ( 'a -> unit)

  type separator = Space | Comma
  type acontent = private
    | AFloat of float
    | AInt of int
    | AStr of string
    | AStrL of separator * string list
  val acontent : attrib -> acontent

  type racontent =
    | RA of acontent
    | RACamlEventHandler of Dom_html.event caml_event_handler
    | RALazyStr of string Eliom_lazy.request
    | RALazyStrL of separator * string Eliom_lazy.request list
  val racontent : attrib -> racontent

  val aname : attrib -> aname

  val float_attrib : aname -> float -> attrib
  val int_attrib : aname -> int -> attrib
  val string_attrib : aname -> string -> attrib
  val space_sep_attrib : aname -> string list -> attrib
  val comma_sep_attrib : aname -> string list -> attrib
  val event_handler_attrib : aname -> event_handler -> attrib
  val uri_attrib : aname -> uri -> attrib
  val uris_attrib : aname -> uri list -> attrib

  (* Deprecated alias. *)
  val event_attrib : aname -> event_handler -> attrib

  val content : elt -> econtent

  val pcdata : string -> elt
  val encodedpcdata : string -> elt
  val entity : string -> elt

  val empty : unit -> elt
  val comment : string -> elt

  val leaf : ?a:(attrib list) -> ename -> elt
  val node : ?a:(attrib list) -> ename -> elt list -> elt
  val lazy_node : ?a:(attrib list) -> ename -> elt list Eliom_lazy.request -> elt

  val cdata : string -> elt
  val cdata_script : string -> elt
  val cdata_style : string -> elt

  type node_id =
    | NoId
    | ProcessId of string
    | RequestId of string
  val make : ?id:node_id -> econtent -> elt
  val make_dom : ?id:node_id -> Dom.node Js.t -> elt

  val make_process_node : ?id:string -> elt -> elt
  val make_request_node : elt -> elt
  val get_node_id : elt -> node_id

  type node =
    | DomNode of Dom.node Js.t
    | TyXMLNode of econtent
  val get_node : elt -> node
  val set_dom_node : elt -> Dom.node Js.t -> unit

  module ClosureMap : Map.S with type key = string
  type event_handler_table = ((poly -> unit) client_expr) ClosureMap.t

end

(** Building SVG tree. *)
module Svg : sig

  (** See the Eliom manual for more information on{% <<a_manual
      chapter="client" fragment="unique"| dom semantics vs. functional
      semantics>> %} for SVG tree manipulated by client/server
      application. *)

  type +'a elt = 'a Eliom_content_core.Svg.elt
  type 'a attrib = 'a Eliom_content_core.Svg.attrib
  type uri = Eliom_content_core.Svg.uri

  (** {2 Dom semantics} *)

  (** Typed interface for building valid SVG tree (DOM semantics). See
      {% <<a_api project="tyxml" | module type Svg_sigs.T >> %}. *)
  module D: Svg_sigs.T with module Xml := Xml
		         and type 'a elt = 'a elt
		         and type 'a attrib = 'a attrib
		         and type uri = uri

  (** {2 Functional semantics} *)

  (** Typed interface for building valid SVG tree (functional
      semantics). See {% <<a_api project="tyxml" | module type
      Svg_sigs.T >> %}. *)
  module F : Svg_sigs.T with module Xml := Xml
		        and type 'a elt = 'a elt
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

end

(** Building Html5 tree. *)
module Html5 : sig

  (** See the Eliom manual for more information on{% <<a_manual
      chapter="client" fragment="unique"| dom semantics vs. functional
      semantics>> %} for SVG tree manipulated by client/server
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
    include Html5_sigs.T with module Xml := Xml and module Svg := Svg.F
		         and type 'a elt = 'a elt
		         and type 'a attrib = 'a attrib
		         and type uri = uri


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

  (** {2 DOM semantics} *)

  (** Typed interface for building valid HTML5 tree (DOM semantics). See
      {% <<a_api project="tyxml" | module type Html5_sigs.T >> %}. *)
  module D: sig

    include Html5_sigs.T with module Xml := Xml
		         and module Svg := Svg.D
		         and type 'a elt = 'a elt
		         and type 'a attrib = 'a attrib
		         and type uri = uri

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

    (**/**)
    val string_of_id : 'a id -> string
  end

  (** Conversion of Javascript DOM elements to HTML5 elts (with DOM semantics of course).
      One conversion function per source type (stressed by the [of_] prefix). *)
  module Of_dom : sig
    val of_element : Dom_html.element Js.t -> 'a elt
    val of_html : Dom_html.htmlElement Js.t -> Html5_types.html elt
    val of_head : Dom_html.headElement Js.t -> Html5_types.head elt
    val of_link : Dom_html.linkElement Js.t -> Html5_types.link elt
    val of_title : Dom_html.titleElement Js.t -> Html5_types.title elt
    val of_meta : Dom_html.metaElement Js.t -> Html5_types.meta elt
    val of_base : Dom_html.baseElement Js.t -> Html5_types.base elt
    val of_style : Dom_html.styleElement Js.t -> Html5_types.style elt
    val of_body : Dom_html.bodyElement Js.t -> Html5_types.body elt
    val of_form : Dom_html.formElement Js.t -> Html5_types.form elt
    val of_optGroup : Dom_html.optGroupElement Js.t -> Html5_types.optgroup elt
    val of_option : Dom_html.optionElement Js.t -> Html5_types.selectoption elt
    val of_select : Dom_html.selectElement Js.t -> Html5_types.select elt
    val of_input : Dom_html.inputElement Js.t -> Html5_types.input elt
    val of_textArea : Dom_html.textAreaElement Js.t -> Html5_types.textarea elt
    val of_button : Dom_html.buttonElement Js.t -> Html5_types.button elt
    val of_label : Dom_html.labelElement Js.t -> Html5_types.label elt
    val of_fieldSet : Dom_html.fieldSetElement Js.t -> Html5_types.fieldset elt
    val of_legend : Dom_html.legendElement Js.t -> Html5_types.legend elt
    val of_uList : Dom_html.uListElement Js.t -> Html5_types.ul elt
    val of_oList : Dom_html.oListElement Js.t -> Html5_types.ol elt
    val of_dList : Dom_html.dListElement Js.t -> [`Dl] elt
    val of_li : Dom_html.liElement Js.t -> Html5_types.li elt
    val of_div : Dom_html.divElement Js.t -> Html5_types.div elt
    val of_paragraph : Dom_html.paragraphElement Js.t -> Html5_types.p elt
    val of_heading : Dom_html.headingElement Js.t -> Html5_types.heading elt
    val of_quote : Dom_html.quoteElement Js.t -> Html5_types.blockquote elt
    val of_pre : Dom_html.preElement Js.t -> Html5_types.pre elt
    val of_br : Dom_html.brElement Js.t -> Html5_types.br elt
    val of_hr : Dom_html.hrElement Js.t -> Html5_types.hr elt
    val of_anchor : Dom_html.anchorElement Js.t -> 'a Html5_types.a elt
    val of_image : Dom_html.imageElement Js.t -> [`Img] elt
    val of_object : Dom_html.objectElement Js.t -> 'a Html5_types.object_ elt
    val of_param : Dom_html.paramElement Js.t -> Html5_types.param elt
    val of_area : Dom_html.areaElement Js.t -> Html5_types.area elt
    val of_map : Dom_html.mapElement Js.t -> 'a Html5_types.map elt
    val of_script : Dom_html.scriptElement Js.t -> Html5_types.script elt
    val of_tableCell : Dom_html.tableCellElement Js.t -> [ Html5_types.td | Html5_types.td ] elt
    val of_tableRow : Dom_html.tableRowElement Js.t -> Html5_types.tr elt
    val of_tableCol : Dom_html.tableColElement Js.t -> Html5_types.col elt
    val of_tableSection : Dom_html.tableSectionElement Js.t -> [ Html5_types.tfoot | Html5_types.thead | Html5_types.tbody ] elt
    val of_tableCaption : Dom_html.tableCaptionElement Js.t -> Html5_types.caption elt
    val of_table : Dom_html.tableElement Js.t -> Html5_types.table elt
    val of_canvas : Dom_html.canvasElement Js.t -> 'a Html5_types.canvas elt
    val of_iFrame : Dom_html.iFrameElement Js.t -> Html5_types.iframe elt
  end

  (** Conversion from HTML5 [elt]s to Javascript DOM elements ([<: Dom_html.element Js.t]).
      One conversion function per source type (stressed by the [of_] prefix). *)
  module To_dom : sig

    val of_element : 'a elt -> Dom_html.element Js.t
    val of_heading : Html5_types.heading elt -> Dom_html.headingElement Js.t

    val of_pcdata : [> `Pcdata] elt -> Dom.text Js.t

    val of_abbr : [> `Abbr] elt -> Dom_html.element Js.t
    val of_acronym : [> `Acronym] elt -> Dom_html.element Js.t
    val of_address : [> `Address] elt -> Dom_html.element Js.t
    val of_applet : [> `Applet] elt -> Dom_html.element Js.t
    val of_article : [> `Article] elt -> Dom_html.element Js.t
    val of_aside : [> `Aside] elt -> Dom_html.element Js.t
    val of_audio : [> `Audio] elt -> Dom_html.element Js.t
    val of_b : [> `B] elt -> Dom_html.element Js.t
    val of_basefont : [> `basefont] elt -> Dom_html.element Js.t
    val of_bdi : [> `Bdi] elt -> Dom_html.element Js.t
    val of_bdo : [> `Bdo] elt -> Dom_html.element Js.t
    val of_big : [> `Big] elt -> Dom_html.element Js.t
    val of_center : [> `Center] elt -> Dom_html.element Js.t
    val of_cite : [> `Cite] elt -> Dom_html.element Js.t
    val of_code : [> `Code] elt -> Dom_html.element Js.t
    val of_colgroup : [> `Colgroup] elt -> Dom_html.element Js.t
    val of_command : [> `Command] elt -> Dom_html.element Js.t
    val of_datalist : [> `Datalist] elt -> Dom_html.element Js.t
    val of_dd : [> `Dd] elt -> Dom_html.element Js.t
    val of_del : [> `Del] elt -> Dom_html.element Js.t
    val of_details : [> `Details] elt -> Dom_html.element Js.t
    val of_dfn : [> `Dfn] elt -> Dom_html.element Js.t
    val of_dir : [> `Dir] elt -> Dom_html.element Js.t
    val of_dt : [> `Dt] elt -> Dom_html.element Js.t
    val of_em : [> `Em] elt -> Dom_html.element Js.t
    val of_embed : [> `Embed] elt -> Dom_html.element Js.t
    val of_figcaption : [> `Figcaption] elt -> Dom_html.element Js.t
    val of_figure : [> `Figure] elt -> Dom_html.element Js.t
    val of_font : [> `Font] elt -> Dom_html.element Js.t
    val of_footer : [> `Footer] elt -> Dom_html.element Js.t
    val of_frame : [> `Frame] elt -> Dom_html.element Js.t
    val of_frameset : [> `Frameset] elt -> Dom_html.element Js.t
    val of_h1 : Html5_types.heading elt -> Dom_html.headingElement Js.t
    val of_h2 : Html5_types.heading elt -> Dom_html.headingElement Js.t
    val of_h3 : Html5_types.heading elt -> Dom_html.headingElement Js.t
    val of_h4 : Html5_types.heading elt -> Dom_html.headingElement Js.t
    val of_h5 : Html5_types.heading elt -> Dom_html.headingElement Js.t
    val of_h6 : Html5_types.heading elt -> Dom_html.headingElement Js.t
    val of_header : [> `Header] elt -> Dom_html.element Js.t
    val of_hgroup : [> `Hgroup] elt -> Dom_html.element Js.t
    val of_i : [> `I] elt -> Dom_html.element Js.t
    val of_ins : [> `Ins] elt -> Dom_html.element Js.t
    val of_keygen : [> `Keygen] elt -> Dom_html.element Js.t
    val of_kbd : [> `Kbd] elt -> Dom_html.element Js.t
    val of_mark : [> `Mark] elt -> Dom_html.element Js.t
    val of_menu : [> `Menu] elt -> Dom_html.element Js.t
    val of_meter : [> `Meter] elt -> Dom_html.element Js.t
    val of_nav : [> `Nav] elt -> Dom_html.element Js.t
    val of_noframes : [> `Noframes] elt -> Dom_html.element Js.t
    val of_noscript : [> `Noscript] elt -> Dom_html.element Js.t
    val of_output : [> `Output] elt -> Dom_html.element Js.t
    val of_progress : [> `Progress] elt -> Dom_html.element Js.t
    val of_q : [> `Q] elt -> Dom_html.element Js.t
    val of_rp : [> `Rp] elt -> Dom_html.element Js.t
    val of_rt : [> `Rt] elt -> Dom_html.element Js.t
    val of_ruby : [> `Ruby] elt -> Dom_html.element Js.t
    val of_s : [> `S] elt -> Dom_html.element Js.t
    val of_samp : [> `Samp] elt -> Dom_html.element Js.t
    val of_section : [> `Section] elt -> Dom_html.element Js.t
    val of_small : [> `Small] elt -> Dom_html.element Js.t
    val of_source : [> `Source] elt -> Dom_html.element Js.t
    val of_span : [> `Span] elt -> Dom_html.element Js.t
    val of_strike : [> `Strike] elt -> Dom_html.element Js.t
    val of_strong : [> `Strong] elt -> Dom_html.element Js.t
    val of_sub : [> `Sub] elt -> Dom_html.element Js.t
    val of_summary : [> `Summary] elt -> Dom_html.element Js.t
    val of_sup : [> `Sup] elt -> Dom_html.element Js.t
    val of_th : [> `Th] elt -> Dom_html.element Js.t
    val of_time : [> `Time] elt -> Dom_html.element Js.t
    val of_track : [> `Track] elt -> Dom_html.element Js.t
    val of_tt : [> `Tt] elt -> Dom_html.element Js.t
    val of_u : [> `U] elt -> Dom_html.element Js.t
    val of_var : [> `Var] elt -> Dom_html.element Js.t
    val of_video : [> `Video] elt -> Dom_html.element Js.t
    val of_wbr : [> `Wbr] elt -> Dom_html.element Js.t

    val of_html : Html5_types.html elt -> Dom_html.htmlElement Js.t
    val of_head : Html5_types.head elt -> Dom_html.headElement Js.t
    val of_link : Html5_types.link elt -> Dom_html.linkElement Js.t
    val of_title : Html5_types.title elt -> Dom_html.titleElement Js.t
    val of_meta : Html5_types.meta elt -> Dom_html.metaElement Js.t
    val of_base : Html5_types.base elt -> Dom_html.baseElement Js.t
    val of_style : Html5_types.style elt -> Dom_html.styleElement Js.t
    val of_body : Html5_types.body elt -> Dom_html.bodyElement Js.t
    val of_form : Html5_types.form elt -> Dom_html.formElement Js.t
    val of_optgroup : Html5_types.optgroup elt -> Dom_html.optGroupElement Js.t
    val of_option : Html5_types.selectoption elt -> Dom_html.optionElement Js.t
    val of_select : Html5_types.select elt -> Dom_html.selectElement Js.t
    val of_input : Html5_types.input elt -> Dom_html.inputElement Js.t
    val of_textarea : Html5_types.textarea elt -> Dom_html.textAreaElement Js.t
    val of_button : Html5_types.button elt -> Dom_html.buttonElement Js.t
    val of_label : Html5_types.label elt -> Dom_html.labelElement Js.t
    val of_fieldset : Html5_types.fieldset elt -> Dom_html.fieldSetElement Js.t
    val of_legend : Html5_types.legend elt -> Dom_html.legendElement Js.t
    val of_ul : Html5_types.ul elt -> Dom_html.uListElement Js.t
    val of_ol : Html5_types.ol elt -> Dom_html.oListElement Js.t
    val of_dl : [`Dl] elt -> Dom_html.dListElement Js.t
    val of_li : Html5_types.li elt -> Dom_html.liElement Js.t
    val of_div : Html5_types.div elt -> Dom_html.divElement Js.t
    val of_p : Html5_types.p elt -> Dom_html.paragraphElement Js.t
    val of_blockquote : Html5_types.blockquote elt -> Dom_html.quoteElement Js.t
    val of_pre : Html5_types.pre elt -> Dom_html.preElement Js.t
    val of_br : Html5_types.br elt -> Dom_html.brElement Js.t
    val of_hr : Html5_types.hr elt -> Dom_html.hrElement Js.t
    val of_a : 'a Html5_types.a elt -> Dom_html.anchorElement Js.t
    val of_img : [`Img] elt -> Dom_html.imageElement Js.t
    val of_object : 'a Html5_types.object_ elt -> Dom_html.objectElement Js.t
    val of_param : Html5_types.param elt -> Dom_html.paramElement Js.t
    val of_area : Html5_types.area elt -> Dom_html.areaElement Js.t
    val of_map : 'a Html5_types.map elt -> Dom_html.mapElement Js.t
    val of_script : Html5_types.script elt -> Dom_html.scriptElement Js.t
    val of_td : [ Html5_types.td | Html5_types.td ] elt -> Dom_html.tableCellElement Js.t
    val of_tr : Html5_types.tr elt -> Dom_html.tableRowElement Js.t
    val of_col : Html5_types.col elt -> Dom_html.tableColElement Js.t
    val of_tfoot : Html5_types.tfoot elt -> Dom_html.tableSectionElement Js.t
    val of_thead : Html5_types.thead elt -> Dom_html.tableSectionElement Js.t
    val of_tbody : Html5_types.tbody elt -> Dom_html.tableSectionElement Js.t
    val of_caption : Html5_types.caption elt -> Dom_html.tableCaptionElement Js.t
    val of_table : Html5_types.table elt -> Dom_html.tableElement Js.t
    val of_canvas : 'a Html5_types.canvas elt -> Dom_html.canvasElement Js.t
    val of_iframe : Html5_types.iframe elt -> Dom_html.iFrameElement Js.t

  end

  (** DOM-like manipulation functions.

      In this module, all the functions apply only to HTML5 element with
      {% <<a_manual chapter="client" fragment="unique"|Dom semantics>>
      %}.
  *)
  module Manip : sig
    (** The function [appendChild e1 e2] inserts the element [e2] as last
        child of [e1]. If the optional parameter [~before:e3] is present
        and if [e3] is a child of [e1], then [e2] is inserted before [e3]
        in the list of [e1] children. *)
    val appendChild: ?before:'a elt -> 'b elt ->  'c elt -> unit

    (** The function [appendChilds e1 elts] inserts [elts] as last children
        of [e1]. If the optional parameter [~before:e3] is present and if
        [e3] is a child of [e1], then [elts] are inserted before [e3] in
        the list of [e1] children. *)
    val appendChilds: ?before:'a elt -> 'b elt ->  'c elt list -> unit

    (** The function [removeChild e1 e2] removes for [e2] from the list of
        [e1] children. *)
    val removeChild: 'a elt -> 'b elt -> unit

    (** The function [replace e1 e2 e3] replaces for [e2] by [e3] in the
        list of [e1] children. *)
    val replaceChild: 'a elt -> 'b elt -> 'c elt -> unit

    (** The function [removeAllChild e1] removes [e1] children. *)
    val removeAllChild: 'a elt -> unit

    (** The function [replaceAllChild e1 elts] replaces all the children of
        [e1] by [elt]. *)
    val replaceAllChild: 'a elt -> 'b elt list -> unit

    (** The function [addEventListener elt evt handler] attach the
        [handler] for the event [evt] on the element [elt]. See the
        Js_of_ocaml manual, for a list of {% <<a_api project="js_of_ocaml"
        text="available events"| module Dom_html.Event >>%}. *)
    val addEventListener:
      ?capture:bool ->
      'a elt ->
      (#Dom_html.event as 'b) Js.t Dom_html.Event.typ ->
      ('a elt -> 'b Js.t -> bool) ->
      Dom_html.event_listener_id

    (** Dom manipulation by element identifier. *)
    module Named: sig

      (** The module [Named] defines the same functions as
          [Eliom_dom]. They take as parameter an element identifier
          instead of an element with Dom semantics. Those functions only
          works if the element is available in the application (sent in
          the page or along the page). If the element is not available,
          those functions raise with [Not_found]. *)

      (** see [appendChild] *)
      val appendChild: ?before:'a elt -> 'b Id.id -> 'c elt -> unit
      (** see [appendChilds] *)
      val appendChilds: ?before:'a elt -> 'b Id.id ->  'c elt list -> unit
      (** see [removeChild] *)
      val removeChild: 'a Id.id -> 'b elt -> unit
      (** see [replaceChild] *)
      val replaceChild: 'a Id.id -> 'b elt -> 'c elt -> unit
      (** see [removeAllChild] *)
      val removeAllChild: 'a Id.id -> unit
      (** see [replaceAllChild] *)
      val replaceAllChild: 'a Id.id -> 'b elt list -> unit

      (** see [addEventListener] *)
      val addEventListener:
        ?capture:bool ->
        'a Id.id ->
        (#Dom_html.event as 'b) Js.t Dom_html.Event.typ ->
        ('a elt -> 'b Js.t -> bool) ->
        Dom_html.event_listener_id

    end

    (** The function [scrollIntoView elt] scroll the page to a position
        where [elt] is displayed at the top of the window. If the optional
        parameter [~bottom:true] is present, the page is scrolled to a
        position where [elt] is displayed at the bottom of the window. *)
    val scrollIntoView: ?bottom:bool -> 'a elt -> unit

    (**/**)
    val childNodes: 'a elt -> Dom.node Js.t list
    val childElements: 'a elt -> Dom.element Js.t list
    (**/**)

    (** Read the CSS properties of DOM elements. *)
    module Css : sig
      val background: 'a elt -> string
      val backgroundAttachment: 'a elt -> string
      val backgroundColor: 'a elt -> string
      val backgroundImage: 'a elt -> string
      val backgroundPosition: 'a elt -> string
      val backgroundRepeat: 'a elt -> string
      val border: 'a elt -> string
      val borderBottom: 'a elt -> string
      val borderBottomColor: 'a elt -> string
      val borderBottomStyle: 'a elt -> string
      val borderBottomWidth: 'a elt -> string
      val borderCollapse: 'a elt -> string
      val borderColor: 'a elt -> string
      val borderLeft: 'a elt -> string
      val borderLeftColor: 'a elt -> string
      val borderLeftStyle: 'a elt -> string
      val borderLeftWidth: 'a elt -> string
      val borderRight: 'a elt -> string
      val borderRightColor: 'a elt -> string
      val borderRightStyle: 'a elt -> string
      val borderRightWidth: 'a elt -> string
      val borderSpacing: 'a elt -> string
      val borderStyle: 'a elt -> string
      val borderTop: 'a elt -> string
      val borderTopColor: 'a elt -> string
      val borderTopStyle: 'a elt -> string
      val borderTopWidth: 'a elt -> string
      val borderWidth: 'a elt -> string
      val bottom: 'a elt -> string
      val captionSide: 'a elt -> string
      val clear: 'a elt -> string
      val clip: 'a elt -> string
      val color: 'a elt -> string
      val content: 'a elt -> string
      val counterIncrement: 'a elt -> string
      val counterReset: 'a elt -> string
      val cssFloat: 'a elt -> string
      val cssText: 'a elt -> string
      val cursor: 'a elt -> string
      val direction: 'a elt -> string
      val display: 'a elt -> string
      val emptyCells: 'a elt -> string
      val font: 'a elt -> string
      val fontFamily: 'a elt -> string
      val fontSize: 'a elt -> string
      val fontStyle: 'a elt -> string
      val fontVariant: 'a elt -> string
      val fontWeight: 'a elt -> string
      val height: 'a elt -> string
      val left: 'a elt -> string
      val letterSpacing: 'a elt -> string
      val lineHeight: 'a elt -> string
      val listStyle: 'a elt -> string
      val listStyleImage: 'a elt -> string
      val listStylePosition: 'a elt -> string
      val listStyleType: 'a elt -> string
      val margin: 'a elt -> string
      val marginBottom: 'a elt -> string
      val marginLeft: 'a elt -> string
      val marginRight: 'a elt -> string
      val marginTop: 'a elt -> string
      val maxHeight: 'a elt -> string
      val maxWidth: 'a elt -> string
      val minHeight: 'a elt -> string
      val minWidth: 'a elt -> string
      val opacity: 'a elt -> string option
      val outline: 'a elt -> string
      val outlineColor: 'a elt -> string
      val outlineOffset: 'a elt -> string
      val outlineStyle: 'a elt -> string
      val outlineWidth: 'a elt -> string
      val overflow: 'a elt -> string
      val overflowX: 'a elt -> string
      val overflowY: 'a elt -> string
      val padding: 'a elt -> string
      val paddingBottom: 'a elt -> string
      val paddingLeft: 'a elt -> string
      val paddingRight: 'a elt -> string
      val paddingTop: 'a elt -> string
      val pageBreakAfter: 'a elt -> string
      val pageBreakBefore: 'a elt -> string
      val position: 'a elt -> string
      val right: 'a elt -> string
      val tableLayout: 'a elt -> string
      val textAlign: 'a elt -> string
      val textDecoration: 'a elt -> string
      val textIndent: 'a elt -> string
      val textTransform: 'a elt -> string
      val top: 'a elt -> string
      val verticalAlign: 'a elt -> string
      val visibility: 'a elt -> string
      val whiteSpace: 'a elt -> string
      val width: 'a elt -> string
      val wordSpacing: 'a elt -> string
      val zIndex: 'a elt -> string
    end

    (** Modify the CSS properties of DOM elements. *)
    module SetCss : sig
      val background: 'a elt -> string -> unit
      val backgroundAttachment: 'a elt -> string -> unit
      val backgroundColor: 'a elt -> string -> unit
      val backgroundImage: 'a elt -> string -> unit
      val backgroundPosition: 'a elt -> string -> unit
      val backgroundRepeat: 'a elt -> string -> unit
      val border: 'a elt -> string -> unit
      val borderBottom: 'a elt -> string -> unit
      val borderBottomColor: 'a elt -> string -> unit
      val borderBottomStyle: 'a elt -> string -> unit
      val borderBottomWidth: 'a elt -> string -> unit
      val borderCollapse: 'a elt -> string -> unit
      val borderColor: 'a elt -> string -> unit
      val borderLeft: 'a elt -> string -> unit
      val borderLeftColor: 'a elt -> string -> unit
      val borderLeftStyle: 'a elt -> string -> unit
      val borderLeftWidth: 'a elt -> string -> unit
      val borderRight: 'a elt -> string -> unit
      val borderRightColor: 'a elt -> string -> unit
      val borderRightStyle: 'a elt -> string -> unit
      val borderRightWidth: 'a elt -> string -> unit
      val borderSpacing: 'a elt -> string -> unit
      val borderStyle: 'a elt -> string -> unit
      val borderTop: 'a elt -> string -> unit
      val borderTopColor: 'a elt -> string -> unit
      val borderTopStyle: 'a elt -> string -> unit
      val borderTopWidth: 'a elt -> string -> unit
      val borderWidth: 'a elt -> string -> unit
      val bottom: 'a elt -> string -> unit
      val captionSide: 'a elt -> string -> unit
      val clear: 'a elt -> string -> unit
      val clip: 'a elt -> string -> unit
      val color: 'a elt -> string -> unit
      val content: 'a elt -> string -> unit
      val counterIncrement: 'a elt -> string -> unit
      val counterReset: 'a elt -> string -> unit
      val cssFloat: 'a elt -> string -> unit
      val cssText: 'a elt -> string -> unit
      val cursor: 'a elt -> string -> unit
      val direction: 'a elt -> string -> unit
      val display: 'a elt -> string -> unit
      val emptyCells: 'a elt -> string -> unit
      val font: 'a elt -> string -> unit
      val fontFamily: 'a elt -> string -> unit
      val fontSize: 'a elt -> string -> unit
      val fontStyle: 'a elt -> string -> unit
      val fontVariant: 'a elt -> string -> unit
      val fontWeight: 'a elt -> string -> unit
      val height: 'a elt -> string -> unit
      val left: 'a elt -> string -> unit
      val letterSpacing: 'a elt -> string -> unit
      val lineHeight: 'a elt -> string -> unit
      val listStyle: 'a elt -> string -> unit
      val listStyleImage: 'a elt -> string -> unit
      val listStylePosition: 'a elt -> string -> unit
      val listStyleType: 'a elt -> string -> unit
      val margin: 'a elt -> string -> unit
      val marginBottom: 'a elt -> string -> unit
      val marginLeft: 'a elt -> string -> unit
      val marginRight: 'a elt -> string -> unit
      val marginTop: 'a elt -> string -> unit
      val maxHeight: 'a elt -> string -> unit
      val maxWidth: 'a elt -> string -> unit
      val minHeight: 'a elt -> string -> unit
      val minWidth: 'a elt -> string -> unit
      val opacity: 'a elt -> string option -> unit
      val outline: 'a elt -> string -> unit
      val outlineColor: 'a elt -> string -> unit
      val outlineOffset: 'a elt -> string -> unit
      val outlineStyle: 'a elt -> string -> unit
      val outlineWidth: 'a elt -> string -> unit
      val overflow: 'a elt -> string -> unit
      val overflowX: 'a elt -> string -> unit
      val overflowY: 'a elt -> string -> unit
      val padding: 'a elt -> string -> unit
      val paddingBottom: 'a elt -> string -> unit
      val paddingLeft: 'a elt -> string -> unit
      val paddingRight: 'a elt -> string -> unit
      val paddingTop: 'a elt -> string -> unit
      val pageBreakAfter: 'a elt -> string -> unit
      val pageBreakBefore: 'a elt -> string -> unit
      val position: 'a elt -> string -> unit
      val right: 'a elt -> string -> unit
      val tableLayout: 'a elt -> string -> unit
      val textAlign: 'a elt -> string -> unit
      val textDecoration: 'a elt -> string -> unit
      val textIndent: 'a elt -> string -> unit
      val textTransform: 'a elt -> string -> unit
      val top: 'a elt -> string -> unit
      val verticalAlign: 'a elt -> string -> unit
      val visibility: 'a elt -> string -> unit
      val whiteSpace: 'a elt -> string -> unit
      val width: 'a elt -> string -> unit
      val wordSpacing: 'a elt -> string -> unit
      val zIndex: 'a elt -> string -> unit
    end
  end

end

