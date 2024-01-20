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

open Js_of_ocaml

(** XML building and deconstructing. Cf. {% <<a_api subproject="server" |
    module Eliom_content_core.Xml >> %}. *)

module Xml : sig
  module W :
    Xml_wrap.T
    with type 'a t = 'a
     and type 'a tlist = 'a list
     and type (-'a, 'b) ft = 'a -> 'b

  type uri = string

  val uri_of_string : uri -> string
  val string_of_uri : string -> uri
  val uri_of_fun : (unit -> string) -> uri

  type aname = string
  type attrib

  type caml_event_handler =
    | CE_registered_closure of string * Eliom_lib.poly
    (* 'a Js.t -> unit) client_value_server *)
    | CE_client_closure of (Dom_html.event Js.t -> unit) (* Client side-only *)
    | CE_client_closure_mouse of (Dom_html.mouseEvent Js.t -> unit)
      (* Client side-only *)
    | CE_client_closure_keyboard of (Dom_html.keyboardEvent Js.t -> unit)
      (* Client side-only *)
    | CE_client_closure_touch of (Dom_html.touchEvent Js.t -> unit)
      (* Client side-only *)
    | CE_call_service of
        ([`A | `Form_get | `Form_post]
        * (bool * string list) option
        * string option
        * Ocsigen_lib_base.poly)
          (* (unit -> bool) client_value *)
          option
          Eliom_lazy.request

  type internal_event_handler = Raw of string | Caml of caml_event_handler
  type event_handler = Dom_html.event Js.t -> unit
  type mouse_event_handler = Dom_html.mouseEvent Js.t -> unit
  type keyboard_event_handler = Dom_html.keyboardEvent Js.t -> unit
  type touch_event_handler = Dom_html.touchEvent Js.t -> unit
  type ename = string
  type elt
  type 'a wrap = 'a
  type 'a list_wrap = 'a list

  type econtent = private
    | Empty
    | Comment of string
    | EncodedPCDATA of string
    | PCDATA of string
    | Entity of string
    | Leaf of ename * attrib list
    | Node of ename * attrib list * elt list

  (**/**)

  val internal_event_handler_of_service :
     ([`A | `Form_get | `Form_post]
     * (bool * string list) option
     * string option
     * Eliom_lib.poly)
       option
       Eliom_lazy.request
    -> internal_event_handler

  type separator = Space | Comma

  type acontent = private
    | AFloat of float
    | AInt of int
    | AStr of string
    | AStrL of separator * string list

  val acontent : attrib -> acontent

  type racontent =
    | RA of acontent
    | RAReact of acontent option React.signal
    | RACamlEventHandler of caml_event_handler
    | RALazyStr of string Eliom_lazy.request
    | RALazyStrL of separator * string Eliom_lazy.request list
    | RAClient of string * attrib option * Eliom_lib.poly

  (* attrib Eliom_client_value.t *)
  val racontent : attrib -> racontent
  val aname : attrib -> aname
  val float_attrib : aname -> float -> attrib
  val int_attrib : aname -> int -> attrib
  val string_attrib : aname -> string -> attrib
  val space_sep_attrib : aname -> string list -> attrib
  val comma_sep_attrib : aname -> string list -> attrib
  val internal_event_handler_attrib : aname -> internal_event_handler -> attrib
  val event_handler_attrib : aname -> event_handler -> attrib
  val mouse_event_handler_attrib : aname -> mouse_event_handler -> attrib
  val keyboard_event_handler_attrib : aname -> keyboard_event_handler -> attrib
  val touch_event_handler_attrib : aname -> touch_event_handler -> attrib
  val uri_attrib : aname -> uri -> attrib
  val uris_attrib : aname -> uri list -> attrib
  val content : elt -> econtent
  val pcdata : string -> elt
  val encodedpcdata : string -> elt
  val entity : string -> elt
  val empty : unit -> elt
  val comment : string -> elt
  val leaf : ?a:attrib list -> ename -> elt
  val node : ?a:attrib list -> ename -> elt list -> elt
  val lazy_node : ?a:attrib list -> ename -> elt list Eliom_lazy.request -> elt
  val cdata : string -> elt
  val cdata_script : string -> elt
  val cdata_style : string -> elt

  type node_id = NoId | ProcessId of string | RequestId of string

  val string_of_node_id : node_id -> string
  val make : ?id:node_id -> econtent -> elt
  val make_dom : ?id:node_id -> Dom.node Js.t -> elt
  val make_lazy : ?id:node_id -> elt lazy_t -> elt
  val force_lazy : elt -> unit
  val make_react : ?id:node_id -> elt React.signal -> elt
  val make_process_node : ?id:string -> elt -> elt
  val make_request_node : ?reset:bool -> elt -> elt
  val get_node_id : elt -> node_id

  type node =
    | DomNode of Dom.node Js.t
    | TyXMLNode of econtent
    | ReactNode of elt React.signal
    | ReactChildren of econtent * elt ReactiveData.RList.t

  val get_node : elt -> node
  val set_dom_node : elt -> Dom.node Js.t -> unit
  val set_classes_of_elt : elt -> elt
end

module Xml_wed : sig
  include
    Xml_sigs.T
    with module W = Js_of_ocaml_tyxml.Tyxml_js.Wrap
     and type elt = Xml.elt
     and type aname = Xml.aname
     and type attrib = Xml.attrib
     and type uri = Xml.uri
     and type 'a W.t = 'a React.signal
     and type 'a W.tlist = 'a ReactiveData.RList.t
     and type ('a, 'b) W.ft = 'a -> 'b

  val float_attrib : aname -> float React.S.t -> attrib
  val int_attrib : aname -> int React.S.t -> attrib
  val string_attrib : aname -> string React.S.t -> attrib
  val space_sep_attrib : aname -> string list React.S.t -> attrib
  val comma_sep_attrib : aname -> string list React.S.t -> attrib
  val uri_attrib : aname -> uri React.S.t -> attrib
  val uris_attrib : aname -> uri list React.S.t -> attrib
  val node : ?a:attrib list -> string -> elt list_wrap -> elt
end

(** Building SVG tree. *)
module Svg : sig
  (** See the Eliom manual for more information on{% <<a_manual
      chapter="clientserver-html" fragment="unique"| dom semantics vs. functional
      semantics>> %} for SVG tree manipulated by client/server
      application. *)

  type +'a elt
  type 'a wrap = 'a
  type 'a list_wrap = 'a list
  type +'a attrib
  type uri = Xml.uri

  (** {2 Functional semantics} *)

  (** Typed interface for building valid SVG tree (functional semantics).
      See {% <<a_api project="tyxml" | module type Svg_sigs.T >> %}. *)
  module F : sig
    module Raw :
      Svg_sigs.Make(Xml).T
      with type +'a elt = 'a elt
       and type +'a attrib = 'a attrib

    include module type of Raw
    (** See {% <<a_api project="tyxml" | module type Svg_sigs.T >> %}. *)
  end

  (** {2 DOM semantics} *)

  (** Typed interface for building valid SVG tree (DOM semantics). See
      {% <<a_api project="tyxml" | module type Svg_sigs.T >> %}. *)
  module D : sig
    module Raw :
      Svg_sigs.Make(Xml).T
      with type +'a elt = 'a elt
       and type +'a attrib = 'a attrib

    include module type of Raw
    (** See {% <<a_api project="tyxml" | module type Svg_sigs.T >> %}. *)
  end

  (** {2 Reactive DOM} *)

  (** Typed interface for building valid reactive SVG tree. *)
  module R : sig
    val node : 'a elt React.signal -> 'a elt
    (** The function [node s] creates an SVG [elt] from a signal [s].
        The resulting SVG [elt] can then be used like any other SVG
        [elt]. *)

    module Raw :
      Svg_sigs.Make(Xml_wed).T
      with type +'a elt = 'a elt
       and type +'a attrib = 'a attrib

    include module type of Raw
    (** See {% <<a_api project="tyxml" | module type Svg_sigs.T >> %}. *)
  end

  (** {2 Global node} *)

  module Id : sig
    type +'a id
    (** The type of global SVG element identifier. *)

    val new_elt_id : ?global:bool -> unit -> 'a id
    (** See {!Eliom_content.Html.Id.new_elt_id} *)

    val create_named_elt : id:'a id -> 'a elt -> 'a elt
    (** See {!Eliom_content.Html.Id.create_named_elt} *)

    val create_global_elt : 'a elt -> 'a elt
    (** See {!Eliom_content.Html.Id.create_global_elt} *)

    val create_request_elt : ?reset:bool -> 'a elt -> 'a elt
    (** See {!Eliom_content.Html.Id.create_request_elt} *)

    (**/**)

    val string_of_id : 'a id -> string
  end

  module Of_dom : sig
    val of_element : Dom_html.element Js.t -> 'a elt
  end
end

(** Building Html tree. *)
module Html : sig
  (** See the Eliom manual for more information on {% <<a_manual
      chapter="clientserver-html" fragment="unique"| dom semantics vs. functional
      semantics>> %} for SVG tree manipulated by client/server
      application. *)

  type +'a elt
  type 'a wrap = 'a
  type 'a list_wrap = 'a list
  type +'a attrib
  type uri = Xml.uri

  (**/**)

  module Ev' (A : sig
      type 'a attrib

      module Unsafe : sig
        val string_attrib : string -> string -> 'a attrib
      end
    end) : sig
    val a_onabort : string -> [> `OnAbort] A.attrib
    val a_onafterprint : string -> [> `OnAfterPrint] A.attrib
    val a_onbeforeprint : string -> [> `OnBeforePrint] A.attrib
    val a_onbeforeunload : string -> [> `OnBeforeUnload] A.attrib
    val a_onblur : string -> [> `OnBlur] A.attrib
    val a_oncanplay : string -> [> `OnCanPlay] A.attrib
    val a_oncanplaythrough : string -> [> `OnCanPlayThrough] A.attrib
    val a_onchange : string -> [> `OnChange] A.attrib
    val a_onclose : string -> [> `OnClose] A.attrib
    val a_ondurationchange : string -> [> `OnDurationChange] A.attrib
    val a_onemptied : string -> [> `OnEmptied] A.attrib
    val a_onended : string -> [> `OnEnded] A.attrib
    val a_onerror : string -> [> `OnError] A.attrib
    val a_onfocus : string -> [> `OnFocus] A.attrib
    val a_onformchange : string -> [> `OnFormChange] A.attrib
    val a_onforminput : string -> [> `OnFormInput] A.attrib
    val a_onhashchange : string -> [> `OnHashChange] A.attrib
    val a_oninput : string -> [> `OnInput] A.attrib
    val a_oninvalid : string -> [> `OnInvalid] A.attrib
    val a_onmousewheel : string -> [> `OnMouseWheel] A.attrib
    val a_onoffline : string -> [> `OnOffLine] A.attrib
    val a_ononline : string -> [> `OnOnLine] A.attrib
    val a_onpause : string -> [> `OnPause] A.attrib
    val a_onplay : string -> [> `OnPlay] A.attrib
    val a_onplaying : string -> [> `OnPlaying] A.attrib
    val a_onpagehide : string -> [> `OnPageHide] A.attrib
    val a_onpageshow : string -> [> `OnPageShow] A.attrib
    val a_onpopstate : string -> [> `OnPopState] A.attrib
    val a_onprogress : string -> [> `OnProgress] A.attrib
    val a_onratechange : string -> [> `OnRateChange] A.attrib
    val a_onreadystatechange : string -> [> `OnReadyStateChange] A.attrib
    val a_onredo : string -> [> `OnRedo] A.attrib
    val a_onresize : string -> [> `OnResize] A.attrib
    val a_onscroll : string -> [> `OnScroll] A.attrib
    val a_onseeked : string -> [> `OnSeeked] A.attrib
    val a_onseeking : string -> [> `OnSeeking] A.attrib
    val a_onselect : string -> [> `OnSelect] A.attrib
    val a_onshow : string -> [> `OnShow] A.attrib
    val a_onstalled : string -> [> `OnStalled] A.attrib
    val a_onstorage : string -> [> `OnStorage] A.attrib
    val a_onsubmit : string -> [> `OnSubmit] A.attrib
    val a_onsuspend : string -> [> `OnSuspend] A.attrib
    val a_ontimeupdate : string -> [> `OnTimeUpdate] A.attrib
    val a_onundo : string -> [> `OnUndo] A.attrib
    val a_onunload : string -> [> `OnUnload] A.attrib
    val a_onvolumechange : string -> [> `OnVolumeChange] A.attrib
    val a_onwaiting : string -> [> `OnWaiting] A.attrib
    val a_onload : string -> [> `OnLoad] A.attrib
    val a_onloadeddata : string -> [> `OnLoadedData] A.attrib
    val a_onloadedmetadata : string -> [> `OnLoadedMetaData] A.attrib
    val a_onloadstart : string -> [> `OnLoadStart] A.attrib
    val a_onmessage : string -> [> `OnMessage] A.attrib
    val a_onclick : string -> [> `OnClick] A.attrib
    val a_oncontextmenu : string -> [> `OnContextMenu] A.attrib
    val a_ondblclick : string -> [> `OnDblClick] A.attrib
    val a_ondrag : string -> [> `OnDrag] A.attrib
    val a_ondragend : string -> [> `OnDragEnd] A.attrib
    val a_ondragenter : string -> [> `OnDragEnter] A.attrib
    val a_ondragleave : string -> [> `OnDragLeave] A.attrib
    val a_ondragover : string -> [> `OnDragOver] A.attrib
    val a_ondragstart : string -> [> `OnDragStart] A.attrib
    val a_ondrop : string -> [> `OnDrop] A.attrib
    val a_onmousedown : string -> [> `OnMouseDown] A.attrib
    val a_onmouseup : string -> [> `OnMouseUp] A.attrib
    val a_onmouseover : string -> [> `OnMouseOver] A.attrib
    val a_onmousemove : string -> [> `OnMouseMove] A.attrib
    val a_onmouseout : string -> [> `OnMouseOut] A.attrib
    val a_ontouchstart : string -> [> `OnTouchStart] A.attrib
    val a_ontouchend : string -> [> `OnTouchEnd] A.attrib
    val a_ontouchmove : string -> [> `OnTouchMove] A.attrib
    val a_ontouchcancel : string -> [> `OnTouchCancel] A.attrib
    val a_onkeypress : string -> [> `OnKeyPress] A.attrib
    val a_onkeydown : string -> [> `OnKeyDown] A.attrib
    val a_onkeyup : string -> [> `OnKeyUp] A.attrib
  end

  (**/**)

  (** Typed interface for building valid HTML5 tree (functional semantics).
      See {% <<a_api project="tyxml" | module type Html_sigs.T >> %}. *)
  module F : sig
    (**/**)

    module Raw' :
      Html_sigs.Make(Xml)(Svg.F.Raw).T
      with type +'a elt = 'a elt
       and type +'a attrib = 'a attrib

    (**/**)

    module Raw : sig
      include module type of Raw'
      include module type of Ev' (Raw')
    end

    include module type of Raw'
    (** See {% <<a_api project="tyxml" | module type Html_sigs.T >> %}. *)

    (*BB TODO Hide untyped [input]. *)

    (**/**)

    type ('a, 'b, 'c) lazy_star =
      ?a:'a attrib list -> 'b elt list Eliom_lazy.request -> 'c elt

    val lazy_form :
      ( [< Html_types.form_attrib]
        , [< Html_types.form_content_fun]
        , [> Html_types.form] )
        lazy_star
  end

  (** Typed interface for building valid HTML5 tree (DOM semantics). See
      {% <<a_api project="tyxml" | module type Html_sigs.T >> %}. *)
  module D : sig
    (**/**)

    module Raw' :
      Html_sigs.Make(Xml)(Svg.D.Raw).T
      with type +'a elt = 'a elt
       and type +'a attrib = 'a attrib

    (**/**)

    module Raw : sig
      include module type of Raw'
      include module type of Ev' (Raw')
    end

    include module type of Raw'

    (**/**)

    type ('a, 'b, 'c) lazy_star =
      ?a:'a attrib list -> 'b elt list Eliom_lazy.request -> 'c elt

    val lazy_form :
      ( [< Html_types.form_attrib]
        , [< Html_types.form_content_fun]
        , [> Html_types.form] )
        lazy_star
  end

  (** Typed interface for building valid HTML5 tree from
      {{:http://erratique.ch/software/react} React} signals.
      HTML5's trees are automatically updated whenever
      corresponding signals change.

      {% <<a_api project="tyxml" | module type Html_sigs.T >> %}. *)

  module R : sig
    val node : 'a elt React.signal -> 'a elt
    (** the function [node s] create an HTML5 [elt] from a signal [s].
    The resulting HTML5 [elt] can then be used like any other HTML5 [elt] *)

    val filter_attrib : 'a attrib -> bool React.signal -> 'a attrib

    module Raw :
      Html_sigs.Make(Xml_wed)(Svg.R.Raw).T
      with type +'a elt = 'a elt
       and type +'a attrib = 'a attrib

    include module type of Raw
  end

  (** Node identifiers *)
  module Id : sig
    type +'a id
    (** The type of global HTML5 element identifier. *)

    val new_elt_id : ?global:bool -> unit -> 'a id
    (** The function [new_elt_id ()] creates a new HTML5 element
        identifier. (see the Eliom manual for more information on {%
        <<a_manual project="eliom" chapter="clientserver-html"
        fragment="global"|global element>>%}).*)

    val create_named_elt : id:'a id -> 'a elt -> 'a elt
    (** The function [create_named_elt ~id elt] create a copy of the
        element [elt] that will be accessible through the name [id]. *)

    val create_global_elt : 'a elt -> 'a elt
    (** The function [create_named_elt elt] is equivalent to
        [create_named_elt ~id:(new_elt_id ()) elt]. *)

    val create_request_elt : ?reset:bool -> 'a elt -> 'a elt
    (** See {!Eliom_content.Svg.Id.create_request_elt} *)

    (**/**)

    val string_of_id : 'a id -> string
  end

  (** Type-safe custom data for HTML5. See the {% <<a_manual chapter="clientserver-html"
      fragment="custom_data"|examples in the manual>> %}. *)
  module Custom_data : sig
    type 'a t
    (** Custom data with values of type ['a]. *)

    val create :
       name:string
      -> ?default:'a
      -> to_string:('a -> string)
      -> of_string:(string -> 'a)
      -> unit
      -> 'a t
    (** Create a custom data field by providing string conversion functions.
        If the [default] is provided, calls to {% <<a_api project="eliom" subproject="client" |
        val Eliom_content.Html.Custom_data.get_dom>> %} return that instead of throwing an
        exception [Not_found].  *)

    val create_json : name:string -> ?default:'a -> 'a Deriving_Json.t -> 'a t
    (** Create a custom data from a Json-deriving type.  *)

    val attrib : 'a t -> 'a -> [> `User_data] attrib
    (** [attrib my_data value ] creates a HTML5 attribute for the custom-data
        type [my_data] with value [value] for injecting it into an a HTML5 tree
        ({% <<a_api | type Eliom_content.Html.elt >> %}). *)

    val get_dom : Dom_html.element Js.t -> 'a t -> 'a
    (** [get_dom element custom_data] gets the [custom_data] from a JavaScript [element]
        ({% <<a_api project="js_of_ocaml"|class type Js_of_ocaml.Dom_html.element>> %}).
        @return The value encoded in the respective custom data attribute of [element], or the default value, if any.
        @raise Not_found if the element does not contain the respective custom
          data attribute and the [custom_data] was created without [default].
      *)

    val set_dom : Dom_html.element Js.t -> 'a t -> 'a -> unit
    (** [set_dom element custom_data value] sets the custom data attribute for
        [custom_data] of an JavaScript [element]
        ({% <<a_api project="js_of_ocaml"|class type Js_of_ocaml.Dom_html.element>> %})
        to [value]. *)
  end

  module Of_dom : Js_of_ocaml_tyxml.Tyxml_cast_sigs.OF with type 'a elt = 'a elt
  (** Conversion of Javascript DOM elements to HTML5 elts (with DOM semantics of course).
      One conversion function per source type (stressed by the [of_] prefix). *)

  (**/**)

  val set_classes_of_elt : 'a elt -> 'a elt
end
