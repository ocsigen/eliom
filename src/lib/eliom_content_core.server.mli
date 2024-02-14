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

open Js_of_ocaml

module Xml : sig
  include
    Xml_sigs.Iterable
    with type 'a wrap = 'a
     and type 'a list_wrap = 'a list
     and type event_handler = (Dom_html.event Js.t -> unit) Eliom_client_value.t
     and type mouse_event_handler =
      (Dom_html.mouseEvent Js.t -> unit) Eliom_client_value.t
     and type keyboard_event_handler =
      (Dom_html.keyboardEvent Js.t -> unit) Eliom_client_value.t
     and type touch_event_handler =
      (Dom_html.touchEvent Js.t -> unit) Eliom_client_value.t

  type caml_event_handler

  (**/**)

  val make_process_node : ?id:string -> elt -> elt
  val make_request_node : ?reset:bool -> elt -> elt
  val uri_of_fun : (unit -> string) -> uri

  (* Building ref tree. *)
  type node_id

  val get_node_id : elt -> node_id
  val make_event_handler_table : elt -> Eliom_runtime.RawXML.event_handler_table
  val make_client_attrib_table : elt -> Eliom_runtime.RawXML.client_attrib_table

  type internal_event_handler = Raw of string | Caml of caml_event_handler

  val internal_event_handler_attrib : aname -> internal_event_handler -> attrib

  val internal_event_handler_of_service :
     ([`A | `Form_get | `Form_post]
     * (bool * string list) option
     * string option
     * Eliom_lib.poly)
       option
       Eliom_lazy.request
    -> internal_event_handler

  val caml_event_handler :
     (Dom_html.event Js.t -> unit) Eliom_client_value.t
    -> caml_event_handler

  type racontent =
    | RA of acontent
    | RAReact of acontent option React.signal
    | RACamlEventHandler of caml_event_handler
    | RALazyStr of string Eliom_lazy.request
    | RALazyStrL of separator * string Eliom_lazy.request list
    | RAClient of string * attrib option * Eliom_lib.poly
  (* attrib client_value *)

  val racontent : attrib -> racontent
  val lazy_node : ?a:attrib list -> ename -> elt list Eliom_lazy.request -> elt

  (**/**)

  val wrap : elt -> 'a -> 'a Eliom_wrap.wrapped_value
  val client_attrib : ?init:attrib -> attrib Eliom_client_value.t -> attrib
end

module Svg : sig
  type 'a wrap = 'a
  type 'a list_wrap = 'a list
  type +'a elt
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
    val a_onactivate : string -> [> `OnActivate] A.attrib
    val a_onbegin : string -> [> `OnBegin] A.attrib
    val a_onend : string -> [> `OnEnd] A.attrib
    val a_onerror : string -> [> `OnError] A.attrib
    val a_onfocusin : string -> [> `OnFocusIn] A.attrib
    val a_onfocusout : string -> [> `OnFocusOut] A.attrib

    val a_onload : string -> [> `OnLoad] A.attrib
    [@@ocaml.deprecated "Removed in SVG2"]
    (** @deprecated Removed in SVG2 *)

    val a_onrepeat : string -> [> `OnRepeat] A.attrib
    val a_onresize : string -> [> `OnResize] A.attrib
    val a_onscroll : string -> [> `OnScroll] A.attrib
    val a_onunload : string -> [> `OnUnload] A.attrib
    val a_onzoom : string -> [> `OnZoom] A.attrib
    val a_onclick : string -> [> `OnClick] A.attrib
    val a_onmousedown : string -> [> `OnMouseDown] A.attrib
    val a_onmouseup : string -> [> `OnMouseUp] A.attrib
    val a_onmouseover : string -> [> `OnMouseOver] A.attrib
    val a_onmouseout : string -> [> `OnMouseOut] A.attrib
    val a_onmousemove : string -> [> `OnMouseMove] A.attrib
    val a_ontouchstart : string -> [> `OnTouchStart] A.attrib
    val a_ontouchend : string -> [> `OnTouchEnd] A.attrib
    val a_ontouchmove : string -> [> `OnTouchMove] A.attrib
    val a_ontouchcancel : string -> [> `OnTouchCancel] A.attrib
  end

  (**/**)

  module F : sig
    (**/**)

    module Raw' :
      Svg_sigs.Make(Xml).T
      with type +'a elt = 'a elt
       and type +'a attrib = 'a attrib

    (**/**)

    module Raw : sig
      include module type of Raw'
      include module type of Ev' (Raw')
    end

    include module type of Raw'
  end

  module D : sig
    (**/**)

    module Raw' :
      Svg_sigs.Make(Xml).T
      with type +'a elt = 'a elt
       and type +'a attrib = 'a attrib

    (**/**)

    module Raw : sig
      include module type of Raw'
      include module type of Ev' (Raw')
    end

    include module type of Raw'

    val client_attrib :
       ?init:'a attrib
      -> 'a attrib Eliom_client_value.t
      -> 'a attrib
  end

  module Make
      (Xml : Xml_sigs.T with type elt = Xml.elt and type attrib = Xml.attrib)
      (_ : Svg_sigs.Wrapped_functions with module Xml = Xml) :
    Svg_sigs.Make(Xml).T
    with type +'a elt = 'a elt
     and type +'a attrib = 'a attrib

  module Id : sig
    type +'a id

    val new_elt_id : ?global:bool -> unit -> 'a id
    val create_named_elt : id:'a id -> 'a elt -> 'a elt
    val create_global_elt : 'a elt -> 'a elt
    val create_request_elt : ?reset:bool -> 'a elt -> 'a elt
  end

  module Printer :
    Xml_sigs.Typed_pp with type +'a elt := 'a F.elt and type doc := F.doc
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

  module F : sig
    (**/**)

    module Raw' :
      Html_sigs.Make(Xml)(Svg.F.Raw').T
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

  module D : sig
    (**/**)

    module Raw' :
      Html_sigs.Make(Xml)(Svg.F.Raw').T
      with type +'a elt = 'a elt
       and type +'a attrib = 'a attrib

    (**/**)

    module Raw : sig
      include module type of Raw'
      include module type of Ev' (Raw')
    end

    include module type of Raw'

    val client_attrib :
       ?init:'a attrib
      -> 'a attrib Eliom_client_value.t
      -> 'a attrib

    (**/**)

    type ('a, 'b, 'c) lazy_star =
      ?a:'a attrib list -> 'b elt list Eliom_lazy.request -> 'c elt

    val lazy_form :
      ( [< Html_types.form_attrib]
        , [< Html_types.form_content_fun]
        , [> Html_types.form] )
        lazy_star
  end

  module Make
      (Xml : Xml_sigs.T with type elt = Xml.elt and type attrib = Xml.attrib)
      (_ : Html_sigs.Wrapped_functions with module Xml = Xml)
      (Svg : Svg_sigs.T with module Xml := Xml) :
    Html_sigs.Make(Xml)(Svg).T
    with type +'a elt = 'a elt
     and type +'a attrib = 'a attrib

  module Id : sig
    type +'a id

    val new_elt_id : ?global:bool -> unit -> 'a id
    val create_named_elt : id:'a id -> 'a elt -> 'a elt
    val create_global_elt : 'a elt -> 'a elt
    val create_request_elt : ?reset:bool -> 'a elt -> 'a elt

    (**/**)

    val have_id : 'a id -> 'b elt -> bool
  end

  module Custom_data : sig
    type 'a t

    val create :
       name:string
      -> ?default:'a
      -> to_string:('a -> string)
      -> of_string:(string -> 'a)
      -> unit
      -> 'a t

    val create_json : name:string -> ?default:'a -> 'a Deriving_Json.t -> 'a t
    val attrib : 'a t -> 'a -> [> `User_data] attrib
  end

  module Printer :
    Xml_sigs.Typed_pp with type +'a elt := 'a F.elt and type doc := F.doc
end
