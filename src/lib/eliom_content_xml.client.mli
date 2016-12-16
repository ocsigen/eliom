module Xml : sig
  include Xml_sigs.NoWrap
    with type uri = string
     and type event_handler = Dom_html.event Js.t -> unit
     and type mouse_event_handler = Dom_html.mouseEvent Js.t -> unit
     and type keyboard_event_handler = Dom_html.keyboardEvent Js.t -> unit

  type caml_event_handler =
    | CE_registered_closure of string * Eliom_lib.poly
                                    (* 'a Js.t -> unit) client_value_server *)
    | CE_client_closure of
        (Dom_html.event Js.t -> unit) (* Client side-only *)
    | CE_client_closure_mouse of
        (Dom_html.mouseEvent Js.t -> unit) (* Client side-only *)
    | CE_client_closure_keyboard of
        (Dom_html.keyboardEvent Js.t -> unit) (* Client side-only *)
    | CE_call_service of
        ( [ `A | `Form_get | `Form_post] *
          ((bool * string list) option) *
          string option *
          Ocsigen_lib.poly (* (unit -> bool) client_value *)
        ) option Eliom_lazy.request

  type internal_event_handler = Raw of string | Caml of caml_event_handler
  val internal_event_handler_attrib : aname -> internal_event_handler -> attrib
  val internal_event_handler_of_service :
    (  [ `A | `Form_get | `Form_post]
       * (bool * string list) option
       * string option
       * Eliom_lib.poly
    )  option Eliom_lazy.request
    -> internal_event_handler

  type econtent =
    | Empty
    | Comment of string
    | EncodedPCDATA of string
    | PCDATA of string
    | Entity of string
    | Leaf of ename * attrib list
    | Node of ename * attrib list * elt list

  type separator = Space | Comma
  type acontent =
    | AFloat of float
    | AInt of int
    | AStr of string
    | AStrL of separator * string list

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

  val attrib : aname -> racontent -> attrib

  type node =
    | DomNode of Dom.node Js.t
    | TyXMLNode of econtent
    | ReactNode of elt React.signal
    | ReactChildren of econtent * elt ReactiveData.RList.t

  val get_node : elt -> node
  val set_dom_node : elt -> Dom.node Js.t -> unit

  type node_id =
    | NoId
    | ProcessId of string
    | RequestId of string

  val string_of_node_id : node_id -> string
  val make : ?id:node_id -> econtent -> elt
  val make_dom : ?id:node_id -> Dom.node Js.t -> elt
  val make_lazy : ?id:node_id -> elt lazy_t -> elt
  val make_react : ?id:node_id -> elt React.signal -> elt
  val get_node_id : elt -> node_id

  val make_node_name : ?global:bool -> unit -> string

  val make_process_node : ?id:string -> elt -> elt
  val make_request_node : ?reset:bool -> elt -> elt

  val set_classes_of_elt : elt -> elt

  val content : elt -> econtent

  val lazy_node :
    ?a:(attrib list) -> ename -> elt list Eliom_lazy.request -> elt
  val uri_of_fun : (unit -> string) -> uri
  val force_lazy : elt -> unit
end
module Xml_wed : sig
  include Xml_sigs.T
    with type 'a W.t = 'a React.signal
     and type 'a W.tlist = 'a ReactiveData.RList.t
     and type ('a,'b) W.ft = 'a -> 'b
     and type uri = string
     and type event_handler = Dom_html.event Js.t -> unit
     and type mouse_event_handler = Dom_html.mouseEvent Js.t -> unit
     and type keyboard_event_handler = Dom_html.keyboardEvent Js.t -> unit
     and type elt = Xml.elt
     and type attrib = Xml.attrib
end
