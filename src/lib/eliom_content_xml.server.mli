module Xml : sig
  include Xml_sigs.NoWrap
    with type event_handler =
           (Dom_html.event Js.t -> unit) Eliom_client_value.t
     and type mouse_event_handler =
           (Dom_html.mouseEvent Js.t -> unit) Eliom_client_value.t
     and type keyboard_event_handler =
           (Dom_html.keyboardEvent Js.t -> unit) Eliom_client_value.t

  type econtent =
    | Empty
    | Comment of string
    | EncodedPCDATA of string
    | PCDATA of string
    | Entity of string
    | Leaf of ename * attrib list
    | Node of ename * attrib list * elt list
  val content : elt -> econtent

  type separator = Space | Comma
  type acontent =
    | AFloat of float
    | AInt of int
    | AStr of string
    | AStrL of separator * string list

  val aname : attrib -> aname
  val acontent : attrib -> acontent

  val make : econtent -> elt
  val make_lazy : econtent Eliom_lazy.request -> elt

  type node_id =
    | NoId
    | ProcessId of string
    | RequestId of string

  val get_node_id : elt -> node_id

  val make_node_name : global:bool -> unit -> string

  val make_process_node : ?id:string -> elt -> elt
  val make_request_node : ?reset:bool -> elt -> elt

  val uri_of_fun : (unit -> string) -> uri

  val lazy_node :
    ?a:(attrib list) -> ename -> elt list Eliom_lazy.request -> elt

  val client_attrib : ?init:attrib -> attrib Eliom_client_value.t -> attrib

  type caml_event_handler
  type internal_event_handler = Raw of string | Caml of caml_event_handler
  val internal_event_handler_attrib : aname -> internal_event_handler -> attrib
  val internal_event_handler_of_service :
    (  [ `A | `Form_get | `Form_post]
       * (bool * string list) option
       * string option
       * Eliom_lib.poly
    )  option Eliom_lazy.request
    -> internal_event_handler

  val wrap : elt -> 'a -> 'a Eliom_wrap.wrapped_value
  val make_event_handler_table : elt -> Eliom_runtime.RawXML.event_handler_table
  val make_client_attrib_table : elt -> Eliom_runtime.RawXML.client_attrib_table
end
