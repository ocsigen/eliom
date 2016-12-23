type +'a elt = 'a Eliom_content_html_raw.elt
type +'a attrib = 'a Eliom_content_html_raw.attrib
type uri = Eliom_content_xml.Xml.uri
type 'a form_param

(** Creation of {b F}unctional HTML5 content (copy-able but not
    referable, see also {% <<a_api|module Eliom_content>> %}). *)
module F = Eliom_content_html_f

(** Creation of HTML content with {b D}OM semantics (referable, see
    also {% <<a_api|module Eliom_content>> %}). *)
module D = Eliom_content_html_d

(** Creation of HTML content from client-side values.  This makes
    possible to insert in server side generated pages some nodes
    that will be computed on client side (for example reactive
    nodes).  *)
module C : sig

  (** {2 Content injection} *)

  (** See Eliom manual for more detail on {% <<a_manual
      chapter="clientserver-html" fragment="inject" | DOM &
      Client-values >>%}. *)

  (** [node e] is a server-side node corresponding to the
      client-side node [e] . [node e] can be used like any other
      server-side node.

      The implementation uses an initial placeholder node that is
      later replaced by the client node. By default, the placeholder
      node is [span]. The [~init] argument can be used to provide a
      custom placeholder node (e.g., one with the same tag as the
      client node). This can be useful in contexts where [span] is
      not allowed. *)
  val node :
    ?init:'a elt -> 'a elt Eliom_client_value.t -> 'a elt

(*XXXXXXXXXXXXXXXX
  val attr :
    ?init:'a attrib -> 'a attrib Eliom_client_value.t -> 'a attrib
*)

end

(** Node identifiers *)
module Id : sig

  (** The type of global HTML element identifier. *)
  type +'a id

  (** The function [new_elt_id ()] creates a new global HTML element
      identifier (see the Eliom manual for more information on {%
      <<a_manual project="eliom" chapter="clientserver-html"
      fragment="global"|global element>>%}).*)
  val new_elt_id: ?global:bool -> unit -> 'a id

  (** The function [create_named_elt ~id elt] create a copy of the
      element [elt] that will be sent to client with the reference
      [id]. *)
  val create_named_elt: id:'a id -> 'a elt -> 'a elt

  (** The function [create_named_elt elt] is equivalent to
      [create_named_elt ~id:(new_elt_id ()) elt]. *)
  val create_global_elt: 'a elt -> 'a elt

  (** [create_request_elt ?reset elt] creates a referable copy of
      [elt]. If [~reset = true] is provided (default: false), a new
      ID is created even if [elt] has an ID already. *)
  val create_request_elt: ?reset:bool -> 'a elt -> 'a elt

  (* XXX: This function must be hidden in documentation but hidden rest of
   * file *)
  val have_id: 'a id -> 'b elt -> bool

end


(** Creation of HTML content from shared reactive signals and data
    ({% <<a_api project="eliom" subproject="server"|module Eliom_shared>> %}).
    For the operations provided, see
    {% <<a_api project="tyxml" | module Html_sigs.T >> %}. *)
module R = Eliom_content_html_r

(** Type-safe custom data for HTML.
    See the {% <<a_manual chapter="clientserver-html"
    fragment="custom_data"|examples in the manual>> %}. *)
module Custom_data : sig

  (** Custom data with values of type ['a]. *)
  type 'a t

  (** Create a custom data field by providing string conversion functions.
      If the [default] is provided, calls to {% <<a_api project="eliom" subproject="client" |
      val Eliom_content.Html.Custom_data.get_dom>> %} return that instead of throwing an
      exception [Not_found].  *)
  val create : name:string -> ?default:'a -> to_string:('a -> string) -> of_string:(string -> 'a) -> unit -> 'a t

  (** Create a custom data from a Json-deriving type.  *)
  val create_json : name:string -> ?default:'a -> 'a Deriving_Json.t -> 'a t

  (** [attrib my_data value ] creates a HTML attribute for the custom-data
      type [my_data] with value [value] for injecting it into an a HTML tree
      ({% <<a_api | type Eliom_content.Html.elt >> %}). *)
  val attrib : 'a t -> 'a -> [> | `User_data ] attrib

end

(** {{:http://dev.w3.org/html5/html-xhtml-author-guide/}"Polyglot"}
    HTML printer. See
    {% <<a_api project="tyxml" | module Xml_sigs.Typed_pp >> %}. *)
module Printer : Xml_sigs.Typed_pp
  with type +'a elt := 'a elt
   and type doc := F.doc
