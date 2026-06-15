
# Module `Eliom_client_core`

```ocaml
module Xml = Eliom_content_core.Xml
```
```ocaml
val section : Logs.src
```
```ocaml
val create_buffer : 
  unit ->
  ('a -> unit) * (unit -> 'a list) * (unit -> 'a list) * (unit -> unit)
```
```ocaml
module Client_closure : sig ... end
```
```ocaml
module Client_value : sig ... end
```
```ocaml
val middleClick : Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t -> bool
```
```ocaml
module Injection : sig ... end
```
```ocaml
type compilation_unit_global_data = {
  mutable server_section : Eliom_runtime.client_value_datum array list;
  mutable client_section : Eliom_runtime.injection_datum array list;
}
```
```ocaml
val global_data : compilation_unit_global_data Eliom_lib.String_map.t ref
```
```ocaml
val do_next_server_section_data : 
  compilation_unit_id:Eliom_lib.String_map.key ->
  unit
```
```ocaml
val do_next_client_section_data : 
  compilation_unit_id:Eliom_lib.String_map.key ->
  unit
```
```ocaml
val register_unwrapped_elt : Xml.elt -> unit
```
```ocaml
val force_unwrapped_elts : unit -> unit
```
```ocaml
val register_process_node : 
  Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t ->
  Js_of_ocaml.Dom.node Js_of_ocaml.Js.t ->
  unit
```
```ocaml
val find_process_node : 
  Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t ->
  Js_of_ocaml.Dom.node Js_of_ocaml.Js.t Js_of_ocaml.Js.optdef
```
```ocaml
val registered_process_node : Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t -> bool
```
```ocaml
val getElementById : string -> Js_of_ocaml.Dom.node Js_of_ocaml.Js.t
```
```ocaml
val register_request_node : 
  Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t ->
  Js_of_ocaml.Dom.node Js_of_ocaml.Js.t ->
  unit
```
```ocaml
val find_request_node : 
  Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t ->
  Js_of_ocaml.Dom.node Js_of_ocaml.Js.t Js_of_ocaml.Js.optdef
```
```ocaml
val reset_request_nodes : unit -> unit
```
```ocaml
val load_mutex : Lwt_mutex.t
```
```ocaml
val in_onload : unit -> bool
```
```ocaml
val broadcast_load_end : unit -> unit
```
```ocaml
val wait_load_end : unit -> unit Lwt.t
```
```ocaml
val set_loading_phase : unit -> unit
```
```ocaml
val change_page_uri_ : 
  (?cookies_info:(bool * string list) -> ?tmpl:string -> string -> unit) ref
```
```ocaml
val change_page_get_form_ : 
  (?cookies_info:(bool * string list) ->
    ?tmpl:string ->
    Js_of_ocaml.Dom_html.formElement Js_of_ocaml.Js.t ->
    string ->
    unit)
    ref
```
```ocaml
val change_page_post_form_ : 
  (?cookies_info:(bool * string list) ->
    ?tmpl:string ->
    Js_of_ocaml.Dom_html.formElement Js_of_ocaml.Js.t ->
    string ->
    unit)
    ref
```
```ocaml
type client_form_handler =
  Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t ->
  bool Lwt.t
```
```ocaml
val raw_a_handler : 
  'a Js_of_ocaml__Js.t ->
  (bool * string list) option ->
  string option ->
  Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t ->
  bool
```
```ocaml
val raw_form_handler : 
  Js_of_ocaml.Dom_html.formElement Js_of_ocaml.Js.t ->
  [< `Form_get | `Form_post ] ->
  (bool * string list) option ->
  string option ->
  'a ->
  ('a -> bool Lwt.t) ->
  bool
```
```ocaml
val raw_event_handler : 
  'a ->
  Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t ->
  bool
```
```ocaml
val closure_name_prefix : string
```
```ocaml
val closure_name_prefix_len : int
```
```ocaml
val reify_caml_event : 
  string ->
  Js_of_ocaml__Dom_html.element Js_of_ocaml.Js.t ->
  Xml.caml_event_handler ->
  string
  * [> `Keyboard of Js_of_ocaml.Dom_html.keyboardEvent Js_of_ocaml.Js.t -> bool
    | `Mouse of Js_of_ocaml.Dom_html.mouseEvent Js_of_ocaml.Js.t -> bool
    | `Other of Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t -> bool
    | `Touch of Js_of_ocaml.Dom_html.touchEvent Js_of_ocaml.Js.t -> bool ]
```
```ocaml
val register_event_handler : 
  Js_of_ocaml__Dom_html.element Js_of_ocaml.Js.t ->
  (string * Xml.caml_event_handler) ->
  unit
```
```ocaml
val flush_load_script : unit -> unit
```
```ocaml
val rebuild_attrib_val : 
  Xml.acontent ->
  Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t
```
```ocaml
val class_list_of_racontent : Xml.acontent -> string list
```
```ocaml
val class_list_of_racontent_o : Xml.acontent option -> string list
```
```ocaml
val rebuild_class_list : 'a list -> 'a list -> 'a list -> 'a list
```
```ocaml
val rebuild_class_string : 
  string list ->
  string list ->
  string list ->
  Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t
```
```ocaml
val get_prop : 'a -> 'b -> 'b option
```
```ocaml
val iter_prop : 'a -> 'b -> ('b -> unit) -> unit
```
```ocaml
val iter_prop_protected : 'a -> 'b -> ('b -> unit) -> unit
```
```ocaml
val space_re : Js_of_ocaml.Regexp.regexp
```
```ocaml
val current_classes : 
  < getAttribute : 
    Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t ->
    Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t Js_of_ocaml.Js.Opt.t
      Js_of_ocaml.Js.meth.. >
    Js_of_ocaml.Js.t ->
  string list
```
```ocaml
val rebuild_reactive_class_rattrib : 
  < getAttribute : 
    Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t ->
    Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t Js_of_ocaml.Js.Opt.t
      Js_of_ocaml.Js.meth
    ; setAttribute : 
      Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t ->
      Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t ->
      'res Js_of_ocaml.Js.meth.. >
    Js_of_ocaml.Js.t ->
  Xml.acontent option React.signal ->
  unit
```
```ocaml
val rebuild_rattrib : 
  Js_of_ocaml__Dom_html.element Js_of_ocaml.Js.t ->
  Xml.attrib ->
  unit
```
```ocaml
val delay : (unit -> 'a) -> unit
```
```ocaml
module ReactState : sig ... end
```
```ocaml
type content_ns = [ 
  | `HTML5
  | `SVG
 ]
```
```ocaml
val rebuild_node' : 
  [< `HTML5 | `SVG SVG ] ->
  Xml.elt ->
  Js_of_ocaml.Dom.node Js_of_ocaml.Js.t
```
```ocaml
val raw_rebuild_node : 
  [< `HTML5 | `SVG SVG ] ->
  Xml.econtent ->
  Js_of_ocaml.Dom.node Js_of_ocaml.Js.t
```
```ocaml
val is_before_initial_load : unit -> bool
```
```ocaml
val set_initial_load : unit -> unit
```
```ocaml
val rebuild_node_ns : 
  [< `HTML5 | `SVG SVG ] ->
  string ->
  Xml.elt ->
  'a Js_of_ocaml__Js.t
```
```ocaml
val rebuild_node_svg : 
  string ->
  'a Eliom_content_core.Svg.F.elt ->
  'b Js_of_ocaml__Js.t
```
```ocaml
val rebuild_node : 
  string ->
  'a Eliom_content_core.Html.F.elt ->
  'b Js_of_ocaml__Js.t
```
The first argument describes the calling function (if any) in case of an error.

```ocaml
module Syntax_helpers : sig ... end
```