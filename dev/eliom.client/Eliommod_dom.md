# Module `Eliommod_dom`

Cross browser dom manipulation functions

```ocaml
val get_body : 
  Js_of_ocaml.Dom.element Js_of_ocaml.Js.t ->
  Js_of_ocaml.Dom.element Js_of_ocaml.Js.t
```
```ocaml
val get_head : 
  Js_of_ocaml.Dom.element Js_of_ocaml.Js.t ->
  Js_of_ocaml.Dom.element Js_of_ocaml.Js.t
```
`select_nodes root` finds the nodes below `root` in the page annotated to be: \* eliom links \* eliom forms \* process unique nodes \* nodes with closures ( events ) \* nodes with attributes

```ocaml
val select_nodes : 
  Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t ->
  Js_of_ocaml.Dom_html.anchorElement Js_of_ocaml.Dom.nodeList Js_of_ocaml.Js.t
  * Js_of_ocaml.Dom_html.formElement Js_of_ocaml.Dom.nodeList Js_of_ocaml.Js.t
  * Js_of_ocaml.Dom_html.element Js_of_ocaml.Dom.nodeList Js_of_ocaml.Js.t
  * Js_of_ocaml.Dom_html.element Js_of_ocaml.Dom.nodeList Js_of_ocaml.Js.t
  * Js_of_ocaml.Dom_html.element Js_of_ocaml.Dom.nodeList Js_of_ocaml.Js.t
```
```ocaml
val select_request_nodes : 
  Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t ->
  Js_of_ocaml.Dom_html.element Js_of_ocaml.Dom.nodeList Js_of_ocaml.Js.t
```
`select_request_nodes root` finds the nodes below `root` in the page annotated to be: \* request unique nodes

```ocaml
val ancessor : 
  Js_of_ocaml.Dom.node Js_of_ocaml.Js.t ->
  Js_of_ocaml.Dom.node Js_of_ocaml.Js.t ->
  bool
```
`ancessor n1 n2` is true if `n1` is an ancessor of `n2`

```ocaml
val createEvent : 
  Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t ->
  Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t
```
```ocaml
val copy_element : 
  Js_of_ocaml.Dom.element Js_of_ocaml.Js.t ->
  (Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t -> bool) ->
  Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t
```
`copy_element e` creates recursively a fresh html from any xml element avoiding browser bugs

```ocaml
val html_document : 
  Js_of_ocaml.Dom.element Js_of_ocaml.Dom.document Js_of_ocaml.Js.t ->
  (Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t -> bool) ->
  Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t
```
Assuming `d` has a body and head element, `html_document d` will return the same document as html

```ocaml
val preload_css : Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t -> unit Lwt.t
```
`preload_css e` downloads every css included in every link elements that is a descendant of `e` and replace it and its linked css by inline `<style>` elements

```ocaml
val iter_nodeList : 
  'a Js_of_ocaml.Dom.nodeList Js_of_ocaml.Js.t ->
  ('a Js_of_ocaml.Js.t -> unit) ->
  unit
```
```ocaml
val iter_attrList : 
  Js_of_ocaml.Dom.attr Js_of_ocaml.Dom.namedNodeMap Js_of_ocaml.Js.t ->
  (Js_of_ocaml.Dom.attr Js_of_ocaml.Js.t -> unit) ->
  unit
```
Window scrolling.

```ocaml
type position = {
  html_top : float;
  html_left : float;
  body_top : float;
  body_left : float;
}
```
```ocaml
val position_of_json : Deriving_Json_lexer.lexbuf -> position
```
```ocaml
val position_to_json : Buffer.t -> position -> unit
```
```ocaml
val position_json : position Deriving_Json.t
```
```ocaml
val top_position : position
```
```ocaml
val getDocumentScroll : unit -> position
```
```ocaml
val setDocumentScroll : position -> unit
```
```ocaml
val test_pageshow_pagehide : unit -> bool
```
```ocaml
val onhashchange : (Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t -> unit) -> unit
```
