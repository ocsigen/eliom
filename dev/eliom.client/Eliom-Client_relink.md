# Module `Eliom.Client_relink`

Relinking of a page received from the server: registration of the unique nodes, and binding of Eliom's links, forms, event handlers and client attributes. Internal module.

```ocaml
val relink_request_nodes : 
  Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t ->
  unit
```
Registers the request nodes below the given root, or replaces them with the nodes already known under the same id.

```ocaml
val relink_page_but_client_values : 
  Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t ->
  Mod_dom.selected_nodes
```
Relinks the links, forms and process nodes below the given root. The selected nodes are returned, so that the closure and attribute nodes can be relinked once the client values are initialised.

```ocaml
val relink_closure_nodes : 
  Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t ->
  Ocsigen_lib_base.poly Runtime.RawXML.ClosureMap.t ->
  Js_of_ocaml.Dom_html.element Js_of_ocaml.Dom.nodeList Js_of_ocaml.Js.t ->
  unit ->
  unit
```
`relink_closure_nodes root table nodes` binds the event handlers of `nodes`, taken from `table`. It returns a function running all the `onload` handlers found, which stops at the first one returning `false`.

```ocaml
val relink_attribs : 
  Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t ->
  Ocsigen_lib_base.poly Runtime.RawXML.ClosureMap.t ->
  Js_of_ocaml.Dom_html.element Js_of_ocaml.Dom.nodeList Js_of_ocaml.Js.t ->
  unit
```
`relink_attribs root table nodes` sets the client attributes of `nodes`, taken from `table`.
