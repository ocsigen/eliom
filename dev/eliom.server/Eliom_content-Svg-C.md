
# Module `Svg.C`

Creation of content from client-side values. This makes possible to insert in server side generated pages some nodes that will be computed on client side (for example reactive nodes).

```ocaml
val node : ?init:'a elt -> 'a elt Eliom_client_value.t -> 'a elt
```
`node e` is a server-side node corresponding to the client-side node `e` . `node e` can be used like any other server-side node.

The implementation uses an initial placeholder node that is later replaced by the client node. By default, the placeholder node is `span`. The `~init` argument can be used to provide a custom placeholder node (e.g., one with the same tag as the client node). This can be useful in contexts where `span` is not allowed.

```ocaml
val attr : ?init:'a attrib -> 'a attrib Eliom_client_value.t -> 'a attrib
```