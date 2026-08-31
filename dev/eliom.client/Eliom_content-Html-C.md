# Module `Html.C`

Creation of HTML5 content from client-side values. This module is available on client side only to make possible to use C-nodes in shared sections.

### Content injection

```ocaml
val node : ?init:'a D.elt -> 'a elt Eliom_client_value.t -> 'a D.elt
```
Those two functions are the identity on client-side (the `init` argument is ignored). See Eliom manual for more detail on [Dom & Client-values](./../clientserver-html.md#inject).

```ocaml
val attr : ?init:'a attrib -> 'a attrib Eliom_client_value.t -> 'a attrib
```
