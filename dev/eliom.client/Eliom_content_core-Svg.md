# Module `Eliom_content_core.Svg`

Building SVG tree.

See the Eliom manual for more information on[dom semantics vs. functional semantics](./../clientserver-html.md#unique) for SVG tree manipulated by client/server application.

```ocaml
type +'a elt
```
```ocaml
type 'a wrap = 'a
```
```ocaml
type 'a list_wrap = 'a list
```
```ocaml
type +'a attrib
```
```ocaml
type uri = Xml.uri
```

### Functional semantics

```ocaml
module F : sig ... end
```
Typed interface for building valid SVG tree (functional semantics). See [`Svg_sigs.T`](./../../tyxml/tyxml.functor/Svg_sigs-module-type-T.md).

### DOM semantics

```ocaml
module D : sig ... end
```
Typed interface for building valid SVG tree (DOM semantics). See [`Svg_sigs.T`](./../../tyxml/tyxml.functor/Svg_sigs-module-type-T.md).

### Reactive DOM

```ocaml
module R : sig ... end
```
Typed interface for building valid reactive SVG tree.

### Global node

```ocaml
module Id : sig ... end
```
```ocaml
module Of_dom : sig ... end
```
