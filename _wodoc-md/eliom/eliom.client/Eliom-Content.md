
# Module `Eliom.Content`

This module provides the creation of valid XML content, i.e. XML, SVG, and (X)HTML5.

**Please read [Eliom's manual](./../clientserver-html.md) for more information on HTML generation.** You can also have a look at the server API of [`Content`](#) for an explication of the modules `F` and `D`.

```ocaml
module Xml : module type of Content_core.Xml
```
Low-level XML manipulation.

```ocaml
module Svg : sig ... end
```
Building valid SVG .

```ocaml
module Html : sig ... end
```
Building valid (X)HTML5.

```ocaml
val force_link : unit
```