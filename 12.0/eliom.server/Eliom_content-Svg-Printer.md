
# Module `Svg.Printer`

SVG printer. See [`Xml_sigs.Typed_pp`](./../../tyxml/tyxml.functor/Xml_sigs-module-type-Typed_pp.md).

```ocaml
val pp_elt : 
  ?encode:(string -> string) ->
  ?indent:bool ->
  unit ->
  Format.formatter ->
  'a elt ->
  unit
```
`pp_elt ()` is a [`Format`](./../../ocaml-compiler/stdlib/Stdlib-Format.md) printer for individual elements.

A custom encoding function can be provided with the `~encode` argument. Various implementations of `encode` are available in [`Xml_print`](./../../tyxml/tyxml.functor/Xml_print.md).

```ocaml
val pp : 
  ?encode:(string -> string) ->
  ?indent:bool ->
  ?advert:string ->
  unit ->
  Format.formatter ->
  F.doc ->
  unit
```
`pp ()` is a [`Format`](./../../ocaml-compiler/stdlib/Stdlib-Format.md) printer for complete documents.

It can be used in combination with `"%a"`. For example, to get a string:

```ocaml
let s = Format.asprintf "%a" (pp ()) my_document
```
A custom encoding function can be provided with the `~encode` argument. Various implementations of `encode` are available in [`Xml_print`](./../../tyxml/tyxml.functor/Xml_print.md).
