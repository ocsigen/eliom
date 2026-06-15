
# Module `Raw.Unsafe`

Unsafe features.

Using this module can break HTML validity and may introduce security problems like code injection. Use it with care.

```ocaml
val data : string wrap -> 'a elt
```
Insert raw text without any encoding

```ocaml
val node : string -> ?a:'a attrib list -> 'b elt list_wrap -> 'c elt
```
Insert an XML node that is not implemented in this module. If it is a standard HTML node which is missing, please report to the Ocsigen team.

```ocaml
val leaf : string -> ?a:'a attrib list -> unit -> 'b elt
```
Insert an XML node without children that is not implemented in this module. If it is a standard HTML node which is missing, please report to the Ocsigen team.

```ocaml
val coerce_elt : 'a elt -> 'b elt
```
Remove phantom type annotation on an element, to make it usable everywhere.

```ocaml
val string_attrib : string -> string wrap -> 'a attrib
```
Insert an attribute that is not implemented in this module. If it is a standard HTML attribute which is missing, please report to the Ocsigen team.

```ocaml
val float_attrib : string -> float wrap -> 'a attrib
```
Same, for float attribute

```ocaml
val int_attrib : string -> int wrap -> 'a attrib
```
Same, for int attribute

```ocaml
val uri_attrib : string -> uri wrap -> 'a attrib
```
Same, for URI attribute

```ocaml
val space_sep_attrib : string -> string list wrap -> 'a attrib
```
Same, for a space separated list of values

```ocaml
val comma_sep_attrib : string -> string list wrap -> 'a attrib
```
Same, for a comma separated list of values
