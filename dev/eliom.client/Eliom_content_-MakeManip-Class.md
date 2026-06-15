
# Module `MakeManip.Class`

```ocaml
val contain : 'a Kind.elt -> string -> bool
```
```ocaml
val add_raw : 
  < classList : 
    < get : 
      < add : 
        Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t ->
        unit Js_of_ocaml.Js.meth
        ; contains : 
          Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t ->
          bool Js_of_ocaml.Js.t Js_of_ocaml.Js.meth.. >
        Js_of_ocaml.Js.t.. >
      Js_of_ocaml.Js.gen_prop.. >
    Js_of_ocaml.Js.t ->
  string ->
  unit
```
```ocaml
val add : 'a Kind.elt -> string -> unit
```
```ocaml
val adds : 'a Kind.elt -> string list -> unit
```
```ocaml
val remove_raw : 
  < classList : 
    < get : 
      < contains : 
        Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t ->
        bool Js_of_ocaml.Js.t Js_of_ocaml.Js.meth
        ; remove : 
          Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t ->
          unit Js_of_ocaml.Js.meth.. >
        Js_of_ocaml.Js.t.. >
      Js_of_ocaml.Js.gen_prop.. >
    Js_of_ocaml.Js.t ->
  string ->
  unit
```
```ocaml
val remove : 'a Kind.elt -> string -> unit
```
```ocaml
val removes : 'a Kind.elt -> string list -> unit
```
```ocaml
val replace : 'a Kind.elt -> string -> string -> unit
```
```ocaml
val clear : 'a Kind.elt -> unit
```
```ocaml
val toggle : 'a Kind.elt -> string -> unit
```
```ocaml
val toggle2 : 'a Kind.elt -> string -> string -> unit
```