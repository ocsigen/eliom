
# Module `Eliom.Eliom_form`

```ocaml
val set_error_handler : (unit -> bool Lwt.t) -> unit
```
```ocaml
module type Html = sig ... end
```
```ocaml
type 'a param
```
```ocaml
module Make_links
  (H : Html) : 
  Form_sigs.LINKS
    with type +'a elt := 'a H.elt
     and type +'a attrib := 'a H.attrib
     and type uri := H.uri
```
```ocaml
module Make
  (H : Html) : 
  Form_sigs.S
    with type +'a elt := 'a H.elt
     and type +'a attrib := 'a H.attrib
     and type uri := H.uri
     and type 'a param = 'a param
```