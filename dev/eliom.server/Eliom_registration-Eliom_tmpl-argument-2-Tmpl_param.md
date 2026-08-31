# Parameter `Eliom_tmpl.Tmpl_param`

```ocaml
type t
```
```ocaml
val name : string
```
```ocaml
val make_page : t -> Html_types.html Eliom_content.Html.elt Lwt.t
```
```ocaml
val update : t -> unit Eliom_client_value.t
```
