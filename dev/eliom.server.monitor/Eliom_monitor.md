# Module `Eliom_monitor`

```ocaml
val uptime : unit -> float
```
```ocaml
val pid : unit -> int
```
```ocaml
val fd : pid:int -> [ `Ok of int | `Error of string ]
```
```ocaml
val content_div : unit -> [> Html_types.div ] Eliom_content.Html.elt Lwt.t
```
```ocaml
val content_html : unit -> [> Html_types.html ] Eliom_content.Html.elt Lwt.t
```
