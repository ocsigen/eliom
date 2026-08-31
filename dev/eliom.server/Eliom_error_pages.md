# Module `Eliom_error_pages`

```ocaml
val page_error_param_type : 
  (string Eliom_content_core.Html.F.wrap * 'a) list ->
  [> Html_types.html ] Eliom_content_core.Html.F.elt
```
```ocaml
val page_bad_param : 
  bool ->
  (string Eliom_content_core.Html.F.wrap
   * string Eliom_content_core.Html.F.wrap)
    list ->
  string Eliom_content_core.Html.F.wrap Eliom_content_core.Html.F.wrap list ->
  [> Html_types.html ] Eliom_content_core.Html.F.elt
```
```ocaml
val page_session_expired : [> Html_types.html ] Eliom_content_core.Html.F.elt
```
