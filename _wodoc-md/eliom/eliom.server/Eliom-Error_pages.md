
# Module `Eliom.Error_pages`

```ocaml
val page_error_param_type : 
  (string Content_core.Html.F.wrap * 'a) list ->
  [> Html_types.html ] Content_core.Html.F.elt
```
```ocaml
val page_bad_param : 
  bool ->
  (string Content_core.Html.F.wrap * string Content_core.Html.F.wrap) list ->
  string Content_core.Html.F.wrap Content_core.Html.F.wrap list ->
  [> Html_types.html ] Content_core.Html.F.elt
```
```ocaml
val page_session_expired : [> Html_types.html ] Content_core.Html.F.elt
```