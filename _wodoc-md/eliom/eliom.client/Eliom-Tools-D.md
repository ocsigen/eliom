
# Module `Tools.D`

Menus with DOM semantics


### Simple menu

```ocaml
val menu : 
  ?classe:Html_types.nmtoken list ->
  ?id:string ->
  ((unit,
     unit,
     Service.get,
     _,
     _,
     _,
     _,
     [ `WithoutSuffix ],
     unit,
     unit,
     Service.non_ocaml)
     Service.t
   * [< Html_types.flow5_without_interactive ] Content.Html.elt list)
    list ->
  ?service:
    (unit,
      unit,
      Service.get,
      _,
      _,
      _,
      _,
      [ `WithoutSuffix ],
      unit,
      unit,
      Service.non_ocaml)
      Service.t ->
  unit ->
  [> `Ul ] Content.Html.elt
```
The function `menu elts ()`, where `elts` is a list of pair `(service, content)`, creates a list of link towards the `service`s. See the Eliom manual for an [example of menu](./../misc.md#basic_menu).

The optional parameter `service` is used to find which item(s) to highlight (by adding the class `eliomtools_current` to the corresponding `<li>` node). The default is to highlight the item corresponding to the current url.

The optional parameters `id` and `classe` allow to specify the corresponding attributes in the generated `<ul>` node. The default class for the `<ul>` node is `eliomtools_menu`.


### Hierchical sites

```ocaml
val hierarchical_menu_depth_first : 
  ?classe:Html_types.nmtoken list ->
  ?id:string ->
  ?whole_tree:bool ->
  [< Html_types.a_content ] Content.Html.elt list hierarchical_site ->
  ?service:
    (unit,
      unit,
      Service.get,
      _,
      _,
      _,
      _,
      [ `WithoutSuffix ],
      unit,
      unit,
      Service.non_ocaml)
      Service.t ->
  unit ->
  [> `Ul ] Content.Html.elt list
```
The function `hierarchical_menu_depth_first site ()` constructs a hieranrchical menu by exploring the hierarchical `site` description using a depth-first algorithm: the first menu item will be displayed, followed by the whole sub-menu for this item, then the second menu item with its sub-menu, and so on.

By default, only the sub-menus for to the url corresponding to the optional argument `service` are displayed, others sub-menu are collapsed. If you want all the sub-menus to be displayed, specify `~whole_tree:true`. If the optional parameter `service` is not given, the current page is used.

See [`menu`](./#val-menu) for a description of the optional parameters `id` and `classe`.

```ocaml
val hierarchical_menu_breadth_first : 
  ?classe:Html_types.nmtoken list ->
  ?id:string ->
  [< Html_types.a_content ] Content.Html.elt list hierarchical_site ->
  ?service:
    (unit,
      unit,
      Service.get,
      _,
      _,
      _,
      _,
      [ `WithoutSuffix ],
      unit,
      unit,
      Service.non_ocaml)
      Service.t ->
  unit ->
  [> `Ul ] Content.Html.elt list
```
The function `hierarchical_menu_breadth_first site ()` constructs a hierarchical menu by exploring the hierarchical `site` description using a breadth\_first algorithm: the whole menu for one level will be displayed, followed by the sub-menu leading to the current service, and so one.

By default the current service correspond to the current url. The optional parameter `service` allow to override the current service.

See [`menu`](./#val-menu) for a description of the optional parameters `id` and `classe`.

```ocaml
val structure_links : 
  [< Html_types.a_content ] Content.Html.elt list hierarchical_site ->
  ?service:
    (unit,
      unit,
      Service.get,
      _,
      _,
      _,
      _,
      [ `WithoutSuffix ],
      unit,
      unit,
      Service.non_ocaml)
      Service.t ->
  unit ->
  [> `Link ] Content.Html.elt list
```
The function `structure_links site ()` returns the tags `<link rel="subsection" ...>` and `<link rev="subsection" ...>` for the given hierarchical `site`.

By default the current service correspond to the current url. The optional parameter `service` allow to override the current service.

```ocaml
val head : 
  title:string ->
  ?css:string list list ->
  ?js:string list list ->
  ?other:[< Html_types.head_content_fun ] Content.Html.elt list ->
  unit ->
  [ `Head ] Content.Html.elt
```
An auxiliary function for creating an HTML head elements. Resources (JS, CSS) are taken from the static directory.

```ocaml
val html : 
  title:string ->
  ?a:[< Html_types.html_attrib ] Content.Html.attrib list ->
  ?css:string list list ->
  ?js:string list list ->
  ?other_head:[< Html_types.head_content_fun ] Content.Html.elt list ->
  [ `Body ] Content.Html.elt ->
  [ `Html ] Content.Html.elt
```