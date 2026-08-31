# Module `Eliom_tools`

Helpers for (hierarchical) menu generation in HTML5. See the Eliom manual for more information about [menu](./../misc.md#basic_menu) or [hierarchical site](./../misc.md#hier_menu).

```ocaml
type srv = 
  | Srv : (unit,
          unit,
          Eliom_service.get,
          _,
          _,
          _,
          _,
          [ `WithoutSuffix ],
          unit,
          unit,
          Eliom_service.non_ocaml)
          Eliom_service.t -> srv
```
```ocaml
type 'a hierarchical_site = main_page * ('a * 'a hierarchical_site_item) list
```
Hierarchical sites description. This is a pair `(main page, subpages list)`. Each subpage is defined by the text to be displayed in menus and a [`hierarchical_site_item`](./#type-hierarchical_site_item).

```ocaml
and 'a hierarchical_site_item = 
  | Disabled
  | Site_tree of 'a hierarchical_site
```
Menu entry description in a hierarchical site.

```ocaml
and main_page = 
  | Main_page of srv (* Main page for your subsite: all the subpages are subsections of that page. *)
  | Default_page of srv (* Like Main_page but is not taken into account for computing which is the current page in the menu. Use it for example when there is no main page, but you want one of the subpages to be the default page for your subsite. The service you use as default page must appear another time in the subtree! *)
  | Not_clickable (* When you do not want the menu entry to be a link but you want subpages. *)
```
Main page description for a section of a hierarchical site.

### Tools for generating certain HTML elements

```ocaml
module type HTML5_TOOLS = sig ... end
```
```ocaml
module F : HTML5_TOOLS
```
Menus with functional node semantics

```ocaml
module D : HTML5_TOOLS
```
Menus with DOM semantics

```ocaml
val with_js_file : string list -> unit
```
Record an (external) JavaScript file to be included in [`Eliom_tools.F.html`](./Eliom_tools-F.md#val-html).

```ocaml
val with_css_file : string list -> unit
```
Record an CSS file to be included in [`Eliom_tools.F.html`](./Eliom_tools-F.md#val-html).

### Other tools

```ocaml
val wrap_handler : 
  (unit -> 'a option Lwt.t) ->
  ('get -> 'post -> 'res Lwt.t) ->
  ('a -> 'get -> 'post -> 'res Lwt.t) ->
  'get ->
  'post ->
  'res Lwt.t
```
This function allows one to wrap a service handler easily depending on whether certain information is available or not.

The first arguments provides that information (`Some value`) of not (`None`), the second argument is called just with two arguments when the information is not available (the two arguments are suggesting GET and POST parameters of a request). The third argument is called with that information if available and the parameters.

` <<code language="ocaml"|
    let user_eref = Eliom_reference.eref ~scope None
    let anonymous_handler _ _ =
      Lwt.return (html (head (title "not allowed")) (body []))
    let authenticated_handler f =
      Eliom_tools.wrap_handler
        (fun () -> Eliom_reference.get user_eref)
        anonymous_handler f
    let guarded_service =
      My_app.register_service ~path ~get_param
        (authenticated_handler
           (fun user get () ->
              Lwt.return (html (head (title ("hello "^user))) (body []))))
    >> `
