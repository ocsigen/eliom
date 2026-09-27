# Miscellaneous

## HTML widgets

### Images, CSS, Javascript

To include an image, simply use function [`Eliom.Content.Html.D.img`](./eliom.server/Eliom-Content-Html-D.md#val-img):

```ocaml
img ~alt:"Ocsigen"
    ~src:(Eliom.Content.Html.F.make_uri
            ~service:(Eliom.Service.static_dir ())
            ["images"; "ocsigen1024.jpg"])
    ()
```
The function [`Eliom.Content.Html.D.make_uri`](./eliom.server/Eliom-Content-Html-D.md#val-make_uri) creates a relative URL string from current URL (see above) to the URL of the image (here in the static directory configured in the configuration file).

To simplify the creation of `<link>` tags for CSS or `<script>` tags for Javascript, use the following functions:

```ocaml
Eliom.Content.Html.F.css_link
  ~uri:(Eliom.Content.Html.F.make_uri
         ~service:(Eliom.Service.static_dir ()) ["style.css"]) ()
```
```ocaml
Eliom.Content.Html.F.js_script
  ~uri:(Eliom.Content.Html.F.make_uri
         ~service:(Eliom.Service.static_dir ()) ["funs.js"]) ()
```

### Basic menus

To generate a context-aware menu on your Web page, you can use the function [`Eliom.Tools.HTML5_TOOLS.menu`](./eliom.server/Eliom-Tools-module-type-HTML5_TOOLS.md#val-menu). This function can be used from either of the modules [`Eliom.Tools.D`](./eliom.server/Eliom-Tools-D.md) or [`Eliom.Tools.F`](./eliom.server/Eliom-Tools-F.md). (See [HTML element manipulation, by value and by reference](./clientserver-html.md#unique)).

Here is a simple example:

```ocaml
let mymenu =
  let items =
    [(home,     [txt "Home"]);
     (info,     [txt "More info"]);
     (tutorial, [txt "Documentation"])]
  in
  Eliom.Tools.D.menu ~classe:["menuprincipal"] items
```
`items` is a list of pairs correlating the services to be linked with the text to be displayed: `home`, `info`, and `tutorial` are our three services (generated, for example, by [`Eliom.Service.create`](./eliom.server/Eliom-Service.md#val-create) and registered with an application module made with [`Eliom.Registration.App`](./eliom.server/Eliom-Registration-App.md)).

The argument to the optional parameter `classe` adds the class `menuprincipal` to the resulting menu element.

The item of the current page is highlighted. The optional parameter `service` highlights the item of another service instead.

`mymenu ()`, when viewed on the home page, will generate the following HTML:

```ocaml
<ul class="eliomtools_menu menuprincipal caml_r" data-eliom-id="x1TqwAXRlB1I">
  <li class="eliomtools_current eliomtools_first">
    Home
  </li>
  <li>
    <a class="caml_c" href="info/" data-eliom-c-onclick="P7IUw76wowL1">
      More info
    </a>
  </li>
  <li class="eliomtools_last">
    <a class="caml_c" href="tutorial/" data-eliom-c-onclick="P7IUw76wowL2">
      Documentation
    </a>
  </li>
</ul>
```
You may then personalize the element in your CSS stylesheet as normal.

Note: [`Eliom.Tools.D.menu`](./eliom.server/Eliom-Tools-D.md#val-menu) takes a list of services without GET parameters. If you want one of the links to contain GET parameters, pre-apply the service.

### Hierarchical menus

```ocaml
(* Hierarchical menu *)
open Eliom.Content.Html.F
open Eliom.Tools

let hier i =
  Eliom.Service.create
    ~path:(Eliom.Service.Path ["hier" ^ string_of_int i])
    ~meth:(Eliom.Service.Get Eliom.Parameter.unit)
    ()

let hier1 = hier 1
let hier2 = hier 2
let hier3 = hier 3
let hier4 = hier 4
let hier5 = hier 5
let hier6 = hier 6
let hier7 = hier 7
let hier8 = hier 8
let hier9 = hier 9
let hier10 = hier 10

let mymenu =
  ( Main_page (Srv hier1)
  , [ [txt "page 1"], Site_tree (Main_page (Srv hier1), [])
    ; [txt "page 2"], Site_tree (Main_page (Srv hier2), [])
    ; ( [txt "submenu 4"]
      , Site_tree
          ( Default_page (Srv hier4)
          , [ ( [txt "submenu 3"]
              , Site_tree
                  ( Not_clickable
                  , [ [txt "page 3"], Site_tree (Main_page (Srv hier3), [])
                    ; [txt "page 4"], Site_tree (Main_page (Srv hier4), [])
                    ; [txt "page 5"], Site_tree (Main_page (Srv hier5), []) ]
                  ) )
            ; [txt "page 6"], Site_tree (Main_page (Srv hier6), []) ] ) )
    ; [txt "page 7"], Site_tree (Main_page (Srv hier7), [])
    ; [txt "disabled"], Disabled
    ; ( [txt "submenu 8"]
      , Site_tree
          ( Main_page (Srv hier8)
          , [ [txt "page 9"], Site_tree (Main_page (Srv hier9), [])
            ; [txt "page 10"], Site_tree (Main_page (Srv hier10), []) ] ) ) ] )

let css =
  {|li.eliomtools_current > a {color: blue;}
.breadthmenu li {display: inline; padding: 0 1em; border-right: solid 1px black;}
.breadthmenu li.eliomtools_last {border: none;}|}

let page i service () () =
  Lwt.return
    (html
       (head
          (title (txt ("Page " ^ string_of_int i)))
          (style [txt css] :: F.structure_links mymenu ~service ()))
       (body
          [ h1 [txt ("Page " ^ string_of_int i)]
          ; h2 [txt "Depth first, whole tree:"]
          ; div
              (F.hierarchical_menu_depth_first ~whole_tree:true mymenu ~service
                 ())
          ; h2 [txt "Depth first, only current submenu:"]
          ; div (F.hierarchical_menu_depth_first mymenu ~service ())
          ; h2 [txt "Breadth first:"]
          ; div
              (F.hierarchical_menu_breadth_first ~classe:["breadthmenu"] mymenu
                 ~service ()) ]))

let () =
  List.iteri
    (fun i service ->
       Eliom.Registration.Html.register ~service (page (i + 1) service))
    [hier1; hier2; hier3; hier4; hier5; hier6; hier7; hier8; hier9; hier10]
```
