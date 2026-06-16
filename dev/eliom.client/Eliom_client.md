
# Module `Eliom_client`

Call server side services and change the current page.

```ocaml
val lock_request_handling : unit -> unit
```
```ocaml
val unlock_request_handling : unit -> unit
```

### Mobile applications

```ocaml
val init_client_app : 
  app_name:string ->
  ?ssl:bool ->
  hostname:string ->
  ?port:int ->
  site_dir:Eliom_lib.Url.path ->
  unit ->
  unit
```
Call this function if you want to be able to run your client side app before doing the first request, that is, when the client side app is not sent by the server. This may be the case for example if you are developing a mobile app. The parameters correspond to the base URL of the server side of your application.

Alternatively, and to make sure it is done early enough, define JS variables called `__eliom_server` and `__eliom_app_name` at the beginning of your html file, containing the full URL of your server and Eliom app name.

`site_dir` (if given) specifies the path that the application runs under. It should correspond to the \<site\> tag of your server configuration. Calls to server functions use this path.

```ocaml
val is_client_app : unit -> bool
```
Returns whether the application is sent by a server or started on client side. If called on server side, always returns `false`. Otherwise, it tests the presence of JS variables added automatically by Eliom when the page is sent by a server. Example:

```ocaml
    if not (Eliom_client.is_client_app ())
then Eliom_client.init_client_app ... 
```

### Calling services

```ocaml
val change_page : 
  ?ignore_client_fun:bool ->
  ?replace:bool ->
  ?window_name:string ->
  ?window_features:string ->
  ?absolute:bool ->
  ?absolute_path:bool ->
  ?https:bool ->
  service:
    ('a, 'b, _, _, _, _, _, _, _, _, Eliom_service.non_ocaml) Eliom_service.t ->
  ?hostname:string ->
  ?port:int ->
  ?fragment:string ->
  ?keep_nl_params:[ `All | `None | `Persistent ] ->
  ?nl_params:Eliom_parameter.nl_params_set ->
  ?keep_get_na_params:bool ->
  ?progress:(int -> int -> unit) ->
  ?upload_progress:(int -> int -> unit) ->
  ?override_mime_type:string ->
  'a ->
  'b ->
  unit Lwt.t
```
Call a service and change the current page. If the service belongs to the same application, the client side program is not stopped, and only the content (not the container) is reloaded. If the `replace` flag is set, the new page will replace the current page in the browser history if the service belongs to the same application. The last two parameters are respectively the GET and POST parameters to send to the service. If `window_name` is provided and not "\_self" (for example `"_blank"`), will behave as `exit_to`.

```ocaml
val call_ocaml_service : 
  ?absolute:bool ->
  ?absolute_path:bool ->
  ?https:bool ->
  service:
    ('a, 'b, _, _, _, _, _, _, _, _, 'return Eliom_service.ocaml)
      Eliom_service.t ->
  ?hostname:string ->
  ?port:int ->
  ?fragment:string ->
  ?keep_nl_params:[ `All | `None | `Persistent ] ->
  ?nl_params:Eliom_parameter.nl_params_set ->
  ?keep_get_na_params:bool ->
  ?progress:(int -> int -> unit) ->
  ?upload_progress:(int -> int -> unit) ->
  ?override_mime_type:string ->
  'a ->
  'b ->
  'return Lwt.t
```
Call a server side service that return an OCaml value.

If the service raises an exception, the call to the `call_ocaml_service` raises an exception `Exception_on_server` whose argument describes the server-side exception. (NB that we cannot send the original exception as-it, because OCaml permits the marshalling of exceptions ...)

```ocaml
val exit_to : 
  ?window_name:string ->
  ?window_features:string ->
  ?absolute:bool ->
  ?absolute_path:bool ->
  ?https:bool ->
  service:
    ('a, 'b, _, _, _, _, _, _, _, _, Eliom_service.non_ocaml) Eliom_service.t ->
  ?hostname:string ->
  ?port:int ->
  ?fragment:string ->
  ?keep_nl_params:[ `All | `None | `Persistent ] ->
  ?nl_params:Eliom_parameter.nl_params_set ->
  ?keep_get_na_params:bool ->
  'a ->
  'b ->
  unit
```
Stop current program and load a new page. Note that for string arguments, sole line feed or sole carriage return characters are substituted by the string `"\r\n"`. If `window_name` is specified (for example `"_blank"`), open in new window by calling Javascript function `window.open`. In that case, you can add `window_features` parameter (as in Javascript function `window.open`). Warning: opening in other window may not work on mobile apps if `window.open` is not implemented.

```ocaml
val window_open : 
  window_name:Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t ->
  ?window_features:Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t ->
  ?absolute:bool ->
  ?absolute_path:bool ->
  ?https:bool ->
  service:
    ('a, unit, Eliom_service.get, _, _, _, _, _, _, unit, _) Eliom_service.t ->
  ?hostname:string ->
  ?port:int ->
  ?fragment:string ->
  ?keep_nl_params:[ `All | `None | `Persistent ] ->
  ?nl_params:Eliom_parameter.nl_params_set ->
  ?keep_get_na_params:bool ->
  'a ->
  Js_of_ocaml.Dom_html.window Js_of_ocaml.Js.t Js_of_ocaml.Js.opt
```
Loads an Eliom service in a window (cf. Javascript's `window.open`).

```ocaml
val change_url : 
  ?replace:bool ->
  ?absolute:bool ->
  ?absolute_path:bool ->
  ?https:bool ->
  service:
    ('get, unit, Eliom_service.get, _, _, _, _, _, _, unit, _) Eliom_service.t ->
  ?hostname:string ->
  ?port:int ->
  ?fragment:string ->
  ?keep_nl_params:[ `All | `None | `Persistent ] ->
  ?nl_params:Eliom_parameter.nl_params_set ->
  'get ->
  unit
```
Changes the URL, without doing a request. It takes a GET (co-)service as parameter and its parameters. If the `replace` flag is set, the current page is not saved in the history.

```ocaml
val call_service : 
  ?absolute:bool ->
  ?absolute_path:bool ->
  ?https:bool ->
  service:('a, 'b, _, _, _, _, _, _, _, _, _) Eliom_service.t ->
  ?hostname:string ->
  ?port:int ->
  ?fragment:string ->
  ?keep_nl_params:[ `All | `None | `Persistent ] ->
  ?nl_params:Eliom_parameter.nl_params_set ->
  ?keep_get_na_params:bool ->
  ?progress:(int -> int -> unit) ->
  ?upload_progress:(int -> int -> unit) ->
  ?override_mime_type:string ->
  'a ->
  'b ->
  string Lwt.t
```
(low level) Call a server side service and return the content of the resulting HTTP frame as a string.


### Misc

```ocaml
val onload : (unit -> unit) -> unit
```
Registers some code to be executed after loading the client application, or after changing the page the next time.

It complements as a toplevel expression in the client module with the side effect from client values while creating the response of a service: While the latter are executed each time the service has been called; the former is executed only once; but each at a time where the document is in place:

Beware of using `onload` when using the DOM caching functionality, i.e. `push_history_dom`. When switching to a cached page (e.g. by going back) the onload event is not triggered (as the page is not loaded). To avoid this problem rely on `Page_status.onactive` which is triggered for freshly generated pages as well as pages served from the DOM cache.

```ocaml
[%%shared open Eliom_lib]
[%%client
  let () = alert "Once only during initialization of the client, \
                  i.e. before the document is available."
  let () =
    Eliom_client.onload
      (fun () -> alert "Once only when the document is put in place.")
]
[%%server
  let _ = My_app.register_service ~path ~get_params
    (fun () () ->
       ignore {unit{
         alert "Each time this service is called and the sent document \
                is put in place."
       }};
       Lwt.return html
]
```
```ocaml
val lwt_onload : unit -> unit Lwt.t
```
Returns a Lwt thread that waits until the next page is loaded.

```ocaml
type changepage_event = {
  in_cache : bool;
  origin_uri : string;
  target_uri : string;
  origin_id : int;
  target_id : int option;
}
```
`changepage_event` is a record of some parameters related to page changes. `in_cache` is true if the dom of the page is cached by `push_history_dom`. `origin_uri` is the uri of the current page and `target_uri` is the uri of the next page. `origin_id` is the state\_id of the current page and `target_id` is the state\_id of the next page. `target_id` is not `None` if and only if the onchangepage event takes place during a navigation in history.

```ocaml
val onchangepage : (changepage_event -> unit Lwt.t) -> unit
```
Run some code \*before\* the next page change, that is, before each call to a page-producing service handler. Just like onpreload, handlers registered with onchangepage only apply to the next page change.

```ocaml
module Page_status : sig ... end
```
```ocaml
val onbeforeunload : (unit -> string option) -> unit
```
`onbeforeunload f` registers `f` as a handler to be called before changing the page the next time. If `f` returns `Some s`, then we ask the user to confirm quitting. We try to use `s` in the confirmation pop-up. `None` means no confirmation needed.

The callback `f` is sometimes triggered by internal service calls, and sometimes by the browser `onbeforeunload` event. In the `onbeforeunload` case, the confirmation pop-up is managed by the browser. For Firefox, the string `s` returned by `f` is ignored: https://bugzilla.mozilla.org/show\_bug.cgi?id=641509

`onbeforeunload` can be used to register multiple callbacks.

```ocaml
val onunload : (unit -> unit) -> unit
```
`onunload f` registers `f` as a handler to be called before page change. The callback `f` is sometimes triggered by internal service calls, and sometimes by the browser `onunload` event. `onunload` can be used to register multiple callbacks.

```ocaml
val wait_load_end : unit -> unit Lwt.t
```
Wait for the initialization phase to terminate

```ocaml
val get_application_name : unit -> string
```
Returns the name of currently running Eliom application, defined while applying `Eliom_registration.App` functor.

```ocaml
val persist_document_head : unit -> unit
```
After this function is called, the document head is no longer changed on page change.


### RPC / Server functions

See the [manual](./../clientserver-communication.md#rpc).

```ocaml
type ('a, +'b) server_function = 'a -> 'b Lwt.t
```
A `('a, 'b) server_function` provides transparently access to a server side function which has been created by [`Eliom_client.server_function`](./#type-server_function).

See also [the opaque server side representation](./#type-server_function).

The handling of exception on the server corresponds to that of [`Eliom_client.call_ocaml_service`](./#val-call_ocaml_service).

```ocaml
val change_page_uri : ?replace:bool -> string -> unit Lwt.t
```
`change_page_uri ?replace uri` identifies and calls the client-side service that implements `uri`.

We fallback to a server service call if the service is not registered on the client.

If the `replace` flag is set to `true`, the current page is not saved in the history.

```ocaml
val set_client_html_file : string -> unit
```
Set the name of the HTML file loading our client app. The default is "eliom.html". A wrong value will not allow the app to initialize itself correctly.
