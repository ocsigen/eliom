# Module `Eliom.Client_call`

Low-level calls to services: building the request, sending it, and leaving the application for another page. Internal module.

```ocaml
val create_request_ : 
  ?absolute:bool ->
  ?absolute_path:bool ->
  ?https:bool ->
  service:
    ('a, 'b, _, _, _, _, _, [< `WithSuffix | `WithoutSuffix ], _, _, _)
      Service.t ->
  ?hostname:string ->
  ?port:int ->
  ?fragment:string ->
  ?keep_nl_params:[ `All | `None | `Persistent ] ->
  ?nl_params:Parameter.nl_params_set ->
  ?keep_get_na_params:bool ->
  'a ->
  'b ->
  [ `Get of string * (string * Mod_parameters.param) list
  | `Post of
    string
    * (string * Mod_parameters.param) list
    * (string * Mod_parameters.param) list
  | `Put of
    string
    * (string * Mod_parameters.param) list
    * (string * Mod_parameters.param) list
  | `Delete of
    string
    * (string * Mod_parameters.param) list
    * (string * Mod_parameters.param) list ]
```
The URI and the parameters of a request to a service, tagged with its HTTP method. For a ``Get`, the parameters are the GET ones; otherwise they are the GET and the POST ones.

```ocaml
val raw_call_service : 
  ?absolute:bool ->
  ?absolute_path:bool ->
  ?https:bool ->
  service:
    ('a, 'b, _, _, _, _, _, [< `WithSuffix | `WithoutSuffix ], _, _, _)
      Service.t ->
  ?hostname:string ->
  ?port:int ->
  ?fragment:string ->
  ?keep_nl_params:[ `All | `None | `Persistent ] ->
  ?nl_params:Parameter.nl_params_set ->
  ?keep_get_na_params:bool ->
  ?progress:(int -> int -> unit) ->
  ?upload_progress:(int -> int -> unit) ->
  ?override_mime_type:string ->
  'a ->
  'b ->
  (string * string) Lwt.t
```
Calls a service and returns the URI of the response and its content. Fails with `Request.Failed_request 204` if there is no content.

```ocaml
val call_service : 
  ?absolute:bool ->
  ?absolute_path:bool ->
  ?https:bool ->
  service:
    ('a, 'b, _, _, _, _, _, [< `WithSuffix | `WithoutSuffix ], _, _, _)
      Service.t ->
  ?hostname:string ->
  ?port:int ->
  ?fragment:string ->
  ?keep_nl_params:[ `All | `None | `Persistent ] ->
  ?nl_params:Parameter.nl_params_set ->
  ?keep_get_na_params:bool ->
  ?progress:(int -> int -> unit) ->
  ?upload_progress:(int -> int -> unit) ->
  ?override_mime_type:string ->
  'a ->
  'b ->
  string Lwt.t
```
See [`Client.call_service`](./Eliom-Client.md#val-call_service).

```ocaml
val exit_to : 
  ?window_name:string ->
  ?window_features:string ->
  ?absolute:bool ->
  ?absolute_path:bool ->
  ?https:bool ->
  service:
    ('a, 'b, _, _, _, _, _, [< `WithSuffix | `WithoutSuffix ], _, _, _)
      Service.t ->
  ?hostname:string ->
  ?port:int ->
  ?fragment:string ->
  ?keep_nl_params:[ `All | `None | `Persistent ] ->
  ?nl_params:Parameter.nl_params_set ->
  ?keep_get_na_params:bool ->
  'a ->
  'b ->
  unit
```
See [`Client.exit_to`](./Eliom-Client.md#val-exit_to).

```ocaml
val window_open : 
  window_name:Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t ->
  ?window_features:Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t ->
  ?absolute:bool ->
  ?absolute_path:bool ->
  ?https:bool ->
  service:
    ('a, unit, _, _, _, _, _, [< `WithSuffix | `WithoutSuffix ], _, _, _)
      Service.t ->
  ?hostname:string ->
  ?port:int ->
  ?fragment:string ->
  ?keep_nl_params:[ `All | `None | `Persistent ] ->
  ?nl_params:Parameter.nl_params_set ->
  ?keep_get_na_params:bool ->
  'a ->
  Js_of_ocaml.Dom_html.window Js_of_ocaml.Js.t Js_of_ocaml.Js.opt
```
See [`Client.window_open`](./Eliom-Client.md#val-window_open).
