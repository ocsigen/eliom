# Module `Eliom.Request`

```ocaml
exception Looping_redirection
```
```ocaml
exception Failed_request of int
```
```ocaml
exception Program_terminated
```
```ocaml
exception Non_xml_content
```
```ocaml
val redirect_get : 
  ?window_name:string ->
  ?window_features:string ->
  string ->
  unit
```
```ocaml
val redirect_post : 
  ?window_name:string ->
  string ->
  (string * Mod_parameters.param) list ->
  unit
```
```ocaml
val redirect_put : 
  ?window_name:string ->
  string ->
  (string * Mod_parameters.param) list ->
  unit
```
```ocaml
val redirect_delete : 
  ?window_name:string ->
  string ->
  (string * Mod_parameters.param) list ->
  unit
```
```ocaml
type 'a result
```
```ocaml
val xml_result : 
  Js_of_ocaml.Dom.element Js_of_ocaml.Dom.document Js_of_ocaml.Js.t result
```
```ocaml
val string_result : string result
```
```ocaml
val locked : bool React.signal
```
```ocaml
val lock : unit -> unit
```
```ocaml
val unlock : unit -> unit
```
```ocaml
module Additional_headers : sig ... end
```
```ocaml
val send : 
  ?with_credentials:bool ->
  ?expecting_process_page:bool ->
  ?cookies_info:(bool * string list) ->
  ?get_args:(string * string) list ->
  ?post_args:(string * Mod_parameters.param) list ->
  ?override_method:
    [ `GET | `POST | `HEAD | `PUT | `DELETE | `OPTIONS | `PATCH ] ->
  ?progress:(int -> int -> unit) ->
  ?upload_progress:(int -> int -> unit) ->
  ?override_mime_type:string ->
  string ->
  'a result ->
  (string * 'a option) Lwt.t
```
```ocaml
val send_get_form : 
  ?with_credentials:bool ->
  ?expecting_process_page:bool ->
  ?cookies_info:(bool * string list) ->
  ?get_args:(string * string) list ->
  ?post_args:(string * Mod_parameters.param) list ->
  ?progress:(int -> int -> unit) ->
  ?upload_progress:(int -> int -> unit) ->
  ?override_mime_type:string ->
  ?submitter:Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t ->
  Js_of_ocaml.Dom_html.formElement Js_of_ocaml.Js.t ->
  string ->
  'a result ->
  (string * 'a option) Lwt.t
```
`send_get_form form url result` sends the fields of `form` to `url` through XHR, in the query string, after `~get_args`. With `~post_args`, the request is a POST whose body holds `~post_args`, the fields staying in the query string. `~submitter` is the button that submitted `form`, the `submitter` of its submit event: its name and value are sent, before the fields, as a native submission sends them.

```ocaml
val send_post_form : 
  ?with_credentials:bool ->
  ?expecting_process_page:bool ->
  ?cookies_info:(bool * string list) ->
  ?get_args:(string * string) list ->
  ?post_args:(string * Mod_parameters.param) list ->
  ?progress:(int -> int -> unit) ->
  ?upload_progress:(int -> int -> unit) ->
  ?override_mime_type:string ->
  ?submitter:Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t ->
  Js_of_ocaml.Dom_html.formElement Js_of_ocaml.Js.t ->
  string ->
  'a result ->
  (string * 'a option) Lwt.t
```
`send_post_form form url result` is like [`send_get_form`](./#val-send_get_form) but sends the fields of `form` in the body of a POST request, before `~post_args`. `~get_args` go in the query string.

```ocaml
val http_get : 
  ?with_credentials:bool ->
  ?expecting_process_page:bool ->
  ?cookies_info:(bool * string list) ->
  ?progress:(int -> int -> unit) ->
  ?upload_progress:(int -> int -> unit) ->
  ?override_mime_type:string ->
  string ->
  (string * string) list ->
  'a result ->
  (string * 'a option) Lwt.t
```
```ocaml
val http_post : 
  ?with_credentials:bool ->
  ?expecting_process_page:bool ->
  ?cookies_info:(bool * string list) ->
  ?progress:(int -> int -> unit) ->
  ?upload_progress:(int -> int -> unit) ->
  ?override_mime_type:string ->
  string ->
  (string * Mod_parameters.param) list ->
  'a result ->
  (string * 'a option) Lwt.t
```
```ocaml
val http_put : 
  ?with_credentials:bool ->
  ?expecting_process_page:bool ->
  ?cookies_info:(bool * string list) ->
  ?progress:(int -> int -> unit) ->
  ?upload_progress:(int -> int -> unit) ->
  ?override_mime_type:string ->
  string ->
  (string * Mod_parameters.param) list ->
  'a result ->
  (string * 'a option) Lwt.t
```
```ocaml
val http_delete : 
  ?with_credentials:bool ->
  ?expecting_process_page:bool ->
  ?cookies_info:(bool * string list) ->
  ?progress:(int -> int -> unit) ->
  ?upload_progress:(int -> int -> unit) ->
  ?override_mime_type:string ->
  string ->
  (string * Mod_parameters.param) list ->
  'a result ->
  (string * 'a option) Lwt.t
```
```ocaml
val get_cookie_info_for_uri_js : 
  Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t ->
  bool * string list
```
```ocaml
val max_redirection_level : int
```
