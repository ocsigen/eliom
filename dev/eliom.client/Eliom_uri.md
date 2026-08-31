# Module `Eliom_uri`

Low-level functions for relative or absolute URL calculation.

### Compute service's URL

Please note that for many functions of this section, the returned URL depends on whether the function is called from a service handler or not:

- relative URL could not be computed outside of a service handler.
- "kept" non localized parameters outside a service handler are restricted to preapplied parameters.
To define *global* link (i.e. outside of a service handler) and recompute a relative URL at each request, use `Eliom_registration.Html.a` or other specialized functions from [`Eliom_registration.Html`](./Eliom_registration-Html.md).

```ocaml
val make_string_uri : 
  ?absolute:bool ->
  ?absolute_path:bool ->
  ?https:bool ->
  service:
    ('get, unit, Eliom_service.get, _, _, _, _, _, _, unit, 'return)
      Eliom_service.t ->
  ?hostname:string ->
  ?port:int ->
  ?fragment:string ->
  ?keep_nl_params:[ `All | `None | `Persistent ] ->
  ?nl_params:Eliom_parameter.nl_params_set ->
  'get ->
  string
```
The function `make_string_uri ~service get_params` creates the string corresponding to the URL of the service `service` applied to the GET parameters `get_params`.

See `Eliom_registration.Html.make_string_uri` or any other [`Eliom_registration`](./Eliom_registration.md)`.*.make_string_uri` for a detailed description of optional parameters.

*Warning: The function `make_string_uri` should not be called outside of a service handler, unless `hostname` is not `None` and one of the following condition is met:*

- the optional parameter `~absolute_path` is `true`.
- the optional parameter `~absolute` is `true`.
- the optional parameter `~https` is `true`.
- the `service` has been created with `~https:true`.
- the `service` is an external service.
```ocaml
val make_uri_components : 
  ?absolute:bool ->
  ?absolute_path:bool ->
  ?https:bool ->
  service:('get, _, Eliom_service.get, _, _, _, _, _, _, _, _) Eliom_service.t ->
  ?hostname:string ->
  ?port:int ->
  ?fragment:string ->
  ?keep_nl_params:[ `All | `None | `Persistent ] ->
  ?nl_params:Eliom_parameter.nl_params_set ->
  'get ->
  string * (string * Eliommod_parameters.param) list * string option
```
The function `make_uri_components service get_params` returns the a triplet `(path, get_params, fragment)` that is a decomposition of the URL of `service` applied to the GET parameters `get_params`.

See `Eliom_registration.Html.make_uri_components` or any other [`Eliom_registration`](./Eliom_registration.md)`.*.make_uri_components` for a detailed description.

```ocaml
val make_post_uri_components : 
  ?absolute:bool ->
  ?absolute_path:bool ->
  ?https:bool ->
  service:
    ('get, 'post, Eliom_service.post, _, _, _, _, _, _, _, _) Eliom_service.t ->
  ?hostname:string ->
  ?port:int ->
  ?fragment:string ->
  ?keep_nl_params:[ `All | `Persistent | `None ] ->
  ?nl_params:Eliom_parameter.nl_params_set ->
  ?keep_get_na_params:bool ->
  'get ->
  'post ->
  string
  * (string * Eliommod_parameters.param) list
  * string option
  * (string * Eliommod_parameters.param) list
```
Same a [`make_uri_components`](./#val-make_uri_components), but also returns a table of post parameters.

```ocaml
val make_string_uri_from_components : 
  (string * (string * Eliommod_parameters.param) list * string option) ->
  string
```
The function `make_string_uri_from_components path get_params fragment` build the corresponding string URL. The `path` should be URL encoded.

The function [`make_string_uri`](./#val-make_string_uri) is the composition of [`make_uri_components`](./#val-make_uri_components) and `make_string_uri_from_components`.

### Relative paths

```ocaml
val reconstruct_relative_url_path : string list -> string list -> string list
```
The function `reconstruct_relative_url_path src dest` returns a path to `dest` that is relative to `src`.
