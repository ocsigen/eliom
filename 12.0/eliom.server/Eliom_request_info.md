
# Module `Eliom_request_info`

This module contains the functions you need to get (or set) information about current request.


### Getting information about the request

```ocaml
val in_request_handler : unit -> bool
```
returns `true` when currently handling a request.

```ocaml
val get_http_method : unit -> Cohttp.Code.meth
```
returns the HTTP method used for the request (usually GET or POST).

```ocaml
val get_user_agent : unit -> string
```
returns the name of the user agent that did the request (usually the name of the browser).

```ocaml
val get_full_url : unit -> string
```
returns the full URL as a string

```ocaml
val get_client_conn_to_string : unit -> string
```
returns the address of the client in textual format. This is an IP address if the client is connected through the internet.

```ocaml
val get_original_full_path_string : unit -> string
```
returns the full path of the URL as first sent by the browser (not changed by previous extensions like rewritemod)

```ocaml
val get_original_full_path : unit -> Eliom_lib.Url.path
```
returns the full path of the URL as first sent by the browser (not changed by previous extensions like rewritemod)

```ocaml
val get_current_sub_path_string : unit -> string
```
returns the sub path of the URL as a string. The sub-path is the full path without the path of the site (set in the configuration file).

```ocaml
val get_current_sub_path : unit -> Eliom_lib.Url.path
```
returns the sub path of the URL using the type `Eliom_lib.Url.path`. The sub-path is the full path without the path of the site (set in the configuration file).

```ocaml
val get_header_hostname : unit -> string option
```
returns the hostname that has been sent by the user agent. For HTTP/1.0, the Host field is not mandatory in the request.

```ocaml
val get_hostname : unit -> string
```
returns the hostname used for absolute links. It is either the `Host` header sent by the browser or the default hostname set in the configuration file, depending on server configuration (`<usedefaulthostname/>` option).

```ocaml
val get_server_port : unit -> int
```
returns the port of the server. It is either the default port in the configuration file (if `<usedefaulthostname/>` is present is the configuration file), or the port in the Host header of the request (if present), or the port on which the request has been done (otherwise).

```ocaml
val get_ssl : unit -> bool
```
returns true if https is used, false if http.

```ocaml
val get_accept_language : unit -> (string * float option) list
```
returns the (string \* float option) list corresponding to accept\_language HTTP header of the request.

```ocaml
val get_suffix : unit -> Eliom_lib.Url.path option
```
returns the suffix of the current URL

```ocaml
val get_cookies : 
  ?cookie_level:Eliom_common.cookie_level ->
  unit ->
  string Ocsigen_cookie_map.Map_inner.t
```
returns the cookies sent by the browser

```ocaml
val get_timeofday : unit -> float
```
returns an Unix timestamp associated to the request


#### Exceptions and fallbacks

```ocaml
val get_request_cache : unit -> Polytables.t
```
returns a table in which you can store all the data you want during a request. It can also be used to send information after an action. Keep an eye on this information to know what succeeded before the current service was called (failed connection, timeout ...) The table is created at the beginning of the request.

```ocaml
val get_link_too_old : unit -> bool
```
returns `true` if the coservice called has not been found. In that case, the current service is the fallback.

```ocaml
val get_expired_service_sessions : 
  unit ->
  Eliom_common.full_state_name list * Eliom_common.full_state_name list
```
returns the list of names of service sessions expired for the current request, for browser sessions and tab sessions.

```ocaml
val get_previous_extension_error_code : unit -> int
```
returns the HTTP error code sent by the Ocsigen extension that tried to answer to the request before Eliom. It is 404 by default.

```ocaml
val get_tmp_filename : Ocsigen_extensions.file_info -> string
```
returns the filename used by Ocsigen for the uploaded file.

```ocaml
val get_filesize : Ocsigen_extensions.file_info -> int64
```
returns the size of the uploaded file.

```ocaml
val get_original_filename : Ocsigen_extensions.file_info -> string
```
returns the name the file had on the client when it has been sent.

```ocaml
val get_file_content_type : 
  Ocsigen_extensions.file_info ->
  ((string * string) * (string * string) list) option
```
returns the content type sent by the browser with the file (if any).

```ocaml
val get_site_dir : unit -> Eliom_lib.Url.path
```
returns the root of the site. Raises `Eliom_common.Eliom_site_information_not_available` if unavailable.

```ocaml
val get_site_dir_option : unit -> Eliom_lib.Url.path option
```
returns the root of the site.


### Getting parameters (low level)

The usual way to get parameters with Eliom is to use the second and third parameters of the service handlers. These are low level functions you may need for more advanced use.

```ocaml
val get_get_params : unit -> (string * string list) list
```
returns the parameters of the URL (GET parameters) that concern the running service. For example in the case of a non-attached coservice called from a page with GET parameters, only the parameters of that non-attached coservice are returned (even if the other are still in the URL).

```ocaml
val get_all_current_get_params : unit -> (string * string) list
```
returns current parameters of the URL (GET parameters) (even those that are for subsequent services, but not previous actions)

```ocaml
val get_other_get_params : unit -> (string * string) list
```
returns the parameters of the URL (GET parameters) that do not concern the running service.

```ocaml
val get_nl_get_params : unit -> (string * string) list Eliom_lib.String.Table.t
```
returns non localized parameters in the URL.

```ocaml
val get_persistent_nl_get_params : 
  unit ->
  (string * string) list Eliom_lib.String.Table.t
```
returns persistent non localized parameters in the URL.

```ocaml
val get_nl_post_params : 
  unit ->
  (string * string) list Eliom_lib.String.Table.t
```
returns non localized POST parameters.

```ocaml
val get_post_params : unit -> (string * string) list Lwt.t option
```
returns the parameters in the body of the HTTP request (POST parameters) that concern the running service. None means that POST data where neither urlencoded form data or multipart data.

```ocaml
val get_all_post_params : unit -> (string * string) list option
```
returns all parameters in the body of the HTTP request (POST parameters) (even those that are for another service)

```ocaml
val get_all_files : unit -> (string * Ocsigen_extensions.file_info) list option
```
returns all files in he HTTP request (even those that are for another service)

```ocaml
val get_ignored_get_params : unit -> (string * string) list
```
returns the GET parameters that have been ignored using \<ignoredgetparams/\> in config file.

```ocaml
val get_ignored_post_params : unit -> (string * string) list
```
returns the POST parameters that have been ignored using \<ignoredpostparams/\> in config file.


### Other low level functions

You probably don't need these functions.

```ocaml
val get_ri : unit -> Ocsigen_request.t
```
returns all the information about the request.

```ocaml
val get_request : unit -> Ocsigen_extensions.request
```
returns all the information about the request and config.

```ocaml
val get_state_name : unit -> Eliom_common.full_state_name option
```
returns the name of the sessions to which belongs the running service (`None` if it is not a session service)

```ocaml
val get_persistent_cookies : 
  unit ->
  string Eliom_common.Full_state_name_table.t
```
returns the values of the Eliom's cookies for persistent sessions sent by the browser.

```ocaml
val get_data_cookies : unit -> string Eliom_common.Full_state_name_table.t
```
returns the values of Eliom's cookies for non persistent sessions sent by the browser.

```ocaml
val expecting_process_page : unit -> bool
```
Returns `true` if the request was done by a client side Eliom program, which was expecting to receive a new HTML page to display inside the process.


#### Getting information about the URL of the client side process (csp)

Warning: it is different from the URL to which the request has been made.

```ocaml
val get_csp_original_full_path : unit -> Eliom_lib.Url.path
```
returns the full path of the URL where the client-side process is running. If there is no client side process, same as [`get_original_full_path`](./#val-get_original_full_path).

```ocaml
val get_csp_hostname : unit -> string
```
returns the hostname used for absolute links, computed when launching the client side process for the first time. If there is no client side process, same as [`get_hostname`](./#val-get_hostname).

It is either the `Host` header sent by the browser or the default hostname set in the configuration file, depending on server configuration (`<usedefaulthostname/>` option).

```ocaml
val get_csp_server_port : unit -> int
```
returns the port of the server, used when launching the client side process (not the current request). It corresponds to the port in the URL of the browser. If there is no client side process, same as [`get_server_port`](./#val-get_server_port).

```ocaml
val get_csp_ssl : unit -> bool
```
returns true if https is used in the URL of the browser, false if http. If there is no client side process, same as [`get_ssl`](./#val-get_ssl).

```ocaml
val get_sitedata : unit -> Eliom_common.sitedata
```