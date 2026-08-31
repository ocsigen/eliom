# Module `Eliom_request_info`

This module contains the functions you need to get (or set) information about current request.

### Getting information about the request

```ocaml
val get_original_full_path_string : unit -> string
```
returns the full path of the URL as first sent by the browser (not changed by previous extensions like rewritemod)

```ocaml
val get_nl_get_params : unit -> (string * string) list Eliom_lib.String.Table.t
```
returns non localized parameters in the URL.

```ocaml
val get_site_dir : unit -> Eliom_lib.Url.path
```
returns the root of the site.

```ocaml
val get_site_dir_option : unit -> Eliom_lib.Url.path option
```
returns the root of the site.

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

#### Getting information about the URL of the client side process (csp)

Warning: it is different from the URL to which the request has been made.

```ocaml
val get_csp_original_full_path : unit -> Eliom_lib.Url.path
```
returns the full path of the URL where the client-side process is running. If there is no client side process, same as `get_original_full_path`.

```ocaml
val get_csp_ssl : unit -> bool
```
returns true if https is used in the URL of the browser, false if http. If there is no client side process, same as `get_ssl`.
