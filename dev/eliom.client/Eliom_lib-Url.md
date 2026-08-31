# Module `Eliom_lib.Url`

URL manipulation

See `Ocsigen_lib.Url_base`.

```ocaml
type t = string
```
```ocaml
type uri = string
```
```ocaml
val make_absolute_url : https:bool -> host:string -> port:int -> uri -> t
```
`make_absolute_url https host port path` generates a new absolute url

```ocaml
type path = string list
```
```ocaml
val remove_dotdot : path -> path
```
`remove_dotdot path` cleans the path of `..`

```ocaml
val remove_end_slash : string -> string
```
`remove_end_slash str` removes last `/`

```ocaml
val remove_internal_slash : path -> path
```
`remove_internal_slash path` cleans the path of empty string

```ocaml
val change_empty_list : path -> path
```
```ocaml
val add_end_slash_if_missing : path -> path
```
```ocaml
val remove_slash_at_end : path -> path
```
```ocaml
val remove_slash_at_beginning : path -> path
```
```ocaml
val is_prefix_skip_end_slash : string list -> string list -> bool
```
`is_prefix_skip_end_slash path1 path2` returns `true` if `path1` is the same as `path2` before a first slash

```ocaml
val split_fragment : string -> string * string option
```
`split_fragment str` splits `str` at first '\#'

```ocaml
val join_path : path -> string
```
See [`Js_of_ocaml.Url`](./../../js_of_ocaml/js_of_ocaml/Js_of_ocaml-Url.md).

The first functions are mainly from and to string conversion functions for the different parts of a url.

```ocaml
val urldecode : string -> string
```
`urldecode s` swaps percent encoding characters for their usual representation.

```ocaml
val urlencode : ?with_plus:bool -> string -> string
```
`urlencode ?with_plus s` replace characters for their percent encoding representation. Note that the '/' (slash) character is escaped as well. If `with_plus` is `true` (default) then `'+'`'s are escaped as `"%2B"`. If not, `'+'`'s are left as is.

```ocaml
type http_url = Js_of_ocaml.Url.http_url = {
  hu_host : string; (* The host part of the url. *)
  hu_port : int; (* The port for the connection if any. *)
  hu_path : string list; (* The path split on '/' characters. *)
  hu_path_string : string; (* The original entire path. *)
  hu_arguments : (string * string) list; (* Arguments as a field-value association list. *)
  hu_fragment : string; (* The fragment part (after the '#' character). *)
}
```
The type for HTTP(s) url.

```ocaml
type file_url = Js_of_ocaml.Url.file_url = {
  fu_path : string list;
  fu_path_string : string;
  fu_arguments : (string * string) list;
  fu_fragment : string;
}
```
The type for local file urls.

```ocaml
type url = Js_of_ocaml.Url.url = 
  | Http of http_url (* Non secure HTTP urls *)
  | Https of http_url (* Secure HTTPS urls *)
  | File of file_url (* Local files *)
```
The type for urls.

```ocaml
val default_http_port : int
```
The default port for `Http` communications (80).

```ocaml
val default_https_port : int
```
The default port for `Https` communications (443).

```ocaml
val path_of_path_string : string -> string list
```
`path_of_path_string s` splits `s` on each `"/"` character.

```ocaml
val encode_arguments : (string * string) list -> string
```
`encode_arguments a` expects a list of pair of values of the form `(name,value)` were `name` is the name of an argument and `value` it's associated value.

```ocaml
val decode_arguments : string -> (string * string) list
```
`decode_arguments s` parses `s` returning the sliced-diced association list. `s` should be only the arguments part (after the '?') not the whole url.

The following part allow one to handle Url object in a much higher level than what a string provides.

```ocaml
val url_of_string : string -> url option
```
`url_of_string s` parses `s` and builds a value of type `url` if `s` is not a valid url string, it returns `None`.

```ocaml
val string_of_url : url -> string
```
`string_of_url u` returns a valid string representation of `u`. Note that \* `string_of_url ((fun Some u -> u) (url_of_string s))` is NOT necessarily \* equal to `s`. However `url_of_string (string_of_url u) = u`.

```ocaml
module Current = Js_of_ocaml.Url.Current
```
This module can be used to handle the Url associated to the current document.

```ocaml
val decode : string -> string
```
```ocaml
val encode : ?plus:bool -> string -> string
```
```ocaml
val make_encoded_parameters : (string * string) list -> string
```
```ocaml
val split_path : string -> string list
```
```ocaml
val get_ssl : string -> bool option
```
```ocaml
val resolve : string -> string
```
```ocaml
val add_get_args : string -> (string * string) list -> string
```
```ocaml
val string_of_url_path : encode:bool -> string list -> string
```
```ocaml
val path_of_url : url -> string list
```
```ocaml
val path_of_url_string : string -> string list
```
Extracts path from a URL string. Works on a best-effort basis for relative URLs
