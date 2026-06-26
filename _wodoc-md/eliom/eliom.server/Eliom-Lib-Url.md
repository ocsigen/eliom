
# Module `Lib.Url`

```ocaml
type t = Url_base.t
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
```ocaml
val fixup_url_string : t -> t
```
```ocaml
val encode : ?plus:bool -> string -> string
```
```ocaml
val decode : ?plus:bool -> string -> string
```
```ocaml
val make_encoded_parameters : (string * string) list -> string
```
```ocaml
val string_of_url_path : encode:bool -> path -> uri
```
```ocaml
val parse : 
  t ->
  bool option
  * string option
  * int option
  * string
  * string list
  * string option
  * (string * string) list Lazy.t
```
`parse url` returns a tuple containing information about `url`

- If url contains scheme 'https'
- host of url (ex: http://www.ocsigen.org/ \-\> www.ocsigen.org)
- port of url
- path as `string` without first '/'
- path as `string list`
- GET query of url
- lazy value to decode GET query
```ocaml
val prefix_and_path_of_t : string -> string * string list
```
`prefix_and_path_of_t url` splits `url` in a couple `(prefix, path)` where `prefix` is `"http(s)://host:port"` and `path` is the path as `string list`

Example: `prefix_and_path_of_t "http://ocsigen.org:80/tuto/manual"` returns `("http://ocsigen.org:80", ["tuto", "manual"])`.
