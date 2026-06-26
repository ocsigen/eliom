
# Module `Lib.Url_base`

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