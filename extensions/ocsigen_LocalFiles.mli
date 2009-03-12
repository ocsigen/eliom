(** The requested file does not exists *)
exception Failed_404
(** The requested file cannot be served: does not exists, not
    enough permissions ... *)
exception Failed_403
(** The file is a directory which we should not display *)
exception NotReadableDirectory



(*
(** Default options:
    - never follow symlinks
    - use "index.html" as default index
    - do not list the content of directories
*)
val default_options : options
*)


(** Local file corresponding to a request. The string argument
   represents the real file or directory to serve, eg. foo/index.html
   instead of foo *)
type resolved =
  | RFile of string
  | RDir of string


(** Finds [filename] in the filesystem, with a possible redirection
    if it is a directory. Takes into account the fact that [filename]
    does not exists, is a symlink or is a directory, and raises
    Failed_404 or Failed_403 accordingly. [no_check_for] is supposed
    to be a prefix of [filename] ; directories above [no_check_for]
    are not checked for symlinks *)
val resolve :
  ?no_check_for:string ->
  request:Ocsigen_extensions.request ->
  filename:string -> resolved


(** Given the local file [file], with a request originating at url
    [url], returns a viewable content of [file]. Currently, the [url]
    parameter is used only if [url] is a directory *)
val content:
  request:Ocsigen_extensions.request -> file:resolved -> Ocsigen_http_frame.result Lwt.t
