(** The requested file does not exists *)
exception Failed_404
(** The requested file cannot be served: does not exists, not
    enough permissions ... *)
exception Failed_403



(** Options to permit or forbid the display of a local file. Options
    are per directory *)
type options = {
  (** Should the list of files in the directory be displayed? *)
  list_directory_content : bool;

  (** Should symlinks be followed when accessign a file? *)
  follow_symlinks: follow_symlink;

  (** Default name to use as index file when a directory is requested.
      Use [None] if no index should be tried. The various indexes
      are tried in the given order. If no index is specified,
      or the index does not exists, the content of the directory
      might be listed, according to [list_directry_content] *)
  default_directory_index : string list;
}
and follow_symlink =
  | DoNotFollow (** Never follow a symlink *)
  | FollowIfOwnerMatch (** Follow a symlink if the symlink and its
                          target have the same owner *)
  | AlwaysFollow (** Always follow symlinks *)


(*
(** Default options:
    - never display a file (this field must thus be overridden)
    - follow symlinks if owner match
    - use "index.html" as default index
    - do not list the content of directories
*)
val default_display_options : options
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
    Failed_404 or Failed_403 accordingly *)
val resolve :
  filename:string -> options:options -> resolved


(** Given the local file [file], with a request originating at url
    [url], returns a viewable content of [file]. Currently, the [url]
    parameter is used only if [url] is a directory *)
val content:
  url:string list -> file:resolved -> Ocsigen_http_frame.result Lwt.t
