(** The forms of a page, submitted as a browser does. The page must be XHTML,
    as Eliom prints it. *)

type t =
  { meth : [`GET | `POST]
  ; action : string  (** The path and query of the URL of the form *)
  ; fields : (string * string) list
    (** The name and value of each field, in the order of the page *) }
(** A form, with the values that a browser submits when nothing is changed:
    text and hidden inputs, checked checkboxes and radio buttons, selected
    options (the first one of a select without selected option) and text
    areas. Buttons are not submitted. *)

val forms : url:string -> string -> t list
(** [forms ~url page] are the forms of [page], the page at [url] (a path and
    query), whose actions are resolved against [url]. *)

val set : string -> string -> t -> t
(** [set name value form] is [form] where the fields named [name] are
    replaced by one field of value [value]. *)

val submit : Browser.t -> t -> Browser.response Lwt.t
(** [submit b form] sends [form] from [b]: a GET form replaces the query of
    its action by its fields, a POST form sends them url-encoded. *)
