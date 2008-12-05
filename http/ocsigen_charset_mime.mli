type extension = string


(** Charset *)

(** By convention, "no specified charset" is represented by the empty
    string *)
type charset = string
val no_charset: charset

(** Association between extensions and charset, with a default value. *)
type charset_assoc

val empty_charset_assoc: ?default:charset -> unit -> charset_assoc

val default_charset: charset_assoc:charset_assoc -> charset


val update_charset_assoc:
  charset_assoc:charset_assoc -> extension:extension -> charset:charset ->
  charset_assoc

val set_default_charset:
  charset:charset -> charset_assoc:charset_assoc -> charset_assoc


(** Find the charset of a file with extension [extension] *)
val find_charset: charset_assoc:charset_assoc -> extension:extension -> charset

(** Find the charset of a file *)
val find_charset_file: charset_assoc:charset_assoc -> filename:string -> charset




(** Content-type *)

(** MIME types; the default value is ["application/octet-stream"] *)
type mime_type = string

val default_mime_type : mime_type

(** association between extensions and mime types, with default value *)
type mime_assoc

val default_mime_assoc: unit -> mime_assoc

val parse_mime_types: filename:string -> mime_assoc


val update_mime_assoc:
  mime_assoc:mime_assoc -> extension:string -> mime:mime_type -> mime_assoc

val set_default_mime:
  mime_type:mime_type -> mime_assoc:mime_assoc -> mime_assoc


(** Search for the mime-type of a file. Same syntax as for charsets *)
val find_mime_type:
  mime_assoc:mime_assoc -> extension:extension -> mime_type

val find_mime_type_file:
  mime_assoc:mime_assoc -> filename:string -> mime_type

