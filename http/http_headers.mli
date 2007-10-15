
(*XXX Can have multiple headers with the same name...*)
type name

val name : string -> name
val name_to_string : name -> string

module NameHtbl : Hashtbl.S with type key = name

(****)

val accept : name
val accept_charset : name
val accept_encoding : name
val accept_language : name
val accept_ranges : name
val cache_control : name
val connection : name
val content_encoding : name
val content_length : name
val content_type : name
val cookie : name
val date : name
val etag : name
val expires : name
val host : name
val if_match : name
val if_modified_since : name
val if_none_match : name
val if_unmodified_since : name
val last_modified : name
val location : name
val server : name
val set_cookie : name
val status : name
val transfer_encoding : name
val user_agent : name
val referer : name

(****)

type t

val empty : t
val add : name -> string -> t -> t
val replace : name -> string -> t -> t
val replace_opt : name -> string option -> t -> t
val find : name -> t -> string
val find_all : name -> t -> string list
val iter : (name -> string -> unit) -> t -> unit
val fold : (name -> string list -> 'a -> 'a) -> t -> 'a -> 'a

val with_defaults : t -> t -> t
