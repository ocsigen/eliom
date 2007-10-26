exception Input_is_too_large
exception Ocsigen_Bad_Request
exception Ocsigen_Request_too_long
val id : 'a -> 'a
val comp : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
val list_remove_first_if_any : 'a -> 'a list -> 'a list
val list_remove_first_if_any_q : 'a -> 'a list -> 'a list
val list_remove_first : 'a -> 'a list -> 'a list
val list_remove_first_q : 'a -> 'a list -> 'a list
val list_remove_all : 'a -> 'a list -> 'a list
val list_remove_all_q : 'a -> 'a list -> 'a list
val list_remove_all_assoc : 'a -> ('a * 'b) list -> ('a * 'b) list
val list_remove_all_assoc_q : 'a -> ('a * 'b) list -> ('a * 'b) list
val list_last : 'a list -> 'a
val list_assoc_remove : 'a -> ('a * 'b) list -> 'b * ('a * 'b) list
val list_is_prefix : 'a list -> 'a list -> bool
val defaultpagename : string
val remove_dotdot : string list -> string list
val remove_slash_at_beginning : string list -> string list
val recursively_remove_slash_at_beginning : string list -> string list
val remove_slash_at_end : string list -> string list
val remove_internal_slash : string list -> string list
val add_end_slash_if_missing : string list -> string list
val change_empty_list : string list -> string list
val remove_end_slash : string -> string
val string_of_url_path : string list -> string
val string_first_diff : string -> string -> int -> int -> int
val add_to_string : string -> string -> string -> string
val concat_strings : string -> string -> string -> string
val basic_sep : char -> string -> string * string
val remove_spaces : string -> int -> int -> string
val sep : char -> string -> string * string
val split : char -> string -> string list
val string_of_exn : exn -> string
