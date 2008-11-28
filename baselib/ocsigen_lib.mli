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
val list_is_prefix_skip_end_slash : string list -> string list -> bool
val remove_dotdot : string list -> string list

val remove_slash_at_beginning : string list -> string list
(** Remove the slash at beginning but if it is also at the end *)

val recursively_remove_slash_at_beginning : string list -> string list
(** Recursively remove the slash at beginning but if it is also at the end *)

val remove_slash_at_end : string list -> string list
val remove_internal_slash : string list -> string list
val add_end_slash_if_missing : string list -> string list
val change_empty_list : string list -> string list
val remove_end_slash : string -> string
val string_of_url_path : encode:bool -> string list -> string
val string_first_diff : string -> string -> int -> int -> int
val add_to_string : string -> string -> string -> string
val concat_strings : string -> string -> string -> string
val basic_sep : char -> string -> string * string
val remove_spaces : string -> int -> int -> string

(** Cut a string to the next separator, removing spaces.
   Raises Not_found if the separator connot be found.
 *)
val sep : char -> string -> string * string

val split : ?multisep:bool -> char -> string -> string list

val string_of_exn : exn -> string
  (** [string_of_exn e] returns a (hopefully) meaningful explanation of
      the exception [e]. *)

val register_exn_printer : ((exn -> string) -> exn -> string) -> unit
  (** [register_exn_printer p] registers [p] so that a call to [p
      string_of_exn e] is tried first in [string_of_exn e]. [p] must raise
      [e] if it doesn't handle it. *)

val fst3 : 'a * 'b * 'c -> 'a
val snd3 : 'a * 'b * 'c -> 'b
val thd3 : 'a * 'b * 'c -> 'c

type ('a, 'b) leftright = Left of 'a | Right of 'b

val get_inet_addr : string -> Unix.inet_addr Lwt.t
(** returns the first inet address for one host *)

(** IP address parsing *)
type ip_address =
  | IPv4 of int32
  | IPv6 of int64 * int64
exception Invalid_ip_address of string
val parse_ip : string -> ip_address * (ip_address option)
val match_ip : ip_address * (ip_address option) -> ip_address -> bool

val getnameinfo : Unix.inet_addr -> int -> string Lwt.t
(** calls Lwt_lib.getnameinfo and returns the result,
    but if it fails returns the IP number,
    with [ before and ] after IPv6 addresses. *)

val basename : string -> string
(** Extension of a file. Raises [Not_found] if the argument has no
  extension *)
val extension : string -> string
val fixup_url_string : string -> string
val parse_url : string ->
  string option * int option *
    string * Neturl.url * string list * string option *
    (string * string) list Lazy.t

module StringSet : Set.S with type elt = string
