(** Cookie jars of simulated browsers, kept as RFC 6265 says, for a single
    host reached without HTTPS. *)

type t
(** A set of cookies. *)

val empty : t
(** [empty] is the jar with no cookie. *)

val store : now:float -> path:string -> string list -> t -> t
(** [store ~now ~path set_cookies jar] is [jar] with the cookies of
    [set_cookies], the values of the [Set-Cookie] headers of the response to
    a request for [path], received at [now]. A cookie replaces the one of
    [jar] with the same name and path, and an expired cookie removes it. *)

val header : now:float -> path:string -> t -> string option
(** [header ~now ~path jar] is the value of the [Cookie] header of a request
    for [path] at [now], if a cookie of [jar] matches: its path matches
    [path], it has not expired and it is not secure. Cookies with longer
    paths come first. *)

val cookies : now:float -> t -> (string * string) list
(** [cookies ~now jar] are the names and values of the cookies of [jar] not
    expired at [now], sorted by name. *)

val parse_date : string -> float option
(** [parse_date s] is the time of the cookie date [s], in the format written
    by Ocsigen Server, e.g. ["Thu, 01 Jan 1970 00:00:00 GMT"], if [s] is a
    valid date in this format. *)
