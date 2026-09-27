(** Tabs of simulated browsers, as the client-side program of Eliom handles
    them: a tab shares the cookies of its browser, and has its own tab
    cookies, sent in a header of its requests and updated from a header of
    the responses. *)

type t
(** A tab. *)

val create : Eliom_test_server.Browser.t -> t
(** [create b] is a new tab of [b], with no tab cookie. *)

val get : t -> string -> Eliom_test_server.Browser.response Lwt.t
(** [get t url] sends a GET request for [url] from [t], as
    {!Eliom_test_server.Browser.get}, with the tab cookies of [t], and keeps
    the tab cookies set by the response. *)

val post :
   t
  -> string
  -> (string * string) list
  -> Eliom_test_server.Browser.response Lwt.t
(** [post t url params] sends a POST request from [t], as {!get}. *)

val tab_cookies : t -> (string * string) list
(** [tab_cookies t] are the names and values of the tab cookies of [t]. *)

val set_tab_cookies : t -> (string * string) list -> unit
(** [set_tab_cookies t cookies] replaces the tab cookies of [t]. *)
