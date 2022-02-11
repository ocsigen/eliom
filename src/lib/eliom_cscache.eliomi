(* Copyright Vincent Balat *)

(** Client-server cache for Eliom applications, that is used both for
    keeping data on client side or keeping values in memory during a
    request.

    When you get a piece of data through Eliom_cscache from client-side,
    the request to the server is done only if the data is not already in
    the client-side cache.

    On server side, Eliom_cscache is using a temporary cache
    (with "request" scope) to avoid fetching the data several time from the
    database during the same request. This server-side cache is
    automatically sent to the client to fill the client-side
    cache. If you want to avoid too many requests from the
    client, prefill the server-side cache with the data the
    client program will need.

    This module hides many subtelties that are useful for
    client-server (possibly reactive) applications, like, for example,
    handling delays correctly both on client and server sides.

    In the near future, Eliom_cscache will make it possible to save persistent
    data, which is useful for implementing off-line applications.

*)

[%%shared.start]

type ('a, 'b) t
(** The type of association tables (implemented using Hashtbl) *)

[%%shared.start]

val find : ('a, 'b) t -> ('a -> 'b Lwt.t) -> 'a -> 'b Lwt.t
(** [find cache get_from_db key] returns the value associated to [key]
    in cache.  If not present, it calls [get_from_db] to fetch the
    data from the database. [get_from_db] must be implemented both
    server and client sides.  Several simultaneous call to [find] will
    result in a single call to [get_from_db]. Exceptions are not
    cached.  *)

val do_cache : ('a, 'b) t -> 'a -> 'b -> unit
(** [do_cache cache key value] adds the association from [key] to
    [value] in [cache], or replaces it if not already present.  Called
    from client side, it affects only client side cache.  Called from
    server side, it will have effect both on the server cache (scope:
    request) and the client side cache.  *)

val local_find : ('a, 'b) t -> 'a -> 'b Lwt.t
(** Find a piece of data in local cache, without trying to fetch it
    from server. Raises [Not_found] instead.  If the value is currently
    being retrieved, it waits for it to be ready before returning.  *)

exception Not_ready

val find_if_ready : ('a, 'b) t -> 'a -> 'b
(** Find a piece of data in local cache, without trying to fetch it
    from server. Raises [Not_found] instead.  If the value is currently
    being retrieved, it raises [Not_ready].  *)

[%%client.start]

val load : ('a, 'b) t -> ('a -> 'b Lwt.t) -> 'a -> 'b Lwt.t
(** Load (or reload) in cache the piece of data from server  *)

[%%server.start]

val create : unit -> ('a, 'b) t
(** Create a new table. Must be called from server side. *)
