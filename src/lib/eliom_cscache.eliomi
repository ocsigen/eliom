(* Copyright Vincent Balat *)

(** Client-server cache for Eliom applications,
    that is used both for keeping data on client side
    or keeping values in memory during a request.
    This module hides many subtelties that are useful for
    client-server (possibly reactive) applications, like, for
    example, handling delays correctly both on client and server sides.
*)

{shared{

   (** The type of association tables (implemented using Hashtbl) *)
type ('a, 'b) t

}}

{server{

   (** Create a new table. Must be called from server side. *)
val create : unit -> ('a, 'b) t

}}

{shared{

  (** [find cache get_from_db key]
      returns the value associated to [key] in cache.
      If not present, it calls [get_from_db] to fetch the data from
      the database. [get_from_db] must be implemented both on server
      and client sides.
  *)
val find : ('a, 'b) t -> ('a -> 'b Lwt.t) -> 'a -> 'b Lwt.t

}}

{shared{

   (** [do_cache cache key value] adds the association from [key]
       to [value] in [cache], if not already present.
       Called from client side, it affects only client side cache.
       Called from server side, it will have effect both on the
       server cache (scope: request) and the client side cache.
   *)
val do_cache : ('a, 'b) t -> 'a -> 'b -> unit

}}

{client{

  (** Find a piece of data in local cache,
      without trying to fetch it from server. Raises [Not_found] instead.
      If the value is currently beeing retrieved, it waits for it to be
      ready before returning.
  *)
  val local_find : ('a, 'b) t -> 'a -> 'b Lwt.t


  exception Not_ready

  (** Find a piece of data in local cache,
      without trying to fetch it from server. Raises [Not_found] instead.
      If the value is currently beeing retrieved, it raises [Not_ready].
  *)
  val find_if_ready : ('a, 'b) t -> 'a -> 'b

  (** Load (or reload) in cache the piece of data from server  *)
  val load : ('a, 'b) t -> ('a -> 'b Lwt.t) -> 'a -> 'b Lwt.t

}}
