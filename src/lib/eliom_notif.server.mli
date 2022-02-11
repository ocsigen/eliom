(** Server to client notifications.

    This module makes it possible for client side applications to be
    notified of changes on some indexed data (resources) on the server.

    Apply functor [Make] or [Make_Simple] for each type of data you want to be
    able to listen on. Each client starts listening on one piece of data by
    calling function [listen] with the index of that piece of data as parameter.
    Client stops listening by calling function [unlisten], or when the client
    side state is closed (by timeout or when the client disconnects for
    example).

    When the data is modified on server side, call function [notify]
    with the index of the data, and all clients listening to that piece
    of data will receive a notification.

    The functor will also create a client side react signal that will
    be updated every time the client is notified.

    See module Os_notif in Ocsigen Start for an example of use.
*)

(* TODO: allow for specifying the scope instead of hard-wiring
         ~scope:Eliom_common.default_process_scope *)
(* TODO: terminology: identity/client/user/listener *)

(** Signature of the functors [Eliom_notif.Make] and [Eliom_notif.Make_Simple].

    [S] has two types of notifications ([server_notif] and [client_notif])
    because we might need to serialise and deserialise the notification twice
    (in case of a multi-server set-up). Once for broadcasting it to other
    servers and once for transferring it to the client (after possibly
    transforming the message using information which is only locally available
    (see [prepare] below).
*)
module type S = sig
  type identity
  (** [identity] is the type of values used to differentiate one listener
      from another. Typically it will be a user, but it could also for
      instance be a chat window. *)

  type key
  (** [key] is the type of values designating a given resource. *)

  type server_notif
  (** server notification type; Can be different from [client_notif]. *)

  type client_notif
  (** client notification type; Can be different from [server_notif]. *)

  val init : unit -> unit Lwt.t
  (** Initialise the notification module for the current client. This function
      needs to be called before using most other functions of this module. It
      isn't called implicitly during module instantiation because it relies on
      identity data which might not be available yet. *)

  val deinit : unit -> unit
  (** Deinitialise/deactivate the notification module for the current client. *)

  val listen : key -> unit
  (** Make client process listen on data whose index is [key] *)

  val unlisten : key -> unit
  (** Stop listening on data [key] *)

  module Ext : sig
    val unlisten
      :  ?sitedata:Eliom_common.sitedata
      -> ([< `Client_process], [< `Data]) Eliom_state.Ext.state
      -> key
      -> unit
    (** Make a listener stop listening on data [key].
      If this function is called during a request it will be able to determine
      [sitedata] by itself, otherwise it needs to be supplied by the caller. *)
  end

  val notify : ?notfor:[`Me | `Id of identity] -> key -> server_notif -> unit
  (** Call [notify key n] to send a notification [n] to all clients currently
      listening on data referenced by [key].

      If [~notfor] is [`Me], notification will not be sent to the tab currently
      doing the request (the one which caused the notification to happen).
      Note that if [notify] is called with [~notfor:`Me] outside of a request it
      will fail. If it is [`Id id] it won't be sent to the destination defined
      by [id].
  *)

  val client_ev : unit -> (key * client_notif) Eliom_react.Down.t
  (** Returns the client react event.

      ['a Eliom_react.Down.t] = ['a React.E.t] on client side.

      Map a function on this event to react
      to notifications from the server.
      For example:

      let%client handle_notification some_stuff ev =
         ...

      let%server something some_stuff =
         ignore
           [%client
              (ignore (React.E.map
                (handle_notification ~%some_stuff)
                ~%(Notif_module.client_ev ())
              ) : unit)
           ]

  *)

  val clean : unit -> unit
  (** Call [clean ()] to clear the tables from empty data. *)
end

(** [ARG] is for making [Make] *)
module type ARG = sig
  type identity
  (** see [S.identity] *)

  type key
  (** see [S.key] *)

  type server_notif
  (** see [S.server_notif] *)

  type client_notif
  (** see [S.client_notif] *)

  val prepare : identity -> server_notif -> client_notif option Lwt.t
  (** [prepare f] transforms server notifications into client
      notifications. It provides the [identity] as a parameter which identifies
      the client. You can suppress notifications for a specific client (for
      instance because of missing authorisation) by having [f] return [None]. *)

  val equal_key : key -> key -> bool
  (** [equal_key] is a function testing the equality between two values
      of type [key].*)

  val equal_identity : identity -> identity -> bool
  (** [equal_identity] is the same as [equal_key] but for values of type
      [identity].*)

  val get_identity : unit -> identity Lwt.t
  (** [get_identity] is a function returning a value of type [identity]
      corresponding to a client. *)

  val max_resource : int
  (** [max_resource] is the initial size for the hash table storing the data of
      clients listening on resources, for best results it should be on the
      order of the expected number of different resources one may want to be
      able to listen to. *)

  val max_identity_per_resource : int
  (** [max_identity_per_resource] is the initial size for the tables storing the
      data of clients listening on one given resource, fo best results it
      should be on the order of the expected number of clients that may listen
      on a given resource. *)
end

(** Use this functor if you need to customise your notifications with
    client-specific data (or block notifications for specific clients).
    This is made to work specifically in a multi-server set-up as well, where
    In a multi-server set-up notifications might need to be serialised twice,
    once before broadcasting them to the other servers (without client
    information present), and then once more to forward them to the clients
    possibly augmenting it with client-specific data or block for specific
    clients; see [ARG.prepare].

    Note: The communication between servers is not implemented in this module.
    To `plug in' your method of transporting notifications between servers you
    can override [S.notify]. See the manual for an example (coming soon).
*)
module Make (A : ARG) :
  S
    with type identity = A.identity
     and type key = A.key
     and type server_notif = A.server_notif
     and type client_notif = A.client_notif

(** [ARG_SIMPLE] is for making {!Make_Simple} *)
module type ARG_SIMPLE = sig
  type identity
  (** see {!S.identity} *)

  type key
  (** see {!S.key} *)

  type notification

  val get_identity : unit -> identity Lwt.t
  (** see {!ARG.equal_key} *)
end

(** Use this functor if you have no need of customising your notifications with
    client-specific data.
*)
module Make_Simple (A : ARG_SIMPLE) :
  S
    with type key = A.key
     and type server_notif = A.notification
     and type client_notif = A.notification
