
# Module `Notif.Make`

Use this functor if you need to customise your notifications with client-specific data (or block notifications for specific clients). This is made to work specifically in a multi-server set-up as well, where In a multi-server set-up notifications might need to be serialised twice, once before broadcasting them to the other servers (without client information present), and then once more to forward them to the clients possibly augmenting it with client-specific data or block for specific clients; see `ARG.prepare`.

Note: The communication between servers is not implemented in this module. To \`plug in' your method of transporting notifications between servers you can override `S.notify`. See the manual for an example (coming soon).


## Parameters

```ocaml
module A : ARG
```

## Signature

```ocaml
type identity = A.identity
```
`identity` is the type of values used to differentiate one listener from another. Typically it will be a user, but it could also for instance be a chat window.

```ocaml
type key = A.key
```
`key` is the type of values designating a given resource.

```ocaml
type server_notif = A.server_notif
```
server notification type; Can be different from `client_notif`.

```ocaml
type client_notif = A.client_notif
```
client notification type; Can be different from `server_notif`.

```ocaml
val init : unit -> unit Lwt.t
```
Initialise the notification module for the current client. This function needs to be called before using most other functions of this module. It isn't called implicitly during module instantiation because it relies on identity data which might not be available yet.

```ocaml
val deinit : unit -> unit
```
Deinitialise/deactivate the notification module for the current client.

```ocaml
val listen : key -> unit
```
Make client process listen on data whose index is `key`

```ocaml
val unlisten : key -> unit
```
Stop listening on data `key`

```ocaml
module Ext : sig ... end
```
```ocaml
val notify : ?notfor:[ `Me | `Id of identity ] -> key -> server_notif -> unit
```
Call `notify key n` to send a notification `n` to all clients currently listening on data referenced by `key`.

If `~notfor` is ``Me`, notification will not be sent to the tab currently doing the request (the one which caused the notification to happen). Note that if `notify` is called with `~notfor:`Me` outside of a request it will fail. If it is ``Id id` it won't be sent to the destination defined by `id`.

```ocaml
val client_ev : unit -> (key * client_notif) Eliom_react.Down.t
```
Returns the client react event.

`'a Eliom_react.Down.t` \= `'a React.E.t` on client side.

Map a function on this event to react to notifications from the server. For example:

let%client handle\_notification some\_stuff ev \= ...

let%server something some\_stuff \= ignore `%client (ignore (React.E.map (handle_notification ~%some_stuff) ~%(Notif_module.client_ev ()) ) : unit) `

```ocaml
val clean : unit -> unit
```
Call `clean ()` to clear the tables from empty data.
