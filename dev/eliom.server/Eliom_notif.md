# Module `Eliom_notif`

Server to client notifications.

This module makes it possible for client side applications to be notified of changes on some indexed data (resources) on the server.

Apply functor `Make` or `Make_Simple` for each type of data you want to be able to listen on. Each client starts listening on one piece of data by calling function `listen` with the index of that piece of data as parameter. Client stops listening by calling function `unlisten`, or when the client side state is closed (by timeout or when the client disconnects for example).

When the data is modified on server side, call function `notify` with the index of the data, and all clients listening to that piece of data will receive a notification.

The functor will also create a client side react signal that will be updated every time the client is notified.

See module Os\_notif in Ocsigen Start for an example of use.

```ocaml
module type S = sig ... end
```
Signature of the functors `Eliom_notif.Make` and `Eliom_notif.Make_Simple`.

```ocaml
module type ARG = sig ... end
```
`ARG` is for making `Make`

```ocaml
module Make
  (A : ARG) : 
  S
    with type identity = A.identity
     and type key = A.key
     and type server_notif = A.server_notif
     and type client_notif = A.client_notif
```
Use this functor if you need to customise your notifications with client-specific data (or block notifications for specific clients). This is made to work specifically in a multi-server set-up as well, where In a multi-server set-up notifications might need to be serialised twice, once before broadcasting them to the other servers (without client information present), and then once more to forward them to the clients possibly augmenting it with client-specific data or block for specific clients; see `ARG.prepare`.

```ocaml
module type ARG_SIMPLE = sig ... end
```
`ARG_SIMPLE` is for making [`Make_Simple`](./Eliom_notif-Make_Simple.md)

```ocaml
module Make_Simple
  (A : ARG_SIMPLE) : 
  S
    with type key = A.key
     and type server_notif = A.notification
     and type client_notif = A.notification
```
Use this functor if you have no need of customising your notifications with client-specific data.
