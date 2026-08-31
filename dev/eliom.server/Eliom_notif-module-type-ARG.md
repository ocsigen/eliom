# Module type `Eliom_notif.ARG`

`ARG` is for making `Make`

```ocaml
type identity
```
see `S.identity`

```ocaml
type key
```
see `S.key`

```ocaml
type server_notif
```
see `S.server_notif`

```ocaml
type client_notif
```
see `S.client_notif`

```ocaml
val prepare : identity -> server_notif -> client_notif option Lwt.t
```
`prepare f` transforms server notifications into client notifications. It provides the `identity` as a parameter which identifies the client. You can suppress notifications for a specific client (for instance because of missing authorisation) by having `f` return `None`.

```ocaml
val equal_key : key -> key -> bool
```
`equal_key` is a function testing the equality between two values of type `key`.

```ocaml
val equal_identity : identity -> identity -> bool
```
`equal_identity` is the same as `equal_key` but for values of type `identity`.

```ocaml
val get_identity : unit -> identity Lwt.t
```
`get_identity` is a function returning a value of type `identity` corresponding to a client.

```ocaml
val max_resource : int
```
`max_resource` is the initial size for the hash table storing the data of clients listening on resources, for best results it should be on the order of the expected number of different resources one may want to be able to listen to.

```ocaml
val max_identity_per_resource : int
```
`max_identity_per_resource` is the initial size for the tables storing the data of clients listening on one given resource, fo best results it should be on the order of the expected number of clients that may listen on a given resource.
