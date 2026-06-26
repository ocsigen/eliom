
# Module `Eliom.Comet`

Primitives to push data to the client, without explicit request.

**Please read the [chapter on communication](./../clientserver-communication.md) of Eliom's manual before this page to learn how client and server parts communicate.**

```ocaml
module Channel : sig ... end
```
Basic primitives needed for server push.

```ocaml
val section : Logs.src
```