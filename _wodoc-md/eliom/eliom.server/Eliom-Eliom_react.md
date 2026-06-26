
# Module `Eliom.Eliom_react`

Propagate events occurrences from the server to the client and the other way around. Occurrence propagation is done asynchronously.

Warning: it is not possible to catch exceptions on each channel using Eliom\_react (for example channel closed or full). If you need an error handling, use Comet.Channel instead.

**Please read the [chapter on communication](./../clientserver-communication.md) of Eliom's manual before this page to learn how client and server parts communicate.**

The use of this module is pretty much useless without it's client counter part.

```ocaml
module Down : sig ... end
```
Event from server to client.

```ocaml
module Up : sig ... end
```
Event from client to server.

```ocaml
module S : sig ... end
```