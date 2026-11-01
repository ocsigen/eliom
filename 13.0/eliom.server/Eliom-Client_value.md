# Module `Eliom.Client_value`

### Client and shared values

See the [manual](./../clientserver/multitier.md).

```ocaml
type +'a t
```
Client values on the server are created by client fragments `[%client (expr : typ)]` in server code (cf. [the manual](./../clientserver/multitier.md#clientvalues)). They are abstract, but become concrete once sent to the client. See also [the concrete representation on the client](./#type-t).

```ocaml
exception Client_value_creation_invalid_context of string
```
Raised if a global client value (a client fragment evaluated by the toplevel code of a module) of the given closure ID is created during a request. Global client values are created during the initialization of the server program, the other ones during the processing of a request.
