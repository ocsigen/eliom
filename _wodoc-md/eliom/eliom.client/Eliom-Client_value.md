
# Module `Eliom.Client_value`


### Client and shared values

See the [manual](./../eliom-language.md).

```ocaml
type 'a t = 'a
```
An `'a` client value on the client is just an `'a`. See also [the abstract representation on the server](./#type-t).

```ocaml
exception Exception_on_server of string
```
This exception is raised (in Lwt) on the client if a call to a server function [`Client.server_function`](./Eliom-Client.md#type-server_function) fails (in Lwt) on the server side.

The argument describes the original exception by [`Printexc.to_string`](./../../ocaml-compiler/stdlib/Stdlib-Printexc.md#val-to_string).

Event handlers like [`Content.Html.F.a_onclick`](./Eliom-Content-Html-F.md#val-a_onclick) may raise `False` to cancel the event (as if the JavaScript function returned `false`).

```ocaml
exception False
```