# Module `Eliom_client_value`

### Client and shared values

See the [manual](./../eliom-language.md).

```ocaml
type 'a t = 'a
```
An `'a` client value on the client is just an `'a`. See also [the abstract representation on the server](./#type-t).

```ocaml
exception Exception_on_server of string
```
This exception is raised (in Lwt) on the client if a call to a server function [`Eliom_client.server_function`](./Eliom_client.md#type-server_function) fails (in Lwt) on the server side.

The argument describes the original exception by [`Printexc.to_string`](./../../ocaml-compiler/stdlib/Stdlib-Printexc.md#val-to_string).

Event handlers like [`Eliom_content.Html.F.a_onclick`](./Eliom_content-Html-F.md#val-a_onclick) may raise `False` to cancel the event (as if the JavaScript function returned `false`).

```ocaml
exception False
```
