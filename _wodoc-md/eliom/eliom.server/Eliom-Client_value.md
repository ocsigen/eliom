
# Module `Eliom.Client_value`


### Client and shared values

See the [manual](./../eliom-language.md).

```ocaml
type +'a t
```
Client values on the server are created by the syntax `{typ{ expr }}` in the server section (cf. [the manual](./../eliom-language.md#clientvalues)). They are abstract, but become concrete once sent to the client. See also [the concrete representation on the client](./#type-t).

```ocaml
exception Client_value_creation_invalid_context of string
```
Raised if a client value of the given closure ID is created at a point in time where it is neither global (i.e. during the initialization of the server program), nor request (i.e. during the processing of a request).
