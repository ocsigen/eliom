
# Module `Eliom.Extension`

Allows Ocsigen's extension to access Eliom data. See the Eliom manual for more information about [Eliom's extensions](./../workflow-configuration.md#extensions)

```ocaml
type eliom_extension_sig = unit -> Ocsigen.Extensions.answer Lwt.t
```
Type of the function that must be registered to declare an eliom extension.

```ocaml
val register_eliom_extension : eliom_extension_sig -> unit
```