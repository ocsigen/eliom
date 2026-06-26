
# Module type `Registration_sigs.APP_PARAM`

Signature for application creation.

```ocaml
val application_name : string
```
Name of the application. Applications must have distinct names.

```ocaml
val global_data_path : string list option
```
If a path is provided, we export through it a service for accessing the global data. Reading this data makes global injections work in client apps.
