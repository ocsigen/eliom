
# Module `S.Ext`

```ocaml
val unlisten : 
  ?sitedata:Common.sitedata ->
  ([< `Client_process ], [< `Data ]) State.Ext.state ->
  key ->
  unit
```
Make a listener stop listening on data `key`. If this function is called during a request it will be able to determine `sitedata` by itself, otherwise it needs to be supplied by the caller.
