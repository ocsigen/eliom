
# Module `A.Table`

```ocaml
type t = table
```
```ocaml
val add : 
  Common.page_table_key ->
  'a ->
  ([> `Ptc of 'a ] as 'b) Raw_table.t ->
  'b Raw_table.t
```
```ocaml
val find : Common.page_table_key -> [< `Ptc of 'a ] Raw_table.t -> 'a
```
```ocaml
val empty : unit -> 'a Raw_table.t
```
```ocaml
val remove : Common.page_table_key -> 'a Raw_table.t -> 'a Raw_table.t
```