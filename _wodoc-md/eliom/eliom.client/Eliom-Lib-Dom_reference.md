
# Module `Lib.Dom_reference`

```ocaml
type key
```
```ocaml
val new_key : unit -> key
```
```ocaml
val retain : ?key:key -> _ Js_of_ocaml.Js.t -> keep:_ -> unit
```
`retain v ~keep` prevents `keep` from being garbage collected while `v` is live. An optional key can be specified if one needs to remove this association later one.

```ocaml
val retain_generic : ?key:key -> _ -> keep:_ -> unit
```
Same as `retain` but works with any object. More error-prone

```ocaml
val release : key:key -> _ -> unit
```
`release ~key o` removes the association between the value `v` and the value associated to `key`.

```ocaml
val transfer : key:key -> src:_ -> dst:_ -> unit
```
`transfer ~key ~src ~dst` transfers the association between the value `src` and the value associated to key `key` to value `dst`.
