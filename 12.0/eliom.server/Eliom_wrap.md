
# Module `Eliom_wrap`

```ocaml
type 'a wrapped_value
```
```ocaml
type +'a wrapper
```
`'a wrapper` is the type of values to include into a value of type 'a for it to be wrapped specifically

```ocaml
val create_wrapper : ('a -> 'b) -> 'a wrapper
```
`create f` create a new tag that can be included into a value. if `wrap` is called on a father of a value `v` containing a tag, the value `v` will be replaced by `f v` before marshaling.

marshal a value, taking into account the tags.

```ocaml
val wrap : 'a -> 'a wrapped_value
```
```ocaml
val empty_wrapper : 'a wrapper
```
a wrapper that do not change the value
