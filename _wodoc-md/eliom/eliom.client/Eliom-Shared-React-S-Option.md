
# Module `S.Option`

```ocaml
val none : 'a option React.signal
```
`none` is `S.const None`.

```ocaml
val some : 'a React.signal -> 'a option React.signal
```
`some s` is `S.map ~eq (fun v -> Some v) None`, where `eq` uses `s`'s equality function to test the `Some v`'s equalities.

```ocaml
val value : 
  ?eq:('a -> 'a -> bool) ->
  default:[ `Init of 'a React.signal | `Always of 'a React.signal ] ->
  'a option React.signal ->
  'a React.signal
```
`value default s` is `s` with only its `Some v` values. Whenever `s` is `None`, if `default` is ``Always dv` then the current value of `dv` is used instead. If `default` is ``Init dv` the current value of `dv` is only used if there's no value at creation time, otherwise the last `Some v` value of `s` is used.

- \[`value ~default s`\]t `= v` if \[`s`\]t `= Some v`
- \[`value ~default:(`Always d) s`\]t `=` \[`d`\]t if \[`s`\]t `= None`
- \[`value ~default:(`Init d) s`\]0 `=` \[`d`\]0 if \[`s`\]0 `= None`
- \[`value ~default:(`Init d) s`\]t `=` \[`value ~default:(`Init d) s`\]t' if \[`s`\]t `= None` and t' is the greatest t' \< t with \[`s`\]t' `<> None` or 0 if there is no such `t'`.