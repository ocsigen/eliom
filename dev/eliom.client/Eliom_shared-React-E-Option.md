# Module `E.Option`

Events with option occurences.

```ocaml
val some : 'a React.event -> 'a option React.event
```
`some e` is `map (fun v -> Some v) e`.

```ocaml
val value : ?default:'a React.signal -> 'a option React.event -> 'a React.event
```
`value default e` either silences `None` occurences if `default` is unspecified or replaces them by the value of `default` at the occurence time.

- \[`value ~default e`\]t` = v` if \[`e`\]t `= Some (Some v)`.
- \[`value ?default:None e`\]t` = None` if \[`e`\]t \= `None`.
- \[`value ?default:(Some s) e`\]t` = v` if \[`e`\]t \= `None` and \[`s`\]t `= v`.
