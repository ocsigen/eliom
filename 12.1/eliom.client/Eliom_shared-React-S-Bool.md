
# Module `S.Bool`

```ocaml
val zero : bool React.signal
```
```ocaml
val one : bool React.signal
```
```ocaml
val not : bool React.signal -> bool React.signal
```
```ocaml
val (&&) : bool React.signal -> bool React.signal -> bool React.signal
```
```ocaml
val (||) : bool React.signal -> bool React.signal -> bool React.signal
```
```ocaml
val edge : bool React.signal -> bool React.event
```
`edge s` is `changes s`.

```ocaml
val rise : bool React.signal -> unit React.event
```
`rise s` is `E.fmap (fun b -> if b then Some () else None) (edge s)`.

```ocaml
val fall : bool React.signal -> unit React.event
```
`fall s` is `E.fmap (fun b -> if b then None else Some ()) (edge s)`.

```ocaml
val flip : bool -> 'a React.event -> bool React.signal
```
`flip b e` is a signal whose boolean value flips each time `e` occurs. `b` is the initial signal value.

- \[`flip b e`\]0 `= not b` if \[`e`\]0 `= Some _`
- \[`flip b e`\]t `= b` if \[`e`\]\<=t `= None`
- \[`flip b e`\]t `=` `not` \[`flip b e`\]t-dt if \[`e`\]t `= Some _`