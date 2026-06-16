
# Module `React.E`

Event combinators.

Consult their `semantics.`


## Primitive and basics

```ocaml
type 'a t = 'a React.event
```
The type for events with occurrences of type `'a`.

```ocaml
val never : 'a React.event
```
A never occuring event. For all t, \[`never`\]t `= None`.

```ocaml
val create : unit -> 'a React.event * (?step:React.step -> 'a -> unit)
```
`create ()` is a primitive event `e` and a `send` function. The function `send` is such that:

- `send v` generates an occurrence `v` of `e` at the time it is called and triggers an `update step`.
- `send ~step v` generates an occurence `v` of `e` on the step `step` when `step` is `executed`.
- `send ~step v` raises `Invalid_argument` if it was previously called with a step and this step has not executed yet or if the given `step` was already executed.
**Warning.** `send` must not be executed inside an update step.

```ocaml
val retain : 'a React.event -> (unit -> unit) -> [ `R of unit -> unit ]
```
`retain e c` keeps a reference to the closure `c` in `e` and returns the previously retained value. `c` will *never* be invoked.

**Raises.** `Invalid_argument` on [`E.never`](./#val-never).

```ocaml
val stop : ?strong:bool -> 'a React.event -> unit
```
`stop e` stops `e` from occuring. It conceptually becomes [`never`](./#val-never) and cannot be restarted. Allows to disable `effectful` events.

The `strong` argument should only be used on platforms where weak arrays have a strong semantics (i.e. JavaScript). See `details`.

**Note.** If executed in an `update step` the event may still occur in the step.

```ocaml
val equal : 'a React.event -> 'a React.event -> bool
```
`equal e e'` is `true` iff `e` and `e'` are equal. If both events are different from [`never`](./#val-never), physical equality is used.

```ocaml
val trace : 
  ?iff:bool React.signal ->
  ('a -> unit) ->
  'a React.event ->
  'a React.event
```
`trace iff tr e` is `e` except `tr` is invoked with e's occurence when `iff` is `true` (defaults to `S.const true`). For all t where \[`e`\]t `= Some v` and \[`iff`\]t \= `true`, `tr` is invoked with `v`.


## Transforming and filtering

```ocaml
val once : 'a React.event -> 'a React.event
```
`once e` is `e` with only its next occurence.

- \[`once e`\]t `= Some v` if \[`e`\]t `= Some v` and \[`e`\]\<t `= None`.
- \[`once e`\]t `= None` otherwise.
```ocaml
val drop_once : 'a React.event -> 'a React.event
```
`drop_once e` is `e` without its next occurrence.

- \[`drop_once e`\]t `= Some v` if \[`e`\]t `= Some v` and \[`e`\]\<t `= Some _`.
- \[`drop_once e`\]t `= None` otherwise.
```ocaml
val app : ('a -> 'b) React.event -> 'a React.event -> 'b React.event
```
`app ef e` occurs when both `ef` and `e` occur `simultaneously`. The value is `ef`'s occurence applied to `e`'s one.

- \[`app ef e`\]t `= Some v'` if \[`ef`\]t `= Some f` and \[`e`\]t `= Some v` and `f v = v'`.
- \[`app ef e`\]t `= None` otherwise.
```ocaml
val map : ('a -> 'b) -> 'a React.event -> 'b React.event
```
`map f e` applies `f` to `e`'s occurrences.

- \[`map f e`\]t `= Some (f v)` if \[`e`\]t `= Some v`.
- \[`map f e`\]t `= None` otherwise.
```ocaml
val stamp : 'b React.event -> 'a -> 'a React.event
```
`stamp e v` is `map (fun _ -> v) e`.

```ocaml
val filter : ('a -> bool) -> 'a React.event -> 'a React.event
```
`filter p e` are `e`'s occurrences that satisfy `p`.

- \[`filter p e`\]t `= Some v` if \[`e`\]t `= Some v` and `p v = true`
- \[`filter p e`\]t `= None` otherwise.
```ocaml
val fmap : ('a -> 'b option) -> 'a React.event -> 'b React.event
```
`fmap fm e` are `e`'s occurrences filtered and mapped by `fm`.

- \[`fmap fm e`\]t `= Some v` if `fm` \[`e`\]t `= Some v`
- \[`fmap fm e`\]t `= None` otherwise.
```ocaml
val diff : ('a -> 'a -> 'b) -> 'a React.event -> 'b React.event
```
`diff f e` occurs whenever `e` occurs except on the next occurence. Occurences are `f v v'` where `v` is `e`'s current occurrence and `v'` the previous one.

- \[`diff f e`\]t `= Some r` if \[`e`\]t `= Some v`, \[`e`\]\<t `= Some v'` and `f v v' = r`.
- \[`diff f e`\]t `= None` otherwise.
```ocaml
val changes : ?eq:('a -> 'a -> bool) -> 'a React.event -> 'a React.event
```
`changes eq e` is `e`'s occurrences with occurences equal to the previous one dropped. Equality is tested with `eq` (defaults to structural equality).

- \[`changes eq e`\]t `= Some v` if \[`e`\]t `= Some v` and either \[`e`\]\<t `= None` or \[`e`\]\<t `= Some v'` and `eq v v' = false`.
- \[`changes eq e`\]t `= None` otherwise.
```ocaml
val on : bool React.signal -> 'a React.event -> 'a React.event
```
`on c e` is the occurrences of `e` when `c` is `true`.

- \[`on c e`\]t `= Some v` if \[`c`\]t `= true` and \[`e`\]t `= Some v`.
- \[`on c e`\]t `= None` otherwise.
```ocaml
val when_ : bool React.signal -> 'a React.event -> 'a React.event
```
deprecated Use on.
```ocaml
val dismiss : 'b React.event -> 'a React.event -> 'a React.event
```
`dismiss c e` is the occurences of `e` except the ones when `c` occurs.

- \[`dimiss c e`\]t `= Some v` if \[`c`\]t `= None` and \[`e`\]t `= Some v`.
- \[`dimiss c e`\]t `= None` otherwise.
```ocaml
val until : 'a React.event -> 'b React.event -> 'b React.event
```
`until c e` is `e`'s occurences until `c` occurs.

- \[`until c e`\]t `= Some v` if \[`e`\]t `= Some v` and \[`c`\]\<=t `= None`
- \[`until c e`\]t `= None` otherwise.

## Accumulating

```ocaml
val accum : ('a -> 'a) React.event -> 'a -> 'a React.event
```
`accum ef i` accumulates a value, starting with `i`, using `e`'s functional occurrences.

- \[`accum ef i`\]t `= Some (f i)` if \[`ef`\]t `= Some f` and \[`ef`\]\<t `= None`.
- \[`accum ef i`\]t `= Some (f acc)` if \[`ef`\]t `= Some f` and \[`accum ef i`\]\<t `= Some acc`.
- \[`accum ef i`\] `= None` otherwise.
```ocaml
val fold : ('a -> 'b -> 'a) -> 'a -> 'b React.event -> 'a React.event
```
`fold f i e` accumulates `e`'s occurrences with `f` starting with `i`.

- \[`fold f i e`\]t `= Some (f i v)` if \[`e`\]t `= Some v` and \[`e`\]\<t `= None`.
- \[`fold f i e`\]t `= Some (f acc v)` if \[`e`\]t `= Some v` and \[`fold f i e`\]\<t `= Some acc`.
- \[`fold f i e`\]t `= None` otherwise.

## Combining

```ocaml
val select : 'a React.event list -> 'a React.event
```
`select el` is the occurrences of every event in `el`. If more than one event occurs `simultaneously` the leftmost is taken and the others are lost.

- \[`select el`\]t `=` \[`List.find (fun e -> `\[`e`\]t `<> None) el`\]t.
- \[`select el`\]t `= None` otherwise.
```ocaml
val merge : ('a -> 'b -> 'a) -> 'a -> 'b React.event list -> 'a React.event
```
`merge f a el` merges the `simultaneous` occurrences of every event in `el` using `f` and the accumulator `a`.

\[`merge f a el`\]t `= List.fold_left f a (List.filter (fun o -> o <> None) (List.map` \[\]t` el))`.

```ocaml
val switch : 'a React.event -> 'a React.event React.event -> 'a React.event
```
`switch e ee` is `e`'s occurrences until there is an occurrence `e'` on `ee`, the occurrences of `e'` are then used until there is a new occurrence on `ee`, etc..

- \[`switch e ee`\]t `=` \[`e`\]t if \[`ee`\]\<=t `= None`.
- \[`switch e ee`\]t `=` \[`e'`\]t if \[`ee`\]\<=t `= Some e'`.
```ocaml
val fix : ('a React.event -> 'a React.event * 'b) -> 'b
```
`fix ef` allows to refer to the value an event had an infinitesimal amount of time before.

In `fix ef`, `ef` is called with an event `e` that represents the event returned by `ef` delayed by an infinitesimal amount of time. If `e', r = ef e` then `r` is returned by `fix` and `e` is such that :

- \[`e`\]t `=` `None` if t \= 0
- \[`e`\]t `=` \[`e'`\]t-dt otherwise
**Raises.** `Invalid_argument` if `e'` is directly a delayed event (i.e. an event given to a fixing function).


## Lifting

Lifting combinators. For a given `n` the semantics is:

- \[`ln f e1 ... en`\]t `= Some (f v1 ... vn)` if for all i : \[`ei`\]t `= Some vi`.
- \[`ln f e1 ... en`\]t `= None` otherwise.
```ocaml
val l1 : ('a -> 'b) -> 'a React.event -> 'b React.event
```
```ocaml
val l2 : ('a -> 'b -> 'c) -> 'a React.event -> 'b React.event -> 'c React.event
```
```ocaml
val l3 : 
  ('a -> 'b -> 'c -> 'd) ->
  'a React.event ->
  'b React.event ->
  'c React.event ->
  'd React.event
```
```ocaml
val l4 : 
  ('a -> 'b -> 'c -> 'd -> 'e) ->
  'a React.event ->
  'b React.event ->
  'c React.event ->
  'd React.event ->
  'e React.event
```
```ocaml
val l5 : 
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f) ->
  'a React.event ->
  'b React.event ->
  'c React.event ->
  'd React.event ->
  'e React.event ->
  'f React.event
```
```ocaml
val l6 : 
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g) ->
  'a React.event ->
  'b React.event ->
  'c React.event ->
  'd React.event ->
  'e React.event ->
  'f React.event ->
  'g React.event
```

## Stdlib support

```ocaml
module Option : sig ... end
```
Events with option occurences.
