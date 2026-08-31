# Module `React.S`

## Primitive and basics

```ocaml
type 'a t = 'a React.signal
```
The type for signals of type `'a`.

```ocaml
val retain : 'a React.signal -> (unit -> unit) -> [ `R of unit -> unit ]
```
`retain s c` keeps a reference to the closure `c` in `s` and returns the previously retained value. `c` will *never* be invoked.

**Raises.** `Invalid_argument` on constant signals.

```ocaml
val stop : ?strong:bool -> 'a React.signal -> unit
```
`stop s`, stops updating `s`. It conceptually becomes [`const`](./#val-const) with the signal's last value and cannot be restarted. Allows to disable `effectful` signals.

The `strong` argument should only be used on platforms where weak arrays have a strong semantics (i.e. JavaScript). See `details`.

**Note.** If executed in an update step the signal may still update in the step.

```ocaml
val equal : 
  ?eq:('a -> 'a -> bool) ->
  'a React.signal ->
  'a React.signal ->
  bool
```
`equal s s'` is `true` iff `s` and `s'` are equal. If both signals are [`const`](./#val-const)ant `eq` is used between their value (defauts to structural equality). If both signals are not [`const`](./#val-const)ant, physical equality is used.

```ocaml
val trace : ?iff:bool t -> ('a -> unit) -> 'a React.signal -> 'a React.signal
```
`trace iff tr s` is `s` except `tr` is invoked with `s`'s current value and on `s` changes when `iff` is `true` (defaults to `S.const true`). For all t where \[`s`\]t `= v` and (t \= 0 or (\[`s`\]t-dt`= v'` and `eq v v' = false`)) and \[`iff`\]t \= `true`, `tr` is invoked with `v`.

## From events

```ocaml
val hold : ?eq:('a -> 'a -> bool) -> 'a -> 'a React.event -> 'a React.signal
```
`hold i e` has the value of `e`'s last occurrence or `i` if there wasn't any.

- \[`hold i e`\]t `= i` if \[`e`\]\<=t `= None`
- \[`hold i e`\]t `= v` if \[`e`\]\<=t `= Some v`

## Transforming and filtering

```ocaml
val app : 
  ?eq:('b -> 'b -> bool) ->
  ('a -> 'b) React.signal ->
  'a React.signal ->
  'b React.signal
```
`app sf s` holds the value of `sf` applied to the value of `s`, \[`app sf s`\]t `=` \[`sf`\]t \[`s`\]t.

```ocaml
val filter : 
  ?eq:('a -> 'a -> bool) ->
  ('a -> bool) ->
  'a ->
  'a React.signal ->
  'a React.signal
```
`filter f i s` is `s`'s values that satisfy `p`. If a value does not satisfy `p` it holds the last value that was satisfied or `i` if there is none.

- \[`filter p s`\]t `=` \[`s`\]t if `p` \[`s`\]t` = true`.
- \[`filter p s`\]t `=` \[`s`\]t' if `p` \[`s`\]t` = false` and t' is the greatest t' \< t with `p` \[`s`\]t'` = true`.
- \[`filter p e`\]t `= i` otherwise.
```ocaml
val diff : ('a -> 'a -> 'b) -> 'a React.signal -> 'b React.event
```
`diff f s` is an event with occurrences whenever `s` changes from `v'` to `v` and `eq v v'` is `false` (`eq` is the signal's equality function). The value of the occurrence is `f v v'`.

- \[`diff f s`\]t `= Some d` if \[`s`\]t `= v` and \[`s`\]t-dt `= v'` and `eq v v' = false` and `f v v' = d`.
- \[`diff f s`\]t `= None` otherwise.
```ocaml
val changes : 'a React.signal -> 'a React.event
```
`changes s` is `diff (fun v _ -> v) s`.

```ocaml
val sample : 
  ('b -> 'a -> 'c) ->
  'b React.event ->
  'a React.signal ->
  'c React.event
```
`sample f e s` samples `s` at `e`'s occurrences.

- \[`sample f e s`\]t `= Some (f ev sv)` if \[`e`\]t `= Some ev` and \[`s`\]t `= sv`.
- \[`sample e s`\]t `= None` otherwise.
```ocaml
val on : 
  ?eq:('a -> 'a -> bool) ->
  bool React.signal ->
  'a ->
  'a React.signal ->
  'a React.signal
```
`on c i s` is the signal `s` whenever `c` is `true`. When `c` is `false` it holds the last value `s` had when `c` was the last time `true` or `i` if it never was.

- \[`on c i s`\]t `=` \[`s`\]t if \[`c`\]t `= true`
- \[`on c i s`\]t `=` \[`s`\]t' if \[`c`\]t `= false` where t' is the greatest t' \< t with \[`c`\]t' `= true`.
- \[`on c i s`\]t `=` `i` otherwise.
```ocaml
val when_ : 
  ?eq:('a -> 'a -> bool) ->
  bool React.signal ->
  'a ->
  'a React.signal ->
  'a React.signal
```
deprecated Use on.
```ocaml
val dismiss : 
  ?eq:('a -> 'a -> bool) ->
  'b React.event ->
  'a ->
  'a React.signal ->
  'a React.signal
```
`dismiss c i s` is the signal `s` except changes when `c` occurs are ignored. If `c` occurs initially `i` is used.

- \[`dismiss c i s`\]t `=` \[`s`\]t' where t' is the greatest t' \<= t with \[`c`\]t' `= None` and \[`s`\]t'-dt `<>` \[`s`\]t'
- \[`dismiss_ c i s`\]0 `=` `v` where `v = i` if \[`c`\]0 `= Some _` and `v =` \[`s`\]0 otherwise.

## Accumulating

```ocaml
val accum : 
  ?eq:('a -> 'a -> bool) ->
  ('a -> 'a) React.event ->
  'a ->
  'a React.signal
```
`accum e i` is `S.hold i (`[`E.accum`](./Eliom_shared-React-E.md#val-accum)` e i)`.

```ocaml
val fold : 
  ?eq:('a -> 'a -> bool) ->
  ('a -> 'b -> 'a) ->
  'a ->
  'b React.event ->
  'a React.signal
```
`fold f i e` is `S.hold i (`[`E.fold`](./Eliom_shared-React-E.md#val-fold)` f i e)`.

## Combining

```ocaml
val bind : 
  ?eq:('b -> 'b -> bool) ->
  'a React.signal ->
  ('a -> 'b React.signal) ->
  'b React.signal
```
`bind s sf` is `switch (map ~eq:( == ) sf s)`.

```ocaml
val fix : 
  ?eq:('a -> 'a -> bool) ->
  'a ->
  ('a React.signal -> 'a React.signal * 'b) ->
  'b
```
`fix i sf` allow to refer to the value a signal had an infinitesimal amount of time before.

In `fix sf`, `sf` is called with a signal `s` that represents the signal returned by `sf` delayed by an infinitesimal amount time. If `s', r = sf s` then `r` is returned by `fix` and `s` is such that :

- \[`s`\]t `=` `i` for t \= 0\.
- \[`s`\]t `=` \[`s'`\]t-dt otherwise.
`eq` is the equality used by `s`.

**Raises.** `Invalid_argument` if `s'` is directly a delayed signal (i.e. a signal given to a fixing function).

**Note.** Regarding values depending on the result `r` of `s', r = sf s` the following two cases need to be distinguished :

- After `sf s` is applied, `s'` does not depend on a value that is in a step and `s` has no dependents in a step (e.g in the simple case where `fix` is applied outside a step).

  In that case if the initial value of `s'` differs from `i`, `s` and its dependents need to be updated and a special update step will be triggered for this. Values depending on the result `r` will be created only after this special update step has finished (e.g. they won't see the `i` of `s` if `r = s`).

- Otherwise, values depending on `r` will be created in the same step as `s` and `s'` (e.g. they will see the `i` of `s` if `r = s`).

## Lifting

Lifting combinators. For a given `n` the semantics is :

\[`ln f a1` ... `an`\]t \= f \[`a1`\]t ... \[`an`\]t

```ocaml
val l1 : 
  ?eq:('b -> 'b -> bool) ->
  ('a -> 'b) ->
  'a React.signal ->
  'b React.signal
```
The following modules lift some of `Stdlib` functions and operators.

```ocaml
module Bool : sig ... end
```
```ocaml
module Int : sig ... end
```
```ocaml
module Float : sig ... end
```
```ocaml
module Pair : sig ... end
```
```ocaml
module Option : sig ... end
```
```ocaml
module Compare : sig ... end
```

## Combinator specialization

Given an equality function `equal` and a type `t`, the functor [`Make`](./Eliom_shared-React-S-Make.md) automatically applies the `eq` parameter of the combinators. The outcome is combinators whose *results* are signals with values in `t`.

Basic types are already specialized in the module [`Special`](./Eliom_shared-React-S-Special.md), open this module to use them.

```ocaml
module type EqType = sig ... end
```
Input signature of [`Make`](./Eliom_shared-React-S-Make.md)

```ocaml
module type S = sig ... end
```
Output signature of [`Make`](./Eliom_shared-React-S-Make.md)

```ocaml
module Make (Eq : EqType) : S with type 'a v = 'a Eq.t
```
Functor specializing the combinators for the given signal value type

```ocaml
module Special : sig ... end
```
Specialization for booleans, integers and floats.

```ocaml
val const : 'a -> 'a t
```
```ocaml
val value : 'a t -> 'a Value.t
```
```ocaml
val map : ?eq:('b -> 'b -> bool) Value.t -> ('a -> 'b) Value.t -> 'a t -> 'b t
```
```ocaml
val fmap : 
  ?eq:('b -> 'b -> bool) Value.t ->
  ('a -> 'b option) Value.t ->
  'b Value.t ->
  'a t ->
  'b t
```
```ocaml
val merge : 
  ?eq:('a -> 'a -> bool) Value.t ->
  ('a -> 'b -> 'a) Value.t ->
  'a ->
  'b t list ->
  'a t
```
```ocaml
val l2 : 
  ?eq:('c -> 'c -> bool) Value.t ->
  ('a -> 'b -> 'c) Value.t ->
  'a t ->
  'b t ->
  'c t
```
```ocaml
val l3 : 
  ?eq:('d -> 'd -> bool) Value.t ->
  ('a -> 'b -> 'c -> 'd) Value.t ->
  'a t ->
  'b t ->
  'c t ->
  'd t
```
```ocaml
val l4 : 
  ?eq:('e -> 'e -> bool) Value.t ->
  ('a -> 'b -> 'c -> 'd -> 'e) Value.t ->
  'a t ->
  'b t ->
  'c t ->
  'd t ->
  'e t
```
```ocaml
val l5 : 
  ?eq:('f -> 'f -> bool) Value.t ->
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f) Value.t ->
  'a t ->
  'b t ->
  'c t ->
  'd t ->
  'e t ->
  'f t
```
```ocaml
val l6 : 
  ?eq:('g -> 'g -> bool) Value.t ->
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g) Value.t ->
  'a t ->
  'b t ->
  'c t ->
  'd t ->
  'e t ->
  'f t ->
  'g t
```
```ocaml
val switch : ?eq:('a -> 'a -> bool) Value.t -> 'a t t -> 'a t
```
```ocaml
module Infix : sig ... end
```
Infix operators

```ocaml
module Lwt : sig ... end
```
Cooperative versions of the React operators

```ocaml
val create : 
  ?eq:('a -> 'a -> bool) ->
  ?default:('a t * (?step:React.step -> 'a -> unit)) option ->
  ?reset_default:bool ->
  'a ->
  'a React.signal * (?step:React.step -> 'a -> unit)
```
`create ?eq ?default ?reset_default x` produces a pair `s, f`, where `s` is a reactive signal, and `f` is a function for updating the signal.

The initial value of the signal is `x`, unless `default` is provided. `default`, if provided, is used as the signal. `reset_default`, if set to true (default: false), resets the value of `default` to `x`.
