# Module `String.Set`

## Sets

```ocaml
type elt = string
```
The type of the set elements.

```ocaml
type t
```
The type of sets.

```ocaml
val empty : t
```
The empty set.

```ocaml
val add : elt -> t -> t
```
`add x s` returns a set containing all elements of `s`, plus `x`. If `x` was already in `s`, `s` is returned unchanged (the result of the function is then physically equal to `s`).

before 4\.03 Physical equality was not ensured.
```ocaml
val singleton : elt -> t
```
`singleton x` returns the one-element set containing only `x`.

```ocaml
val remove : elt -> t -> t
```
`remove x s` returns a set containing all elements of `s`, except `x`. If `x` was not in `s`, `s` is returned unchanged (the result of the function is then physically equal to `s`).

before 4\.03 Physical equality was not ensured.
```ocaml
val union : t -> t -> t
```
Set union.

```ocaml
val inter : t -> t -> t
```
Set intersection.

```ocaml
val disjoint : t -> t -> bool
```
Test if two sets are disjoint.

since 4\.08
```ocaml
val diff : t -> t -> t
```
Set difference: `diff s1 s2` contains the elements of `s1` that are not in `s2`.

```ocaml
val cardinal : t -> int
```
Return the number of elements of a set.

## Elements

```ocaml
val elements : t -> elt list
```
Return the list of all elements of the given set. The returned list is sorted in increasing order with respect to the ordering `Ord.compare`, where `Ord` is the argument given to `Set.Make`.

```ocaml
val min_elt : t -> elt
```
Return the smallest element of the given set (with respect to the `Ord.compare` ordering), or raise `Not_found` if the set is empty.

```ocaml
val min_elt_opt : t -> elt option
```
Return the smallest element of the given set (with respect to the `Ord.compare` ordering), or `None` if the set is empty.

since 4\.05
```ocaml
val max_elt : t -> elt
```
Same as [`min_elt`](./#val-min_elt), but returns the largest element of the given set.

```ocaml
val max_elt_opt : t -> elt option
```
Same as [`min_elt_opt`](./#val-min_elt_opt), but returns the largest element of the given set.

since 4\.05
```ocaml
val choose : t -> elt
```
Return one element of the given set, or raise `Not_found` if the set is empty. Which element is chosen is unspecified, but equal elements will be chosen for equal sets.

```ocaml
val choose_opt : t -> elt option
```
Return one element of the given set, or `None` if the set is empty. Which element is chosen is unspecified, but equal elements will be chosen for equal sets.

since 4\.05

## Searching

```ocaml
val find : elt -> t -> elt
```
`find x s` returns the element of `s` equal to `x` (according to `Ord.compare`), or raise `Not_found` if no such element exists.

since 4\.01
```ocaml
val find_opt : elt -> t -> elt option
```
`find_opt x s` returns the element of `s` equal to `x` (according to `Ord.compare`), or `None` if no such element exists.

since 4\.05
```ocaml
val find_first : (elt -> bool) -> t -> elt
```
`find_first f s`, where `f` is a monotonically increasing function, returns the lowest element `e` of `s` such that `f e`, or raises `Not_found` if no such element exists.

For example, `find_first (fun e -> Ord.compare e x >= 0) s` will return the first element `e` of `s` where `Ord.compare e x >= 0` (intuitively: `e >= x`), or raise `Not_found` if `x` is greater than any element of `s`.

since 4\.05
```ocaml
val find_first_opt : (elt -> bool) -> t -> elt option
```
`find_first_opt f s`, where `f` is a monotonically increasing function, returns an option containing the lowest element `e` of `s` such that `f e`, or `None` if no such element exists.

since 4\.05
```ocaml
val find_last : (elt -> bool) -> t -> elt
```
`find_last f s`, where `f` is a monotonically decreasing function, returns the highest element `e` of `s` such that `f e`, or raises `Not_found` if no such element exists.

since 4\.05
```ocaml
val find_last_opt : (elt -> bool) -> t -> elt option
```
`find_last_opt f s`, where `f` is a monotonically decreasing function, returns an option containing the highest element `e` of `s` such that `f e`, or `None` if no such element exists.

since 4\.05

## Traversing

```ocaml
val iter : (elt -> unit) -> t -> unit
```
`iter f s` applies `f` in turn to all elements of `s`. The elements of `s` are presented to `f` in increasing order with respect to the ordering over the type of the elements.

```ocaml
val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
```
`fold f s init` computes `(f xN ... (f x2 (f x1 init))...)`, where `x1 ... xN` are the elements of `s`, in increasing order.

## Transforming

```ocaml
val map : (elt -> elt) -> t -> t
```
`map f s` is the set whose elements are `f a0`,`f a1`... `f aN`, where `a0`,`a1`...`aN` are the elements of `s`.

The elements are passed to `f` in increasing order with respect to the ordering over the type of the elements.

If no element of `s` is changed by `f`, `s` is returned unchanged. (If each output of `f` is physically equal to its input, the returned set is physically equal to `s`.)

since 4\.04
```ocaml
val filter : (elt -> bool) -> t -> t
```
`filter f s` returns the set of all elements in `s` that satisfy predicate `f`. If `f` satisfies every element in `s`, `s` is returned unchanged (the result of the function is then physically equal to `s`).

before 4\.03 Physical equality was not ensured.
```ocaml
val filter_map : (elt -> elt option) -> t -> t
```
`filter_map f s` returns the set of all `v` such that `f x = Some v` for some element `x` of `s`.

For example,

```ocaml
filter_map (fun n -> if n mod 2 = 0 then Some (n / 2) else None) s
```
is the set of halves of the even elements of `s`.

If no element of `s` is changed or dropped by `f` (if `f x = Some x` for each element `x`), then `s` is returned unchanged: the result of the function is then physically equal to `s`.

since 4\.11
```ocaml
val partition : (elt -> bool) -> t -> t * t
```
`partition f s` returns a pair of sets `(s1, s2)`, where `s1` is the set of all the elements of `s` that satisfy the predicate `f`, and `s2` is the set of all the elements of `s` that do not satisfy `f`.

```ocaml
val split : elt -> t -> t * bool * t
```
`split x s` returns a triple `(l, present, r)`, where `l` is the set of elements of `s` that are strictly less than `x`; `r` is the set of elements of `s` that are strictly greater than `x`; `present` is `false` if `s` contains no element equal to `x`, or `true` if `s` contains an element equal to `x`.

## Predicates and comparisons

```ocaml
val is_empty : t -> bool
```
Test whether a set is empty or not.

```ocaml
val mem : elt -> t -> bool
```
`mem x s` tests whether `x` belongs to the set `s`.

```ocaml
val equal : t -> t -> bool
```
`equal s1 s2` tests whether the sets `s1` and `s2` are equal, that is, contain equal elements.

```ocaml
val compare : t -> t -> int
```
Total ordering between sets. Can be used as the ordering function for doing sets of sets.

```ocaml
val subset : t -> t -> bool
```
`subset s1 s2` tests whether the set `s1` is a subset of the set `s2`.

```ocaml
val for_all : (elt -> bool) -> t -> bool
```
`for_all f s` checks if all elements of the set satisfy the predicate `f`.

```ocaml
val exists : (elt -> bool) -> t -> bool
```
`exists f s` checks if at least one element of the set satisfies the predicate `f`.

## Converting

```ocaml
val to_list : t -> elt list
```
`to_list s` is [`elements`](./#val-elements)` s`.

since 5\.1
```ocaml
val of_list : elt list -> t
```
`of_list l` creates a set from a list of elements. This is usually more efficient than folding `add` over the list, except perhaps for lists with many duplicated elements.

since 4\.02
```ocaml
val to_seq_from : elt -> t -> elt Seq.t
```
`to_seq_from x s` iterates on a subset of the elements of `s` in ascending order, from `x` or above.

since 4\.07
```ocaml
val to_seq : t -> elt Seq.t
```
Iterate on the whole set, in ascending order

since 4\.07
```ocaml
val to_rev_seq : t -> elt Seq.t
```
Iterate on the whole set, in descending order

since 4\.12
```ocaml
val add_seq : elt Seq.t -> t -> t
```
Add the given elements to the set, in order.

since 4\.07
```ocaml
val of_seq : elt Seq.t -> t
```
Build a set from the given bindings

since 4\.07
