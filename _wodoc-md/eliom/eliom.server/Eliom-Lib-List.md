
# Module `Lib.List`

Improvement of module List

```ocaml
type 'a t = 'a list = 
  | []
  | :: of 'a * 'a list
```
An alias for the type of lists.

```ocaml
val length : 'a list -> int
```
Return the length (number of elements) of the given list.

```ocaml
val compare_lengths : 'a list -> 'b list -> int
```
Compare the lengths of two lists. `compare_lengths l1 l2` is equivalent to `compare (length l1) (length l2)`, except that the computation stops after reaching the end of the shortest list.

since 4\.05
```ocaml
val compare_length_with : 'a list -> int -> int
```
Compare the length of a list to an integer. `compare_length_with l len` is equivalent to `compare (length l) len`, except that the computation stops after at most `len` iterations on the list.

since 4\.05
```ocaml
val is_empty : 'a list -> bool
```
`is_empty l` is true if and only if `l` has no elements. It is equivalent to `compare_length_with l 0 = 0`.

since 5\.1
```ocaml
val cons : 'a -> 'a list -> 'a list
```
`cons x xs` is `x :: xs`

since 4\.03 (4.05 in ListLabels)
```ocaml
val singleton : 'a -> 'a list
```
`singleton x` returns the one-element list `[x]`.

since 5\.4
```ocaml
val hd : 'a list -> 'a
```
Return the first element of the given list.

raises [`Failure`](./../../ocaml-compiler/stdlib/Stdlib.md#exception-Failure) if the list is empty.
```ocaml
val tl : 'a list -> 'a list
```
Return the given list without its first element.

raises [`Failure`](./../../ocaml-compiler/stdlib/Stdlib.md#exception-Failure) if the list is empty.
```ocaml
val nth : 'a list -> int -> 'a
```
Return the `n`\-th element of the given list. The first element (head of the list) is at position 0\.

raises [`Failure`](./../../ocaml-compiler/stdlib/Stdlib.md#exception-Failure) if the list is too short.
raises [`Invalid_argument`](./../../ocaml-compiler/stdlib/Stdlib.md#exception-Invalid_argument) if n is negative.
```ocaml
val nth_opt : 'a list -> int -> 'a option
```
Return the `n`\-th element of the given list. The first element (head of the list) is at position 0\. Return `None` if the list is too short.

raises [`Invalid_argument`](./../../ocaml-compiler/stdlib/Stdlib.md#exception-Invalid_argument) if n is negative.
since 4\.05
```ocaml
val rev : 'a list -> 'a list
```
List reversal.

```ocaml
val init : int -> (int -> 'a) -> 'a list
```
`init len f` is `[f 0; f 1; ...; f (len-1)]`, evaluated left to right.

raises [`Invalid_argument`](./../../ocaml-compiler/stdlib/Stdlib.md#exception-Invalid_argument) if len \< 0.
since 4\.06
```ocaml
val append : 'a list -> 'a list -> 'a list
```
`append l0 l1` appends `l1` to `l0`. Same function as the infix operator `@`.

since 5\.1 this function is tail-recursive.
```ocaml
val rev_append : 'a list -> 'a list -> 'a list
```
`rev_append l1 l2` reverses `l1` and concatenates it with `l2`. This is equivalent to `(`[`rev`](./#val-rev)` l1) @ l2`.

```ocaml
val concat : 'a list list -> 'a list
```
Concatenate a list of lists. The elements of the argument are all concatenated together (in the same order) to give the result. Not tail-recursive (length of the argument \+ length of the longest sub-list).

```ocaml
val flatten : 'a list list -> 'a list
```
Same as [`concat`](./#val-concat). Not tail-recursive (length of the argument \+ length of the longest sub-list).


## Comparison

```ocaml
val equal : ('a -> 'a -> bool) -> 'a list -> 'a list -> bool
```
`equal eq [a1; ...; an] [b1; ..; bm]` holds when the two input lists have the same length, and for each pair of elements `ai`, `bi` at the same position we have `eq ai bi`.

Note: the `eq` function may be called even if the lists have different length. If you know your equality function is costly, you may want to check [`compare_lengths`](./#val-compare_lengths) first.

since 4\.12
```ocaml
val compare : ('a -> 'a -> int) -> 'a list -> 'a list -> int
```
`compare cmp [a1; ...; an] [b1; ...; bm]` performs a lexicographic comparison of the two input lists, using the same `'a -> 'a -> int` interface as [`Stdlib.compare`](./../../ocaml-compiler/stdlib/Stdlib.md#val-compare):

- `a1 :: l1` is smaller than `a2 :: l2` (negative result) if `a1` is smaller than `a2`, or if they are equal (0 result) and `l1` is smaller than `l2`
- the empty list `[]` is strictly smaller than non-empty lists
Note: the `cmp` function will be called even if the lists have different lengths.

since 4\.12

## Iterators

```ocaml
val iter : ('a -> unit) -> 'a list -> unit
```
`iter f [a1; ...; an]` applies function `f` in turn to `[a1; ...; an]`. It is equivalent to `f a1; f a2; ...; f an`.

```ocaml
val iteri : (int -> 'a -> unit) -> 'a list -> unit
```
Same as [`iter`](./#val-iter), but the function is applied to the index of the element as first argument (counting from 0\), and the element itself as second argument.

since 4\.00
```ocaml
val map : ('a -> 'b) -> 'a list -> 'b list
```
`map f [a1; ...; an]` applies function `f` to `a1, ..., an`, and builds the list `[f a1; ...; f an]` with the results returned by `f`.

```ocaml
val mapi : (int -> 'a -> 'b) -> 'a list -> 'b list
```
Same as [`map`](./#val-map), but the function is applied to the index of the element as first argument (counting from 0\), and the element itself as second argument.

since 4\.00
```ocaml
val rev_map : ('a -> 'b) -> 'a list -> 'b list
```
`rev_map f l` gives the same result as [`rev`](./#val-rev)` (`[`map`](./#val-map)` f l)`, but is more efficient.

```ocaml
val filter_map : ('a -> 'b option) -> 'a list -> 'b list
```
`filter_map f l` applies `f` to every element of `l`, filters out the `None` elements and returns the list of the arguments of the `Some` elements.

since 4\.08
```ocaml
val concat_map : ('a -> 'b list) -> 'a list -> 'b list
```
`concat_map f l` gives the same result as [`concat`](./#val-concat)` (`[`map`](./#val-map)` f l)`. Tail-recursive.

since 4\.10
```ocaml
val fold_left_map : 
  ('acc -> 'a -> 'acc * 'b) ->
  'acc ->
  'a list ->
  'acc * 'b list
```
`fold_left_map` is a combination of `fold_left` and `map` that threads an accumulator through calls to `f`.

since 4\.11
```ocaml
val fold_left : ('acc -> 'a -> 'acc) -> 'acc -> 'a list -> 'acc
```
`fold_left f init [b1; ...; bn]` is `f (... (f (f init b1) b2) ...) bn`.

```ocaml
val fold_right : ('a -> 'acc -> 'acc) -> 'a list -> 'acc -> 'acc
```
`fold_right f [a1; ...; an] init` is `f a1 (f a2 (... (f an init) ...))`. Not tail-recursive.


## Iterators on two lists

```ocaml
val iter2 : ('a -> 'b -> unit) -> 'a list -> 'b list -> unit
```
`iter2 f [a1; ...; an] [b1; ...; bn]` calls in turn `f a1 b1; ...; f an bn`.

raises [`Invalid_argument`](./../../ocaml-compiler/stdlib/Stdlib.md#exception-Invalid_argument) if the two lists are determined to have different lengths.
```ocaml
val map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
```
`map2 f [a1; ...; an] [b1; ...; bn]` is `[f a1 b1; ...; f an bn]`.

raises [`Invalid_argument`](./../../ocaml-compiler/stdlib/Stdlib.md#exception-Invalid_argument) if the two lists are determined to have different lengths.
```ocaml
val rev_map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
```
`rev_map2 f l1 l2` gives the same result as [`rev`](./#val-rev)` (`[`map2`](./#val-map2)` f l1 l2)`, but is more efficient.

```ocaml
val fold_left2 : 
  ('acc -> 'a -> 'b -> 'acc) ->
  'acc ->
  'a list ->
  'b list ->
  'acc
```
`fold_left2 f init [a1; ...; an] [b1; ...; bn]` is `f (... (f (f init a1 b1) a2 b2) ...) an bn`.

raises [`Invalid_argument`](./../../ocaml-compiler/stdlib/Stdlib.md#exception-Invalid_argument) if the two lists are determined to have different lengths.
```ocaml
val fold_right2 : 
  ('a -> 'b -> 'acc -> 'acc) ->
  'a list ->
  'b list ->
  'acc ->
  'acc
```
`fold_right2 f [a1; ...; an] [b1; ...; bn] init` is `f a1 b1 (f a2 b2 (... (f an bn init) ...))`.

raises [`Invalid_argument`](./../../ocaml-compiler/stdlib/Stdlib.md#exception-Invalid_argument) if the two lists are determined to have different lengths. Not tail-recursive.

## List scanning

```ocaml
val for_all : ('a -> bool) -> 'a list -> bool
```
`for_all f [a1; ...; an]` checks if all elements of the list satisfy the predicate `f`. That is, it returns `(f a1) && (f a2) && ... && (f an)` for a non-empty list and `true` if the list is empty.

```ocaml
val exists : ('a -> bool) -> 'a list -> bool
```
`exists f [a1; ...; an]` checks if at least one element of the list satisfies the predicate `f`. That is, it returns `(f a1) || (f a2) || ... || (f an)` for a non-empty list and `false` if the list is empty.

```ocaml
val for_all2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
```
Same as [`for_all`](./#val-for_all), but for a two-argument predicate.

raises [`Invalid_argument`](./../../ocaml-compiler/stdlib/Stdlib.md#exception-Invalid_argument) if the two lists are determined to have different lengths.
```ocaml
val exists2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
```
Same as [`exists`](./#val-exists), but for a two-argument predicate.

raises [`Invalid_argument`](./../../ocaml-compiler/stdlib/Stdlib.md#exception-Invalid_argument) if the two lists are determined to have different lengths.
```ocaml
val mem : 'a -> 'a list -> bool
```
`mem a set` is true if and only if `a` is equal to an element of `set`.

```ocaml
val memq : 'a -> 'a list -> bool
```
Same as [`mem`](./#val-mem), but uses physical equality instead of structural equality to compare list elements.


## List searching

```ocaml
val find : ('a -> bool) -> 'a list -> 'a
```
`find f l` returns the first element of the list `l` that satisfies the predicate `f`.

raises [`Not_found`](./../../ocaml-compiler/stdlib/Stdlib.md#exception-Not_found) if there is no value that satisfies f in the list l.
```ocaml
val find_opt : ('a -> bool) -> 'a list -> 'a option
```
`find f l` returns the first element of the list `l` that satisfies the predicate `f`. Returns `None` if there is no value that satisfies `f` in the list `l`.

since 4\.05
```ocaml
val find_index : ('a -> bool) -> 'a list -> int option
```
`find_index f xs` returns `Some i`, where `i` is the index of the first element of the list `xs` that satisfies `f x`, if there is such an element.

It returns `None` if there is no such element.

since 5\.1
```ocaml
val find_map : ('a -> 'b option) -> 'a list -> 'b option
```
`find_map f l` applies `f` to the elements of `l` in order, and returns the first result of the form `Some v`, or `None` if none exist.

since 4\.10
```ocaml
val find_mapi : (int -> 'a -> 'b option) -> 'a list -> 'b option
```
Same as `find_map`, but the predicate is applied to the index of the element as first argument (counting from 0\), and the element itself as second argument.

since 5\.1
```ocaml
val filter : ('a -> bool) -> 'a list -> 'a list
```
`filter f l` returns all the elements of the list `l` that satisfy the predicate `f`. The order of the elements in the input list is preserved.

```ocaml
val find_all : ('a -> bool) -> 'a list -> 'a list
```
`find_all` is another name for [`filter`](./#val-filter).

```ocaml
val filteri : (int -> 'a -> bool) -> 'a list -> 'a list
```
Same as [`filter`](./#val-filter), but the predicate is applied to the index of the element as first argument (counting from 0\), and the element itself as second argument.

since 4\.11

## List manipulation

```ocaml
val take : int -> 'a list -> 'a list
```
`take n l` returns the prefix of `l` of length `n`, or a copy of `l` if `n > length l`. This is the empty list if `n` is negative.

**Warning.** In version 5\.3 only, this function raises `Invalid_argument` for negative `n` values.

since 5\.3
```ocaml
val drop : int -> 'a list -> 'a list
```
`drop n l` returns the suffix of `l` after `n` elements, or `[]` if `n > length l`. This is `l` if `n` is negative.

**Warning.** In version 5\.3 only, this function raises `Invalid_argument` for negative `n` values.

since 5\.3
```ocaml
val take_while : ('a -> bool) -> 'a list -> 'a list
```
`take_while p l` is the longest (possibly empty) prefix of `l` containing only elements that satisfy `p`.

since 5\.3
```ocaml
val drop_while : ('a -> bool) -> 'a list -> 'a list
```
`drop_while p l` is the longest (possibly empty) suffix of `l` starting at the first element that does not satisfy `p`.

since 5\.3
```ocaml
val partition : ('a -> bool) -> 'a list -> 'a list * 'a list
```
`partition f l` returns a pair of lists `(l1, l2)`, where `l1` is the list of all the elements of `l` that satisfy the predicate `f`, and `l2` is the list of all the elements of `l` that do not satisfy `f`. The order of the elements in the input list is preserved.

```ocaml
val partition_map : ('a -> ('b, 'c) Either.t) -> 'a list -> 'b list * 'c list
```
`partition_map f l` returns a pair of lists `(l1, l2)` such that, for each element `x` of the input list `l`:

- if `f x` is `Left y1`, then `y1` is in `l1`, and
- if `f x` is `Right y2`, then `y2` is in `l2`.
The output elements are included in `l1` and `l2` in the same relative order as the corresponding input elements in `l`.

In particular, `partition_map (fun x -> if f x then Left x else Right x) l` is equivalent to `partition f l`.

since 4\.12

## Association lists

```ocaml
val assoc : 'a -> ('a * 'b) list -> 'b
```
`assoc a l` returns the value associated with key `a` in the list of pairs `l`. That is, `assoc a [ ...; (a,b); ...] = b` if `(a,b)` is the leftmost binding of `a` in list `l`.

raises [`Not_found`](./../../ocaml-compiler/stdlib/Stdlib.md#exception-Not_found) if there is no value associated with a in the list l.
```ocaml
val assoc_opt : 'a -> ('a * 'b) list -> 'b option
```
`assoc_opt a l` returns the value associated with key `a` in the list of pairs `l`. That is, `assoc_opt a [ ...; (a,b); ...] = Some b` if `(a,b)` is the leftmost binding of `a` in list `l`. Returns `None` if there is no value associated with `a` in the list `l`.

since 4\.05
```ocaml
val assq : 'a -> ('a * 'b) list -> 'b
```
Same as [`assoc`](./#val-assoc), but uses physical equality instead of structural equality to compare keys.

```ocaml
val assq_opt : 'a -> ('a * 'b) list -> 'b option
```
Same as [`assoc_opt`](./#val-assoc_opt), but uses physical equality instead of structural equality to compare keys.

since 4\.05
```ocaml
val mem_assoc : 'a -> ('a * 'b) list -> bool
```
Same as [`assoc`](./#val-assoc), but simply return `true` if a binding exists, and `false` if no bindings exist for the given key.

```ocaml
val mem_assq : 'a -> ('a * 'b) list -> bool
```
Same as [`mem_assoc`](./#val-mem_assoc), but uses physical equality instead of structural equality to compare keys.

```ocaml
val remove_assoc : 'a -> ('a * 'b) list -> ('a * 'b) list
```
`remove_assoc a l` returns the list of pairs `l` without the first pair with key `a`, if any. Not tail-recursive.

```ocaml
val remove_assq : 'a -> ('a * 'b) list -> ('a * 'b) list
```
Same as [`remove_assoc`](./#val-remove_assoc), but uses physical equality instead of structural equality to compare keys. Not tail-recursive.


## Lists of pairs

```ocaml
val split : ('a * 'b) list -> 'a list * 'b list
```
Transform a list of pairs into a pair of lists: `split [(a1,b1); ...; (an,bn)]` is `([a1; ...; an], [b1; ...; bn])`. Not tail-recursive.

```ocaml
val combine : 'a list -> 'b list -> ('a * 'b) list
```
Transform a pair of lists into a list of pairs: `combine [a1; ...; an] [b1; ...; bn]` is `[(a1,b1); ...; (an,bn)]`.

raises [`Invalid_argument`](./../../ocaml-compiler/stdlib/Stdlib.md#exception-Invalid_argument) if the two lists have different lengths. Not tail-recursive.

## Sorting

```ocaml
val sort : ('a -> 'a -> int) -> 'a list -> 'a list
```
Sort a list in increasing order according to a comparison function. The comparison function must return 0 if its arguments compare as equal, a positive integer if the first is greater, and a negative integer if the first is smaller (see Array.sort for a complete specification). For example, [`Stdlib.compare`](./../../ocaml-compiler/stdlib/Stdlib.md#val-compare) is a suitable comparison function. The resulting list is sorted in increasing order. [`sort`](./#val-sort) is guaranteed to run in constant heap space (in addition to the size of the result list) and logarithmic stack space.

The current implementation uses Merge Sort. It runs in constant heap space and logarithmic stack space.

```ocaml
val stable_sort : ('a -> 'a -> int) -> 'a list -> 'a list
```
Same as [`sort`](./#val-sort), but the sorting algorithm is guaranteed to be stable (i.e. elements that compare equal are kept in their original order).

The current implementation uses Merge Sort. It runs in constant heap space and logarithmic stack space.

```ocaml
val fast_sort : ('a -> 'a -> int) -> 'a list -> 'a list
```
Same as [`sort`](./#val-sort) or [`stable_sort`](./#val-stable_sort), whichever is faster on typical input.

```ocaml
val sort_uniq : ('a -> 'a -> int) -> 'a list -> 'a list
```
Same as [`sort`](./#val-sort), but also remove duplicates: if multiple elements compare equal, keep only the first.

since 4\.02 (4.03 in ListLabels)
before 5\.4 the element kept was not necessarily the first one.
```ocaml
val merge : ('a -> 'a -> int) -> 'a list -> 'a list -> 'a list
```
Merge two lists: Assuming that `l1` and `l2` are sorted according to the comparison function `cmp`, `merge cmp l1 l2` will return a sorted list containing all the elements of `l1` and `l2`. If several elements compare equal, the elements of `l1` will be before the elements of `l2`. Not tail-recursive (sum of the lengths of the arguments).


## Lists and Sequences

```ocaml
val to_seq : 'a list -> 'a Seq.t
```
Iterate on the list.

since 4\.07
```ocaml
val of_seq : 'a Seq.t -> 'a list
```
Create a list from a sequence.

since 4\.07
```ocaml
val map_filter : ('a -> 'b option) -> 'a list -> 'b list
```
```ocaml
val last : 'a list -> 'a
```
```ocaml
val assoc_remove : 'a -> ('a * 'b) list -> 'b * ('a * 'b) list
```
```ocaml
val remove_first_if_any : 'a -> 'a list -> 'a list
```
```ocaml
val remove_first_if_any_q : 'a -> 'a list -> 'a list
```
```ocaml
val remove_first : 'a -> 'a list -> 'a list
```
```ocaml
val remove_first_q : 'a -> 'a list -> 'a list
```
```ocaml
val remove_all : 'a -> 'a list -> 'a list
```
```ocaml
val remove_all_q : 'a -> 'a list -> 'a list
```
```ocaml
val remove_all_assoc : 'a -> ('a * 'b) list -> ('a * 'b) list
```
```ocaml
val remove_all_assoc_q : 'a -> ('a * 'b) list -> ('a * 'b) list
```
```ocaml
val is_prefix : 'a list -> 'a list -> bool
```
```ocaml
val chop : int -> 'a list -> 'a list
```
```ocaml
val split_at : int -> 'a list -> 'a list * 'a list
```