# Module `ReactiveData.RList`

```ocaml
type 'a p = 
  | I of int * 'a (* I (i, v) adds v at position i *)
  | R of int (* R i removes i-th element *)
  | U of int * 'a (* U (i, v) substitutes i-th element with v *)
  | X of int * int (* X (i, j) swaps the i-th and j-th elements *)
```
Patch operation on lists. All operations are of linear complexity.

```ocaml
type 'a patch = 'a p list
```
A patch is a list of patch operations. The operations are applied in the order they appear in the list.

The indices correspond to list contents after the operations that appear earlier in the list have been applied, not to the contents before the whole patch operation.

A patch comprised of `I`, `R`, and `U` steps with increasing indices can be applied in time O(m \+ n), where m is the patch length and n is the current size of the list. (Arbitrary patches are slower, requiring O(m \* n).)

```ocaml
type 'a t = 'a ReactiveData.RList.t
```
Reactive version of the data container

```ocaml
type 'a data = 'a list
```
Raw (non-reactive) version of the data container

```ocaml
type 'a msg = 
  | Patch of 'a patch (* Patch p triggers the application of p on the current contents *)
  | Set of 'a data (* With Set d, d becomes the new content *)
```
Message format

```ocaml
type 'a handle = 'a ReactiveData.RList.handle
```
Handle that permits applying incremental updates

```ocaml
val empty : 'a t
```
Empty data structure

```ocaml
val from_event : 'a data -> 'a msg React.E.t -> 'a t
```
`from_event d e` is a container whose initial value is `d`, and which gets updated for every occurrence of `e`

```ocaml
val const : 'a data -> 'a t
```
Produce a constant container

```ocaml
val patch : 'a handle -> 'a patch -> unit
```
`patch h p` applies `p` on the container corresponding to `h`

```ocaml
val set : 'a handle -> 'a data -> unit
```
`set h d` sets the contents of the container corresponding to `h`, disregarding previous contents

```ocaml
val map_msg : ('a -> 'b) -> 'a msg -> 'b msg
```
Transform a message

```ocaml
val fold : ('a -> 'b msg -> 'a) -> 'b t -> 'a -> 'a React.signal
```
`fold f c v` accumulates the updates on `c` with `f` starting from `v`.

The result is a signal of value `f m_n (f ... (f m_1 v))`, where `m_1` ... `m_n` are the messages that have been applied since the beginning of `fold`. `m_1` is a pseudo-message `Set l`, accounting for the contents `l` of `c` at the time when accumulation starts.

```ocaml
val event : 'a t -> 'a msg React.E.t
```
Event whose occurrences correspond to container updates

```ocaml
val cons : 'a -> 'a handle -> unit
```
Add element to the beginning

```ocaml
val snoc : 'a -> 'a handle -> unit
```
Add element to the end

```ocaml
val insert : 'a -> int -> 'a handle -> unit
```
`insert v i h` adds `v` as the `i`\-th position in the container corresponding to `h`. The indices of the subsequent elements change.

```ocaml
val remove : int -> 'a handle -> unit
```
`remove i h` removes the `i`\-th position from the container corresponding to `h`. The indices of the subsequent elements change.

```ocaml
val remove_last : ('a t * 'a handle) -> unit
```
`remove_last a` removes the last element of `a`

```ocaml
val remove_eq : ?eq:('a -> 'a -> bool) -> ('a t * 'a handle) -> 'a -> unit
```
`remove_eq l x` removes the first occurence of `x` from `l`

```ocaml
val update : 'a -> int -> 'a handle -> unit
```
`update v i h` substitutes the `i`\-th element of the container corresponding to `h` with `v`

```ocaml
val update_eq : 
  ?eq:('a -> 'a -> bool) ->
  ('a t * 'a handle) ->
  'a ->
  'a ->
  unit
```
`update_eq l a b` substitutes the first occurence of `a` (according to `eq`) in `l` with `b`

```ocaml
val move : int -> int -> 'a handle -> unit
```
`move i offset h` moves the `i`\-th element of the container corresponding by `offset` positions in `h`, modifying the indices of other elements

```ocaml
val singleton : 'a -> 'a t
```
Produce container list containing a single, constant element

```ocaml
val rev : 'a t -> 'a t
```
`rev a` is the reversal of `a`; `rev a` gets updated along with `a`

```ocaml
val filter : ('a -> bool) -> 'a t -> 'a t
```
`filter pred l` keeps the elements of `l` matching `pred`; gets updated when `l` is. `pred` should be a pure function

```ocaml
val for_all : ('a -> bool) -> 'a t -> bool React.S.t
```
`for_all fn l` is a `bool React.S.t` verifying that all elements `x` of `l` satisfy `fn x`

```ocaml
val create : 
  ?default:
    ('a ReactiveData.RList.t * 'a ReactiveData.RList.handle) option
      Eliom_client_value.t ->
  ?reset_default:bool ->
  'a list ->
  'a t * 'a handle
```
`create ?default ?reset_default l` produces a pair `l, f`, where `s` is a (shared) reactive list, and `f` is a handle for manipulating the list.

The initial value of the list is `l`, unless `default` is provided. `default`, if provided, is used as the client-side list (and corresponding handle). `reset_default`, if set to true (default: false), resets the value of `default` to `l`.

```ocaml
val concat : 'a t -> 'a t -> 'a t
```
```ocaml
val value : 'a t -> 'a list Value.t
```
```ocaml
val signal : ?eq:('a -> 'a -> bool) Value.t -> 'a t -> 'a list React.S.t
```
```ocaml
val singleton_s : 'a React.S.t -> 'a t
```
```ocaml
val map : ('a -> 'b) Value.t -> 'a t -> 'b t
```
```ocaml
val from_signal : ?eq:('a -> 'a -> bool) Value.t -> 'a list React.S.t -> 'a t
```
```ocaml
val acc_e : 
  ?init:('a t * 'a handle) ->
  'a React.E.t Eliom_client_value.t ->
  'a t
```
```ocaml
module Lwt : sig ... end
```
Cooperative versions of the ReactiveData operators
