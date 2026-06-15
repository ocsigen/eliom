
# Module `Eliom_shared`

This module implements shared (i.e., client-server) versions of the React and ReactiveData libraries.

Client-side signals and data are type-wise and behavior-wise equivalent to those provided by the underlying React and ReactiveData libraries. Thus, all the operations from React and ReactiveData apply. We provide extended versions of these libraries.

```ocaml
val to_signal : 
  init:'a ->
  ?eq:('a -> 'a -> bool) ->
  'a React.S.t Lwt.t ->
  'a React.S.t
```
`to_signal ~init s` converts the Lwt-wrapped signal `s` into a regular signal with initial value `init`.

```ocaml
module Value : Eliom_shared_sigs.VALUE with type +'a t = 'a
```
Accessing shared values

```ocaml
module React : sig ... end
```
Shared implementation of React; client-side behavior is like standard React

```ocaml
module FakeReactiveData : sig ... end
```
This is a dummy ReactiveData module that allows us to refer to client-side ReactiveData types on the server side, without actually linking against ReactiveData.

```ocaml
module ReactiveData : sig ... end
```
Shared implementation of ReactiveData; client-side behavior is like standard ReactiveData
