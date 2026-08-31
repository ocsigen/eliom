# Module `Eliom_shared`

This module implements shared (i.e., client-server) versions of the React and ReactiveData libraries.

On the server side, the reactive signals and data structures are comprised of a server-side version and a client-side version. The server-side signals (and data structures) are evaluated only once.

All operations on signals and data need to be provided in the form of shared functions, i.e., functions that have both a client-side and a server-side implementation.

```ocaml
val to_signal : 
  init:'a ->
  ?eq:('a -> 'a -> bool) ->
  'a React.S.t Lwt.t ->
  'a React.S.t
```
`to_signal ~init s` converts the Lwt-wrapped signal `s` into a regular signal with initial value `init`.

```ocaml
module Value : Eliom_shared_sigs.VALUE
```
Accessing shared values

```ocaml
module React : sig ... end
```
Shared implementation of React

```ocaml
module FakeReactiveData : sig ... end
```
This is a dummy ReactiveData module that allows us to refer to client-side ReactiveData types on the server side, without actually linking against ReactiveData.

```ocaml
module ReactiveData : sig ... end
```
Shared implementation of ReactiveData
