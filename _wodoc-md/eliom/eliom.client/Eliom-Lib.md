
# Module `Eliom.Lib`

Eliom standard library

See [`Ocsigen_lib_base`](./../../ocsigenserver/ocsigenserver.baselib.base/Ocsigen_lib_base.md).

```ocaml
exception Ocsigen_Internal_Error of string
```
```ocaml
exception Input_is_too_large
```
```ocaml
exception Ocsigen_Bad_Request
```
```ocaml
exception Ocsigen_Request_too_long
```
```ocaml
val (>>=) : 'a Lwt.t -> ('a -> 'b Lwt.t) -> 'b Lwt.t
```
`p >>= f` is the same as [`Lwt.bind`](./../../lwt/lwt/Lwt.md#val-bind)` p f`. It requires `Lwt.Infix` to be opened in scope:

```ocaml
open Lwt.Infix

let () =
  Lwt_main.run
    (Lwt_io.(read_line stdin) >>= Lwt_io.printl)

(* ocamlfind opt -linkpkg -thread -package lwt.unix code.ml && ./a.out *)
```
It is recommended to use the PPX `let%lwt` syntax instead. This operator is the next-best choice. It is frequently found while reading existing Lwt code.

```ocaml
val (>|=) : 'a Lwt.t -> ('a -> 'b) -> 'b Lwt.t
```
`p >|= f` is the same as [`Lwt.map`](./../../lwt/lwt/Lwt.md#val-map)` f p`. It requires `Lwt.Infix` to be opened in scope.

```ocaml
open Lwt.Infix

let () =
  Lwt_main.run
    (Lwt_io.(read_line stdin) >|= ignore)

(* ocamlfind opt -linkpkg -thread -package lwt.unix code.ml && ./a.out *)
```
```ocaml
val (<&>) : unit Lwt.t -> unit Lwt.t -> unit Lwt.t
```
`p1 <&> p2` is the same as [`Lwt.join`](./../../lwt/lwt/Lwt.md#val-join)` [p1; p2]`. It requires `Lwt.Infix` to be opened in scope.

Unlike with [`Lwt.bind`](./../../lwt/lwt/Lwt.md#val-bind) and [`Lwt.map`](./../../lwt/lwt/Lwt.md#val-map), there are no problems with explicit [`Lwt.join`](./../../lwt/lwt/Lwt.md#val-join) syntax, so using this operator is not recommended.

```ocaml
val (<?>) : 'a Lwt.t -> 'a Lwt.t -> 'a Lwt.t
```
`p1 <?> p2` is the same as [`Lwt.choose`](./../../lwt/lwt/Lwt.md#val-choose)` [p1; p2]`. It requires `Lwt.Infix` to be opened in scope.

Unlike with [`Lwt.bind`](./../../lwt/lwt/Lwt.md#val-bind) and [`Lwt.map`](./../../lwt/lwt/Lwt.md#val-map), there are no problems with explicit [`Lwt.choose`](./../../lwt/lwt/Lwt.md#val-choose) syntax, so using this operator is not recommended.

Furthermore, most users actually need [`Lwt.pick`](./../../lwt/lwt/Lwt.md#val-pick) instead of [`Lwt.choose`](./../../lwt/lwt/Lwt.md#val-choose).

```ocaml
val (=<<) : ('a -> 'b Lwt.t) -> 'a Lwt.t -> 'b Lwt.t
```
`f =<< p` is the same as [`Lwt.bind`](./../../lwt/lwt/Lwt.md#val-bind)` p f`. It requires `Lwt.Infix` to be opened in scope.

This operator is obscure and its use is discouraged. It is the same as `p >>= f`.

```ocaml
val (=|<) : ('a -> 'b) -> 'a Lwt.t -> 'b Lwt.t
```
`f =|< p` is the same as [`Lwt.map`](./../../lwt/lwt/Lwt.md#val-map)` f p`. It requires `Lwt.Infix` to be opened in scope.

This operator is obscure and its use is discouraged. It is the same as `p >|= f`.

```ocaml
module Let_syntax : sig ... end
```
This module provides support for [ppx\_let](https://github.com/janestreet/ppx_let).

```ocaml
val (!!) : 'a Lazy.t -> 'a
```
```ocaml
val (|>) : 'a -> ('a -> 'b) -> 'b
```
```ocaml
val (@@) : ('a -> 'b) -> 'a -> 'b
```
```ocaml
val id : 'a -> 'a
```
```ocaml
val comp : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
```
```ocaml
val curry : (('a * 'b) -> 'c) -> 'a -> 'b -> 'c
```
```ocaml
val uncurry : ('a -> 'b -> 'c) -> ('a * 'b) -> 'c
```
```ocaml
module Tuple3 : sig ... end
```
```ocaml
type poly = Ocsigen_lib_base.poly
```
```ocaml
val to_poly : 'a -> poly
```
```ocaml
val from_poly : poly -> 'a
```
```ocaml
type yesnomaybe = Ocsigen_lib_base.yesnomaybe = 
  | Yes
  | No
  | Maybe
```
```ocaml
type ('a, 'b) leftright = ('a, 'b) Ocsigen_lib_base.leftright = 
  | Left of 'a
  | Right of 'b
```
```ocaml
val advert : string
```
```ocaml
module Option : sig ... end
```
Module Option to compute type `'a option`

```ocaml
module List : sig ... end
```
Improvement of module List

```ocaml
module Clist : sig ... end
```
Circular lists

```ocaml
module Int : sig ... end
```
```ocaml
module String_base : sig ... end
```
Improvement of module String

```ocaml
module Url_base : sig ... end
```
```ocaml
val debug : string -> unit
```
```ocaml
exception Eliom_Internal_Error of string
```
```ocaml
module type Map_S = sig ... end
```
```ocaml
module Int64_map : 
  Map_S with type key = int64 with type 'a t = 'a Lib_base.Int64_map.t
```
```ocaml
module Int_map : 
  Map_S with type key = int with type 'a t = 'a Lib_base.Int_map.t
```
```ocaml
module String_map : 
  Map_S with type key = string with type 'a t = 'a Lib_base.String_map.t
```
```ocaml
type file_info = Js_of_ocaml.File.file Js_of_ocaml.Js.t
```
```ocaml
val to_json : ?typ:'a Deriving_Json.t -> 'a -> string
```
```ocaml
val of_json : ?typ:'a Deriving_Json.t -> string -> 'a
```
```ocaml
module Url : sig ... end
```
URL manipulation

```ocaml
module String : sig ... end
```
Extension of [`Ocsigen_lib_base.String_base`](./../../ocsigenserver/ocsigenserver.baselib.base/Ocsigen_lib_base-String_base.md).

```ocaml
val raise_error : 
  ?exn:exn ->
  ?section:Logs.src ->
  ('a, unit, string, 'any) format4 ->
  'a
```
```ocaml
val log_inspect : 'a -> unit
```
Print an object in the console.

```ocaml
val eliom_logs_src : Logs.src
```
Log section for logs generated by Eliom.

```ocaml
val alert : ('a, unit, string, unit) format4 -> 'a
```
```ocaml
val jsalert : Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t -> unit
```
```ocaml
val confirm : ('a, unit, string, bool) format4 -> 'a
```
```ocaml
val debug_var : string -> 'a -> unit
```
```ocaml
val trace : ('a, unit, string, unit) format4 -> 'a
```
```ocaml
val unmarshal_js : Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t -> 'a
```
```ocaml
val encode_header_value : typ:'a Deriving_Json.t -> 'a -> string
```
```ocaml
val make_cryptographic_safe_string : ?len:int -> unit -> string
```
Return a base-64 encoded cryptographic safe string of the given length. Not implemented client-side.

```ocaml
module Dom_reference : sig ... end
```