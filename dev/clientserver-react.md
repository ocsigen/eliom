# Shared reactive programming

In reactive programming, the programmer declaratively defines relationships between different pieces of data, and between the data and what is displayed in the interface. This allows rapid development of robust user interfaces. [Another manual section](./clientserver-html.md#reactive) describes Eliom's client-side reactive infrastructure.

Client-side reactive programming in itself does not adequately cover all the requirements of the modern Web. Namely, with client-centric programming, the initialization of the interface happens on the client, sometimes with a noticeable lag. Also, the HTML sent by the server contains little of the actual content, thus being unsuitable for search-engine indexing.

To overcome this limitation, Eliom 5\.0 and higher enable what we call *shared* reactive programming. This means that we operate on signals that have both a server-side and a client-side meaning. The server-side signals produce a first version of the interface that is more than a skeleton, while the client-side signals are responsible for the dynamic updates.

## Shared signals

Our client-side reactive infrastructure heavily relies on the [React](http://erratique.ch/software/react) library. The module [`Eliom_shared.React`](./eliom.server/Eliom_shared-React.md) builds on `React` to provide *shared* signals (type `Eliom_shared.React.S.t`).

We explain the ideas behind [`Eliom_shared.React`](./eliom.server/Eliom_shared-React.md) aided by the following example.

```ocaml
let%server
  (s : int Eliom_shared.React.S.t),
  (f : (?step:React.step -> int -> unit) Eliom_shared.Value.t)
  =
  Eliom_shared.React.S.create 0

let%client incr_s () =
  let v = Eliom_shared.React.S.value ~%s in
  ~%f (v + 1)

let%shared msg_of_int i =
  Printf.sprintf "value is %d" i

let s_as_string () : string Eliom_shared.React.S.t =
  Eliom_shared.React.S.map [%shared msg_of_int] s
```
[`Eliom_shared.React.S`](./eliom.server/Eliom_shared-React-S.md) implements an interface very similar to plain `React.S`. In the example, we create a signal `s` via `create`, which also gives us the function `f` for updating it. `f` can *only* be called on the client side; calling it on the server raises an exception.

The client-side function `incr_s` gets the current value of `s` and uses `f` to increase the value by 1\. Note that we use injections `~%` to pass `s` and `f` to the client.

Similarly to plain `React`, we can use `Eliom_shared.React.S.map` to derive new signals by applying functions on previous signals. The difference is that we need to use a function (in the example, `msg_of_int`) implemented on both sides (`let%shared`). We use `[%shared msg_of_int]` to denote the combination of the two implementations, rather than the server-side implementation. (The example would also work with an anonymous function inside `[%shared ...]`: `[%shared (fun i -> Printf.sprintf "value is %d" i)]`).

Since the signals do not get updated on the server, all server-side computation is one-off. For example, the server-side `msg_of_int` will be called only once. On the client, updates happen just like for plain `React`.

## HTML and SVG content

The server-side module [`Eliom_content.Html.R`](./eliom.server/Eliom_content-Html-R.md) enables constructing HTML elements that get updated automatically based on [`Eliom_shared.React`](./eliom.server/Eliom_shared-React.md) signals. Continuing our example, we can use the signal `s_as_string ()` as follows:

```ocaml
let%server node () =
  Eliom_content.Html.R.txt (s_as_string ())
```
`node ()` can be used similarly to any node produced by [`Eliom_content.Html.D`](./eliom.server/Eliom_content-Html-D.md) or [F](./eliom.server/Eliom_content-Html-F.md):

```ocaml
(* ... *)

let () =
  Shared_reactive_app.register ~service:main_service @@ fun () () ->
  Lwt.return @@ Eliom_tools.F.html
    ~title:"shared_reactive"
    ~css:[["css";"shared_reactive.css"]]
    Eliom_content.Html.(F.body [
      F.h2 [F.txt "Welcome from Eliom's distillery!"];
      node ();
      F.p ~a:[F.a_onclick [%client fun _ -> incr_s ()]]
        [F.txt "incr s"];
    ])
```
[`Eliom_content.Svg.R`](./eliom.server/Eliom_content-Svg-R.md) operates in a similar fashion, allowing for shared reactive graphics.

## ReactiveData

Just like [`Eliom_shared.React`](./eliom.server/Eliom_shared-React.md) is the shared counterpart of `React`, [`Eliom_shared.ReactiveData`](./eliom.server/Eliom_shared-ReactiveData.md) is the shared counterpart of [ReactiveData](https://github.com/ocsigen/reactiveData). We provide an example.

```ocaml
let
  (l : int Eliom_shared.ReactiveData.RList.t),
  (h : int Eliom_shared.ReactiveData.RList.handle)
  =
  Eliom_shared.ReactiveData.RList.create []

let%client cons_to_l () =
  Eliom_shared.ReactiveData.RList.cons 1 ~%h
```
We use `create` to produce a shared reactive list `l` of integers. We also obtain a *handle* that allows us to manipulate the list on the client, e.g., by adding elements as per the function `cons_to_l`.

`Eliom_shared.ReactiveData.RList.map` applies a given shared function to every element of a shared reactive list (including new elements as they are produced), producing a new shared reactive list:

```ocaml
let l_nodes () =
  Eliom_shared.ReactiveData.RList.map
    [%shared
      fun i ->
        Eliom_content.Html.D.txt (Printf.sprintf "[%d]" i)
    ]
    l
```
Shared reactive lists can (and need to) be used wherever [`Eliom_content.Html.R`](./eliom.server/Eliom_content-Html-R.md) expects lists, e.g., we can build a `<div>` as follows:

```ocaml
let l_div () = Eliom_content.Html.R.div (l_nodes ())
```
Adding a new element via `cons_to_l` does not rebuild the whole `l_div ()`, but only adds a new child. Similarly, in the case where existing nodes are updated, only the modified ones are re-rendered after every update operation.

## Links

- [React](http://erratique.ch/software/react)
- [ReactiveData](https://github.com/ocsigen/reactiveData)
- [Our PPX syntax extension](./ppx-syntax.md), which is crucial for enabling the discussed paradigm
