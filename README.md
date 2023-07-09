# Eliom - the full-stack OCaml Web and mobile framework
Eliom is a framework for building client/server Web and mobile
applications in OCaml.

It transforms OCaml into a multi-tier language, making it possible to
implement both the server and client parts of a Web and mobile app
as a single program.

This simplifies a lot the communication between server and client.
Applications can run on any Web browser or mobile device (iOS,
Android), saving from the need to develop one version for each
platform.

Eliom has support for reactive pages (generated on server or client),
advanced session mechanism, server to client communication,
continuation based Web programming, etc.

Eliom is part of the [Ocsigen project][ocsigen].

## Installation Instructions

```
opam install eliom
```

## Getting started

Defining a service on path `/foo`, taking any GET parameters:

```ocaml
let myservice =
  Eliom_service.create
    ~path:(Eliom_service.Path ["foo"])
    ~meth:(Eliom_service.Get (Eliom_parameter.any))
    ()

let () =
  Eliom_registration.Html.register ~service:myservice
    (fun get_params () ->
      Lwt.return
         Eliom_content.Html.F.(html (head (title (txt "")))
                                    (body [h1 [txt "Hello"]])))
```

Inserting a link towards that service, with parameters:

```ocaml
Eliom_content.Html.D.a ~service:myservice [txt "Home"] [("param1", "v1"); ("param2", "v2")]
```

Event handlers are written in OCaml:

```ocaml
div ~a:[a_onclick [%client (fun ev -> ... )]] [ ... ]
```

The client-side and server sides are written as a single program:

```ocaml
let%server a = ... (* code for the server part of the application *)

let%client b = ... (* code for the client part of the application *)

let%shared a = ... (* code that will be included in both parts *)
```

Using a server-side value in client-side code:

```ocaml
let%server a = ...

let%client f () =
  print_endline ~%a ; (* print in browser console *)
  ...
```

Calling a server function from the client program:

```ocaml
let%rpc f (x : int) : string Lwt.t = ... (* server-side code *)

let%client () =
  let%lwt r = f 4 in
  ...
```

Saving session data on the server using Eliom references:

```ocaml
let%server r = Eliom_reference.eref ~scope:Eliom_common.default_session_scope 0

let%server f () =
  let%lwt v = Eliom_reference.get r in
  Eliom_reference.set r (v + 1);
  ...

```
Where scope can be:
* `Eliom_common.default_session_scope` (different value for each browser),
* `Eliom_common.default_process_scope` (different value for each tab),
* `Eliom_common.default_group_scope` (different value for each user),
* `Eliom_common.site_scope` (value for the whole site),
* `Eliom_common.global_scope` (global value for the whole server).
Eliom references are persistant if you add optional parameter `~persistent`
to function `Eliom_reference.eref`.

## Learning Eliom

More documentation [here](https://ocsigen.org/tuto/latest/manual/basics).

Write your first Web and mobile application with Eliom using [Ocsigen Start](https://ocsigen.org/ocsigen-start)

## Authors

* Vincent Balat
* Jérôme Vouillon
* Grégoire Henry
* Pierre Chambart
* Benedikt Becker
* Vasilis Papavasileiou
* Boris Yakobowski
* Hugo Heuzard
* Raphaël Proust
* Jan Rochel
* Idir Lankri
* Stéphane Glondu
* Gabriel Radanne
* Gabriel Kerneis
* Denis Berthod
* Jaap Boender
* Simon Castellan
* Mauricio Fernandez
* Archibald Pontier
* Simon Castellan
* Kate Deplaix

[ocsigen]: https://www.ocsigen.org/
[tutorial]: https://ocsigen.org/tuto/
[opam]: https://opam.ocaml.org/
[opaminst]: https://opam.ocaml.org/doc/Install.html
