
# Module `Eliom`

```ocaml
module App : sig ... end
```
```ocaml
module Bus : sig ... end
```
Broadcasting facilities between clients and server

```ocaml
module Client : sig ... end
```
```ocaml
module Client_base : sig ... end
```
```ocaml
module Client_main : sig ... end
```
```ocaml
module Client_value : sig ... end
```
```ocaml
module Comet : sig ... end
```
Primitives to push data to the client, without explicit request.

```ocaml
module Comet_base : sig ... end
```
```ocaml
module Common : sig ... end
```
Low level functions for Eliom, exceptions and types.

```ocaml
module Common_base : sig ... end
```
```ocaml
module Config : sig ... end
```
This module makes also possible get information from the configuration file.

```ocaml
module Content : sig ... end
```
This module allows creating valid HTML content, or other XML formats.

```ocaml
module Content_core : sig ... end
```
See [`Content`](./Eliom-Content.md) for complete module.

```ocaml
module Content_functor : sig ... end
```
```ocaml
module Content_sigs : sig ... end
```
```ocaml
module Cookies_base : sig ... end
```
```ocaml
module Cscache : sig ... end
```
Client-server cache for Eliom applications, that is used both for keeping data on client side or keeping values in memory during a request.

```ocaml
module Eliom_form : sig ... end
```
```ocaml
module Eliom_lazy : sig ... end
```
```ocaml
module Eliom_react : sig ... end
```
Propagate events occurrences from the server to the client and the other way around. Occurrence propagation is done asynchronously.

```ocaml
module Eliom_uri : sig ... end
```
Low-level functions for relative or absolute URL calculation.

```ocaml
module Error_pages : sig ... end
```
```ocaml
module Extension : sig ... end
```
Allows Ocsigen's extension to access Eliom data. See the Eliom manual for more information about [Eliom's extensions](./../workflow-configuration.md#extensions)

```ocaml
module Form_sigs : sig ... end
```
```ocaml
module Lib : sig ... end
```
A few common functions used by Eliom. Extension of OCaml stdlib. See also [`Ocsigen_base.Lib`](./../../ocsigenserver/ocsigenserver.baselib/Ocsigen_base-Lib.md)

```ocaml
module Lib_base : sig ... end
```
```ocaml
module Mkreg : sig ... end
```
This module defines the functor to use to creates modules generating functions to register services for your own types of pages. It is used for example in [`Registration`](./Eliom-Registration.md).

```ocaml
module Mod_cli : sig ... end
```
```ocaml
module Mod_cookies : sig ... end
```
```ocaml
module Mod_datasess : sig ... end
```
```ocaml
module Mod_gc : sig ... end
```
```ocaml
module Mod_main : sig ... end
```
```ocaml
module Mod_pagegen : sig ... end
```
```ocaml
module Mod_parameters : sig ... end
```
```ocaml
module Mod_persess : sig ... end
```
```ocaml
module Mod_sersess : sig ... end
```
```ocaml
module Mod_sessadmin : sig ... end
```
```ocaml
module Mod_sessexpl : sig ... end
```
```ocaml
module Mod_sessiongroups : sig ... end
```
```ocaml
module Mod_timeouts : sig ... end
```
```ocaml
module Notif : sig ... end
```
Server to client notifications.

```ocaml
module Parameter : sig ... end
```
```ocaml
module Parameter_base : sig ... end
```
```ocaml
module Parameter_sigs : sig ... end
```
Ad-hoc runtime type representation for service parameters.

```ocaml
module Process : sig ... end
```
```ocaml
module Reference : sig ... end
```
```ocaml
module Registration : sig ... end
```
Eliom services registration for various kinds of page content: Eliom application, valid [`Html`](./Eliom-Registration-Html.md), actions, redirections, static files, ...

```ocaml
module Registration_sigs : sig ... end
```
```ocaml
module Request_info : sig ... end
```
This module contains the functions you need to get (or set) information about current request.

```ocaml
module Route : sig ... end
```
```ocaml
module Route_base : sig ... end
```
```ocaml
module Runtime : sig ... end
```
```ocaml
module Service : sig ... end
```
Creation and manipulation of Eliom services.

```ocaml
module Service_base : sig ... end
```
```ocaml
module Service_sigs : sig ... end
```
```ocaml
module Shared : sig ... end
```
This module implements shared (i.e., client-server) versions of the React and ReactiveData libraries.

```ocaml
module Shared_content : sig ... end
```
```ocaml
module Shared_sigs : sig ... end
```
```ocaml
module State : sig ... end
```
Storing server-side values for your applications or sessions.

```ocaml
module Syntax : sig ... end
```
```ocaml
module Tools : sig ... end
```
Helpers for (hierarchical) menu generation in HTML5. See the Eliom manual for more information about [menu](./../misc.md#basic_menu) or [hierarchical site](./../misc.md#hier_menu).

```ocaml
module Types : sig ... end
```
Types shared by client and server.

```ocaml
module Types_base : sig ... end
```
```ocaml
module Wrap : sig ... end
```