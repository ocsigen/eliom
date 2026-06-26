
# Module `Eliom`

```ocaml
module Bus : sig ... end
```
Broadcasting facilities between clients and server.

```ocaml
module Client : sig ... end
```
Call server side services and change the current page.

```ocaml
module Client_base : sig ... end
```
```ocaml
module Client_core : sig ... end
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
Handle unsolicited server to client communications.

```ocaml
module Comet_base : sig ... end
```
```ocaml
module Common : sig ... end
```
```ocaml
module Common_base : sig ... end
```
```ocaml
module Config : sig ... end
```
```ocaml
module Content : sig ... end
```
This module provides the creation of valid XML content, i.e. XML, SVG, and (X)HTML5.

```ocaml
module Content_core : sig ... end
```
XML building and deconstructing. Cf. [`Content_core.Xml`](./Eliom-Content_core-Xml.md).

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
```ocaml
module Eliom_form : sig ... end
```
```ocaml
module Eliom_lazy : sig ... end
```
```ocaml
module Eliom_react : sig ... end
```
Client side type declarations for React event propagation. This module must be linked for events to work properly.

```ocaml
module Eliom_uri : sig ... end
```
Low-level functions for relative or absolute URL calculation.

```ocaml
module Form_sigs : sig ... end
```
```ocaml
module Lib : sig ... end
```
Eliom standard library

```ocaml
module Lib_base : sig ... end
```
```ocaml
module Mod_cookies : sig ... end
```
```ocaml
module Mod_dom : sig ... end
```
Cross browser dom manipulation functions

```ocaml
module Mod_parameters : sig ... end
```
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
module Registration : sig ... end
```
Client-side service registration.

```ocaml
module Registration_sigs : sig ... end
```
```ocaml
module Request : sig ... end
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
module Tools : sig ... end
```
Helpers for (hierarchical) menu generation in HTML5. See the Eliom manual for more information about [menu](./../misc.md#basic_menu) or [hierarchical site](./../misc.md#hier_menu).

```ocaml
module Types : sig ... end
```
```ocaml
module Types_base : sig ... end
```
```ocaml
module Unwrap : sig ... end
```
```ocaml
module Wrap : sig ... end
```