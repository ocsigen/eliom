
# Client-side services

Starting with Eliom 6\.0, services can have client-side handlers. Registering client-side handlers for services allows us to perform most of the operations (e.g., building the user interface) locally, and only call the server for operations that are by necessity non-local (e.g., retrieving database data).

Client-side service implementation is required for building mobile applications with Eliom, but can also be used to change page faster within a Web application. In this manual chapter, we focus on the client-side service mechanics and do not go into the details of setting up a complete mobile application. The complete setup needed is discussed in the [mobile applications chapter](./mobile-apps.md) of this manual.


## Service creation and registering

As described in the chapter [on server-side services](./server-services.md), services are first created via [`Eliom_service.create`](./eliom.server/Eliom_service.md#val-create) and similar functions, and subsequently registered.

The first step (creation) can only happen on the server; allowing the opposite would permit "creating" on the client services that are not actually provided by the server. To allow client-side service manipulation, services can be injected, as demonstrated by the following example:

```ocaml
let%server s =
  Eliom_service.create
    ~meth:(Eliom_service.Get Eliom_parameter.unit)
    ~path:(Eliom_service.Path ["content"])
    ()

let%client s = ~%s
```
The above style makes the service available under the same identifier (here `s`) on both sides. Subsequently, the service can be registered on both sides with the same [`Eliom_registration`](./eliom.server/Eliom_registration.md) APIs. For example:

```ocaml
let%shared () =
  Eliom_registration.Html.register s
    (fun () () ->
       Lwt.return
         (Eliom_tools.F.html
            ~title:"hybrid"
            Html.F.(body [
              h1 [txt "Salut !"];
            ])))
```
The types of the handlers are compatible between the two sides. For instance, [`Eliom_registration.Html`](./eliom.server/Eliom_registration-Html.md) expects a function that produces a `Html_types.html Eliom_content.Html.D.elt Lwt.t`, for [`Eliom_registration.Action`](./eliom.server/Eliom_registration-Action.md) we must return `unit Lwt.t`, and so on. The behavior of all services is compatible to the extent possible, e.g., actions perform a reload after performing their side effect.

For application services, the [`Eliom_registration.App`](./eliom.server/Eliom_registration-App.md) functor needs to be called just like on the server. The signatures are compatible between the two sides, and therefore the functor invocation can happen in a shared section.

A service registered as above can be called via links (e.g., `Eliom_content.Html.D.a`) and forms just like a standard server-only service. Eliom detects the existence of a client-side implementation and calls that instead of performing a request to the server. If no client-side implementation exists, a standard server request is performed, which may cause problems in the context of mobile applications.


## Service routing

Service routing is the operation of choosing a service based on a URL and a collection of POST parameters. This is clearly a key operation on the server, allowing Eliom to respond on different paths. Routing has also become available on the client via the function `Eliom_client.change_page_uri`. This function can be used for example in order to open a link inside a mobile application without launching the Web browser.
