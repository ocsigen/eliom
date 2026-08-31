# Module `Eliom_registration`

Eliom services registration for various kinds of page content: Eliom application, valid [`Html`](./Eliom_registration-Html.md), actions, redirections, static files, ...

**Please read the Eliom manual before this page to learn how to use [services](./../server-services.md) and [predefined outputs](./../server-outputs.md).**

### Type definitions

```ocaml
type 'a kind
```
The type `kind` is an abstract type for the HTTP frame returned by a service. The type parameter indicates the content type, and is one of the following types:

- [`application_content`](./#type-application_content)
- [`browser_content`](./#type-browser_content)
- [`block_content`](./#type-block_content)
- [`unknown_content`](./#type-unknown_content)
- [`ocaml_content`](./#type-ocaml_content)

#### Return types for `Eliom_service.service`

##### Classical content

```ocaml
type browser_content = [ 
  | `Browser
 ]
```
The type `browser_content` is to be used as a phantom type parameter for [`Eliom_registration.kind`](./#type-kind). It means the returned content must be interpreted in the browser as stated by the content-type header. This is most common return type for an eliom service, see for example [`Html`](./Eliom_registration-Html.md), [`CssText`](./Eliom_registration-CssText.md), [`File`](./Eliom_registration-File.md), [`Redirection`](./Eliom_registration-Redirection.md).

```ocaml
type block_content
```
The type `block_content` is to be used as a phantom type parameter for [`Eliom_registration.kind`](./#type-kind). It means the returned content is a subtree of an XML value. See for example `Block5` or `Make_typed_xml_registration`.

```ocaml
type unknown_content
```
The type `unknown_content` is to be used as a phantom type parameter for [`Eliom_registration.kind`](./#type-kind) when the content-type can't be determined statically. See `Text` or [`Any`](./Eliom_registration-Any.md).

##### Application content

```ocaml
type 'a application_content = [ 
  | `Appl of 'a
 ]
```
The type `application_content` is a refinement of `appl_service` to be used as a phantom type parameters for [`Eliom_registration.kind`](./#type-kind). The parameter `'a` is phantom type that is unique for a given application.

```ocaml
type 'a application_name
```
Typed application name

##### OCaml content

```ocaml
type 'a ocaml_content
```
The type `ocaml_content` is an synomyn for `Eliom_service.ocaml_service` to be used as a phantom type parameters for [`Eliom_registration.kind`](./#type-kind). See [`Ocaml`](./Eliom_registration-Ocaml.md).

### Using HTML with services

```ocaml
module Html : 
  Eliom_registration_sigs.S_with_create
    with type page = Html_types.html Eliom_content.Html.elt
     and type options = unit
     and type return = Eliom_service.non_ocaml
     and type result = browser_content kind
```
Eliom service registration for services that return HTML pages.

### Eliom client/server applications

```ocaml
type appl_service_options = {
  do_not_launch : bool;
}
```
Type for the options of an Eliom application service.

If you set `do_not_launch` to `true` when creating an application service, it will send the page without launching the client side program. However, if the program is already lanched, the client side process won't be stopped. Use this if some of your pages are not using the client side program and you want to make them load faster.

```ocaml
val default_appl_service_options : appl_service_options
```
The default options record for an eliom service. See [`appl_service_options`](./#type-appl_service_options).

```ocaml
module type APP = sig ... end
```
```ocaml
val transform_global_app_uri : 
  (Eliom_content.Html.uri -> Eliom_content.Html.uri) ref
```
Function to modify the global\_app URI for some obscure reason

```ocaml
module App (_ : Eliom_registration_sigs.APP_PARAM) : APP
```
Functor for application creation. See [the chapter on applications](./../clientserver-applications.md) in the Eliom manual for details.

```ocaml
module type TMPL_PARAMS = sig ... end
```
```ocaml
module Eliom_tmpl
  (App : APP)
  (Tmpl_param : TMPL_PARAMS) : 
  Eliom_registration_sigs.S_with_create
    with type page = Tmpl_param.t
     and type options = appl_service_options
     and type return = Eliom_service.non_ocaml
     and type result = App.app_id application_content kind
```

#### Services returning HTML (or other TyXML) fragments

```ocaml
module Flow5 : 
  Eliom_registration_sigs.S_with_create
    with type page = Html_types.flow5 Eliom_content.Html.elt list
     and type options = unit
     and type return = Eliom_service.non_ocaml
     and type result = block_content kind
```
Eliom service registration and forms creation for fragment of HTML page.

### Untyped pages

```ocaml
module Html_text : 
  Eliom_registration_sigs.S_with_create
    with type page = string
     and type options = unit
     and type return = Eliom_service.non_ocaml
     and type result = browser_content kind
```
Eliom service registration and forms creation for untyped HTML page. The page content is a `string` that must contains valid HTML and the content type is always `text/html`.

```ocaml
module CssText : 
  Eliom_registration_sigs.S_with_create
    with type page = string
     and type options = int
     and type return = Eliom_service.non_ocaml
     and type result = browser_content kind
```
Eliom service registration for services that returns CSS. The page content is a `string` that must contains valid CSS and the content type is always `text/css`. The option is the optional "Cache-policy: max-age" header value to be sent.

### Other kinds of services

```ocaml
module Action : 
  Eliom_registration_sigs.S_with_create
    with type return = Eliom_service.non_ocaml
     and type page = unit
     and type options = [ `Reload | `NoReload ]
     and type result = browser_content kind
```
Eliom service registration for services that only execute actions. See the Eliom manual for more information about [Actions outputs](./../server-outputs.md#actions).

```ocaml
module Unit : 
  Eliom_registration_sigs.S_with_create
    with type page = unit
     and type options = unit
     and type return = Eliom_service.non_ocaml
     and type result = browser_content kind
```
Similar to `Actions` with ``NoReload` option.

```ocaml
type _ redirection = 
  | Redirection : (unit,
                  unit,
                  Eliom_service.get,
                  _,
                  _,
                  _,
                  _,
                  [ `WithoutSuffix ],
                  unit,
                  unit,
                  'a)
                  Eliom_service.t -> 'a redirection
```
Auxiliarry type to hide non-interesting type parameters

```ocaml
module Redirection : sig ... end
```
Eliom service registration for services that returns a redirections towards another service. See the Eliom manual for more information about [Redirections outputs](./../server-outputs.md#redirections).

```ocaml
module String_redirection : 
  Eliom_registration_sigs.S_with_create
    with type page = Eliom_lib.Url.uri
     and type options =
           [ `MovedPermanently
           | `Found
           | `SeeOther
           | `NotNodifed
           | `UseProxy
           | `TemporaryRedirect ]
     and type return = Eliom_service.non_ocaml
     and type result = browser_content kind
```
Eliom service registration for services that returns a redirections towards a string-URL. See the Eliom manual for more information about [Redirections outputs](./../server-outputs.md#redirections). The URL given must be an absolute URI.

```ocaml
module File : sig ... end
```
Eliom service registration for services that returns file contents. The value returned by service handlers is the name of the file to send. See the Eliom manual for more information on [how to send files with Eliom](./../server-outputs.md#eliomfiles). The option is the optional "Cache-policy: max-age" header value to be sent.

```ocaml
module File_ct : sig ... end
```
Same as file but makes possible to specify the content type for each file. The value returned by service handlers is a pair `(file_name, content_type)`.

```ocaml
module Ocaml : 
  Eliom_registration_sigs.S_poly_with_create_with_send
    with type 'a page = 'a
     and type options = unit
     and type 'a return = 'a Eliom_service.ocaml
     and type 'a result = 'a ocaml_content kind
```
Eliom service registration for services that send marshalled OCaml values.

```ocaml
module Any : 
  Eliom_registration_sigs.S_poly_with_create_with_send
    with type 'a page = 'a kind
     and type options = unit
     and type 'a return = Eliom_service.non_ocaml
     and type 'a result = 'a kind
```
Eliom service registration for services that choose dynamically what they want to send. The content is created using for example [`Html.send`](./Eliom_registration-Html.md#val-send) or [`String.send`](./Eliom_registration-String.md#val-send) functions. See the Eliom manual for more information about [services that choose dynamically what they want to send](./../server-outputs.md#any)

```ocaml
val appl_self_redirect : 
  ('page -> [< 'a application_content | browser_content ] kind Lwt.t) ->
  'page ->
  'appl application_content kind Lwt.t
```
The function `appl_self_redirect send page` is an helper function required for defining [`Any`](./Eliom_registration-Any.md) service usable inside an Eliom application ([`App`](./Eliom_registration-App.md)). It allows casting an Eliom senders that do not returns [`application_content`](./#type-application_content) (like [`File.send`](./Eliom_registration-File.md#val-send), [`String.send`](./Eliom_registration-String.md#val-send), ...) into a senders returns [`application_content`](./#type-application_content).

When the service is called from an Eliom application, this is implemented with half-redirection (a redirection that leaves the application). Hence, the service may be called two times in a row and you should not use this function for services that use POST parameters.

```ocaml
module String : 
  Eliom_registration_sigs.S_with_create
    with type page = string * string
     and type options = int
     and type return = Eliom_service.non_ocaml
     and type result = unknown_content kind
```
Eliom service registration for services that returns "byte"-string contents. The page content is a pair `(raw_content, content_type)`. See also `Streamlist` for another kind of service that returns "byte" contents. The option is the optional "Cache-policy: max-age" header value to be sent.

### Customizing registration

```ocaml
module Customize
  (R : Eliom_registration_sigs.S_with_create)
  (T : sig ... end) : 
  Eliom_registration_sigs.S_with_create
    with type options = R.options
     and type return = R.return
     and type page = T.page
     and type result = R.result
```
The `Customize` functor allows specialization of service registration functions by customizing the page type. See the [Eliom tutorial](https://ocsigen.org/tuto/latest/interaction.html) for example.

### Using your own error pages

```ocaml
val set_exn_handler : (exn -> browser_content kind Lwt.t) -> unit
```
The `set_exn_handler handler` allows redefinition of error pages: `404` or any exception during page generation.

Note that you should not catch every exception here since some Eliom mechanisms are done using exceptions, like redirections. Do not catch exception defined in Eliom except [`Eliom_common.Eliom_404`](./Eliom_common.md#exception-Eliom_404), [`Eliom_common.Eliom_Wrong_parameter`](./Eliom_common.md#exception-Eliom_Wrong_parameter) [`Eliom_common.Eliom_Typing_Error`](./Eliom_common.md#exception-Eliom_Typing_Error).

*Warning: This functions must be called when the site information is available, that is, either during a request or during the initialisation phase of the site. Otherwise, it will raise the exception [`Eliom_common.Eliom_site_information_not_available`](./Eliom_common.md#exception-Eliom_site_information_not_available). If you are using static linking, you must delay the call to this function until the configuration file is read, using [`Eliom_service.register_eliom_module`](./Eliom_service.md#val-register_eliom_module). Otherwise you will also get this exception.*

### Unsafe cast of contents

```ocaml
val cast_unknown_content_kind : unknown_content kind -> 'a kind
```
If you know that the content you generated using `Text.send` or `Streamlist.send` is the same as some other kind, you can cast it with `cast_unknown_content_kind` for use with `Any` module.

```ocaml
val cast_http_result : Ocsigen_response.t -> 'a kind
```
`cast_http_result` should only be used to register new output modules
