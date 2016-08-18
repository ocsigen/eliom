(* Ocsigen
 * http://www.ocsigen.org
 * Module eliom_client.ml
 * Copyright (C) 2010 Vincent Balat
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

(** Call server side services and change the current page. *)

open Eliom_lib

(** {2 Mobile applications} *)
(** Call this function if you want to be able to run your client side app
    before doing the first request, that is, when the client side app
    is not sent by the server. This may be the case for example if you are
    developing a mobile app. The parameters correspond to the base URL of the
    server side of your application.

    Alternatively, and to make sure it is done early enough, define
    a JS variable called [__eliom_server] at the beginning of your
    html file, containing the full URL of your server.
*)
val init_client_app :
  app_name:string ->
  ?ssl:bool ->
  hostname:string ->
  ?port:int ->
  full_path:Eliom_lib.Url.path -> unit -> unit

(** Returns whether the application is sent by a server or started on
    client side. If called on server side, always returns [false].
    Otherwise, it tests the presence of JS variables added automatically by
    Eliom when the page is sent by a server.
    Example:
    {[ if not (Eliom_client.is_client_app ())
 then Eliom_client.init_client_app ... ]}
*)
val is_client_app : unit -> bool


(** {2 Calling services} *)
(** Call a service and change the current page.
    If the service belongs to the same application,
    the client side program is not stopped, and only
    the content (not the container) is reloaded.
    If the [replace] flag is set, the new page will replace the
    current page in the browser history if the service belongs to
    the same application. The last two parameters are respectively the GET and
    POST parameters to send to the service.
*)
val change_page :
  ?replace:bool ->
  ?absolute:bool ->
  ?absolute_path:bool ->
  ?https:bool ->
  service:
    ('a, 'b, _, _, _, _, _, _, _, _, Eliom_service.non_ocaml)
      Eliom_service.t ->
  ?hostname:string ->
  ?port:int ->
  ?fragment:string ->
  ?keep_nl_params:[ `All | `None | `Persistent ] ->
  ?nl_params:Eliom_parameter.nl_params_set ->
  ?keep_get_na_params:bool ->
  ?progress:(int -> int -> unit) ->
  ?upload_progress:(int -> int -> unit) ->
  ?override_mime_type:string ->
  'a -> 'b -> unit Lwt.t

(** Call a server side service that return an OCaml value.

    If the service raises an exception, the call to the
    [call_ocaml_service] raises an exception {% <<a_api|exception
    Exception_on_server>> %} whose argument describes the server-side
    exception.
    (NB that we cannot send the original exception as-it, because
    OCaml permits the marshalling of exceptions ...)
*)
val call_ocaml_service :
  ?absolute:bool ->
  ?absolute_path:bool ->
  ?https:bool ->
  service:
    ('a, 'b, _, _, _, _, _, _, _, _, 'return Eliom_service.ocaml)
      Eliom_service.t ->
  ?hostname:string ->
  ?port:int ->
  ?fragment:string ->
  ?keep_nl_params:[ `All | `None | `Persistent ] ->
  ?nl_params:Eliom_parameter.nl_params_set ->
  ?keep_get_na_params:bool ->
  ?progress:(int -> int -> unit) ->
  ?upload_progress:(int -> int -> unit) ->
  ?override_mime_type:string ->
  'a -> 'b -> 'return Lwt.t


(** Stop current program and load a new page.  Note that for string arguments,
    sole line feed or sole carriage return characters are substituted by the
    string ["\r\n"]. *)
val exit_to :
  ?absolute:bool ->
  ?absolute_path:bool ->
  ?https:bool ->
  service:
    ('a, 'b, _, _, _, _, _, _, _, _, Eliom_service.non_ocaml)
      Eliom_service.t ->
  ?hostname:string ->
  ?port:int ->
  ?fragment:string ->
  ?keep_nl_params:[ `All | `None | `Persistent ] ->
  ?nl_params:Eliom_parameter.nl_params_set ->
  ?keep_get_na_params:bool ->
  'a -> 'b -> unit

(** Loads an Eliom service in a window (cf. Javascript's [window.open]). *)
val window_open :
  window_name:Js.js_string Js.t ->
  ?window_features:Js.js_string Js.t ->
  ?absolute:bool ->
  ?absolute_path:bool ->
  ?https:bool ->
  service:
    ('a, unit, Eliom_service.get, _, _, _, _, _, _, unit, _)
      Eliom_service.t ->
  ?hostname:string ->
  ?port:int ->
  ?fragment:string ->
  ?keep_nl_params:[ `All | `None | `Persistent ] ->
  ?nl_params:Eliom_parameter.nl_params_set ->
  ?keep_get_na_params:bool ->
  'a -> Dom_html.window Js.t

(** Changes the URL, without doing a request.
    It takes a GET (co-)service as parameter and its parameters.
    If the [replace] flag is set, the current page is not saved
    in the history.
 *)
val change_url :
  ?replace:bool ->
  ?absolute:bool ->
  ?absolute_path:bool ->
  ?https:bool ->
  service:
    ('get, unit, Eliom_service.get,
     _, _, _, _, _, _, unit, _) Eliom_service.t ->
  ?hostname:string ->
  ?port:int ->
  ?fragment:string ->
  ?keep_nl_params:[ `All | `None | `Persistent ] ->
  ?nl_params:Eliom_parameter.nl_params_set ->
  'get -> unit

(** (low level) Call a server side service and return the content
    of the resulting HTTP frame as a string. *)
val call_service :
  ?absolute:bool ->
  ?absolute_path:bool ->
  ?https:bool ->
  service:('a, 'b, _, _, _, _, _, _, _, _, _) Eliom_service.t ->
  ?hostname:string ->
  ?port:int ->
  ?fragment:string ->
  ?keep_nl_params:[ `All | `None | `Persistent ] ->
  ?nl_params:Eliom_parameter.nl_params_set ->
  ?keep_get_na_params:bool ->
  ?progress:(int -> int -> unit) ->
  ?upload_progress:(int -> int -> unit) ->
  ?override_mime_type:string ->
  'a -> 'b -> string Lwt.t

(** {2 Misc} *)

(** Registers some code to be executed after loading the client
    application, or after changing the page the next time.

    It complements as a toplevel expression in the client
    module with the side effect from client values while
    creating the response of a service: While the latter are executed
    each time the service has been called; the former is executed only
    once; but each at a time where the document is in place:

    {% <<code language="ocaml"|
    {shared{ open Eliom_lib }}
    {client{
      let () = alert "Once only during initialization of the client, \
                      i.e. before the document is available."
      let () =
        Eliom_client.onload
          (fun () -> alert "Once only when the document is put in place.")
    }}
    {server{
      let _ = My_app.register_service ~path ~get_params
        (fun () () ->
           ignore {unit{
             alert "Each time this service is called and the sent document \
                    is put in place."
           }};
           Lwt.return html
    }}
    >> %}

*)
val onload : (unit -> unit) -> unit

(** Returns a Lwt thread that waits until the next page is loaded. *)
val lwt_onload : unit -> unit Lwt.t

(** [onbeforeunload f] registers [f] as a handler to be called before
    changing the page the next time. If [f] returns [Some s], then we
    ask the user to confirm quitting. We try to use [s] in the
    confirmation pop-up. [None] means no confirmation needed.

    The callback [f] is sometimes trigerred by internal service calls,
    and sometimes by the browser [onbeforeunload] event. In the
    [onbeforeunload] case, the confirmation pop-up is managed by the
    browser. For Firefox, the string [s] returned by [f] is ignored:
    https://bugzilla.mozilla.org/show_bug.cgi?id=641509

    [onbeforeunload] can be used to register multiple callbacks. *)
val onbeforeunload : (unit -> string option) -> unit

(** [onunload f] registers [f] as a handler to be called before page
    change. The callback [f] is sometimes trigerred by internal
    service calls, and sometimes by the browser [onunload] event.
    [onunload] can be used to register multiple callbacks. *)
val onunload : (unit -> unit) -> unit

(** Wait for the initialization phase to terminate *)
val wait_load_end : unit -> unit Lwt.t

(** Returns the name of currently running Eliom application,
    defined while applying [Eliom_registration.App] functor. *)
val get_application_name : unit -> string

(** After this function is called, the document head is no
    longer changed on page change. *)
val persist_document_head : unit -> unit

(** {2 RPC / Server functions}

    See the {% <<a_manual chapter="clientserver-communication" fragment="rpc"|manual>> %}.*)

(** A [('a, 'b) server_function] provides transparently access to a
    server side function which has been created by {% <<a_api
    subproject="server"|Eliom_client.server_function>> %}.

    See also {% <<a_api subproject="server" text="the opaque server
    side representation"| type Eliom_client.server_function>> %}.

    The handling of exception on the server corresponds to that of
    <<a_api subproject="client"|val Eliom_client.call_ocaml_service>>.
*)
type ('a, +'b) server_function = 'a -> 'b Lwt.t

(** [server_function argument_type f] creates a value of type {%
    <<a_api | type Eliom_client.server_function>> %}. This allows
    to call [f] from the client. The first argument [argument_type] is
    an instance of [Deriving_Json] for the type of the argument. It is
    used to safely encode and decode the argument sent to the server.

    The optional parameters correspond directly to the optional
    parameters of {% <<a_api|val
    Eliom_registration.Ocaml.register_service >> %}.

    See also the {% <<a_manual chapter="clientserver-communication"
    fragment="rpc"|manual>> %}.

    Defining server functions in shared or client sections is possible only
    if you give them a name.
*)
val server_function :
  ?scope:[< Eliom_common.scope ] ->
  ?options:unit ->
  ?charset:string ->
  ?code:int ->
  ?content_type:string ->
  ?headers:Http_headers.t ->
  ?secure_session:bool ->
  name:string ->
  ?csrf_safe:bool ->
  ?csrf_scope:[< Eliom_common.user_scope ] ->
  ?csrf_secure:bool ->
  ?max_use:int ->
  ?timeout:float ->
  ?https:bool ->
  ?error_handler:((string * exn) list -> 'b Lwt.t) ->
  'a Deriving_Json.t -> unit -> ('a, 'b) server_function


(** [change_page_uri ?replace uri] identifies and calls the
    client-side service that implements [uri].

    We fallback to a server service call if the service is not
    registered on the client.

    If the [replace] flag is set to [true], the current page is not
    saved in the history. *)
val change_page_uri : ?replace:bool -> string -> unit Lwt.t

(**/**)

(** [change_page_unknown path get_params post_params] calls the
    service corresponding to [(path, get_params, post_params)]. It may
    throw [Eliom_common.Eliom_404] or
    [Eliom_common.Eliom_Wrong_parameter] if there is no appropriate
    service available. *)
val change_page_unknown :
  ?meth:[`Get | `Post | `Put | `Delete] ->
  ?hostname:string ->
  ?replace:bool ->
  string list ->
  (string * string) list ->
  (string * string) list ->
  unit Lwt.t

(* Documentation rather in eliom_client.ml *)

val init : unit -> unit

val reload_function : (unit -> unit -> unit Lwt.t) option ref

(** Lwt_log section for this module.
    Default level is [Lwt_log.Info].
    Use [Lwt_log.Section.set_level Eliom_client.log_section Lwt_log.Debug]
    to see debug messages.
*)
val log_section : Lwt_log.section

(** Is it a middle-click event? *)
val middleClick : Dom_html.mouseEvent Js.t -> bool

val set_content_local :
  ?offset:Eliommod_dom.position ->
  ?fragment:string -> Dom_html.element Js.t -> unit Lwt.t

val do_not_set_uri : bool ref

val change_page_after_action : unit -> unit Lwt.t

type client_form_handler = Dom_html.event Js.t -> bool Lwt.t
