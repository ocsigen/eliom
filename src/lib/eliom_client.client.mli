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

open Js_of_ocaml
open Eliom_lib

val lock_request_handling : unit -> unit
val unlock_request_handling : unit -> unit

(** {2 Mobile applications} *)

val init_client_app :
   app_name:string
  -> ?ssl:bool
  -> hostname:string
  -> ?port:int
  -> site_dir:Eliom_lib.Url.path
  -> unit
  -> unit
(** Call this function if you want to be able to run your client side
    app before doing the first request, that is, when the client side
    app is not sent by the server. This may be the case for example if
    you are developing a mobile app. The parameters correspond to the
    base URL of the server side of your application.

    Alternatively, and to make sure it is done early enough, define
    JS variables called [__eliom_server] and [__eliom_app_name]
    at the beginning of your html
    file, containing the full URL of your server and Eliom app name.

    [site_dir] (if given) specifies the path that the application runs
    under. It should correspond to the <site> tag of your server
    configuration. Calls to server functions use this path. *)

val is_client_app : unit -> bool
(** Returns whether the application is sent by a server or started on
    client side. If called on server side, always returns [false].
    Otherwise, it tests the presence of JS variables added automatically by
    Eliom when the page is sent by a server.
    Example:
    {[ if not (Eliom_client.is_client_app ())
 then Eliom_client.init_client_app ... ]}
*)

(** {2 Calling services} *)

val change_page :
   ?ignore_client_fun:bool
  -> ?replace:bool
  -> ?window_name:string
  -> ?window_features:string
  -> ?absolute:bool
  -> ?absolute_path:bool
  -> ?https:bool
  -> service:
       ('a, 'b, _, _, _, _, _, _, _, _, Eliom_service.non_ocaml) Eliom_service.t
  -> ?hostname:string
  -> ?port:int
  -> ?fragment:string
  -> ?keep_nl_params:[`All | `None | `Persistent]
  -> ?nl_params:Eliom_parameter.nl_params_set
  -> ?keep_get_na_params:bool
  -> ?progress:(int -> int -> unit)
  -> ?upload_progress:(int -> int -> unit)
  -> ?override_mime_type:string
  -> 'a
  -> 'b
  -> unit Lwt.t
(** Call a service and change the current page.  If the service
    belongs to the same application, the client side program is not
    stopped, and only the content (not the container) is reloaded.  If
    the [replace] flag is set, the new page will replace the current
    page in the browser history if the service belongs to the same
    application. The last two parameters are respectively the GET and
    POST parameters to send to the service.
    If [window_name] is provided and not "_self" (for example ["_blank"]),
    will behave as [exit_to].
*)

val call_ocaml_service :
   ?absolute:bool
  -> ?absolute_path:bool
  -> ?https:bool
  -> service:
       ( 'a
         , 'b
         , _
         , _
         , _
         , _
         , _
         , _
         , _
         , _
         , 'return Eliom_service.ocaml )
         Eliom_service.t
  -> ?hostname:string
  -> ?port:int
  -> ?fragment:string
  -> ?keep_nl_params:[`All | `None | `Persistent]
  -> ?nl_params:Eliom_parameter.nl_params_set
  -> ?keep_get_na_params:bool
  -> ?progress:(int -> int -> unit)
  -> ?upload_progress:(int -> int -> unit)
  -> ?override_mime_type:string
  -> 'a
  -> 'b
  -> 'return Lwt.t
(** Call a server side service that return an OCaml value.

    If the service raises an exception, the call to the
    [call_ocaml_service] raises an exception {% <<a_api|exception
    Exception_on_server>> %} whose argument describes the server-side
    exception.
    (NB that we cannot send the original exception as-it, because
    OCaml permits the marshalling of exceptions ...)
*)

val exit_to :
   ?window_name:string
  -> ?window_features:string
  -> ?absolute:bool
  -> ?absolute_path:bool
  -> ?https:bool
  -> service:
       ('a, 'b, _, _, _, _, _, _, _, _, Eliom_service.non_ocaml) Eliom_service.t
  -> ?hostname:string
  -> ?port:int
  -> ?fragment:string
  -> ?keep_nl_params:[`All | `None | `Persistent]
  -> ?nl_params:Eliom_parameter.nl_params_set
  -> ?keep_get_na_params:bool
  -> 'a
  -> 'b
  -> unit
(** Stop current program and load a new page.  Note that for string arguments,
    sole line feed or sole carriage return characters are substituted by the
    string ["\r\n"].
    If [window_name] is specified (for example ["_blank"]), open in new window
    by calling Javascript function [window.open].
    In that case, you can add [window_features] parameter
    (as in Javascript function [window.open]).
    Warning: opening in other window may not work on mobile apps
    if [window.open] is not implemented.
*)

val window_open :
   window_name:Js.js_string Js.t
  -> ?window_features:Js.js_string Js.t
  -> ?absolute:bool
  -> ?absolute_path:bool
  -> ?https:bool
  -> service:
       ('a, unit, Eliom_service.get, _, _, _, _, _, _, unit, _) Eliom_service.t
  -> ?hostname:string
  -> ?port:int
  -> ?fragment:string
  -> ?keep_nl_params:[`All | `None | `Persistent]
  -> ?nl_params:Eliom_parameter.nl_params_set
  -> ?keep_get_na_params:bool
  -> 'a
  -> Dom_html.window Js.t Js.opt
(** Loads an Eliom service in a window (cf. Javascript's [window.open]).
*)

val change_url :
   ?replace:bool
  -> ?absolute:bool
  -> ?absolute_path:bool
  -> ?https:bool
  -> service:
       ( 'get
         , unit
         , Eliom_service.get
         , _
         , _
         , _
         , _
         , _
         , _
         , unit
         , _ )
         Eliom_service.t
  -> ?hostname:string
  -> ?port:int
  -> ?fragment:string
  -> ?keep_nl_params:[`All | `None | `Persistent]
  -> ?nl_params:Eliom_parameter.nl_params_set
  -> 'get
  -> unit
(** Changes the URL, without doing a request.
    It takes a GET (co-)service as parameter and its parameters.
    If the [replace] flag is set, the current page is not saved
    in the history.
 *)

val call_service :
   ?absolute:bool
  -> ?absolute_path:bool
  -> ?https:bool
  -> service:('a, 'b, _, _, _, _, _, _, _, _, _) Eliom_service.t
  -> ?hostname:string
  -> ?port:int
  -> ?fragment:string
  -> ?keep_nl_params:[`All | `None | `Persistent]
  -> ?nl_params:Eliom_parameter.nl_params_set
  -> ?keep_get_na_params:bool
  -> ?progress:(int -> int -> unit)
  -> ?upload_progress:(int -> int -> unit)
  -> ?override_mime_type:string
  -> 'a
  -> 'b
  -> string Lwt.t
(** (low level) Call a server side service and return the content
    of the resulting HTTP frame as a string. *)

(** {2 Misc} *)

val onload : (unit -> unit) -> unit
(** Registers some code to be executed after loading the client
    application, or after changing the page the next time.

    It complements as a toplevel expression in the client
    module with the side effect from client values while
    creating the response of a service: While the latter are executed
    each time the service has been called; the former is executed only
    once; but each at a time where the document is in place:

    Beware of using [onload] when using the DOM caching functionality,
    i.e. [push_history_dom]. When switching to a cached page (e.g. by going
    back) the onload event is not triggered (as the page is not loaded). To
    avoid this problem rely on [Page_status.onactive] which is triggered for
    freshly generated pages as well as pages served from the DOM cache.

    {% <<code language="ocaml"|
    [%%shared open Eliom_lib]
    [%%client
      let () = alert "Once only during initialization of the client, \
                      i.e. before the document is available."
      let () =
        Eliom_client.onload
          (fun () -> alert "Once only when the document is put in place.")
    ]
    [%%server
      let _ = My_app.register_service ~path ~get_params
        (fun () () ->
           ignore {unit{
             alert "Each time this service is called and the sent document \
                    is put in place."
           }};
           Lwt.return html
    ]
    >> %}

*)

val lwt_onload : unit -> unit Lwt.t
(** Returns a Lwt thread that waits until the next page is loaded. *)

type changepage_event =
  { in_cache : bool
  ; origin_uri : string
  ; target_uri : string
  ; origin_id : int
  ; target_id : int option }
(** [changepage_event] is a record of some parameters related to
    page changes. [in_cache] is true if the dom of the page is cached by
    [push_history_dom].
    [origin_uri] is the uri of the current page and [target_uri]
    is the uri of the next page. [origin_id] is the state_id of
    the current page and [target_id] is the state_id of the next page.
    [target_id] is not [None] if and only if the onchangepage event
    takes place during a navigation in history. *)

val onchangepage : (changepage_event -> unit Lwt.t) -> unit
(** Run some code *before* the next page change, that is, before each
    call to a page-producing service handler.
    Just like onpreload, handlers registered with onchangepage only
    apply to the next page change. *)

module Page_status : sig
  (** a page can be in one of the following states:
      - [Generating]: page is currently being generated and not yet instated as
                      the active page
      - [Active]: page is currently being displayed
      - [Cached]: page is in the browser history with its DOM stashed in the cache
      - [Dead]: page is in the browser history without its DOM being cached *)
  type t = Generating | Active | Cached | Dead

  val signal : unit -> t React.S.t
  (** retrieves a react signal for the status of the current page; note that the
      `current page' is not necessarily the page currently being displayed, but
      rather the page in whose context the current code is executed. *)

  (** convenience functions for retrieving a react event for the current page
      that is triggered whenever it reaches the respective status *)
  module Events : sig
    val active : unit -> unit React.E.t
    val cached : unit -> unit React.E.t
    val dead : unit -> unit React.E.t

    val inactive : unit -> unit React.E.t
    (** [inactive] occurs when the [Active] state is left ([Cached] or [Dead]) *)
  end

  val onactive :
     ?now:bool
    -> ?once:bool
    -> ?stop:unit React.E.t
    -> (unit -> unit)
    -> unit
  (** [onactive] is convenience function that attaches a handler to
      [Events.active], which behaves exactly like [fun f -> React.E.map f
      Events.active].
      If [now] is [true] (default) and the page is currently active the function
      is also invoked right away. This is useful to ensure that the function
      is invoked also on server-generated pages which are active right from
      the start and thus have no transition to the active state.
      If [once] is [true] ([false] by default) the action is executed only once.
      By [stop] one can supply an event that will deinstall the handler when
      triggered.

      Typical use cases for this function are processes that need to run
      continually while a page is being viewed. Such processes (including event
      listeners of [Dom_html.window]) are killed on a page change and not
      automatically restored with the DOM (contrary to event listeners attached
      to DOM elements). *)

  val oncached : ?once:bool -> ?stop:unit React.E.t -> (unit -> unit) -> unit
  val ondead : ?stop:unit React.E.t -> (unit -> unit) -> unit
  val oninactive : ?once:bool -> ?stop:unit React.E.t -> (unit -> unit) -> unit

  val while_active :
     ?now:bool
    -> ?stop:unit React.E.t
    -> (unit -> unit Lwt.t)
    -> unit
  (** [while_active] initiates an action as [onactive] but cancels it whenever
      the page is not active anymore. *)
end

val onbeforeunload : (unit -> string option) -> unit
(** [onbeforeunload f] registers [f] as a handler to be called before
    changing the page the next time. If [f] returns [Some s], then we
    ask the user to confirm quitting. We try to use [s] in the
    confirmation pop-up. [None] means no confirmation needed.

    The callback [f] is sometimes triggered by internal service calls,
    and sometimes by the browser [onbeforeunload] event. In the
    [onbeforeunload] case, the confirmation pop-up is managed by the
    browser. For Firefox, the string [s] returned by [f] is ignored:
    https://bugzilla.mozilla.org/show_bug.cgi?id=641509

    [onbeforeunload] can be used to register multiple callbacks. *)

val onunload : (unit -> unit) -> unit
(** [onunload f] registers [f] as a handler to be called before page
    change. The callback [f] is sometimes triggered by internal
    service calls, and sometimes by the browser [onunload] event.
    [onunload] can be used to register multiple callbacks. *)

val wait_load_end : unit -> unit Lwt.t
(** Wait for the initialization phase to terminate *)

val get_application_name : unit -> string
(** Returns the name of currently running Eliom application,
    defined while applying [Eliom_registration.App] functor. *)

val persist_document_head : unit -> unit
(** After this function is called, the document head is no
    longer changed on page change. *)

(** {2 RPC / Server functions}

    See the {% <<a_manual chapter="clientserver-communication" fragment="rpc"|manual>> %}.*)

type ('a, +'b) server_function = 'a -> 'b Lwt.t
(** A [('a, 'b) server_function] provides transparently access to a
    server side function which has been created by {% <<a_api
    subproject="server"| val Eliom_client.server_function>> %}.

    See also {% <<a_api subproject="server" text="the opaque server
    side representation"| type Eliom_client.server_function>> %}.

    The handling of exception on the server corresponds to that of
    <<a_api subproject="client"|val Eliom_client.call_ocaml_service>>.
*)

val change_page_uri : ?replace:bool -> string -> unit Lwt.t
(** [change_page_uri ?replace uri] identifies and calls the
    client-side service that implements [uri].

    We fallback to a server service call if the service is not
    registered on the client.

    If the [replace] flag is set to [true], the current page is not
    saved in the history. *)

val set_client_html_file : string -> unit
(** Set the name of the HTML file loading our client app. The default
    is "eliom.html". A wrong value will not allow the app to
    initialize itself correctly. *)

(**/**)

val change_page_unknown :
   ?meth:[`Get | `Post | `Put | `Delete]
  -> ?hostname:string
  -> ?replace:bool
  -> string list
  -> (string * string) list
  -> (string * string) list
  -> unit Lwt.t
(** [change_page_unknown path get_params post_params] calls the
    service corresponding to [(path, get_params, post_params)]. It may
    throw [Eliom_common.Eliom_404] or
    [Eliom_common.Eliom_Wrong_parameter] if there is no appropriate
    service available. *)

(* Documentation rather in eliom_client.ml *)

val init : unit -> unit
val set_reload_function : (unit -> unit -> Eliom_service.result Lwt.t) -> unit

module History : sig
  val past : unit -> string list
  (** get the URLs of the last pages visited (within the app) in reverse temporal order. *)

  (*tmp*)

  val future : unit -> string list
  (** get the URLs of the pages visited before having navigated backwards in history. *)
  (*tmp*)
end

val push_history_dom : unit -> unit
(** [push_history_dom] stores the document/body of the current page so
    that the next time when we encounter the page while navigating through the
    history, the DOM will be recovered from the cache instead of recharging or
    regenerating the page. Also the original scroll position is restored.

    You can define a limit on the number of stored DOMs using
    [set_max_dist_history_doms].

    A typical use case of this function is storing the dom when loading
    a page. i.e.
    {% <<code language="ocaml"|
    let%shared service_handler =
        fun () () ->
            ignore [%client (Eliom_client.onload Eliom_client.push_history_dom : unit)];
            Lwt.return
                [div [h1 [pcdata "Hello"];
                      p [pcdata "Blablablabla"] ]

    (* In case you want to cache all pages, you can register a global
       onload handler. *)
    let%client () =
        let rec register () =
            Eliom_client.onload (
                fun () ->
                    Eliom_client.push_history_dom ();
                    register ())
        in
        register ()
    >> %}
*)

(* [set_max_dist_history_doms (Some n)] limits the number of cached DOMs
   that are kept in memory. Thereby [n] is the maximum distance in history from
   the active page. Thus if for instance [n = 1] then only the DOMs for the
   previous and the next page are kept. *)
val set_max_dist_history_doms : int option -> unit

val section : Lwt_log.section
(** Lwt_log section for this module. *)

val middleClick : Dom_html.mouseEvent Js.t -> bool
(** Is it a middle-click event? *)

type client_form_handler = Dom_html.event Js.t -> bool Lwt.t

(** headers to add to each of Eliom's HTTP requests. *)
module Additional_headers : sig
  val add : string -> string -> unit
  val remove : string -> unit
end
