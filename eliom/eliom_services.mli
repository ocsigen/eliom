(* Ocsigen
 * http://www.ocsigen.org
 * Module eliomservices.mli
 * Copyright (C) 2007 Vincent Balat
 * Laboratoire PPS - CNRS Université Paris Diderot
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


(** This module allows to define services. *)


open Ocsigen_extensions
open Eliom_parameters



(** This function may be used for services that cannot be interrupted
  (no cooperation point for threads). It is defined by
  [let sync f sp g p = Lwt.return (f sp g p)]
 *)
val sync : ('a -> 'b -> 'c -> 'd) -> 'a -> 'b -> 'c -> 'd Lwt.t


(** Type used for other cookies to set or unset.
    The float option is the timestamp for the expiration date.
    The strings are names and values.
 *)
type cookie =
  | Set of Ocsigen_lib.url_path option * float option * string * string * bool
  | Unset of Ocsigen_lib.url_path option * string

(** Conversion fonction from Eliom cookies to server cookies.
    If [?oldtable] is present, cookies are added to this table
 *)
val cookie_table_of_eliom_cookies :
    ?oldtable:Ocsigen_http_frame.cookieset ->
      sp:Eliom_sessions.server_params -> cookie list -> Ocsigen_http_frame.cookieset







(** {2 Types of services} *)

type suff = [ `WithSuffix | `WithoutSuffix ]

type servcoserv = [ `Service | `Coservice ]

type getpost = [ `Get | `Post ]
      (* `Post means that there is at least one post param
         (possibly only the state post param).
         `Get is for all the other cases.
       *)

type attached_service_kind =
    [ `Internal of servcoserv * getpost
    | `External]

type get_attached_service_kind =
    [ `Internal of servcoserv * [ `Get ]
    | `External ]

type post_attached_service_kind =
    [ `Internal of servcoserv * [ `Post ]
    | `External ]

type internal =
    [ `Internal of servcoserv * getpost ]

type registrable = [ `Registrable | `Unregistrable ]
(** You can call register function only on registrable services *)
(* Registrable means not pre-applied *)

type +'a a_s

type +'a na_s

type service_kind =
    [ `Attached of attached_service_kind a_s
    | `Nonattached of getpost na_s ]

type get_service_kind =
    [ `Attached of get_attached_service_kind a_s
    | `Nonattached of [ `Get ] na_s ]

type post_service_kind =
    [ `Attached of post_attached_service_kind a_s
    | `Nonattached of [ `Post ] na_s ]

type internal_service_kind =
    [ `Attached of internal a_s
    | `Nonattached of getpost na_s ]

type attached =
    [ `Attached of attached_service_kind a_s ]

type nonattached =
    [ `Nonattached of getpost na_s ]

type ('get,'post,+'kind,+'tipo,+'getnames,+'postnames,+'registr) service
(** Type of services.
    - [ 'get] is the type of GET parameters
    - [ 'post] is the type of POST parameters
    - [ 'kind] is a subtype of {!Eliom_services.service_kind} (attached or non-attached
      service, internal or external, GET only or with POST parameters)
    - [ 'tipo] is a phantom type stating the kind of parameters it uses
        (suffix or not)
    - [ 'getnames] is the type of GET parameters names
    - [ 'postnames] is the type of POST parameters names
    - [ 'registrable] is a phantom type, subtype of {!Eliom_services.registrable},
      telling if it is possible to register a handler on this service.
 *)



(***** Static dir and actions do not depend on the type of pages ******)

(** {2 Registration of named modules}

This functionality allows to register module initialization functions for Eliom modules
which will be executed when the corresponding module is initialized in [ocsigen.conf].

*)

val register_eliom_module : string -> (unit -> unit) -> unit
(**
  This function is used to specify the initialization function for Eliom modules
  linked dynamic or statically into the server.
  [register_eliom_module name f] registers the initialization function [f] for
  module [name].  The [f] function will be invoked when the module is
  initialized in the configuration file using [<eliom name="name"> ... </eliom>], which
  is equivalent to [<eliom module="name.cmo"> ... </eliom>] with the exception
  that it does not load the module using [Dynlink].
  *)

(** {2 Definitions of services} *)

(** {3 Main services} *)

val new_service :
  ?sp: Eliom_sessions.server_params ->
  ?https:bool ->
  path:Ocsigen_lib.url_path ->
  ?keep_nl_params:[ `All | `Persistent | `None ] ->
  get_params:('get, [< suff ] as 'tipo,'gn) params_type ->
  unit ->
  ('get,unit,
   [> `Attached of
      [> `Internal of [> `Service ] * [>`Get] ] a_s ],
   'tipo,'gn,
   unit, [> `Registrable ]) service
(** [new_service ~path:p ~get_params:pa ()] creates an {!Eliom_services.service} associated
   to the path [p], taking the GET parameters [pa].

   If [~https] is true, all links towards that service will use https.

   {e Warning: If you use this function after the initialisation phase,
   you must give the [~sp] parameter, otherwise it will raise the
   exception {!Eliom_common.Eliom_function_forbidden_outside_site_loading}.}
*)


val new_external_service :
  prefix: string ->
  path:Ocsigen_lib.url_path ->
  ?keep_nl_params:[ `All | `Persistent | `None ] ->
  get_params:('get, [< suff ] as 'tipo, 'gn) params_type ->
  post_params:('post, [ `WithoutSuffix ], 'pn) params_type ->
  unit ->
  ('get, 'post, [> `Attached of [> `External ] a_s ], 'tipo,
   'gn, 'pn, [> `Unregistrable ]) service
(** Creates an service for an external web site.
   Allows to creates links or forms towards other Web sites using
   Eliom's syntax.

   The parameter labelled [~path] is the URL path, and each element of
   the list will be URL-encoded.

   The parameter labelled [~prefix] contains all what you want to put before
   the path. It usually starts with "http://" plus
   the name of the server. The whole URL is constructed from the prefix,
   the path and parameters. The prefix is not encoded.
   An empty prefix can be used to make a link to another site of the same
   server.
 *)

val new_post_service :
  ?sp: Eliom_sessions.server_params ->
  ?https:bool ->
  fallback: ('get, unit,
             [`Attached of [`Internal of
                              ([ `Service | `Coservice ] as 'kind) * [`Get]] a_s ],
             [< suff] as 'tipo, 'gn, unit,
             [< `Registrable ]) service ->
  ?keep_nl_params:[ `All | `Persistent | `None ] ->
  post_params: ('post, [`WithoutSuffix], 'pn) params_type ->
  unit ->
  ('get, 'post, [> `Attached of
                   [> `Internal of 'kind * [> `Post]] a_s ],
   'tipo, 'gn, 'pn, [> `Registrable ]) service
(** Creates an service that takes POST parameters.
    [fallback] is the a service without POST parameters.
    You can't create an service with POST parameters
    if the same service does not exist without POST parameters.
    Thus, the user can't bookmark a page that does not exist.
 *)
(* fallback must be registrable! (= not preapplied) *)



(** {3 Attached coservices} *)

val new_coservice :
  ?name: string ->
  ?csrf_safe: bool ->
  ?max_use:int ->
  ?timeout:float ->
  ?https:bool ->
  fallback:
    (unit, unit, [ `Attached of [ `Internal of [ `Service ] * [`Get]] a_s ],
     [ `WithoutSuffix ] as 'tipo,
     unit, unit, [< registrable ]) service ->
  ?keep_nl_params:[ `All | `Persistent | `None ] ->
  get_params:
    ('get,[`WithoutSuffix],'gn) params_type ->
  unit ->
  ('get,unit,[> `Attached of
                [> `Internal of [> `Coservice] * [> `Get]] a_s ],
   'tipo, 'gn, unit,
   [> `Registrable ]) service
(** Creates a coservice. A coservice is another version of an
   already existing main service, where you can register another handler.
   The two versions are automatically distinguished using an extra parameter
   added automatically by Eliom.
   It allows to have several links towards the same page,
   that will behave differently, or to create services dedicated to one user.
   See the tutorial for more informations.
   Coservices can be named if the [?name] optional parameter
   is present or anonymous (in that case, a coservice number will be
   generated).

   The [~timeout] parameter specifies a timeout (in seconds)
   after which the coservice will disappear. This amount of time is
   computed from the creation or the last call to the service.

   The [~max_use] parameter specifies that the service can be used only
   a fixed number of times.

   If [~csrf_safe] is [true] (the default is [false]),
   it will create a "CSRF-safe" service. (In that case [~name] is ignored).
   It means that the registration of the service will not actually
   take place when [register] is called, but delayed and performed
   each time a form is created. This allows to protect against CSRF attacks,
   and should be use with a short timeout (and max_use).
   (And you should probably use POST coservices in that case).
 *)

val new_post_coservice :
  ?name: string ->
  ?csrf_safe: bool ->
  ?max_use:int ->
  ?timeout:float ->
  ?https:bool ->
  fallback: ('get, unit, [ `Attached of
                             [`Internal of [<`Service | `Coservice] * [`Get]] a_s ],
             [< suff ] as 'tipo,
             'gn, unit, [< `Registrable ]) service ->
  ?keep_nl_params:[ `All | `Persistent | `None ] ->
  post_params: ('post, [`WithoutSuffix], 'pn) params_type ->
  unit ->
  ('get, 'post,
   [> `Attached of
      [> `Internal of [> `Coservice] * [> `Post]] a_s ],
   'tipo, 'gn, 'pn, [> `Registrable ]) service
(** Creates a coservice with POST parameters *)

(** {3 Non attached coservices} *)

val new_coservice' :
  ?name:string ->
  ?csrf_safe: bool ->
  ?max_use:int ->
  ?timeout:float ->
  ?https:bool ->
  ?keep_nl_params:[ `All | `Persistent | `None ] ->
  get_params:
    ('get, [`WithoutSuffix], 'gn) params_type ->
  unit ->
  ('get, unit, [> `Nonattached of [> `Get] na_s ],
   [`WithoutSuffix], 'gn, unit, [> `Registrable ]) service
(** Creates a non-attached coservice, that is, services that do not
   correspond to a precise path in the URL.
   Links towards such services will not change the URL,
   just add extra parameters.
   Non-attached coservices can be named if the [?name] optional parameter
   is present or anonymous (in that case, a coservice number will be
   generated).
   See the tutorial for more informations.
 *)

val new_post_coservice' :
  ?name:string ->
  ?csrf_safe: bool ->
  ?max_use:int ->
  ?timeout:float ->
  ?https:bool ->
  ?keep_nl_params:[ `All | `Persistent | `None ] ->
  ?keep_get_na_params:bool ->
  post_params: ('post, [`WithoutSuffix], 'pn) params_type ->
  unit ->
  (unit, 'post,
   [> `Nonattached of [> `Post ] na_s ],
   [ `WithoutSuffix ], unit, 'pn, [> `Registrable ]) service
(** Creates a non attached coservice with POST parameters.
    If the optional parameter [~keep_get_na_params] is [false],
    GET non-attached parameters won't be kept in the URL (if any) when you
    create a POST form to this coservice.
    Default is [true].
    See also {!Eliom_mkforms.ELIOMFORMSIG.post_form}.
*)

(*
val new_get_post_coservice' :
    ?max_use:int ->
    ?timeout:float ->
  ?https:bool ->
   fallback: ('get, unit, [`Nonattached of [`Get] na_s ],
   [< suff ] as 'tipo,
   'gn, unit, [< `Registrable ]) service ->
   post_params: ('post,[`WithoutSuffix],'pn) params_type ->
   unit ->
   ('get, 'post,
   [> `Nonattached of [> `Post] na_s ],
   'tipo,'gn,'pn, [> `Registrable ]) service
(* * Creates a non-attached coservice with GET and POST parameters. The fallback is a non-attached coservice with GET parameters. *)
*)


(** {2 Misc} *)

val static_dir :
  sp:Eliom_sessions.server_params ->
  (string list, unit, [> `Attached of
                         [> `Internal of [> `Service ] * [> `Get] ] a_s ],
   [ `WithSuffix ],
   [ `One of string list ] param_name, unit, [> `Unregistrable ])
    service
(** A predefined service
   that correponds to the directory where static pages are.
   This directory is chosen in the config file (ocsigen.conf).
   This service takes the name of the static file as a parameter
   (a string list, slash separated).
 *)

val https_static_dir :
  sp:Eliom_sessions.server_params ->
  (string list, unit, [> `Attached of
                         [> `Internal of [> `Service ] * [> `Get] ] a_s ],
   [ `WithSuffix ],
   [ `One of string list ] param_name, unit, [> `Unregistrable ])
    service
(** The same, but forcing https *)

val static_dir_with_params :
  sp:Eliom_sessions.server_params ->
  ?keep_nl_params:[ `All | `Persistent | `None ] ->
  get_params:('a, [`WithoutSuffix], 'an) params_type ->
  unit ->
  ((string list * 'a), unit,
   [> `Attached of
      [> `Internal of [> `Service ] * [> `Get] ] a_s ],
   [ `WithSuffix ],
   [ `One of string list ] param_name *'an, unit, [> `Unregistrable ])
    service
(** Like [static_dir], but allows to put GET parameters *)

val https_static_dir_with_params :
  sp:Eliom_sessions.server_params ->
  ?keep_nl_params:[ `All | `Persistent | `None ] ->
  get_params:('a, [`WithoutSuffix], 'an) params_type ->
  unit ->
  ((string list * 'a), unit,
   [> `Attached of
      [> `Internal of [> `Service ] * [> `Get] ] a_s ],
   [ `WithSuffix ],
   [ `One of string list ] param_name *'an, unit, [> `Unregistrable ])
    service
(** The same, but forcing https *)




val void_coservice' :
  (unit, unit, [> `Nonattached of 'a na_s ],
   [ `WithoutSuffix ],
   unit, unit, [> `Unregistrable ])
  service
(** A predefined non-attached action with special behaviour:
    it has no parameter at all, even non-attached parameters.
    Use it if you want to make a link to the current page without non-attached
    parameters.
    It is almost equivalent to a POST non-attached service without POST
    parameters, on which you register an action that does nothing,
    but you can use it with <a> links, not only forms.
    It does not keep non attached GET parameters.
 *)

val https_void_coservice' :
  (unit, unit, [> `Nonattached of 'a na_s ],
   [ `WithoutSuffix ],
   unit, unit, [> `Unregistrable ])
  service
(** The same, but forcing https. *)

val void_hidden_coservice' :
  (unit, unit, [> `Nonattached of 'a na_s ],
   [ `WithoutSuffix ],
   unit, unit, [> `Unregistrable ])
  service
(** Same as [void_coservice'] but keeps non attached GET parameters.
 *)

val https_void_hidden_coservice' :
  (unit, unit, [> `Nonattached of 'a na_s ],
   [ `WithoutSuffix ],
   unit, unit, [> `Unregistrable ])
  service
(** The same, but forcing https. *)


val preapply :
    service:('a, 'b, [> `Attached of 'd a_s ] as 'c,
     [< suff ], 'e, 'f, 'g)
    service ->
      'a ->
        (unit, 'b, 'c,
         [ `WithoutSuffix ], unit, 'f, [> `Unregistrable ]) service
(** creates a new service by preapplying a service to GET parameters.
   It is not possible to register a handler on an preapplied service.
   Preapplied services may be used in links or as fallbacks for coservices
 *)

val add_non_localized_get_parameters :
  params:('p, [ `WithoutSuffix ], 'pn) non_localized_params ->
  service:('a, 'b, 'c, 'd, 'e, 'f, 'g) service ->
  ('a * 'p, 'b, 'c, 'd, 'e * 'pn, 'f, 'g) service
(** Adds non localized GET parameters to a service *)

val add_non_localized_post_parameters :
  params:('p, [ `WithoutSuffix ], 'pn) non_localized_params ->
  service:('a, 'b, 'c, 'd, 'e, 'f, 'g) service ->
  ('a, 'b * 'p, 'c, 'd, 'e, 'f * 'pn, 'g) service
(** Adds non localized POST parameters to a service *)


(** {2 Using your own error pages} *)


(** Allows to use your own error pages
   (404, or any exception during page generation).

    {e Warning: If you use this function after the initialisation phase,
    you must give the [~sp] parameter, otherwise it will raise the
    exception {!Eliom_common.Eliom_function_forbidden_outside_site_loading}.}
 *)
val set_exn_handler :
  ?sp:Eliom_sessions.server_params ->
  (Eliom_sessions.server_params -> exn -> Ocsigen_http_frame.result Lwt.t) ->
  unit






(**/**)
val get_kind_ : ('a, 'b, 'c, 'd, 'e, 'f, 'g) service -> 'c
val get_pre_applied_parameters_ : ('a, 'b, 'c, 'd, 'e, 'f, 'g) service ->
  (string * string) list Ocsigen_lib.String_Table.t *
  (string * string) list
val get_get_params_type_ : ('a, 'b, 'c, 'd, 'e, 'f, 'g) service ->
  ('a, 'd, 'e) Eliom_parameters.params_type
val get_post_params_type_ : ('a, 'b, 'c, 'd, 'e, 'f, 'g) service ->
  ('b, [ `WithoutSuffix ], 'f) Eliom_parameters.params_type
val get_att_kind_ : 'a a_s -> 'a
val get_sub_path_ : 'a a_s -> Ocsigen_lib.url_path
val get_full_path_ : 'a a_s -> Ocsigen_lib.url_path
val get_prefix_ : 'a a_s -> string
val get_get_name_ : 'a a_s -> Eliom_common.att_key_serv
val get_post_name_ : 'a a_s -> Eliom_common.att_key_serv
val get_redirect_suffix_ : 'a a_s -> bool
val get_na_name_ : 'a na_s -> Eliom_common.na_key_serv
val get_na_kind_ : 'a na_s -> [ `Get | `Post of bool ]
val get_max_use_ : ('a, 'b, 'c, 'd, 'e, 'f, 'g) service -> int option
val get_timeout_ : ('a, 'b, 'c, 'd, 'e, 'f, 'g) service -> float option
val get_https : ('a, 'b, 'c, 'd, 'e, 'f, 'g) service -> bool
(* val reconstruct_absolute_url_path : Ocsigen_lib.url_path -> Ocsigen_lib.url_path -> Ocsigen_lib.url_path option -> string
val reconstruct_relative_url_path : Ocsigen_lib.url_path -> Ocsigen_lib.url_path -> Ocsigen_lib.url_path option -> string
*)
val escookiel_of_eccookiel : Eliom_common.cookie list -> cookie list
val eccookiel_of_escookiel : cookie list -> Eliom_common.cookie list
val keep_nl_params : ('a, 'b, 'c, 'd, 'e, 'f, 'g) service -> 
  [ `All | `Persistent | `None ]

val change_get_num :
  ('a, 'b, 'c, 'd, 'e, 'f, 'g) service ->
  'h a_s ->
  Eliom_common.att_key_serv ->
  ('a, 'b, [> `Attached of 'h a_s ], 'd, 'e, 'f, 'i) service


val new_state : unit -> string

(*****************************************************************************)

val register_delayed_get_or_na_coservice :
  sp:Eliom_sessions.server_params -> 
  ('a, 'b, 'c, 'd, 'e, 'f, 'g) service -> string

val register_delayed_post_coservice :
  sp:Eliom_sessions.server_params -> 
  ('a, 'b, 'c, 'd, 'e, 'f, 'g) service -> Eliom_common.att_key_serv -> string

val set_delayed_get_or_na_registration_function :
  Eliom_common.tables ->
  ('a, 'b, 'c, 'd, 'e, 'f, 'g) service -> 
  (sp:Eliom_common.server_params -> string) -> unit

val set_delayed_post_registration_function :
  Eliom_common.tables ->
  ('a, 'b, 'c, 'd, 'e, 'f, 'g) service -> 
  (sp:Eliom_common.server_params -> Eliom_common.att_key_serv -> string) -> 
  unit

