include "sigs/eliom_service.mli"

(** The function [external_service ~prefix ~path ~get_params ()]
    creates a service for an external web site, that will use GET
    method and requires [get_params] as parameters. This allows one to
    creates links or forms towards other Web sites using Eliom's
    syntax.

    The parameter labelled [~path] is the URL path. Each element of
    the list will be URL-encoded.

    The parameter labelled [~prefix] contains all what you want to put
    before the path. It usually starts with "http://" plus the name of
    the server. The prefix is not URL encoded.

    The whole URL is constructed from the prefix, the path and GET
    parameters. Hence, an empty prefix can be used to make a link to
    another site of the same server.

    See {!val:service} for a description of the optional
    [~keep_nl_params] and [~rt] parameters.
*)
val external_service :
  prefix: string ->
  path:Url.path ->
  ?rt:'rt rt ->
  ?keep_nl_params:[ `All | `Persistent | `None ] ->
  get_params:('get, [< suff ] as 'tipo, 'gn) params_type ->
  unit ->
  ('get, unit, [>`Get],[> attached_kind],[> `External ],'tipo,
   'gn, unit, [> `Unregistrable ], returnB) service


(** Same as {!external_service} but with POST method. *)
val external_post_service :
  prefix: string ->
  path:Url.path ->
  ?rt:'rt rt ->
  ?keep_nl_params:[ `All | `Persistent | `None ] ->
  get_params:('get, [< suff ] as 'tipo, 'gn) params_type ->
  post_params:('post, [ `WithoutSuffix ], 'pn) params_type ->
  unit ->
  ('get, 'post, [>`Post],[> attached_kind], [> `External ], 'tipo,
   'gn, 'pn, [> `Unregistrable ], returnB) service

(** Same as {!external_service} but with PUT method. *)
val external_put_service :
  prefix: string ->
  path:Url.path ->
  ?rt:'rt rt ->
  ?keep_nl_params:[ `All | `Persistent | `None ] ->
  get_params:('get, [< suff ] as 'tipo, 'gn) params_type ->
  unit ->
  ('get, Eliom_parameter.raw_post_data,
    [> `Put], [> attached_kind], [> `External ], 'tipo,
   'gn, no_param_name, [> `Unregistrable ], returnB) service

(** Same as {!external_service} but with DELETE method. *)
val external_delete_service :
  prefix: string ->
  path:Url.path ->
  ?rt:'rt rt ->
  ?keep_nl_params:[ `All | `Persistent | `None ] ->
  get_params:('get, [< suff ] as 'tipo, 'gn) params_type ->
  unit ->
  ('get, Eliom_parameter.raw_post_data,
   [>`Delete ], [> attached_kind], [> `External], 'tipo,
   'gn, no_param_name, [> `Unregistrable ], returnB) service
