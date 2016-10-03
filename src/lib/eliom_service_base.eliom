(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2007-2010 Vincent Balat
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

(* Manipulation of services - this code can be use on server or client side. *)

[%%shared.start]

module rec Types : Eliom_service_sigs.TYPES = Types

include Types

module Url = Eliom_lib.Url

type suff = [ `WithSuffix | `WithoutSuffix ]

let params_of_meth :
  type m gp gn pp pn x .
  (m, gp, gn, pp, pn, 'tipo, x) meth ->
  (gp, 'tipo, gn) params * (pp, [`WithoutSuffix], pn) params
  = function
    | Get gp -> gp, Eliom_parameter.unit
    | Post (gp, pp) -> gp, pp
    | Put gp -> gp, Eliom_parameter.raw_post_data
    | Delete gp -> gp, Eliom_parameter.raw_post_data

let which_meth_internal
  : type m gp gn pp pn tipo x .
    (m, gp, gn, pp, pn, tipo, x) meth -> m which_meth
  = function
    | Get _ -> Get'
    | Post _ -> Post'
    | Put _ -> Put'
    | Delete _ -> Delete'

let is_post :
  type m gp gn pp pn x . (m, gp, gn, pp, pn, _, x) meth -> bool =
  function Post (_, _) -> true | _ -> false

let is_post' :
  type m . m which_meth -> bool =
  function Post' -> true | _ -> false

type reload_fun = Rf_keep | Rf_client_fun

type att = {
  prefix          : string;   (* name of the server and protocol for
                                 external links, e.g.,
                                 http://ocsigen.org *)
  subpath         : Url.path; (* name of the service without parameters *)
  fullpath        : Url.path; (* full path of the service =
                                 site_dir@subpath *)
  get_name        : Eliom_common.att_key_serv;
  post_name       : Eliom_common.att_key_serv;
  redirect_suffix : bool;
  priority        : int;
}

type non_att = {
  na_name            : Eliom_common.na_key_serv;
  keep_get_na_params : bool
  (* bool is used only for post and means "keep_get_na_params": do we
     keep GET non-attached parameters in links (if any) (31/12/2007 -
     experimental - WAS: 'a, but may be removed (was not used)) *)
}

type 'a attached_info =
  | Attached : att -> att attached_info
  | Nonattached : non_att -> non_att attached_info

type send_appl_content =
  | XNever
  | XAlways
  | XSame_appl of string * string option
  (* the string is the name of the application to which the service
     belongs and the option is the name of template *)
(** Whether the service is capable to send application content or not.
    (application content has type Eliom_service.eliom_appl_answer:
    content of the application container, or xhr redirection ...).  A
    link towards a service with send_appl_content = XNever will always
    answer a regular http frame (this will stop the application if
    used in a regular link or form, but not with XHR).  XAlways means
    "for all applications" (like redirections/actions).  XSame_appl
    means "only for this application".  If there is a client side
    application, and the service has XAlways or XSame_appl when it is
    the same application, then the link (or form or change_page) will
    expect application content. *)

type service_kind =
  [ `Service
  | `AttachedCoservice
  | `NonattachedCoservice
  | `External ]

(* 'return is the value returned by the service *)
type ('get, 'post, 'meth, 'attached, 'co, 'ext, 'reg,
      +'tipo, 'getnames, 'postnames, 'rt) t = {
  pre_applied_parameters :
    (string * Eliommod_parameters.param) list Eliom_lib.String.Table.t
    (* non localized parameters *) *
    (string * Eliommod_parameters.param) list (* regular parameters *);
  get_params_type :
    ('get, 'tipo, 'getnames) Eliom_parameter.params_type;
  post_params_type :
    ('post, [`WithoutSuffix], 'postnames) Eliom_parameter.params_type;
  max_use : int option;   (* Max number of use of this service *)
  timeout : float option; (* Timeout for this service (the service
                             will disappear if it has not been used
                             during this amount of seconds) *)
  meth : 'meth which_meth;
  kind : service_kind;
  info : 'attached attached_info;

  https : bool; (* force https *)
  keep_nl_params : [ `All | `Persistent | `None ];
  mutable send_appl_content : send_appl_content;
  (* XNever when we create the service, then changed at registration
     :/ *)

  (* If the service has a client-side implementation, we put the
     generating function here: *)
  mutable client_fun :
    ('get -> 'post -> unit Lwt.t) Eliom_client_value.t option;

  mutable reload_fun : reload_fun;

  service_mark :
    (unit, unit, 'meth, 'attached, 'co, 'ext, 'reg,
     suff, unit, unit, unit)
      t Eliom_common.wrapper;
}
  constraint 'tipo = [< suff ]

let pre_wrap s = {
  s with
  get_params_type = Eliom_parameter.wrap_param_type s.get_params_type;
  post_params_type = Eliom_parameter.wrap_param_type s.post_params_type;
  service_mark = Eliom_common.empty_wrapper ();
}

let service_mark () = Eliom_common.make_wrapper pre_wrap

let info {info} = info

let pre_applied_parameters s = s.pre_applied_parameters
let get_params_type s = s.get_params_type
let post_params_type s = s.post_params_type
let prefix s = s.prefix
let sub_path s = s.subpath
let redirect_suffix s = s.redirect_suffix
let full_path s = s.fullpath
let get_name s = s.get_name
let post_name s = s.post_name
let na_name s = s.na_name
let na_keep_get_na_params s = s.keep_get_na_params
let max_use s = s.max_use
let timeout s = s.timeout
let https s = s.https
let priority s = s.priority
let client_fun {client_fun} = client_fun

let internal_set_client_fun ~service f = service.client_fun <- Some f

let set_client_fun ?app ~service f =
  Eliom_lib.Option.iter
    (fun name -> service.send_appl_content <- XSame_appl (name, None))
    app;
  service.client_fun <- Some f

let is_external = function {kind = `External} -> true | _ -> false

let default_priority = 0

let meth {meth} = meth

let change_get_num service attser n = {
  service with
  service_mark = service_mark ();
  info = Attached {attser with get_name = n}
}

(** Static directories **)
let static_dir_ ?(https = false) () = {
  pre_applied_parameters = Eliom_lib.String.Table.empty, [];
  get_params_type =
    Eliom_parameter.suffix
      (Eliom_parameter.all_suffix Eliom_common.eliom_suffix_name);
  post_params_type = Eliom_parameter.unit;
  max_use = None;
  timeout = None;
  kind = `Service;
  meth = Get';
  info = Attached {
    prefix = "";
    subpath = [""];
    fullpath = (Eliom_request_info.get_site_dir ()) @
               [Eliom_common.eliom_suffix_internal_name];
    get_name = Eliom_common.SAtt_no;
    post_name = Eliom_common.SAtt_no;
    redirect_suffix = true;
    priority = default_priority;
  };
  https;
  keep_nl_params = `None;
  service_mark = service_mark ();
  send_appl_content = XNever;
  client_fun = None;
  reload_fun = Rf_client_fun
}

let static_dir () = static_dir_ ()

let https_static_dir () = static_dir_ ~https:true ()

let get_static_dir_ ?(https = false)
    ?(keep_nl_params = `None) ~get_params () = {
  pre_applied_parameters = Eliom_lib.String.Table.empty, [];
  get_params_type =
    Eliom_parameter.suffix_prod
      (Eliom_parameter.all_suffix Eliom_common.eliom_suffix_name)
      get_params;
  post_params_type = Eliom_parameter.unit;
  max_use = None;
  timeout = None;
  kind = `Service;
  meth = Get';
  info = Attached {
    prefix = "";
    subpath = [""];
    fullpath = (Eliom_request_info.get_site_dir ()) @
               [Eliom_common.eliom_suffix_internal_name];
    get_name = Eliom_common.SAtt_no;
    post_name = Eliom_common.SAtt_no;
    redirect_suffix = true;
    priority = default_priority;
  };
  https;
  keep_nl_params;
  service_mark = service_mark ();
  send_appl_content = XNever;
  client_fun = None;
  reload_fun = Rf_client_fun
}

let static_dir_with_params ?keep_nl_params ~get_params () =
  get_static_dir_ ?keep_nl_params ~get_params ()

let https_static_dir_with_params ?keep_nl_params ~get_params () =
  get_static_dir_ ~https:true ?keep_nl_params ~get_params ()

let send_appl_content s = s.send_appl_content
let set_send_appl_content s n = s.send_appl_content <- n

type clvpreapp = {
  mutable clvpreapp_f :
    'a 'b.
         (('a -> 'b -> [ `Html ] Eliom_content_core.Html.elt Lwt.t)
            Eliom_client_value.t ->
          'a ->
          (unit -> 'b -> [ `Html ] Eliom_content_core.Html.elt Lwt.t)
            Eliom_client_value.t)
}

let preapply_client_fun = {
  clvpreapp_f = fun f getparams -> failwith "preapply_client_fun"
}

(* will be initialized later (in Eliom_content for now), when client
   syntax is available, with: fun f getparams -> {{ fun _ pp -> %f
   %getparams pp }} *)

let rec append_suffix l m =
  match l with
  | [] ->
    m
  | [eliom_suffix_internal_name] ->
    m
  | a :: ll ->
    a :: append_suffix ll m

let preapply ~service getparams =
  let nlp, preapp = service.pre_applied_parameters in
  let suff, nlp, params =
    Eliom_parameter.construct_params_list_raw
      nlp service.get_params_type getparams
  in {
    service with
    service_mark = service_mark ();
    pre_applied_parameters = nlp, params @ preapp;
    get_params_type = Eliom_parameter.unit;
    info =
      (match service.info with
       | Attached k ->
         Attached
           {k with
            subpath =
              (match suff with
               | Some suff -> append_suffix k.subpath suff
               | _ -> k.subpath);
            fullpath =
              (match suff with
               | Some suff -> append_suffix k.fullpath suff
               | _ -> k.fullpath);
           };
       | k -> k);
    client_fun =
      match service.client_fun with
      | Some f ->
        Some  [%client fun () pp -> (~%f : _ -> _ -> _) ~%getparams pp ]
      | None ->
        None
  }

let reload_action_aux https = {
  max_use = None;
  timeout = None;
  pre_applied_parameters = Eliom_lib.String.Table.empty, [];
  get_params_type = Eliom_parameter.unit;
  post_params_type = Eliom_parameter.unit;
  kind = `NonattachedCoservice;
  meth = Get';
  info = Nonattached {
    na_name = Eliom_common.SNa_void_dontkeep;
    keep_get_na_params= true;
  };
  https;
  keep_nl_params = `All;
  service_mark = service_mark ();
  send_appl_content = XAlways;
  client_fun = None;
  reload_fun = Rf_keep
}

let reload_action = reload_action_aux false

let reload_action_https = reload_action_aux true

let reload_action_hidden_aux https =
  let raa = reload_action_aux https in
  {
    raa (* We must create a reference for each one of the for reload
           actions *)
    with
      kind = `NonattachedCoservice;
      meth = Get';
      info = Nonattached {
        na_name = Eliom_common.SNa_void_keep;
        keep_get_na_params=true;
      };
  }

let reload_action_hidden = reload_action_hidden_aux false

let reload_action_https_hidden = reload_action_hidden_aux true

(*VVV Non localized parameters not implemented for client side
  services *)
let add_non_localized_get_parameters ~params ~service = {
  service with
  get_params_type =
    Eliom_parameter.nl_prod service.get_params_type params;
  client_fun =
    match service.client_fun with
    | None -> None
    | Some f -> Some [%client fun (g, _) p -> (~%f : _ -> _ -> _) g p ]
}

let add_non_localized_post_parameters ~params ~service = {
  service with
  post_params_type =
    Eliom_parameter.nl_prod service.post_params_type params;
  client_fun =
    match service.client_fun with
    | None -> None
    | Some f -> Some [%client fun g (p, _) -> (~%f : _ -> _ -> _) g p ]
}

let keep_nl_params s = s.keep_nl_params

let untype s =
  (s
   :  ('get, 'post, 'meth, 'attached, 'co, 'ext,
       'tipo, 'getnames, 'postnames, 'register, _) t
   :> ('get, 'post, 'meth, 'attached, 'co, 'ext,
       'tipo, 'getnames, 'postnames,'register, _) t)

type (_, _, _) path_option =
  | Path    : Eliom_lib.Url.path -> (att, non_co, _) path_option
  | No_path : (non_att, co, unit) path_option

let eliom_appl_answer_content_type = "application/x-eliom"

let uniqueid =
  let r = ref (-1) in
  fun () -> r := !r + 1; !r

let new_state () =
  (* FIX: we should directly produce a string of the right length *)
  (* 72bit of entropy is large enough: CSRF-safe services are
     short-lived; with 65536 services, the probability of a collision
     is about 2^-41.  *)
  Eliom_lib.make_cryptographic_safe_string ~len:12 ()

let default_csrf_scope = function
  (* We do not use the classical syntax for default value. Otherwise,
     the type for csrf_scope was: [< Eliom_common.user_scope >
     `Session] *)
  | None -> `Session Eliom_common_base.Default_ref_hier
  | Some c -> (c :> Eliom_common.user_scope)

exception Unreachable_exn

let attached_info = function
  | {info = Attached k} ->
    k
  | _ ->
    failwith "attached_info"

let non_attached_info = function
  | {info = Nonattached k} ->
    k
  | _ ->
    failwith "non_attached_info"

let attach_existing :
  fallback:
  (unit, unit, get, att, _, non_ext, 'rg1,
   [< suff ], unit, unit, 'return1) t ->
  service:
  ('get, 'post, 'gp, non_att, co, non_ext, 'rg2,
   [< `WithoutSuffix] as 'sf, 'gn, 'pn, 'return) t ->
  unit ->
  ('get, 'post, 'gp, att, co, non_ext, non_reg,
   'sf, 'gn, 'pn, 'return) t =
  fun ~fallback ~service () ->
    let {na_name} = non_attached_info service in
    let fallbackkind = attached_info fallback in
    let open Eliom_common in
    let error_msg =
      "attach_global_to_fallback' is not implemented for this kind of\
       service. Please report a bug if you need this."
    in
    let get_name = match na_name with
      | SNa_get_ s -> SAtt_na_named s
      | SNa_get' s -> SAtt_na_anon s
      | SNa_get_csrf_safe a -> SAtt_na_csrf_safe a
      | SNa_post_ s -> fallbackkind.get_name (*VVV check *)
      | SNa_post' s -> fallbackkind.get_name (*VVV check *)
      | SNa_post_csrf_safe a -> fallbackkind.get_name (*VVV check *)
      | _ -> failwith error_msg
    (*VVV Do we want to make possible to attach POST na coservices
          on GET attached coservices? *)
    and post_name = match na_name with
      | SNa_get_ s -> SAtt_no
      | SNa_get' s -> SAtt_no
      | SNa_get_csrf_safe a -> SAtt_no
      | SNa_post_ s -> SAtt_na_named s
      | SNa_post' s -> SAtt_na_anon s
      | SNa_post_csrf_safe a -> SAtt_na_csrf_safe a
      | _ -> failwith error_msg
    in {
      service with
      service_mark = service_mark ();
      kind = `AttachedCoservice;
      info = Attached {fallbackkind with get_name ; post_name }
    }

(** Create a main service (not a coservice), internal or external *)
let main_service
    ~https
    ~prefix
    ~(path : Url.path)
    ~site_dir
    ~kind
    ~meth
    ?(redirect_suffix = true)
    ?(keep_nl_params = `None)
    ?(priority = default_priority)
    ~get_params
    (type pp) ~(post_params : (pp, _, _) Eliom_parameter.params_type)
    ?(client_fun : (_ -> pp -> unit Lwt.t) Eliom_client_value.t option)
    ~reload_fun
    () = {
  pre_applied_parameters = Eliom_lib.String.Table.empty, [];
  get_params_type = get_params;
  post_params_type = post_params;
  max_use = None;
  timeout = None;
  meth;
  kind;
  info = Attached {
    prefix;
    subpath = path;
    fullpath = site_dir @ path;
    get_name = Eliom_common.SAtt_no;
    post_name = Eliom_common.SAtt_no;
    redirect_suffix;
    priority;
  };
  https;
  keep_nl_params;
  service_mark = service_mark ();
  send_appl_content = XNever;
  client_fun;
  reload_fun;
}

let extern
    (type m) (type gp) (type gn) (type pp) (type pn) (type gp')
    ?keep_nl_params
    ~prefix
    ~path
    ~(meth : (m, gp, gn, pp, pn, _, gp') meth)
    () =
  let get_params, post_params = params_of_meth meth in
  let suffix = Eliom_parameter.contains_suffix get_params in
  let meth = which_meth_internal meth in
  main_service
    ~https:false (* not used for external links *)
    ~prefix
    ~path:
      (Url.remove_internal_slash
         (match suffix with
          | None -> path
          | _ -> path @ [Eliom_common.eliom_suffix_internal_name]))
    ~site_dir:[]
    ~kind:`External
    ~meth
    ?keep_nl_params
    ~redirect_suffix:false
    ~get_params
    ~post_params
    ~reload_fun:Rf_keep
    ()

let which_meth {meth} = meth

let which_meth_untyped
    (type m) (s : (_, _, m, _, _, _, _, _, _, _, _) t) =
  match which_meth s with
  | Get'    -> `Get
  | Post'   -> `Post
  | Put'    -> `Put
  | Delete' -> `Delete
