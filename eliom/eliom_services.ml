(* Ocsigen
 * http://www.ocsigen.org
 * Module eliomservices.ml
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



open Lwt
open Ocsigen_lib
open Ocsigen_extensions
open Eliom_sessions
open Eliom_parameters
open Lazy


(** Type used for cookies to set.
    The float option is the timestamp for the expiration date.
    The strings are names and values.
 *)
type cookie = Eliom_common.cookie =
  | Set of url_path option * float option * string * string * bool
  | Unset of url_path option * string

let cookie_table_of_eliom_cookies
    ?(oldtable= Ocsigen_http_frame.Cookies.empty) ~sp cl =
  Eliommod_cookies.add_cookie_list_to_send
    (Eliom_sessions.get_sitedata sp)
    cl oldtable





(** This function may be used for services that cannot be interrupted
  (no cooperation point for threads). It is defined by
  [let sync f sp g p = Lwt.return (f sp g p)]
 *)
let sync f sp g p = Lwt.return (f sp g p)


(** Typed services *)
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

type +'a a_s =
    {prefix: string; (* name of the server and protocol,
                        for external links. Ex: http://ocsigen.org *)
     subpath: url_path; (* name of the service without parameters *)
     fullpath: url_path; (* full path of the service = site_dir@subpath *)
     att_kind: 'a; (* < attached_service_kind *)
     get_name: Eliom_common.att_key;
     post_name: Eliom_common.att_key;
   }

type +'a na_s =
    {na_name: Eliom_common.na_key;
     na_kind: [ `Get | `Post of bool ]
       (*
          where bool is "keep_get_na_params":
          do we keep GET non-attached parameters in links (if any)
          (31/12/2007 - experimental -
          WAS: 'a, but may be removed (was not used))
       *)
    }

type service_kind =
    [ `Attached of attached_service_kind a_s
    | `Nonattached of getpost na_s ]

type internal_service_kind =
    [ `Attached of internal a_s
    | `Nonattached of getpost na_s ]

type get_service_kind =
    [ `Attached of get_attached_service_kind a_s
    | `Nonattached of [ `Get ] na_s ]

type post_service_kind =
    [ `Attached of post_attached_service_kind a_s
    | `Nonattached of [ `Post ] na_s ]

type attached =
    [ `Attached of attached_service_kind a_s ]

type nonattached =
    [ `Nonattached of getpost na_s ]

type ('get,'post,+'kind,+'tipo,+'getnames,+'postnames,+'registr) service =
    {
     pre_applied_parameters: (string * string) list;
     get_params_type: ('get, 'tipo, 'getnames) params_type;
     post_params_type: ('post, [`WithoutSuffix], 'postnames) params_type;
     max_use: int option; (* Max number of use of this service *)
     timeout: float option; (* Timeout for this service (the service will
          disappear if it has not been used during this amount of seconds) *)
     kind: 'kind; (* < service_kind *)
     https: bool; (* force https *)
   }

let get_kind_ s = s.kind
let get_att_kind_ s = s.att_kind
let get_pre_applied_parameters_ s = s.pre_applied_parameters
let get_get_params_type_ s = s.get_params_type
let get_post_params_type_ s = s.post_params_type
let get_prefix_ s = s.prefix
let get_sub_path_ s = s.subpath
let get_full_path_ s = s.fullpath
let get_get_name_ s = s.get_name
let get_post_name_ s = s.post_name
let get_na_name_ s = s.na_name
let get_na_kind_ s = s.na_kind
let get_max_use_ s = s.max_use
let get_timeout_ s = s.timeout
let get_https s = s.https

let new_state =
  (* This does not need to be cryptographickly robust.
     We just want to avoid the same values when the server is relaunched.
   *)
  let c = ref (Int64.bits_of_float (Unix.gettimeofday ())) in
  fun () ->
    c := Int64.add !c Int64.one ;
    (Printf.sprintf "%x" (Random.int 0xFFFF))^(Printf.sprintf "%Lx" !c)


(*****************************************************************************)
(*****************************************************************************)
(* Registration of static module initialization functions                    *)
(*****************************************************************************)
(*****************************************************************************)

let register_eliom_module name f =
  Ocsigen_loader.set_module_init_function name f

(*****************************************************************************)
(*****************************************************************************)
(* Page registration, handling of links and forms                            *)
(*****************************************************************************)
(*****************************************************************************)

(** Satic directories **)
let static_dir_ ?(https = false) ~sp () =
    {
     pre_applied_parameters = [];
     get_params_type = suffix (all_suffix Eliom_common.eliom_suffix_name);
     post_params_type = unit;
     max_use= None;
     timeout= None;
     kind = `Attached
       {prefix = "";
        subpath = [""];
        fullpath = (Eliom_sessions.get_site_dir sp) @ [""];
        get_name = Eliom_common.Att_no;
        post_name = Eliom_common.Att_no;
        att_kind = `Internal (`Service, `Get);
      };
     https = https;
   }

let static_dir ~sp = static_dir_ ~sp ()

let https_static_dir ~sp = static_dir_ ~https:true ~sp ()

let get_static_dir_ ?(https = false) ~sp ~get_params () =
    {
     pre_applied_parameters = [];
     get_params_type = 
        suffix_prod 
          (all_suffix Eliom_common.eliom_suffix_name)
          get_params;
     post_params_type = unit;
     max_use= None;
     timeout= None;
     kind = `Attached
       {prefix = "";
        subpath = [""];
        fullpath = (Eliom_sessions.get_site_dir sp) @ [""];
        get_name = Eliom_common.Att_no;
        post_name = Eliom_common.Att_no;
        att_kind = `Internal (`Service, `Get);
      };
     https = https;
   }

let static_dir_with_params ~sp ~get_params = 
  get_static_dir_ ~sp ~get_params ()

let https_static_dir_with_params ~sp ~get_params = 
  get_static_dir_ ~https:true ~sp ~get_params ()


(****************************************************************************)
(****************************************************************************)

(** Definition of services *)
(** Create a main service (not a coservice) internal or external, get only *)
let new_service_aux_aux
    ~https
    ~prefix
    ~(path : url_path)
    ~site_dir
    ~kind
    ~get_params
    ~post_params =
(* ici faire une vérification "duplicate parameter" ? *)
  {
   pre_applied_parameters = [];
   get_params_type = get_params;
   post_params_type = post_params;
   max_use= None;
   timeout= None;
   kind = `Attached
     {prefix = prefix;
      subpath = path;
      fullpath = site_dir @ path;
      att_kind = kind;
      get_name = Eliom_common.Att_no;
      post_name = Eliom_common.Att_no;
    };
   https = https;
 }

let new_service_aux
    ?sp
    ~https
    ~path
    ~get_params =
  match sp with
  | None ->
      (match Eliom_common.global_register_allowed () with
      | Some get_current_sitedata ->
          let sitedata = get_current_sitedata () in
          let path = 
            Ocsigen_lib.remove_internal_slash
              (Ocsigen_lib.change_empty_list 
                 (Ocsigen_lib.remove_slash_at_beginning path))
          in
          let u = new_service_aux_aux
            ~https
            ~prefix:""
            ~path
            ~site_dir: sitedata.Eliom_common.site_dir
            ~kind:(`Internal (`Service, `Get))
            ~get_params
            ~post_params:unit
          in
          Eliom_common.add_unregistered sitedata path;
          u
      | None ->
          raise (Eliom_common.Eliom_function_forbidden_outside_site_loading
                   "new_service"))
  | Some sp ->
      let path = 
        Ocsigen_lib.remove_internal_slash
          (Ocsigen_lib.change_empty_list 
             (Ocsigen_lib.remove_slash_at_beginning path))
      in
      new_service_aux_aux
        ~https
        ~prefix:""
        ~path:path
        ~site_dir:(Eliom_sessions.get_site_dir sp)
        ~kind:(`Internal (`Service, `Get))
        ~get_params
        ~post_params:unit


let new_external_service
    ~prefix
    ~path
    ~get_params
    ~post_params
    () =
  let suffix = contains_suffix get_params in
  new_service_aux_aux
    ~https:false (* not used for external links *)
    ~prefix
    ~path:(remove_internal_slash
            (if suffix
            then path@[Eliom_common.eliom_suffix_internal_name]
            else path))
    ~site_dir:[]
    ~kind:`External
    ~get_params
    ~post_params

let new_service
    ?sp
    ?(https = false)
    ~path
    ~get_params
    () =
  let suffix = contains_suffix get_params in
  new_service_aux
    ?sp
    ~https
    ~path:(if suffix
    then path@[Eliom_common.eliom_suffix_internal_name]
    else path)
    ~get_params

let new_naservice_num () = new_state ()

let new_coservice
    ?name
    ?max_use
    ?timeout
    ?(https = false)
    ~fallback
    ~get_params
    () =
  let `Attached k = fallback.kind in
  (* (match Eliom_common.global_register_allowed () with
  | Some _ -> Eliom_common.add_unregistered k.path;
  | _ -> ()); *)
  {fallback with
   max_use= max_use;
   timeout= timeout;
   get_params_type = add_pref_params Eliom_common.co_param_prefix get_params;
   kind = `Attached
     {k with
      get_name = 
         (match name with
           | None -> Eliom_common.Att_anon (new_state ())
           | Some name -> Eliom_common.Att_named name);
        att_kind = `Internal (`Coservice, `Get);
     };
   https = https || fallback.https
 }
(* Warning: here no GET parameters for the fallback.
   Preapply services if you want fallbacks with GET parameters *)


let new_coservice' ?name ?max_use ?timeout ?(https = false) ~get_params () =
  (* (match Eliom_common.global_register_allowed () with
  | Some _ -> Eliom_common.add_unregistered_na n;
  | _ -> () (* Do we accept unregistered non-attached coservices? *)); *)
  (* (* Do we accept unregistered non-attached named coservices? *)
     match sp with
     | None ->
     ...
  *)
        {
(*VVV allow timeout and max_use for named coservices? *)
          max_use= max_use;
          timeout= timeout;
          pre_applied_parameters = [];
          get_params_type = 
            add_pref_params Eliom_common.na_co_param_prefix get_params;
          post_params_type = unit;
          kind = `Nonattached
            {na_name = 
                match name with
                  | None ->
                      Eliom_common.Na_get' (new_naservice_num ())
                  | Some name -> Eliom_common.Na_get_ name;
            ;
             na_kind = `Get;
            };
          https = https;
        }


(****************************************************************************)
(** Register a service with post parameters in the server *)
let new_post_service_aux ~sp ~https ~fallback ~post_params =
(** Create a main service (not a coservice) internal, post only *)
(* ici faire une vérification "duplicate parameter" ? *)
  let `Attached k1 = fallback.kind in
  let `Internal (k, _) = k1.att_kind in
  {
   pre_applied_parameters = fallback.pre_applied_parameters;
   get_params_type = fallback.get_params_type;
   post_params_type = post_params;
   max_use= None;
   timeout= None;
   kind = `Attached
     {prefix = k1.prefix;
      subpath = k1.subpath;
      fullpath = k1.fullpath;
      att_kind = `Internal (k, `Post);
      get_name = k1.get_name;
      post_name = Eliom_common.Att_no;
    };
   https = https;
 }

let new_post_service ?sp ?(https = false) ~fallback ~post_params () =
  (* (if post_params = TUnit
  then Ocsigen_messages.warning "Probably error in the module: \
      Creation of a POST service without POST parameters.");
      12/07/07
      I remove this warning: POST service without POST parameters means
      that the service will answer to a POST request only.
    *)
  let `Attached k1 = fallback.kind in
  let `Internal (kind, _) = k1.att_kind in
  let path = k1.subpath in
  let u = new_post_service_aux ~sp ~https ~fallback ~post_params in
  match sp with
  | None ->
      (match Eliom_common.global_register_allowed () with
      | Some get_current_sitedata ->
          Eliom_common.add_unregistered (get_current_sitedata ()) path;
          u
      | None ->
          if kind = `Service
          then
            raise (Eliom_common.Eliom_function_forbidden_outside_site_loading
                     "new_post_service")
          else u)
  | _ -> u
(* Warning: strange if post_params = unit... *)
(* if the fallback is a coservice, do we get a coservice or a service? *)


let new_post_coservice 
    ?name ?max_use ?timeout ?(https = false) ~fallback ~post_params () =
  let `Attached k1 = fallback.kind in
  (* (match Eliom_common.global_register_allowed () with
  | Some _ -> Eliom_common.add_unregistered k1.path;
  | _ -> ()); *)
  {fallback with
   post_params_type = post_params;
   max_use= max_use;
   timeout= timeout;
   kind = `Attached
     {k1 with
      att_kind = `Internal (`Coservice, `Post);
      post_name = 
         (match name with
            | None -> Eliom_common.Att_anon (new_state ())
            | Some name -> Eliom_common.Att_named name);
    };
   https = https;
 }
(* It is not possible to make a new_post_coservice function
   with an optional ?fallback parameter
   because the type 'get of the result depends on the 'get of the
   fallback. Or we must impose 'get = unit ...
 *)


(*VVV Warning: keep_get_na_params is experimental *)
let new_post_coservice'
    ?name
    ?max_use ?timeout ?(keep_get_na_params = true) ?(https = false)
    ~post_params () =
  (* match Eliom_common.global_register_allowed () with
  | Some _ -> Eliom_common.add_unregistered None
  | _ -> () *)
  {
(*VVV allow timeout and max_use for named coservices? *)
    max_use= max_use;
    timeout= timeout;
    pre_applied_parameters = [];
    get_params_type = unit;
    post_params_type = post_params;
    kind = `Nonattached
      {na_name = 
          (match name with
            | None ->
                Eliom_common.Na_post' (new_naservice_num ())
            | Some name -> Eliom_common.Na_post_ name);
       na_kind = `Post keep_get_na_params;
      };
    https = https;
  }


(*
let new_get_post_coservice'
   ?max_use
   ?timeout
?(https = false)
    ~fallback
    ~post_params =
  (* match Eliom_common.global_register_allowed () with
  | Some _ ->
  | _ -> ());
   Eliom_common.add_unregistered None; *)
   {
   pre_applied_parameters = fallback.pre_applied_parameters;
   get_params_type = fallback.na_get_params_type;
   post_params_type = post_params;
   max_use= max_use;
   timeout= timeout;
   kind = `Nonattached
   {na_name = (fst fallback.na_name, Some (new_naservice_num ()));
   na_kind = `Internal (`NonAttachedCoservice, `Post);
   }
  https = https;
   }
(* This is a nonattached coservice with GET and POST parameters!
   When reloading, the fallback (a nonattached coservice with only GET
   parameters) will be called.
 *)

Very experimental
Forms towards that kind of service are not implemented
*)


let rec append_suffix l m = match l with
  | [] -> m
  | [eliom_suffix_internal_name] -> m
  | a::ll -> a::(append_suffix ll m)

let preapply ~service getparams =
  let suff, params = construct_params_list service.get_params_type getparams in
  {service with
   pre_applied_parameters = params@service.pre_applied_parameters;
   get_params_type = unit;
   kind = match service.kind with
   | `Attached k -> `Attached {k with
                               subpath = (match suff with
                               | Some suff -> append_suffix k.subpath suff
                               | _ -> k.subpath);
                               fullpath = (match suff with
                               | Some suff -> append_suffix k.fullpath suff
                               | _ -> k.fullpath);
                             }
   | k -> k
 }


let void_coservice' =
  {
    max_use= None;
    timeout= None;
    pre_applied_parameters = [];
    get_params_type = unit;
    post_params_type = unit;
    kind = `Nonattached
      {na_name = Eliom_common.Na_void_dontkeep;
       na_kind = `Get;
      };
    https = false;
  }

let https_void_coservice' =
  {
    max_use= None;
    timeout= None;
    pre_applied_parameters = [];
    get_params_type = unit;
    post_params_type = unit;
    kind = `Nonattached
      {na_name = Eliom_common.Na_void_dontkeep;
       na_kind = `Get;
      };
    https = true;
  }

let void_hidden_coservice' = {void_coservice' with 
                         kind = `Nonattached
    {na_name = Eliom_common.Na_void_keep;
     na_kind = `Get;
    };
                      }

let https_void_hidden_coservice' = {void_coservice' with 
                         kind = `Nonattached
    {na_name = Eliom_common.Na_void_keep;
     na_kind = `Get;
    };
                      }


(*****************************************************************************)
let set_exn_handler ?sp h =
  let sitedata = Eliom_sessions.find_sitedata "set_exn_handler" sp in
  Eliom_sessions.set_site_handler sitedata h

let add_service = Eliommod_services.add_service
let add_naservice = Eliommod_naservices.add_naservice

let eccookiel_of_escookiel = Ocsigen_lib.id
let escookiel_of_eccookiel = Ocsigen_lib.id
