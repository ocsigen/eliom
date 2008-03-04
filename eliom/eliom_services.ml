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
open Extensions
open Eliom_sessions
open Eliom_parameters
open Lazy


(** Type used for cookies to set. 
    The float option is the timestamp for the expiration date.
    The strings are names and values.
 *)
type cookie = Eliom_common.cookie =
  | Set of url_path option * float option * string * string
  | Unset of url_path option * string

let cookie_table_of_eliom_cookies
    ?(oldtable= Http_frame.Cookies.empty) ~sp cl =
  Eliommod_cookies.add_cookie_list_to_send 
    (Eliom_sessions.get_sitedata sp)
    cl oldtable




(** The type to send if you want to create your own modules for generating
   pages
 *)
type result_to_send = Eliom_common.result_to_send =
  | EliomResult of Http_frame.result
  | EliomExn of (exn list * cookie list)



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
     get_state: Eliom_common.internal_state option;
     post_state: Eliom_common.internal_state option;
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
   }

let get_kind_ s = s.kind
let get_att_kind_ s = s.att_kind
let get_pre_applied_parameters_ s = s.pre_applied_parameters
let get_get_params_type_ s = s.get_params_type
let get_post_params_type_ s = s.post_params_type
let get_prefix_ s = s.prefix
let get_sub_path_ s = s.subpath
let get_full_path_ s = s.fullpath
let get_get_state_ s = s.get_state
let get_post_state_ s = s.post_state
let get_na_name_ s = s.na_name
let get_na_kind_ s = s.na_kind
let get_max_use_ s = s.max_use
let get_timeout_ s = s.timeout

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
(* Page registration, handling of links and forms                            *)
(*****************************************************************************)
(*****************************************************************************)

(** Satic directories **)
let static_dir ~sp =
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
        get_state = None;
        post_state = None;
        att_kind = `Internal (`Service, `Get);
      };
   }



(****************************************************************************)
(****************************************************************************)

(** Definition of services *)
(** Create a main service (not a coservice) internal or external, get only *)
let new_service_aux_aux
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
      get_state = None;
      post_state = None;
    };
 }
    
let new_service_aux
    ?sp
    ~path
    ~get_params =
  match sp with
  | None ->
      (match Eliom_common.global_register_allowed () with
      | Some get_current_sitedata ->
          let sitedata = get_current_sitedata () in
          let path = remove_internal_slash (change_empty_list path) in
          let u = new_service_aux_aux
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
      let path = remove_internal_slash (change_empty_list path) in
      new_service_aux_aux
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
    ~path
    ~get_params
    () =
  let suffix = contains_suffix get_params in
  new_service_aux 
    ?sp
    ~path:(if suffix 
    then path@[Eliom_common.eliom_suffix_internal_name] 
    else path)
    ~get_params

let new_naservice_num () = new_state ()

let new_coservice
    ?max_use
    ?timeout
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
      get_state = Some (new_state ());
      att_kind = `Internal (`Coservice, `Get);
    }
 }
(* Warning: here no GET parameters for the fallback.
   Apply services with apply_service 
   if you want fallbacks with GET parameters *)
    

let new_coservice' ?max_use ?timeout ~get_params () =
  let n = Eliom_common.Na_get' (new_naservice_num ()) in
  (* (match Eliom_common.global_register_allowed () with
  | Some _ -> Eliom_common.add_unregistered_na n;
  | _ -> () (* Do we accept unregistered non-attached coservices? *)); *)
  {
   max_use= max_use;
   timeout= timeout;
   pre_applied_parameters = [];
   get_params_type = add_pref_params Eliom_common.na_co_param_prefix get_params;
   post_params_type = unit;
   kind = `Nonattached
     {na_name = n;
      na_kind = `Get;
    };
 }
    
let new_service' ?sp ~name ~get_params () =
  match sp with
  | None ->
      (match Eliom_common.global_register_allowed () with
      | Some get_current_sitedata ->
          let sitedata = get_current_sitedata () in
          let r =
            {
(*VVV allow timeout and max_use? *)
             max_use= None;
             timeout= None;
             pre_applied_parameters = [];
             get_params_type = 
             add_pref_params Eliom_common.na_co_param_prefix get_params;
             post_params_type = unit;
             kind = `Nonattached
               {na_name = Eliom_common.Na_get_ name;
                na_kind = `Get;
              };
           }
          in
          Eliom_common.add_unregistered_na sitedata 
            (Eliom_common.Na_get_ name); 
          r
      | None -> 
          raise (Eliom_common.Eliom_function_forbidden_outside_site_loading
                   "new_service'"))
  | Some sp ->
      {
(*VVV allow timeout and max_use? *)
       max_use= None;
       timeout= None;
       pre_applied_parameters = [];
       get_params_type = 
       add_pref_params Eliom_common.na_co_param_prefix get_params;
       post_params_type = unit;
       kind = `Nonattached
         {na_name = Eliom_common.Na_get_ name;
          na_kind = `Get;
        };
     }
    
(****************************************************************************)
(** Register a service with post parameters in the server *)
let new_post_service_aux ~sp ~fallback ~post_params =
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
      get_state = k1.get_state;
      post_state = None;
    }
 }
    
let new_post_service ?sp ~fallback ~post_params () = 
  (* (if post_params = TUnit
  then Messages.warning "Probably error in the module: \
      Creation of a POST service without POST parameters."); 
      12/07/07
      I remove this warning: POST service without POST parameters means
      that the service will answer to a POST request only.
    *)
  let `Attached k1 = fallback.kind in
  let `Internal (kind, _) = k1.att_kind in
  let path = k1.subpath in
  let u = new_post_service_aux ~sp ~fallback ~post_params in
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


let new_post_coservice ?max_use ?timeout ~fallback ~post_params () = 
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
      post_state = Some (new_state ());
    }
 }
(* It is not possible to make a new_post_coservice function 
   with an optional ?fallback parameter
   because the type 'get of the result depends on the 'get of the
   fallback. Or we must impose 'get = unit ...
 *)


(*VVV Warning: keep_get_na_params is experimental *)
let new_post_coservice' 
    ?max_use ?timeout ?(keep_get_na_params = true) ~post_params () =
  (* match Eliom_common.global_register_allowed () with
  | Some _ -> Eliom_common.add_unregistered None
  | _ -> () *)
  {
   max_use= max_use;
   timeout= timeout;
   pre_applied_parameters = [];
   get_params_type = unit;
   post_params_type = post_params;
   kind = `Nonattached
     {na_name = Eliom_common.Na_post' (new_naservice_num ());
      na_kind = `Post keep_get_na_params;
    }
 }

let new_post_service' 
   ?(keep_get_na_params = true) ~name ~post_params () =
  (* match Eliom_common.global_register_allowed () with
  | Some _ -> Eliom_common.add_unregistered None
  | _ -> () *)
  {
(*VVV allow timeout and max_use? *)
   max_use= None;
   timeout= None;
   pre_applied_parameters = [];
   get_params_type = unit;
   post_params_type = post_params;
   kind = `Nonattached
     {na_name = Eliom_common.Na_post_ name;
      na_kind = `Post keep_get_na_params;
    }
 }

(*
let new_get_post_coservice'
   ?max_use
   ?timeout
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
   }
(* This is a nonattached coservice with GET and POST parameters!
   When reloading, the fallback (a nonattached coservice with only GET 
   parameters) will be called.
 *)

Very experimental
Forms towards that kind of service are not implemented
*)


let preapply ~service getparams =
  let suff, params = construct_params_list service.get_params_type getparams in
  {service with
   pre_applied_parameters = params@service.pre_applied_parameters;
   get_params_type = unit;
   kind = match service.kind with
   | `Attached k -> `Attached {k with 
                               subpath = (match suff with
                               | Some suff -> k.subpath@suff
                               | _ -> k.subpath);
                               fullpath = (match suff with
                               | Some suff -> k.fullpath@suff
                               | _ -> k.fullpath);
                             }
   | k -> k
 }




(*****************************************************************************)
(* Building href *)
let rec string_of_url_path' = function
  | [] -> ""
  | [a] when a = Eliom_common.eliom_suffix_internal_name -> ""
  | [a] -> Netencoding.Url.encode ~plus:false a
  | a::l when a = Eliom_common.eliom_suffix_internal_name -> 
      string_of_url_path' l
  | a::l -> (Netencoding.Url.encode ~plus:false a)^"/"^(string_of_url_path' l)

let rec string_of_url_path_suff u = function
  | None -> string_of_url_path' u
  | Some suff -> let deb = (string_of_url_path' u) in
    if deb = "" 
    then string_of_url_path' suff
    else deb^(string_of_url_path' suff)

let reconstruct_absolute_url_path current_url = string_of_url_path_suff

let reconstruct_relative_url_path current_url u suff =
  let rec drop cururl desturl = match cururl, desturl with
  | a::l, [b] -> l, desturl
  | [a], m -> [], m
  | a::l, b::m when a = b -> drop l m
  | a::l, m -> l, m
  | [], m -> [], m
  in let rec makedotdot = function
    | [] -> ""
(*    | [a] -> "" *)
    | _::l -> "../"^(makedotdot l)
  in 
  let aremonter, aaller = drop current_url u
  in let s = (makedotdot aremonter)^(string_of_url_path_suff aaller suff) in
(*  Messages.debug ((string_of_url_path current_url)^"->"^(string_of_url_path u)^"="^s);*)
  if s = "" then Eliom_common.defaultpagename else s

let rec relative_url_path_to_myself = function
  | []
  | [""] -> Eliom_common.defaultpagename
  | [a] -> a
  | a::l -> relative_url_path_to_myself l
(*****************************************************************************)

let set_exn_handler ?sp h = 
  let sitedata = Eliom_sessions.find_sitedata "set_exn_handler" sp in
  Eliom_sessions.set_site_handler sitedata h

let add_service = Eliommod_services.add_service
let add_naservice = Eliommod_naservices.add_naservice

let erts_of_rst = Ocsigen_lib.id
