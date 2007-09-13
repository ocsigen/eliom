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
open Ocsimisc
open Extensions
open Eliommod
open Eliomsessions
open Eliomparameters
open Lazy


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
    {url: url_path; (* name of the service without parameters *)
     att_kind: 'a; (* < attached_service_kind *)
     get_state: internal_state option;
     post_state: internal_state option;
   }
      
type +'a na_s =
    {na_name: string option * string option;
     na_kind: 'a; (* < getpost *)
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
let get_url_ s = s.url
let get_get_state_ s = s.get_state
let get_post_state_ s = s.post_state
let get_na_name_ s = s.na_name
let get_max_use_ s = s.max_use
let get_timeout_ s = s.timeout

let counter = let c = ref (Random.int 1000000) in fun () -> c := !c + 1 ; !c

let new_state =
  let c : internal_state ref = ref (Random.int 1000000) in
  fun () -> c := !c + 1 ; Some !c



(*****************************************************************************)
(*****************************************************************************)
(* Page registration, handling of links and forms                            *)
(*****************************************************************************)
(*****************************************************************************)

(** Satic directories **)
let static_dir ~sp =
    {
     pre_applied_parameters = [];
     get_params_type = suffix (all_suffix eliom_suffix_name);
     post_params_type = unit;
     max_use= None;
     timeout= None;
     kind = `Attached
       {url = (get_working_dir ~sp)@[""];
        get_state = None;
        post_state = None;
        att_kind = `Internal (`Service, `Get);
      }
   }



(****************************************************************************)
(****************************************************************************)

(** Definition of services *)
(** Create a main service (not a coservice) internal or external, get only *)
let new_service_aux_aux
    ~(url : url_path)
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
     {url = url;
      att_kind = kind;
      get_state = None;
      post_state = None}
 }
    
let new_service_aux
    ?sp
    ~url
    ~get_params =
  match sp with
  | None ->
      (match global_register_allowed () with
        Some get_current_hostdir ->
          let _,curdir = get_current_hostdir () in
          let full_path = 
            remove_middle_slash (curdir@(change_empty_list url)) in
          let u = new_service_aux_aux
              ~url:full_path
              ~kind:(`Internal (`Service, `Get))
              ~get_params
              ~post_params:unit
          in
          add_unregistered (Some full_path); u
      | None -> raise (Eliom_function_forbidden_outside_site_loading
                         "new_service"))
  | Some (_, _, (curdir, (_, _, _), _, _, _)) ->
      let full_path = 
        remove_middle_slash (curdir@(change_empty_list url)) in
      new_service_aux_aux
        ~url:full_path
        ~kind:(`Internal (`Service, `Get))
        ~get_params
        ~post_params:unit

      
let new_external_service
    ~url
    ~get_params
    ~post_params
    () =
  let suffix = contains_suffix get_params in
  new_service_aux_aux
    ~url:(remove_middle_slash 
            (if suffix then url@[eliom_suffix_internal_name] else url))
    ~kind:`External
    ~get_params 
    ~post_params
    
let new_service
    ?sp
    ~url
    ~get_params
    () =
  let suffix = contains_suffix get_params in
  new_service_aux 
    ?sp
    ~url:(if suffix then url@[eliom_suffix_internal_name] else url)
    ~get_params

let new_naservice_name () = string_of_int (counter ())

let new_coservice
    ?max_use
    ?timeout
    ~fallback
    ~get_params
    () =
  let `Attached k = fallback.kind in
  (* (match global_register_allowed () with
    Some _ -> add_unregistered (Some k.url);
  | _ -> ()); *)
  {fallback with
   max_use= max_use;
   timeout= timeout;
   get_params_type = add_pref_params co_param_prefix get_params;
   kind = `Attached
     {k with
      get_state = new_state ();
      att_kind = `Internal (`Coservice, `Get);
    }
 }
(* Warning: here no GET parameters for the fallback.
   Apply services with apply_service 
   if you want fallbacks with GET parameters *)
    

let new_coservice' ?max_use ?timeout ~get_params () =
  (* match global_register_allowed () with
    Some _ -> add_unregistered None;
  | _ -> () *) (* Do we accept unregistered non-attached coservices? *)
  {
   max_use= max_use;
   timeout= timeout;
   pre_applied_parameters = [];
   get_params_type = add_pref_params na_co_param_prefix get_params;
   post_params_type = unit;
   kind = `Nonattached
     {na_name = (Some (new_naservice_name ()), None);
      na_kind = `Get;
    }
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
     {url = k1.url;
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
  let url = Some k1.url in
  let u = new_post_service_aux ~sp ~fallback ~post_params in
  match sp with
  | None ->
      (match global_register_allowed () with
        Some _ ->
          add_unregistered url;
          u
      | None ->
          if kind = `Service
          then raise (Eliom_function_forbidden_outside_site_loading
                        "new_post_service")
          else u)
  | _ -> u
(* Warning: strange if post_params = unit... *)    
(* if the fallback is a coservice, do we get a coservice or a service? *)    


let new_post_coservice ?max_use ?timeout ~fallback ~post_params () = 
  let `Attached k1 = fallback.kind in
  (* (match global_register_allowed () with
    Some _ -> add_unregistered (Some k1.url);
  | _ -> ()); *)
  {fallback with 
   post_params_type = post_params;
   max_use= max_use;
   timeout= timeout;
   kind = `Attached 
     {k1 with 
      att_kind = `Internal (`Coservice, `Post);
      post_state = new_state ();
    }
 }
(* It is not possible to make a new_post_coservice function 
   with an optional ?fallback parameter
   because the type 'get of the result depends on the 'get of the
   fallback. Or we must impose 'get = unit ...
 *)

let new_post_coservice' ?max_use ?timeout ~post_params () =
  (* match global_register_allowed () with
    Some _ -> add_unregistered None
  | _ -> () *)
  {
   max_use= max_use;
   timeout= timeout;
   pre_applied_parameters = [];
   get_params_type = unit;
   post_params_type = post_params;
   kind = `Nonattached
     {na_name = (None, Some (new_naservice_name ()));
      na_kind = `Post;
    }
 }

(*
let new_get_post_coservice'
   ?max_use
   ?timeout
    ~fallback
    ~post_params =
  (* match global_register_allowed () with
    Some _ ->
  | _ -> ());
   add_unregistered None; *)
   {
   pre_applied_parameters = fallback.pre_applied_parameters;
   get_params_type = fallback.na_get_params_type;
   post_params_type = post_params;
   max_use= max_use;
   timeout= timeout;
   kind = `Nonattached
   {na_name = (fst fallback.na_name, Some (new_naservice_name ()));
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
     `Attached k -> `Attached {k with 
                               url = match suff with
                                 Some suff -> k.url@suff
                               | _ -> k.url}
   | k -> k
 }




(*****************************************************************************)
(* Building href *)
let rec string_of_url_path' = function
  | [] -> ""
  | [a] when a = eliom_suffix_internal_name -> ""
  | [a] -> a
  | a::l -> a^"/"^(string_of_url_path' l)

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
  if s = "" then defaultpagename else s

let rec relative_url_path_to_myself = function
  | []
  | [""] -> defaultpagename
  | [a] -> a
  | a::l -> relative_url_path_to_myself l
(*****************************************************************************)

let make_string_uri
    ~service
    ~sp:((_,si,_) as sp)
    getparams : string =
  match get_kind_ service with
  | `Attached attser ->
      begin
        let suff, params_string = 
          construct_params (get_get_params_type_ service) getparams in
        let preapplied_params = 
          construct_params_string (get_pre_applied_parameters_ service) in
        let params_string =
          concat_strings preapplied_params "&" params_string in
        let uri = 
          (if (get_att_kind_ attser) = `External
          then (reconstruct_absolute_url_path
                  (get_current_path sp) (get_url_ attser) suff)
          else (reconstruct_relative_url_path
                  (get_current_path sp) (get_url_ attser) suff))
        in
        match get_get_state_ attser with
          None ->
            add_to_string uri "?" params_string
        | Some i -> 
            add_to_string (uri^"?"^
                           get_state_param_name^"="^(string_of_int i))
              "&" params_string
      end
  | `Nonattached naser ->
      let current_get_params =
        List.remove_assoc
          naservice_name
          (remove_prefixed_param na_co_param_prefix (get_all_get_params sp))
      in
      let _, params_string = 
        construct_params (get_get_params_type_ service) getparams in
      let preapplied_params = 
        construct_params_string (get_pre_applied_parameters_ service) in
      let params_string =
        concat_strings preapplied_params "&" params_string in
      let naservice_param = 
        match fst (get_na_name_ naser) with
          Some n -> naservice_name^"="^n
        | _ -> assert false
      in
      let current_get_params_string = 
        construct_params_string current_get_params in
      (("/"^(get_current_path_string sp))^"?"^
       (concat_strings
          current_get_params_string
          "&"
          (concat_strings naservice_param "&" params_string))
      )

