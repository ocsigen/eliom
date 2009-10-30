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
open Eliom_sessions
open Eliom_parameters
open Lazy

(* Manipulation of services - this code can be use only on server side. *)

include Eliom_services_obrowser


(** Type used for cookies to set.
    The float option is the timestamp for the expiration date.
    The strings are names and values.
 *)
type cookie = Eliom_common.cookie =
  | Set of Ocsigen_lib.url_path option * float option * string * string * bool
  | Unset of Ocsigen_lib.url_path option * string

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



(**********)
let new_state = Eliommod_cookies.make_new_cookie_value
(* WAS:
  (* This does not need to be cryptographickly robust.
     We just want to avoid the same values when the server is relaunched.
   *)
  let c = ref (Int64.bits_of_float (Unix.gettimeofday ())) in
  fun () ->
    c := Int64.add !c Int64.one ;
    (Printf.sprintf "%x" (Random.int 0xFFFF))^(Printf.sprintf "%Lx" !c)

   But I turned this into cryptographickly robust version
   to implement CSRF-safe services.
*)

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


(****************************************************************************)
(****************************************************************************)

(** Definition of services *)
(** Create a main service (not a coservice) internal or external, get only *)
let new_service_aux_aux
    ~https
    ~prefix
    ~(path : Ocsigen_lib.url_path)
    ~site_dir
    ~kind
    ?(redirect_suffix = true)
    ?(keep_nl_params = `None)
    ~get_params
    ~post_params =
(* ici faire une vérification "duplicate parameter" ? *)
  {
   pre_applied_parameters = Ocsigen_lib.String_Table.empty, [];
   get_params_type = get_params;
   post_params_type = post_params;
   max_use= None;
   timeout= None;
   kind = `Attached
     {prefix = prefix;
      subpath = path;
      fullpath = site_dir @ path;
      att_kind = kind;
      get_name = Eliom_common.SAtt_no;
      post_name = Eliom_common.SAtt_no;
      redirect_suffix = redirect_suffix
    };
   https = https;
   keep_nl_params = keep_nl_params;
   delayed_get_or_na_registration_function = None;
   delayed_post_registration_function = None;
 }

let new_service_aux
    ?sp
    ~https
    ~path
    ?redirect_suffix
    ?keep_nl_params
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
            ?redirect_suffix
            ?keep_nl_params
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
        ?redirect_suffix
        ?keep_nl_params
        ~get_params
        ~post_params:unit


let new_external_service
    ~prefix
    ~path
    ?keep_nl_params
    ~get_params
    ~post_params
    () =
  let suffix = contains_suffix get_params in
  new_service_aux_aux
    ~https:false (* not used for external links *)
    ~prefix
    ~path:(remove_internal_slash
            (match suffix with
               | None -> path
               | _ -> path@[Eliom_common.eliom_suffix_internal_name]))
    ~site_dir:[]
    ~kind:`External
    ?keep_nl_params
    ~redirect_suffix:false
    ~get_params
    ~post_params

let new_service
    ?sp
    ?(https = false)
    ~path
    ?keep_nl_params
    ~get_params
    () =
  let suffix = contains_suffix get_params in
  new_service_aux
    ?sp
    ~https
    ~path:(match suffix with
             | None -> path
             | _ -> path@[Eliom_common.eliom_suffix_internal_name])
    ?keep_nl_params
    ?redirect_suffix:suffix
    ~get_params

let new_coservice
    ?name
    ?(csrf_safe = false)
    ?max_use
    ?timeout
    ?(https = false)
    ~fallback
    ?keep_nl_params
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
         (if csrf_safe
          then Eliom_common.SAtt_csrf_safe
          else
            (match name with
               | None -> Eliom_common.SAtt_anon (new_state ())
               | Some name -> Eliom_common.SAtt_named name));
        att_kind = `Internal (`Coservice, `Get);
     };
   https = https || fallback.https;
   keep_nl_params = match keep_nl_params with 
     | None -> fallback.keep_nl_params | Some k -> k;
 }
(* Warning: here no GET parameters for the fallback.
   Preapply services if you want fallbacks with GET parameters *)


let new_coservice' ?name ?(csrf_safe = false) ?max_use ?timeout ?(https = false)
    ?(keep_nl_params = `Persistent) ~get_params () =
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
          pre_applied_parameters = Ocsigen_lib.String_Table.empty, [];
          get_params_type = 
            add_pref_params Eliom_common.na_co_param_prefix get_params;
          post_params_type = unit;
          kind = `Nonattached
            {na_name = 
                (if csrf_safe
                 then Eliom_common.SNa_get_csrf_safe
                 else
                   match name with
                     | None -> Eliom_common.SNa_get' (new_state ())
                     | Some name -> Eliom_common.SNa_get_ name);
             na_kind = `Get;
            };
          https = https;
          keep_nl_params = keep_nl_params;
          delayed_get_or_na_registration_function = None;
          delayed_post_registration_function = None;
        }


(****************************************************************************)
(* Register a service with post parameters in the server *)
let new_post_service_aux ~sp ~https ~fallback 
    ?(keep_nl_params = `None) ~post_params =
(* Create a main service (not a coservice) internal, post only *)
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
      post_name = Eliom_common.SAtt_no;
      redirect_suffix = false;
    };
   https = https;
   keep_nl_params = keep_nl_params;
   delayed_get_or_na_registration_function = None;
   delayed_post_registration_function = None;
 }

let new_post_service ?sp ?(https = false) ~fallback 
    ?keep_nl_params ~post_params () =
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
  let u = new_post_service_aux ~sp ~https ~fallback 
    ?keep_nl_params ~post_params 
  in
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
    ?name
    ?(csrf_safe = false)
    ?max_use
    ?timeout
    ?(https = false)
    ~fallback
    ?keep_nl_params
    ~post_params
    () =
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
         (if csrf_safe
          then Eliom_common.SAtt_csrf_safe
          else
            (match name with
               | None -> Eliom_common.SAtt_anon (new_state ())
               | Some name -> Eliom_common.SAtt_named name));
     };
   https = https;
   keep_nl_params = match keep_nl_params with 
     | None -> fallback.keep_nl_params | Some k -> k;
 }
(* It is not possible to make a new_post_coservice function
   with an optional ?fallback parameter
   because the type 'get of the result depends on the 'get of the
   fallback. Or we must impose 'get = unit ...
 *)


let new_post_coservice'
    ?name
    ?(csrf_safe = false)
    ?max_use ?timeout
    ?(https = false)
    ?(keep_nl_params = `All)
    ?(keep_get_na_params = true)
    ~post_params () =
  (* match Eliom_common.global_register_allowed () with
  | Some _ -> Eliom_common.add_unregistered None
  | _ -> () *)
  {
(*VVV allow timeout and max_use for named coservices? *)
    max_use= max_use;
    timeout= timeout;
    pre_applied_parameters = Ocsigen_lib.String_Table.empty, [];
    get_params_type = unit;
    post_params_type = post_params;
    kind = `Nonattached
      {na_name = 
          (if csrf_safe
           then Eliom_common.SNa_post_csrf_safe
           else
             (match name with
                | None ->
                    Eliom_common.SNa_post' (new_state ())
                | Some name -> Eliom_common.SNa_post_ name));
       na_kind = `Post keep_get_na_params;
      };
    https = https;
    keep_nl_params = keep_nl_params;
    delayed_get_or_na_registration_function = None;
    delayed_post_registration_function = None;
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
   {na_name = (fst fallback.na_name, Some (new_state ()));
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


  

(*****************************************************************************)
let set_exn_handler ?sp h =
  let sitedata = Eliom_sessions.find_sitedata "set_exn_handler" sp in
  Eliom_sessions.set_site_handler sitedata h

let add_service = Eliommod_services.add_service
let add_naservice = Eliommod_naservices.add_naservice

let eccookiel_of_escookiel = Ocsigen_lib.id
let escookiel_of_eccookiel = Ocsigen_lib.id


let set_delayed_get_or_na_registration_function s f =
  s.delayed_get_or_na_registration_function <- Some f

let set_delayed_post_registration_function s f =
  s.delayed_post_registration_function <- Some f
