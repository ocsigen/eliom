(* Ocsigen
 * http://www.ocsigen.org
 * Module eliom_services_obrowser.ml
 * Copyright (C) 2007-2010 Vincent Balat
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

(* Manipulation of services - this code can be use on server or client side. *)

open Lwt
open Ocsigen_lib
open Lazy

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
     subpath: Ocsigen_lib.url_path; (* name of the service without parameters *)
     fullpath: Ocsigen_lib.url_path; (* full path of the service = site_dir@subpath *)
     att_kind: 'a; (* < attached_service_kind *)
     get_name: Eliom_common.att_key_serv;
     post_name: Eliom_common.att_key_serv;
     redirect_suffix: bool;
   }

type +'a na_s =
    {na_name: Eliom_common.na_key_serv;
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
     pre_applied_parameters: 
       (string * string) list Ocsigen_lib.String_Table.t
       (* non localized parameters *) *
       (string * string) list (* regular parameters *);
     get_params_type: ('get, 'tipo, 'getnames) Eliom_parameters.params_type;
     post_params_type: ('post, [`WithoutSuffix], 'postnames) Eliom_parameters.params_type;
     max_use: int option; (* Max number of use of this service *)
     timeout: float option; (* Timeout for this service (the service will
          disappear if it has not been used during this amount of seconds) *)
     kind: 'kind; (* < service_kind *)
     https: bool; (* force https *)
     keep_nl_params: [ `All | `Persistent | `None ];
     mutable delayed_get_or_na_registration_function: (unit -> string) option;
     mutable delayed_post_registration_function: 
       (Eliom_common.att_key_serv -> string) option;
     (* used for csrf safe services: 
        we register a new anonymous coservice
        with these functions each time we create a link or form.
        Attached POST coservices may have both a GET and POST 
        registration function.
     *)
   }

let get_kind_ s = s.kind
let get_att_kind_ s = s.att_kind
let get_pre_applied_parameters_ s = s.pre_applied_parameters
let get_get_params_type_ s = s.get_params_type
let get_post_params_type_ s = s.post_params_type
let get_prefix_ s = s.prefix
let get_sub_path_ s = s.subpath
let get_redirect_suffix_ s = s.redirect_suffix
let get_full_path_ s = s.fullpath
let get_get_name_ s = s.get_name
let get_post_name_ s = s.post_name
let get_na_name_ s = s.na_name
let get_na_kind_ s = s.na_kind
let get_max_use_ s = s.max_use
let get_timeout_ s = s.timeout
let get_https s = s.https

let change_get_num service attser n =
  {service with
     kind = `Attached {attser with
                         get_name = n}}

(** Satic directories **)
let static_dir_ ?(https = false) ~sp () =
    {
     pre_applied_parameters = Ocsigen_lib.String_Table.empty, [];
     get_params_type = Eliom_parameters.suffix 
        (Eliom_parameters.all_suffix Eliom_common.eliom_suffix_name);
     post_params_type = Eliom_parameters.unit;
     max_use= None;
     timeout= None;
     kind = `Attached
       {prefix = "";
        subpath = [""];
        fullpath = (Eliom_sessions.get_site_dir sp) @ 
           [Eliom_common.eliom_suffix_internal_name];
        get_name = Eliom_common.SAtt_no;
        post_name = Eliom_common.SAtt_no;
        att_kind = `Internal (`Service, `Get);
        redirect_suffix = true;
      };
     https = https;
     keep_nl_params = `None;
     delayed_get_or_na_registration_function = None;
     delayed_post_registration_function = None;
   }

let static_dir ~sp = static_dir_ ~sp ()

let https_static_dir ~sp = static_dir_ ~https:true ~sp ()

let get_static_dir_ ?(https = false) ~sp
    ?(keep_nl_params = `None) ~get_params () =
    {
     pre_applied_parameters = Ocsigen_lib.String_Table.empty, [];
     get_params_type = 
        Eliom_parameters.suffix_prod 
          (Eliom_parameters.all_suffix Eliom_common.eliom_suffix_name)
          get_params;
     post_params_type = Eliom_parameters.unit;
     max_use= None;
     timeout= None;
     kind = `Attached
       {prefix = "";
        subpath = [""];
        fullpath = (Eliom_sessions.get_site_dir sp) @ 
           [Eliom_common.eliom_suffix_internal_name];
        get_name = Eliom_common.SAtt_no;
        post_name = Eliom_common.SAtt_no;
        att_kind = `Internal (`Service, `Get);
        redirect_suffix = true;
      };
     https = https;
     keep_nl_params = keep_nl_params;
     delayed_get_or_na_registration_function = None;
     delayed_post_registration_function = None;
   }

let static_dir_with_params ~sp ?keep_nl_params ~get_params () = 
  get_static_dir_ ~sp ?keep_nl_params ~get_params ()

let https_static_dir_with_params ~sp ?keep_nl_params ~get_params () = 
  get_static_dir_ ~https:true ~sp ?keep_nl_params ~get_params ()


(****************************************************************************)
(****************************************************************************)


let rec append_suffix l m = match l with
  | [] -> m
  | [eliom_suffix_internal_name] -> m
  | a::ll -> a::(append_suffix ll m)

let preapply ~service getparams =
  let nlp, preapp = service.pre_applied_parameters in
  let suff, nlp, params =
    Eliom_parameters.construct_params_list_raw
      nlp service.get_params_type getparams 
  in
  {service with
   pre_applied_parameters = nlp, params@preapp;
   get_params_type = Eliom_parameters.unit;
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
    pre_applied_parameters = Ocsigen_lib.String_Table.empty, [];
    get_params_type = Eliom_parameters.unit;
    post_params_type = Eliom_parameters.unit;
    kind = `Nonattached
      {na_name = Eliom_common.SNa_void_dontkeep;
       na_kind = `Get;
      };
    https = false;
    keep_nl_params = `All;
    delayed_get_or_na_registration_function = None;
    delayed_post_registration_function = None;
  }

let https_void_coservice' =
  {
    max_use= None;
    timeout= None;
    pre_applied_parameters = Ocsigen_lib.String_Table.empty, [];
    get_params_type = Eliom_parameters.unit;
    post_params_type = Eliom_parameters.unit;
    kind = `Nonattached
      {na_name = Eliom_common.SNa_void_dontkeep;
       na_kind = `Get;
      };
    https = true;
    keep_nl_params = `All;
    delayed_get_or_na_registration_function = None;
    delayed_post_registration_function = None;
  }

let void_hidden_coservice' = {void_coservice' with 
                         kind = `Nonattached
    {na_name = Eliom_common.SNa_void_keep;
     na_kind = `Get;
    };
                      }

let https_void_hidden_coservice' = {void_coservice' with 
                         kind = `Nonattached
    {na_name = Eliom_common.SNa_void_keep;
     na_kind = `Get;
    };
                      }

let add_non_localized_get_parameters ~params ~service =
  {service with
     get_params_type = 
      Eliom_parameters.nl_prod service.get_params_type params
  }

let add_non_localized_post_parameters ~params ~service =
  {service with
     post_params_type = 
      Eliom_parameters.nl_prod service.post_params_type params
  }

let keep_nl_params s = s.keep_nl_params



exception Unregistered_CSRF_safe_coservice

let register_delayed_get_or_na_coservice s =
  match s.delayed_get_or_na_registration_function with
    | None -> raise Unregistered_CSRF_safe_coservice
    | Some f -> f ()

let register_delayed_post_coservice s getname =
  match s.delayed_post_registration_function with
    | None -> raise Unregistered_CSRF_safe_coservice
    | Some f -> f getname
