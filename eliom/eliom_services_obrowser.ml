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
    [ `Internal of servcoserv
    | `External ]

type internal =
    [ `Internal of servcoserv ]

type registrable = [ `Registrable | `Unregistrable ]

type (+'a, +'b) a_s =
    {prefix: string; (* name of the server and protocol,
                        for external links. Ex: http://ocsigen.org *)
     subpath: Ocsigen_lib.url_path; (* name of the service without parameters *)
     fullpath: Ocsigen_lib.url_path; (* full path of the service = site_dir@subpath *)
     att_kind: 'a; (* < attached_service_kind *)
     get_or_post: 'b; (* < getpost *)
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
    [ `Attached of (attached_service_kind, getpost) a_s
    | `Nonattached of getpost na_s ]

type internal_service_kind =
    [ `Attached of (internal, getpost) a_s
    | `Nonattached of getpost na_s ]

type get_service_kind =
    [ `Attached of (attached_service_kind, [ `Get ]) a_s
    | `Nonattached of [ `Get ] na_s ]

type post_service_kind =
    [ `Attached of (attached_service_kind, [ `Post ]) a_s
    | `Nonattached of [ `Post ] na_s ]

type attached =
    [ `Attached of (attached_service_kind, getpost) a_s ]

type nonattached =
    [ `Nonattached of getpost na_s ]


type ('get,'post,+'kind,+'tipo,+'getnames,+'postnames,+'registr,+'return) service =
(* 'return is the value returned by the service *)
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

let get_get_or_post s =
  match get_kind_ s with
    | `Attached attser -> attser.get_or_post
    | `Nonattached { na_kind = `Post keep_get_na_param } -> `Post
    | _ -> `Get

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
         att_kind = `Internal `Service;
         get_or_post = `Get;
         redirect_suffix = true;
        };
     https = https;
     keep_nl_params = `None;
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
        att_kind = `Internal `Service;
        get_or_post = `Get;
        redirect_suffix = true;
      };
     https = https;
     keep_nl_params = keep_nl_params;
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



let register_delayed_get_or_na_coservice ~sp s =
  failwith "CSRF coservice not implemented in obrowser for now"

let register_delayed_post_coservice  ~sp s getname =
  failwith "CSRF coservice not implemented in obrowser for now"




(* external services *)
(** Create a main service (not a coservice) internal or external, get only *)
let new_service_aux_aux
    ~https
    ~prefix
    ~(path : Ocsigen_lib.url_path)
    ~site_dir
    ~kind
    ~getorpost
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
      get_or_post = getorpost;
      get_name = Eliom_common.SAtt_no;
      post_name = Eliom_common.SAtt_no;
      redirect_suffix = redirect_suffix
    };
   https = https;
   keep_nl_params = keep_nl_params;
 }


let new_external_service_
    ~prefix
    ~path
    ?keep_nl_params
    ~getorpost
    ~get_params
    ~post_params
    () =
  let suffix = Eliom_parameters.contains_suffix get_params in
  new_service_aux_aux
    ~https:false (* not used for external links *)
    ~prefix
    ~path:(remove_internal_slash
            (match suffix with
               | None -> path
               | _ -> path@[Eliom_common.eliom_suffix_internal_name]))
    ~site_dir:[]
    ~kind:`External
    ~getorpost
    ?keep_nl_params
    ~redirect_suffix:false
    ~get_params
    ~post_params

let new_external_post_service
    ~prefix
    ~path
    ?keep_nl_params
    ~get_params
    ~post_params
    () =
  new_external_service_
    ~prefix
    ~path
    ?keep_nl_params
    ~getorpost:`Post
    ~get_params
    ~post_params
    ()

let new_external_service
    ~prefix
    ~path
    ?keep_nl_params
    ~get_params
    () =
  new_external_service_
    ~prefix
    ~path
    ?keep_nl_params
    ~getorpost:`Get
    ~get_params
    ~post_params:Eliom_parameters.unit
    ()
