(* Ocsigen
 * http://www.ocsigen.org
 * Module ocsigen.ml
 * Copyright (C) 2005 Vincent Balat
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

open Http_frame
open Http_com
open Lwt
open Ocsimisc
open Extensions
open Ocsigenmod

let _ = Random.self_init ()

let get_config () = !Ocsigenmod.config

type current_url = Extensions.current_url
type url_path = Extensions.url_path
type server_params = Ocsigenmod.server_params

let get_user_agent (ri,_,_) = ri.ri_user_agent
let get_full_url (ri,_,_) = ri.ri_path_string^ri.ri_params
let get_ip (ri,_,_) = ri.ri_ip
let get_inet_addr (ri,_,_) = ri.ri_inet_addr
let get_get_params (ri,_,_) = ri.ri_get_params
let get_post_params (ri,_,_) = ri.ri_post_params
let get_current_url (ri,_,_) = ri.ri_path
let get_hostname (ri,_,_) = ri.ri_host
let get_port (ri,_,_) = ri.ri_port

let get_tmp_filename fi = fi.tmp_filename
let get_filesize fi = fi.filesize
let get_original_filename fi = fi.original_filename

let sync f sp g p = Lwt.return (f sp g p)
    
let counter = let c = ref (Random.int 1000000) in fun () -> c := !c + 1 ; !c

let new_state =
  let c : internal_state ref = ref (Random.int 1000000) in
  fun () -> c := !c + 1 ; Some !c


(** Type of names in a formular *)
type 'a param_name = string

type ('a,'b) binsum = Inj1 of 'a | Inj2 of 'b;;

(* This is a generalized algebraic datatype *)
type ('a,+'tipo,+'names) params_type =
    (* 'tipo is [`WithSuffix] or [`WithoutSuffix] *)
    TProd of (* 'a1 *) ('a,'tipo,'names) params_type * (* 'a2 *) ('a,'tipo,'names) params_type (* 'a = 'a1 * 'a2 ; 'names = 'names1 * 'names2 *)
  | TOption of (* 'a1 *) ('a,'tipo,'names) params_type (* 'a = 'a1 option *)
  | TList of 'a param_name * (* 'a1 *) ('a,'tipo,'names) params_type (* 'a = 'a1 list *)
  | TSum of (* 'a1 *) ('a,'tipo,'names) params_type * (* 'a2 *) ('a,'tipo,'names) params_type (* 'a = ('a1, 'a2) binsum *)
  | TString of string param_name (* 'a = string *)
  | TInt of int param_name (* 'a = int *)
  | TFloat of float param_name (* 'a = float *)
  | TBool of bool param_name (* 'a = bool *)
  | TFile of file_info param_name (* 'a = file_info *)
  | TUserType of ('a param_name * (string -> 'a) * ('a -> string)) (* 'a = 'a *)
  | TSuffix (* 'a = string *)
  | TUnit (* 'a = unit *);;

type 'an listnames = 
    {it:'el 'a. ('an -> 'el -> 'a list) -> 'el list -> 'a list -> 'a list}

(* As GADT are not implemented in OCaml for the while, we define our own
   constructors for params_type *)
let int (n : string) : (int,[`WithoutSuffix], int param_name) params_type = TInt n
let float (n : string) : (float,[`WithoutSuffix], float param_name) params_type = TFloat n
let bool (n : string) : (bool,[`WithoutSuffix], bool param_name) params_type= TBool n
let string (n : string) : (string,[`WithoutSuffix], string param_name) params_type = 
  TString n
let file (n : string) : (file_info ,[`WithoutSuffix], file_info param_name) params_type = 
  TFile n
let radio_answer (n : string) : (string option,[`WithoutSuffix], string option param_name) params_type= TString n
let unit : (unit,[`WithoutSuffix], unit param_name) params_type = TUnit
let user_type
    (of_string : string -> 'a) (from_string : 'a -> string) (n : string)
    : ('a,[`WithoutSuffix], 'a param_name) params_type =
  Obj.magic (TUserType (n,of_string,from_string))
let sum (t1 : ('a,[`WithoutSuffix], 'an) params_type) 
    (t2 : ('b,[`WithoutSuffix], 'bn) params_type) 
    : (('a,'b) binsum,[`WithoutSuffix], 'an * 'bn) params_type =
  Obj.magic (TSum (t1, t2))
let prod (t1 : ('a,[`WithoutSuffix], 'an) params_type) 
    (t2 : ('b,[`WithoutSuffix], 'bn) params_type)
    : (('a * 'b),[`WithoutSuffix], 'an * 'bn) params_type =
  Obj.magic (TProd ((Obj.magic t1), (Obj.magic t2)))
let opt (t : ('a,[`WithoutSuffix], 'an) params_type) 
    : ('a option,[`WithoutSuffix], 'an) params_type = 
  Obj.magic (TOption t)
let list (n : string) (t : ('a,[`WithoutSuffix], 'an) params_type) 
    : ('a list,[`WithoutSuffix], 'an listnames) params_type = 
  Obj.magic (TList (n,t))
let ( ** ) = prod

let suffix_only : (string, [`WithSuffix], string param_name) params_type = 
  (Obj.magic TSuffix)
let suffix (t : ('a,[`WithoutSuffix], 'an) params_type) : 
    ((string * 'a), [`WithSuffix], string param_name * 'an) params_type = 
  (Obj.magic (TProd (Obj.magic TSuffix, Obj.magic t)))

let make_list_suffix i = "["^(string_of_int i)^"]"

let add_to_string s1 sep = function
    "" -> s1
  | s2 -> s1^sep^s2

let concat_strings s1 sep s2 = match s1,s2 with
  _,"" -> s1
| "",_ -> s2
| _ -> s1^sep^s2

(* The following function reconstruct the value of parameters
   from expected type and GET or POST parameters *)
type 'a res_reconstr_param = 
    Res_ of ('a * 
               (string * string) list * 
               (string * file_info) list)
  | Errors_ of (string * exn) list
let reconstruct_params
    (typ : ('a,[<`WithSuffix|`WithoutSuffix],'b) params_type)
    params files urlsuffix : 'a = 
  let rec aux_list t params files name pref suff =
    let rec aa i lp fl pref suff =
      try 
        match aux t lp fl pref (suff^(make_list_suffix i)) with
          Res_ (v,lp2,f) ->
            (match aa (i+1) lp2 f pref suff with
              Res_ (v2,lp3,f2) -> Res_ ((Obj.magic (v::v2)),lp3,f2)
            | err -> err)
        | Errors_ errs ->
            (match aa (i+1) lp fl pref suff with
              Res_ _ -> Errors_ errs
            | Errors_ errs2 -> Errors_ (errs@errs2))
      with Not_found -> Res_ ((Obj.magic []),lp,files)
    in 
    aa 0 params files (pref^name^".") suff
  and aux (typ : ('a,[<`WithSuffix|`WithoutSuffix],'b) params_type)
      params files pref suff : 'a res_reconstr_param =
    match typ with
      TProd (t1, t2) ->
        (match aux t1 params files pref suff with
          Res_ (v1,l1,f) ->
            (match aux t2 l1 f pref suff with
              Res_ (v2,l2,f2) -> Res_ ((Obj.magic (v1,v2)),l2,f2)
            | err -> err)
        | Errors_ errs ->
            (match aux t2 params files pref suff with
              Res_ _ -> Errors_ errs
            | Errors_ errs2 -> Errors_ (errs2@errs)))
    | TOption t -> 
        (try 
          (match aux t params files pref suff with
            Res_ (v,l,f) -> Res_ ((Obj.magic (Some v)),l,f)
          | err -> err)
        with Not_found -> Res_ ((Obj.magic None), params,files))
    | TBool name -> 
        (try 
          let v,l = (list_assoc_remove (pref^name^suff) params) in
          Res_ ((Obj.magic true),l,files)
        with Not_found -> Res_ ((Obj.magic false), params, files))
    | TList (n,t) -> Obj.magic (aux_list t params files n pref suff)
    | TSum (t1, t2) -> 
        (try 
          match aux t1 params files pref suff with
            Res_ (v,l,files) -> Res_ ((Obj.magic (Inj1 v)),l,files)
          | err -> err
        with Not_found -> 
          (match aux t2 params files pref suff with
            Res_ (v,l,files) -> Res_ ((Obj.magic (Inj2 v)),l,files)
          | err -> err))
    | TString name -> 
        let v,l = list_assoc_remove (pref^name^suff) params in
        Res_ ((Obj.magic v),l,files)
    | TInt name -> 
        let v,l = (list_assoc_remove (pref^name^suff) params) in 
        (try (Res_ ((Obj.magic (int_of_string v)),l,files))
        with e -> Errors_ [(pref^name^suff),e])
    | TFloat name -> 
        let v,l = (list_assoc_remove (pref^name^suff) params) in 
        (try (Res_ ((Obj.magic (float_of_string v)),l,files))
        with e -> Errors_ [(pref^name^suff),e])
    | TFile name -> 
        let v,f = list_assoc_remove (pref^name^suff) files in
        Res_ ((Obj.magic v),params,f)
    | TUserType (name, of_string, string_of) ->
        let v,l = (list_assoc_remove (pref^name^suff) params) in 
        (try (Res_ ((Obj.magic (of_string v)),l,files))
        with e -> Errors_ [(pref^name^suff),e])
    | TUnit -> Res_ ((Obj.magic ()), params, files)
    | TSuffix -> raise (Ocsigen_Internal_Error "Bad use of suffix")
  in
  let aux2 typ =
    match Obj.magic (aux typ params files "" "") with
      Res_ (v,l,files) -> 
        if (l,files) = ([], [])
        then v
        else raise Ocsigen_Wrong_parameter
    | Errors_ errs -> raise (Ocsigen_Typing_Error errs)
  in
  try 
    match typ with
      TProd(TSuffix,t) -> Obj.magic ((string_of_url_path urlsuffix), aux2 t)
    | TSuffix -> Obj.magic (string_of_url_path urlsuffix)
    | _ -> Obj.magic (aux2 typ)
  with Not_found -> raise Ocsigen_Wrong_parameter

(* The following function takes a 'a params_type and a 'a and
   constructs the string of parameters (GET or POST) 
   (This is a marshalling function towards HTTP parameters format) *)
let construct_params (typ : ('a, [<`WithSuffix|`WithoutSuffix],'b) params_type)
    (params : 'a) : string * string =
  let rec aux typ params pref suff =
    match typ with
      TProd (t1, t2) ->
        let s1 = aux t1 (fst (Obj.magic params)) pref suff
        and s2 = aux t2 (snd (Obj.magic params)) pref suff in
        (concat_strings s1 "&" s2)
    | TOption t -> (match ((Obj.magic params) : 'zozo option) with None -> "" 
      | Some v -> aux t v pref suff)
    | TBool name -> 
        (if ((Obj.magic params) : bool)
        then pref^name^suff^"="^"on"
        else "")
    | TList (list_name, t) -> 
        let pref2 = pref^list_name^suff^"." in
        fst 
          (List.fold_left
             (fun (s,i) p -> 
               let ss = 
                 aux t p pref2 (suff^(make_list_suffix i)) in
               ((concat_strings s "&" ss),(i+1))) ("",0) (Obj.magic params))
    | TSum (t1, t2) -> (match Obj.magic params with
        Inj1 v -> aux t1 v pref suff
      | Inj2 v -> aux t2 v pref suff)
    | TString name -> pref^name^suff^"="^(Obj.magic params)
    | TInt name -> pref^name^suff^"="^(string_of_int (Obj.magic params))
    | TFloat name -> pref^name^suff^"="^(string_of_float (Obj.magic params))
    | TFile name -> 
        raise (Failure
                 "Constructing an URL with file parameters not implemented")
    | TUserType (name, of_string, string_of) ->
        pref^name^suff^"="^(string_of (Obj.magic params))
    | TUnit -> ""
    | TSuffix -> raise (Ocsigen_Internal_Error "Bad use of suffix")
  in
  match typ with
    TProd(TSuffix,t) ->
      (fst (Obj.magic params)),(aux t (snd (Obj.magic params)) "" "")
  | TSuffix -> (Obj.magic params),""
  | _ -> "",(aux typ params "" "")




(** Typed services *)
type internal_service_kind = [`Public_Service | `Local_Service]
type service_kind = 
    [ `Internal_Service of internal_service_kind
    | `External_Service]

type ('get,'post,+'kind,+'tipo,+'getnames,+'postnames) service = 
    {url: url_path; (* name of the service without parameters *)
        (* unique_id is here only for registrering on top of this service *)
     unique_id: int;
     url_prefix: bool;
     external_service: bool;
     url_state: internal_state option;
     (* 'kind is just a type information: it can be only 
        `Internal_Service `Public_Service or `Internal_Service `Local_Service
        or `External_Service, so that we can't use session services as fallbacks for
        other session services. If it is a session service, it contains a value
        (internal state) that will allow to differenciate between
        services that have the same url.
      *)
     get_params_type: ('get,'tipo,'getnames) params_type;
     post_params_type: ('post,[`WithoutSuffix],'postnames) params_type;
   }




(*****************************************************************************)
(*****************************************************************************)
(* Page registration, handling of links and forms                            *)
(*****************************************************************************)
(*****************************************************************************)

module type PAGES = 
  sig
(** Type of answers from modules (web pages) *)
    type page
    type form_content_elt
    type form_content_elt_list
    type form_elt
    type a_content_elt
    type a_content_elt_list
    type a_elt
    type a_elt_list
    type div_content_elt
    type div_content_elt_list
    type uri
    type link_elt
    type script_elt
    type textarea_elt
    type input_elt
    type pcdata_elt

    val create_sender : Predefined_senders.create_sender_type
    val send : content:page -> Predefined_senders.send_page_type

    type a_attrib_t
    type form_attrib_t
    type input_attrib_t
    type textarea_attrib_t
    type link_attrib_t
    type script_attrib_t
    type input_type_t

    val hidden : input_type_t
    val text : input_type_t
    val password : input_type_t
    val checkbox : input_type_t
    val radio : input_type_t
    val submit : input_type_t
    val file : input_type_t

    val empty_seq : form_content_elt_list
    val cons_form : form_content_elt -> form_content_elt_list -> form_content_elt_list 

    val make_a : ?a:a_attrib_t -> href:string -> a_content_elt_list -> a_elt
    val make_get_form : ?a:form_attrib_t -> 
      action:string -> 
        form_content_elt -> form_content_elt_list -> form_elt
    val make_post_form : ?a:form_attrib_t ->
      action:string -> ?id:string -> ?inline:bool -> 
        form_content_elt -> form_content_elt_list -> form_elt
    val make_hidden_field : input_elt -> form_content_elt
    val remove_first : form_content_elt_list -> form_content_elt * form_content_elt_list
    val make_input : ?a:input_attrib_t -> ?checked:bool ->
      typ:input_type_t -> ?name:string -> 
        ?value:string -> unit -> input_elt
    val make_textarea : ?a:textarea_attrib_t -> 
      name:string -> rows:int -> cols:int ->
        pcdata_elt -> 
          textarea_elt
    val make_div : classe:(string list) -> a_elt -> form_content_elt
    val make_uri_from_string : string -> uri


    val make_css_link : ?a:link_attrib_t -> uri -> link_elt

    val make_js_script : ?a:script_attrib_t -> uri -> script_elt

  end

module type OCSIGENSIG =
  sig
    type page
    type form_content_elt
    type form_content_elt_list
    type form_elt
    type a_content_elt
    type a_content_elt_list
    type a_elt
    type a_elt_list
    type div_content_elt
    type div_content_elt_list
    type uri
    type link_elt
    type script_elt
    type textarea_elt
    type input_elt
    type pcdata_elt
          
    type a_attrib_t
    type form_attrib_t
    type input_attrib_t
    type textarea_attrib_t
    type link_attrib_t
    type script_attrib_t
    type input_type_t

    val new_external_service :
        url:url_path ->
          ?prefix:bool ->
            get_params:('a, [< `WithSuffix | `WithoutSuffix ] as 'b, 'c)
              params_type ->
                post_params:('d, [ `WithoutSuffix ], 'e) params_type ->
                  unit -> ('a, 'd, [ `External_Service ], 'b, 'c, 'e) service
    val new_service :
        url:url_path ->
          ?prefix:bool ->
            get_params:('a, [< `WithSuffix | `WithoutSuffix ] as 'b, 'c)
              params_type ->
                unit ->
                  ('a, unit, [ `Internal_Service of [ `Public_Service ] ], 'b, 'c,
                   unit param_name)
                    service
    val new_auxiliary_service :
        fallback:('a, unit, [ `Internal_Service of [ `Public_Service ] ], 'b,
                  'c, 'd)
        service ->
          ('a, unit, [ `Internal_Service of [ `Local_Service ] ], 'b, 'c, 'd)
            service
    val register_service :
        service:('a, 'b, [ `Internal_Service of 'c ],
                 [< `WithSuffix | `WithoutSuffix ], 'd, 'e) service ->
                   ?error_handler:(server_params -> (string * exn) list -> page Lwt.t) ->
                     (server_params -> 'a -> 'b -> page Lwt.t) -> unit
    val register_service_for_session :
        server_params ->
          service:('a, 'b, [ `Internal_Service of 'c ],
                   [< `WithSuffix | `WithoutSuffix ], 'd, 'e)
            service ->
              ?error_handler:(server_params -> (string * exn) list -> page Lwt.t) ->
                (server_params -> 'a -> 'b -> page Lwt.t) -> unit
    val register_new_service :
        url:url_path ->
          ?prefix:bool ->
            get_params:('a, [< `WithSuffix | `WithoutSuffix ] as 'b, 'c)
              params_type ->
                ?error_handler:(server_params -> (string * exn) list -> page Lwt.t) ->
                  (server_params -> 'a -> unit -> page Lwt.t) ->
                    ('a, unit, [ `Internal_Service of [ `Public_Service ] ], 'b, 'c,
                     unit param_name)
                      service
    val register_new_auxiliary_service :
        fallback:('a, unit, [ `Internal_Service of [ `Public_Service ] ],
                  [< `WithSuffix | `WithoutSuffix ] as 'b, 'c, 'd)
        service ->
          ?error_handler:(server_params -> (string * exn) list -> page Lwt.t) ->
            (server_params -> 'a -> unit -> page Lwt.t) ->
              ('a, unit, [ `Internal_Service of [ `Local_Service ] ], 'b, 'c, 'd)
                service
    val register_new_auxiliary_service_for_session :
        server_params ->
          fallback:('a, unit, [ `Internal_Service of [ `Public_Service ] ],
                    [< `WithSuffix | `WithoutSuffix ] as 'b, 'c, 'd)
            service ->
              ?error_handler:(server_params -> (string * exn) list -> page Lwt.t) ->
                (server_params -> 'a -> unit -> page Lwt.t) ->
                  ('a, unit, [ `Internal_Service of [ `Local_Service ] ], 'b, 'c, 'd)
                    service
    val new_post_service :
        fallback:('a, unit, [ `Internal_Service of [ `Public_Service ] ], 'b,
                  'c, unit param_name)
        service ->
          post_params:('d, [ `WithoutSuffix ], 'e) params_type ->
            ('a, 'd, [ `Internal_Service of [ `Public_Service ] ], 'b, 'c, 'e)
              service
    val new_post_auxiliary_service :
        fallback:('a, 'b, [ `Internal_Service of [ `Public_Service ] ], 'c,
                  'd, 'e)
        service ->
          post_params:('f, [ `WithoutSuffix ], 'g) params_type ->
            ('a, 'f, [ `Internal_Service of [ `Local_Service ] ], 'c, 'd, 'g)
              service
    val register_new_post_service :
        fallback:('a, unit, [ `Internal_Service of [ `Public_Service ] ],
                  [< `WithSuffix | `WithoutSuffix ] as 'b, 'c,
                  unit param_name)
        service ->
          post_params:('d, [ `WithoutSuffix ], 'e) params_type ->
            ?error_handler:(server_params -> (string * exn) list -> page Lwt.t) ->
              (server_params -> 'a -> 'd -> page Lwt.t) ->
                ('a, 'd, [ `Internal_Service of [ `Public_Service ] ], 'b, 'c, 'e)
                  service
    val register_new_post_auxiliary_service :
        fallback:('a, 'b, [ `Internal_Service of [ `Public_Service ] ],
                  [< `WithSuffix | `WithoutSuffix ] as 'c, 'd, 'e)
        service ->
          post_params:('f, [ `WithoutSuffix ], 'g) params_type ->
            ?error_handler:(server_params -> (string * exn) list -> page Lwt.t) ->
              (server_params -> 'a -> 'f -> page Lwt.t) ->
                ('a, 'f, [ `Internal_Service of [ `Local_Service ] ], 'c, 'd, 'g)
                  service
    val register_new_post_auxiliary_service_for_session :
        server_params ->
          fallback:('a, 'b, [ `Internal_Service of [ `Public_Service ] ],
                    [< `WithSuffix | `WithoutSuffix ] as 'c, 'd, 'e)
            service ->
              post_params:('f, [ `WithoutSuffix ], 'g) params_type ->
                ?error_handler:(server_params -> (string * exn) list -> page Lwt.t) ->
                  (server_params -> 'a -> 'f -> page Lwt.t) ->
                    ('a, 'f, [ `Internal_Service of [ `Local_Service ] ], 'c, 'd, 'g)
                      service
    type ('a, 'b) action
    val new_action :
        post_params:('a, [ `WithoutSuffix ], 'b) params_type ->
          ('a, 'b) action
    val register_action :
        action:('a, 'b) action -> (server_params -> 'a -> unit Lwt.t) -> unit
    val register_new_action :
        post_params:('a, [ `WithoutSuffix ], 'b) params_type ->
          (server_params -> 'a -> unit Lwt.t) -> ('a, 'b) action
    val register_action_for_session :
        server_params ->
          action:('a, 'b) action ->
            (server_params -> 'a -> unit Lwt.t) -> unit
    val register_new_action_for_session :
        server_params ->
          post_params:('a, [ `WithoutSuffix ], 'b) params_type ->
            (server_params -> 'a -> unit Lwt.t) ->
              ('a, 'b) action
    val static_dir :
        server_params ->
          (string, unit, [ `Internal_Service of [ `Public_Service ] ],
           [ `WithSuffix ], string param_name, unit param_name)
            service
    val close_session : server_params -> unit
    val a :
        ?a:a_attrib_t ->
          ('a, unit, 'b, [< `WithSuffix | `WithoutSuffix ], 'c, 'd) service ->
            server_params -> a_content_elt_list -> 'a -> a_elt
    val get_form :
        ?a:form_attrib_t ->
          ('a, unit, 'b, 'c, 'd, unit param_name) service ->
            server_params ->
              ('d -> form_content_elt_list) -> form_elt
    val post_form :
        ?a:form_attrib_t ->
          ('a, 'b, 'c, [< `WithSuffix | `WithoutSuffix ], 'd, 'e) service ->
            server_params ->
              ('e -> form_content_elt_list) -> 'a -> form_elt
    val make_uri :
        ('a, unit, 'b, [< `WithSuffix | `WithoutSuffix ], 'c, 'd) service ->
          server_params -> 'a -> uri
    val action_a :
        ?a:a_attrib_t ->
          ?reload:bool ->
            ('a, 'b) action ->
              server_params -> a_content_elt_list -> form_elt
    val action_form :
        ?a:form_attrib_t ->
          ?reload:bool ->
            ('a, 'b) action ->
              server_params ->
                ('b -> form_content_elt_list) -> form_elt
    val js_script :
        ?a:script_attrib_t -> uri -> script_elt
    val css_link : ?a:link_attrib_t -> uri -> link_elt

    val int_input :
        ?a:input_attrib_t -> ?value:int -> int param_name -> input_elt
    val float_input :
        ?a:input_attrib_t -> ?value:float -> float param_name -> input_elt
    val string_input :
        ?a:input_attrib_t -> ?value:string -> string param_name -> input_elt
    val user_type_input :
        ?a:input_attrib_t -> ?value:'a -> ('a -> string) -> 
          'a param_name -> input_elt
    val int_password_input :
        ?a:input_attrib_t -> ?value:int -> int param_name -> input_elt
    val float_password_input :
        ?a:input_attrib_t -> ?value:float -> float param_name -> input_elt
    val string_password_input :
        ?a:input_attrib_t -> ?value:string -> string param_name -> input_elt
    val user_type_password_input :
        ?a:input_attrib_t -> ?value:'a -> ('a -> string) -> 
          'a param_name -> input_elt
    val hidden_int_input :
        ?a:input_attrib_t -> int param_name -> int -> input_elt
    val hidden_float_input :
        ?a:input_attrib_t -> float param_name -> float -> input_elt
    val hidden_string_input :
        ?a:input_attrib_t -> string param_name -> string -> input_elt
    val hidden_user_type_input :
        ?a:input_attrib_t -> ('a -> string) -> 'a param_name -> 'a -> input_elt
    val bool_checkbox :
        ?a:input_attrib_t -> ?checked:bool -> bool param_name -> input_elt
    val string_radio :
        ?a:input_attrib_t -> ?checked:bool -> 
          string option param_name -> string -> input_elt
    val int_radio :
        ?a:input_attrib_t -> ?checked:bool -> 
           int option param_name -> int -> input_elt
    val float_radio :
        ?a:input_attrib_t -> ?checked:bool -> 
           float option param_name -> float -> input_elt
    val user_type_radio :
        ?a:input_attrib_t -> ?checked:bool -> ('a -> string) ->
           'a option param_name -> 'a -> input_elt
    val textarea :
        ?a:textarea_attrib_t ->
          string param_name ->
            rows:int -> cols:int -> pcdata_elt -> textarea_elt
    val submit_input : ?a:input_attrib_t -> string -> input_elt
    val file_input : ?a:input_attrib_t -> ?value:string -> 
                            file_info param_name-> input_elt
  end


module Make = functor
  (Pages : PAGES) ->
    (struct

      type page = Pages.page
      type form_content_elt = Pages.form_content_elt
      type form_content_elt_list = Pages.form_content_elt_list
      type form_elt = Pages.form_elt
      type a_content_elt = Pages.a_content_elt
      type a_content_elt_list = Pages.a_content_elt_list
      type a_elt = Pages.a_elt
      type a_elt_list = Pages.a_elt_list
      type div_content_elt = Pages.div_content_elt
      type div_content_elt_list = Pages.div_content_elt_list
      type uri = Pages.uri
      type link_elt = Pages.link_elt
      type script_elt = Pages.script_elt
      type textarea_elt = Pages.textarea_elt
      type input_elt = Pages.input_elt
      type pcdata_elt = Pages.pcdata_elt
            
      type a_attrib_t = Pages.a_attrib_t
      type form_attrib_t = Pages.form_attrib_t
      type input_attrib_t = Pages.input_attrib_t
      type textarea_attrib_t = Pages.textarea_attrib_t
      type link_attrib_t = Pages.link_attrib_t
      type script_attrib_t = Pages.script_attrib_t
      type input_type_t = Pages.input_type_t

(** Create a service *)
      let new_service_aux_aux
          ~(url : url_path)
          ~prefix
          ~external_service
          ~(get_params : ('get,[<`WithoutSuffix|`WithSuffix] as 'tipo,'gn) params_type)
          ~(post_params : ('post,[`WithoutSuffix],'pn) params_type)
          : ('get,'post,'kind,'tipo,'gn,'pn) service =
(* ici faire une vérification "duplicate parameter" ? *) 
        {url = url;
         unique_id = counter ();
         url_prefix = prefix;
         url_state = None;
         external_service = external_service;
         get_params_type = get_params;
         post_params_type = post_params;
       }

      let new_service_aux
          ~(url : url_path)
          ~prefix
          ~(get_params : ('get,[<`WithoutSuffix|`WithSuffix] as 'tipo,'gn) params_type)
          : ('get,unit,[`Internal_Service of 'popo],'tipo,'gn,unit param_name) service =
        if global_register_allowed () then
          let _,curdir = get_current_hostdir () in
          let full_path = curdir@(change_empty_list url) in
          let u = new_service_aux_aux
              ~url:full_path
              ~prefix
              ~external_service:false
              ~get_params
              ~post_params:unit
          in
          add_unregistered (u.url,u.unique_id); u
        else raise Ocsigen_service_or_action_created_outside_site_loading

      let new_external_service
          ~(url : url_path)
          ?(prefix=false)
          ~(get_params : ('get,[<`WithoutSuffix|`WithSuffix] as 'tipo,'gn) params_type)
          ~(post_params : ('post,[`WithoutSuffix],'pn) params_type)
          ()
          : ('get,'post,[`External_Service],'tipo,'gn,'pn) service =
        new_service_aux_aux
          ~url
          ~prefix
          ~external_service:true
          ~get_params 
          ~post_params

      let new_service
          ~(url : url_path)
          ?(prefix=false)
          ~(get_params : ('get,[<`WithoutSuffix|`WithSuffix] as 'tipo,'gn) params_type)
          ()
          : ('get,unit,[`Internal_Service of [`Public_Service]],'tipo,'gn, unit param_name) service =
        new_service_aux ~url ~prefix ~get_params

      let new_auxiliary_service
          ~(fallback : ('get,unit, [`Internal_Service of [`Public_Service]],'tipo,'gn,'pn) service)
          : ('get,unit,[`Internal_Service of [`Local_Service]],'tipo,'gn,'pn) service =
        {fallback with url_state = new_state ()}

      let register_service_aux
          current_dir
          tables
          session
          state
          ~(service : ('get,'post,[`Internal_Service of 'popo],'tipo,'gn,'pn) service)
          ?(error_handler = fun sp l -> raise (Ocsigen_Typing_Error l))
          (page_generator : server_params -> 'get -> 'post -> page Lwt.t) =
        add_service tables current_dir session service.url 
          Pages.create_sender
          ({prefix = service.url_prefix;
            state = state},
           (service.unique_id,
            (fun (suff,((ri,_,_) as h)) -> 
              (catch (fun () -> 
                (page_generator h 
                   (reconstruct_params 
                      service.get_params_type
                      ri.ri_get_params
                      []
                      suff)
                   (reconstruct_params
                      service.post_params_type
                      ri.ri_post_params
                      ri.ri_files
                      suff)))
                 (function
                     Ocsigen_Typing_Error l -> error_handler h l
                   | e -> fail e)) >>=
              (fun c -> return (Pages.send ~content:c)))))
      
      let register_service 
          ~(service : ('get,'post,[`Internal_Service of 'g],'tipo,'gn,'pn) service)
          ?error_handler
          (page_gen : server_params -> 'get -> 'post -> page Lwt.t) =
        if global_register_allowed () then begin
          remove_unregistered (service.url,service.unique_id);
          let (globtables,_),curdir = get_current_hostdir () in
          register_service_aux 
            curdir
            globtables
            false service.url_state
            ~service ?error_handler page_gen; 
        end
        else Messages.warning ("URL .../"^
                               (string_of_url_path service.url)^
                               " : Public service registration outside <site></site> or after init forbidden! Please correct your module! (ignored)")

(* WARNING: if we create a new service without registering it,
   we can have a link towards a page that does not exist!!! :-(
   That's why I impose to register all service during init.
   The only other way I see to avoid this is to impose a syntax extension
   like "let rec" for service...
 *)

      let register_service_for_session
          (ri,curdir,sesstab)
          ~(service : ('get,'post,[`Internal_Service of 'g],'tipo,'gn,'pn) 
              service)
           ?error_handler
          page =
        register_service_aux ?error_handler curdir
          !sesstab
          true service.url_state ~service page

      let register_new_service 
          ~url
          ?(prefix=false)
          ~(get_params : ('get,[<`WithoutSuffix|`WithSuffix] as 'tipo,'gn) params_type)
          ?error_handler
          page
          : ('get,unit, [`Internal_Service of [`Public_Service]],'tipo,'gn,unit param_name) service =
        let u = new_service ~prefix ~url ~get_params () in
        register_service ~service:u ?error_handler page;
        u
          
      let register_new_auxiliary_service
          ~(fallback : ('get, unit, [`Internal_Service of [`Public_Service]],'tipo,'gn,'pn) service)         
          ?error_handler
          page
          : ('get, unit, [`Internal_Service of [`Local_Service]],'tipo,'gn,'pn) service =
        let u = (new_auxiliary_service fallback) in
        register_service ~service:u ?error_handler page;
        u

      let register_new_auxiliary_service_for_session
          sp
          ~(fallback : ('get, unit, [`Internal_Service of [`Public_Service]],'tipo,'gn,'pn) service)
          ?error_handler
          page
          : ('get, unit, [`Internal_Service of [`Local_Service]],'tipo,'gn,'pn) service =
        let u = (new_auxiliary_service fallback) in
        register_service_for_session sp ~service:u ?error_handler page;
        u


(** Register an service with post parameters in the server *)
      let new_post_service_aux
          ~(fallback : ('get, unit, [`Internal_Service of [`Public_Service]],'tipo,'gn,unit param_name) service)
          ~(post_params : ('post,[`WithoutSuffix],'pn) params_type)
          : ('get, 'post, [`Internal_Service of [`Public_Service]], 'tipo,'gn,'pn) service = 
(* ici faire une vérification "duplicate parameter" ? *) 
        {url = fallback.url;
         unique_id = counter ();
         url_prefix = fallback.url_prefix;
         external_service = false;
         url_state = None;
         get_params_type = fallback.get_params_type;
         post_params_type = post_params;
       }

      let new_post_service
          ~(fallback : ('get, unit, [`Internal_Service of [`Public_Service]],'tipo,'gn,unit param_name) service)
          ~(post_params : ('post,[`WithoutSuffix],'pn) params_type)
          : ('get, 'post, [`Internal_Service of [`Public_Service]],'tipo,'gn,'pn) service = 
        if global_register_allowed () then
          let u = new_post_service_aux fallback post_params in
          add_unregistered (u.url,u.unique_id); u
        else raise Ocsigen_service_or_action_created_outside_site_loading
            
      let new_post_auxiliary_service
          ~(fallback : ('get, 'post1, [`Internal_Service of [`Public_Service]],'tipo,'gn,'pn1) service)
          ~(post_params : ('post,[`WithoutSuffix],'pn2) params_type)
          : ('get, 'post, [`Internal_Service of [`Local_Service]],'tipo,'gn,'pn2) service = 
        {fallback with 
         url_state = new_state ();
         post_params_type = post_params;
       }

      let register_new_post_service 
          ~(fallback : ('get, unit, [`Internal_Service of [`Public_Service]],'tipo,'gn,unit param_name) service)
          ~(post_params : ('post,[`WithoutSuffix],'pn) params_type)
          ?error_handler
          (page_gen : server_params -> 'get -> 'post -> 'fin)
          : ('get,'post, [`Internal_Service of [`Public_Service]],'tipo,'gn,'pn) service =
        let u = new_post_service ~fallback:fallback ~post_params:post_params in
        register_service ~service:u ?error_handler page_gen;
        u

      let register_new_post_auxiliary_service
          ~(fallback : ('get, 'post1, [`Internal_Service of [`Public_Service]],'tipo,'gn,'pn1) service)
          ~(post_params : ('post,[`WithoutSuffix],'pn) params_type)
          ?error_handler
          page_gen
          : ('get, 'post, [`Internal_Service of [`Local_Service]],'tipo,'gn,'pn) service = 
        let u = new_post_auxiliary_service ~fallback:fallback ~post_params:post_params in
        register_service ~service:u ?error_handler page_gen;
        u

      let register_new_post_auxiliary_service_for_session
          sp
          ~(fallback : ('get, 'post1, [`Internal_Service of [`Public_Service]],'tipo,'gn,'pn1) service)
          ~(post_params : ('post,[`WithoutSuffix],'pn) params_type)
          ?error_handler
          page_gen
          : ('get, 'post, [`Internal_Service of [`Local_Service]],'tipo,'gn,'pn) service = 
        let u = new_post_auxiliary_service ~fallback:fallback ~post_params:post_params in
        register_service_for_session sp ~service:u ?error_handler page_gen;
        u


(** actions (new 10/05) *)
      type ('post,'pn) action =
          {action_name: string;
           action_params_type: ('post,[`WithoutSuffix],'pn) params_type}

      let new_action_name () = string_of_int (counter ())

      let new_action
          ~(post_params : ('post,[`WithoutSuffix],'pn) params_type) =
        {
         action_name = new_action_name ();
         action_params_type = post_params;
       }

      let register_action_aux
          current_dir tables ~action actionfun =
        add_action tables current_dir 
          action.action_name
          (fun ((ri,_,_) as h) -> actionfun h
              (reconstruct_params 
                 action.action_params_type 
                 ri.ri_post_params
                 ri.ri_files []))

      let register_action
          ~(action : ('post,'pn) action)
          (actionfun : (server_params -> 'post -> unit Lwt.t)) : unit =
        if global_register_allowed () then
          (* if during_initialisation () then *)
          let (globtables,_),curdir = get_current_hostdir () in
          register_action_aux curdir globtables action actionfun
        else raise Ocsigen_service_or_action_created_outside_site_loading

      let register_new_action ~post_params actionfun = 
        let a = new_action post_params in
        register_action a actionfun;
        a

      let register_action_for_session (ri,curdir,sesstab) ~action actionfun =
        register_action_aux curdir
          !sesstab action actionfun

      let register_new_action_for_session sp ~post_params actionfun =
        let a = new_action post_params in
        register_action_for_session sp a actionfun;
        a

(** Satic directories **)
      let static_dir (ri,curdir,sesstab) : 
          (string, unit, [`Internal_Service of [`Public_Service]],[`WithSuffix],string param_name, unit param_name) service =
        {url = curdir;
         unique_id = counter ();
         url_state = None;
         url_prefix = true;
         external_service = false;
         get_params_type = suffix_only;
         post_params_type = unit
       }



(** Close a session *)
      let close_session (_,_,sesstab) = sesstab := empty_tables ()


(** Functions to construct web pages: *)

      let a ?a
          (service : ('get, unit, 'kind, 'tipo,'gn,'pn) service) 
          (sp : server_params) content
          (getparams : 'get) =
        let suff,params_string = construct_params service.get_params_type getparams in
        let suff = (if service.url_prefix then Some suff else None) in
        let uri = 
          (if service.external_service 
          then 
            (reconstruct_absolute_url_path
               (get_current_url sp) service.url suff)
          else 
            (reconstruct_relative_url_path
               (get_current_url sp) service.url suff))
        in
        match service.url_state with
          None ->
            Pages.make_a ?a ~href:(add_to_string uri "?" params_string) content
        | Some i -> 
            Pages.make_a ?a
              ~href:(add_to_string 
                       (uri^"?"^state_param_name^"="^(string_of_int i))
                       "&" params_string)
              content

(* avec un formulaire caché (ça marche mais ce n'est pas du xhtml valide
   let stateparam = string_of_int i in
   let formname="hiddenform"^(string_of_int (counter ())) in
   let href="javascript:document."^formname^".submit ()" in
   << <a href=$href$>$str:name$<form name=$formname$ method="post" action=$v$ style="display:none">
   <input type="hidden" name=$state_param_name$
   value=$stateparam$/>
   </form></a> >>) *)

(*            let stateparam = string_of_int i in
   << <form name="hiddenform" method="post" action=$v$>
   <input type="hidden" name=$state_param_name$
   value=$stateparam$/>
   <a href="javascript:document.hiddenform.submit ()">$str:name$</a>
   </form> >>)

   À VOIR ! IMPORTANT ! :

   Pour les form get on peut faire pareil, du style :
   <input type="button"
   onClick="document.form1.submit();document.form2.submit()">
   (problème : on n'a pas accès au bouton)

 *)

      let make_params_names (params : ('t,'tipo,'n) params_type) : 'n =
        let rec aux prefix suffix = function
            TProd (t1, t2) -> Obj.magic (aux prefix suffix t1, aux prefix suffix t2)
          | TInt name -> Obj.magic (prefix^name^suffix)
          | TFloat name -> Obj.magic (prefix^name^suffix)
          | TString name -> Obj.magic (prefix^name^suffix)
          | TFile name -> Obj.magic (prefix^name^suffix)
          | TUserType (name,o,t) -> Obj.magic (prefix^name^suffix)
          | TUnit -> Obj.magic ("")
          | TSuffix -> Obj.magic ocsigen_suffix_name
          | TOption t -> Obj.magic (aux prefix suffix t)
          | TBool name -> Obj.magic (prefix^name^suffix)
          | TSum (t1,t2) -> Obj.magic (aux prefix suffix t1, aux prefix suffix t2)
          | TList (name,t1) -> Obj.magic 
                {it =
                 (fun f l endlist ->
                   let length = List.length l in
                   snd
                     (List.fold_right 
                        (fun el (i,l2) -> 
                          let i'= i-1 in
                          (i',(f (aux (prefix^name^".") (make_list_suffix i') t1) el)
                           @l2))
                        l
                        (length,endlist)))}
        in aux "" "" params
          
      let get_form ?a
          (service : ('get,unit,'kind,'tipo,'gn,unit param_name) service) 
          (sp : server_params)
          (f : 'gn -> Pages.form_content_elt_list) =
        let urlname =
          (if service.external_service
          then (reconstruct_absolute_url_path
                  (get_current_url sp) service.url None)
          else (reconstruct_relative_url_path
                  (get_current_url sp) service.url None)) in
        let state_param =
          (match  service.url_state with
            None -> None
          | Some i -> 
              let i' = string_of_int i in
              Some (Pages.make_input ~typ:Pages.hidden
                      ~name:state_param_name ~value:i' ()))
        in
        let inside = f (make_params_names service.get_params_type) in
        let i1, i =
          match state_param, inside with
            Some s, i -> (Pages.make_hidden_field s),i
          | None, i -> Pages.remove_first i
        in Pages.make_get_form ?a ~action:urlname i1 i

      let post_form ?a
          (service : ('get,'form,'kind,'tipo,'gn,'pn) service) 
          (sp : server_params)
          (f : 'pn -> Pages.form_content_elt_list) (getparams : 'get) =
        let suff,params_string = construct_params service.get_params_type getparams in
        let suff = (if service.url_prefix then Some suff else None) in
        let urlname = 
          (if service.external_service 
          then (reconstruct_absolute_url_path
                  (get_current_url sp) service.url suff)
          else (reconstruct_relative_url_path
                  (get_current_url sp) service.url suff))
        in
        let state_param =
          (match  service.url_state with
            None -> None
          | Some i -> 
              let i' = string_of_int i in
              Some (Pages.make_input ~typ:Pages.hidden
                      ~name:state_param_name ~value:i' ()))
        in
        let inside = f (make_params_names service.post_params_type) in
        let i1, i =
          match state_param, inside with
            Some s, i -> (Pages.make_hidden_field s),i
          | None, i -> Pages.remove_first i
        in Pages.make_post_form ?a
          ~action:(add_to_string urlname "?" params_string)
          i1 i

      let make_uri 
          (service : ('get, unit, 'kind, 'tipo,'gn,'pn) service) sp
          (getparams : 'get) : Pages.uri =
        let suff,params_string = construct_params service.get_params_type getparams in
        let suff = (if service.url_prefix then Some suff else None) in
        let uri = 
          (if service.external_service 
          then (reconstruct_absolute_url_path
                  (get_current_url sp) service.url suff)
          else (reconstruct_relative_url_path
                  (get_current_url sp) service.url suff))
        in
        match service.url_state with
          None ->
            Pages.make_uri_from_string (add_to_string uri "?" params_string)
        | Some i -> 
            Pages.make_uri_from_string 
              (add_to_string (uri^"?"^state_param_name^"="^(string_of_int i))
                 "&" params_string)

(* actions : *)
      let action_a ?a ?(reload=true) action h content =
        let formname="hiddenform"^(string_of_int (counter ())) in
        let href="javascript:document.getElementById(\""^formname^"\").submit ()" in
        let action_param_name = action_prefix^action_name in
        let action_param = (action.action_name) in
        let reload_name = action_prefix^action_reload in
        let reload_param = 
          if reload 
          then 
	    Pages.cons_form
	      (Pages.make_hidden_field
                 (Pages.make_input ~typ:Pages.hidden
                    ~name:reload_name ~value:reload_name ()))
	      Pages.empty_seq
          else Pages.empty_seq in
        let v = get_full_url h in
        Pages.make_post_form ~inline:true 
          ~id:formname ~action:v
          (Pages.make_div ~classe:["inline"]
             (Pages.make_a ?a ~href:href content))
          (Pages.cons_form
	   (Pages.make_hidden_field
              (Pages.make_input ~typ:Pages.hidden ~name:action_param_name
                 ~value:action_param ()))
           reload_param)
          
      let action_form ?a
          ?(reload=true) (action : ('a,'pn) action) h 
          (f : 'pn -> Pages.form_content_elt_list) = 
        let action_param_name = action_prefix^action_name in
        let action_param = (action.action_name) in
        let reload_name = action_prefix^action_reload in
        let action_line = Pages.make_input ~typ:Pages.hidden ~name:action_param_name ~value:action_param () in
        let v = get_full_url h in
        let inside = f (make_params_names action.action_params_type) in
        let inside_reload = 
          if reload 
          then 
	    Pages.cons_form
	      (Pages.make_hidden_field 
		 (Pages.make_input
		    ~typ:Pages.hidden ~name:reload_name ~value:reload_name ()))
              inside
          else inside 
        in
        Pages.make_post_form ?a ~action:v
          (Pages.make_hidden_field action_line)
          inside_reload

          
          
          
      let js_script = Pages.make_js_script
      let css_link = Pages.make_css_link


      let gen_input ?a ?value ?(pwd = false)
          (string_of : 'a -> string) (name : 'a param_name) =
        let typ = if pwd then Pages.password else Pages.text in
        match value with
          None ->
            Pages.make_input ?a ~typ:typ ~name:name ()
        | Some v -> 
            Pages.make_input
              ?a
              ~value:(string_of v)
              ~typ:typ ~name:name ()

      let int_input ?a ?value (name : int param_name) = 
        gen_input ?a ?value string_of_int name
      let float_input ?a ?value (name : float param_name) =
        gen_input ?a ?value string_of_float name
      let string_input ?a ?value (name : string param_name) =
        gen_input ?a ?value id name
      let user_type_input = gen_input ~pwd:false

      let int_password_input ?a ?value (name : int param_name) = 
        gen_input ~pwd:true ?a ?value string_of_int name
      let float_password_input ?a ?value (name : float param_name) =
        gen_input ~pwd:true ?a ?value string_of_float name
      let string_password_input ?a ?value (name : string param_name) =
        gen_input ~pwd:true ?a ?value id name
      let user_type_password_input = gen_input ~pwd:true

      let hidden_gen_input ?a string_of (name : 'a param_name) v = 
        let vv = string_of v in
        Pages.make_input ?a ~typ:Pages.hidden ~name:name ~value:vv ()

      let hidden_int_input ?a (name : int param_name) v = 
        hidden_gen_input ?a string_of_int name v
      let hidden_float_input ?a (name : float param_name) v =
        hidden_gen_input ?a string_of_float name v
      let hidden_string_input ?a (name : string param_name) v =
        hidden_gen_input ?a id name v
      let hidden_user_type_input = hidden_gen_input

      let bool_checkbox ?a ?checked (name : bool param_name) =
        Pages.make_input ?a ?checked ~typ:Pages.checkbox ~name:name ()

      let string_radio ?a ?checked (name : string option param_name) value =
        Pages.make_input
          ?a ?checked ~typ:Pages.radio ~name:name ~value:value ()
      let int_radio ?a ?checked (name : int option param_name) value =
        Pages.make_input
          ?a ?checked ~typ:Pages.radio ~name:name 
          ~value:(string_of_int value) ()
      let float_radio ?a ?checked (name : float option param_name) value =
        Pages.make_input
          ?a ?checked ~typ:Pages.radio ~name:name 
          ~value:(string_of_float value) ()
      let user_type_radio ?a ?checked string_of
          (name : 'a option param_name) (value : 'a) =
        Pages.make_input
          ?a ?checked ~typ:Pages.radio ~name:name ~value:(string_of value) ()

      let textarea ?a (name : string param_name) =
        Pages.make_textarea ?a ~name:name

      let submit_input ?a s =
        Pages.make_input ?a ~typ:Pages.submit ~value:s ()

      let file_input ?a ?value (name : file_info param_name)  = 
        Pages.make_input ?a ~typ:Pages.file ?value ~name:name ()

    end : OCSIGENSIG with 
     type page = Pages.page
     and type form_content_elt = Pages.form_content_elt
     and type form_content_elt_list = Pages.form_content_elt_list
     and type form_elt = Pages.form_elt
     and type a_content_elt = Pages.a_content_elt
     and type a_content_elt_list = Pages.a_content_elt_list
     and type a_elt = Pages.a_elt
     and type a_elt_list = Pages.a_elt_list
     and type div_content_elt = Pages.div_content_elt
     and type div_content_elt_list = Pages.div_content_elt_list
     and type uri = Pages.uri
     and type link_elt = Pages.link_elt
     and type script_elt = Pages.script_elt
     and type textarea_elt = Pages.textarea_elt
     and type input_elt = Pages.input_elt
     and type pcdata_elt = Pages.pcdata_elt
     and type a_attrib_t = Pages.a_attrib_t
     and type form_attrib_t = Pages.form_attrib_t
     and type input_attrib_t = Pages.input_attrib_t
     and type textarea_attrib_t = Pages.textarea_attrib_t
     and type link_attrib_t = Pages.link_attrib_t
     and type script_attrib_t = Pages.script_attrib_t
     and type input_type_t = Pages.input_type_t)


(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)


module Xhtml_ = struct
  open XHTML.M
  open Xhtmltypes

  type page = xhtml elt
  type form_content_elt = form_content elt
  type form_content_elt_list = form_content elt list
  type uri = XHTML.M.uri
  type a_content_elt = a_content elt
  type a_content_elt_list = a_content elt list
  type div_content_elt = div_content elt
  type div_content_elt_list = div_content elt list

  type a_elt = a elt
  type a_elt_list = a elt list
  type form_elt = form elt

  type textarea_elt = textarea elt
  type input_elt = input elt

  type link_elt = link elt
  type script_elt = script elt

  type pcdata_elt = pcdata elt

  type a_attrib_t = Xhtmltypes.a_attrib XHTML.M.attrib list
  type form_attrib_t = Xhtmltypes.form_attrib XHTML.M.attrib list
  type input_attrib_t = Xhtmltypes.input_attrib XHTML.M.attrib list
  type textarea_attrib_t = Xhtmltypes.textarea_attrib XHTML.M.attrib list
  type link_attrib_t = Xhtmltypes.link_attrib XHTML.M.attrib list
  type script_attrib_t = Xhtmltypes.script_attrib XHTML.M.attrib list

  type input_type_t = 
      [`Button | `Checkbox | `File | `Hidden | `Image
    | `Password | `Radio | `Reset | `Submit | `Text]

  let hidden = `Hidden
  let text = `Text
  let password = `Password
  let checkbox = `Checkbox
  let radio = `Radio
  let submit = `Submit
  let file = `File

  let make_uri_from_string = XHTML.M.make_uri_from_string

  let create_sender = Predefined_senders.create_xhtml_sender
  let send = Predefined_senders.send_xhtml_page

  let empty_seq = []
  let cons_form a l = a::l

  let make_a ?(a=[]) ~href l : a_elt = 
    XHTML.M.a ~a:((a_href (make_uri_from_string href))::a) l

  let make_get_form ?(a=[]) ~action elt1 elts : form_elt = 
    form ~a:((a_method `Get)::a) 
      ~action:(make_uri_from_string action) elt1 elts

  let make_post_form ?(a=[]) ~action ?id ?(inline = false) elt1 elts 
      : form_elt = 
    let aa = (match id with
      None -> a
    | Some i -> (a_id i)::a) 
    in
    form ~a:((XHTML.M.a_enctype "multipart/form-data")::
             (* Always Multipart!!! How to test if there is a file?? *)
             (a_method `Post)::
             (if inline then (a_class ["inline"])::aa else aa))
      ~action:(make_uri_from_string action) elt1 elts

  let make_hidden_field content = 
    div ~a:[a_class ["nodisplay"]] [content]

  let make_div ~classe (c : a_elt) =
    div ~a:[a_class classe] [(c :> div_content_elt)]

  let make_empty_form_content () = p [pcdata ""] (**** à revoir !!!!! *)

  let remove_first = function
      a::l -> a,l
    | [] -> (make_empty_form_content ()), []

  let make_input ?(a=[]) ?(checked=false) ~typ ?name ?value () = 
    let a2 = match value with
      None -> a
    | Some v -> (a_value v)::a
    in
    let a3 = match name with
      None -> a2
    | Some v -> (a_name v)::a2
    in
    let a4 = if checked then (a_checked `Checked)::a3 else a3 in
    input ~a:((a_input_type typ)::a4) ()

  let make_textarea ?(a=[]) ~name:name = 
    let a3 = (a_name name)::a in
    textarea ~a:a3

  let make_css_link ?(a=[]) uri =
    link ~a:((a_href uri)::
             (a_type "text/css")::(a_rel [`Stylesheet])::a) ()
      
  let make_js_script ?(a=[]) uri =
    script ~a:((a_src uri)::a) ~contenttype:"text/javascript" (pcdata "")

end



(****************************************************************************)
(*****************************************************************************)

module Xhtml__ = Make(Xhtml_)

(* As we want -> [> a ] elt and not -> [ a ] elt, we define a new module: *)
module Xhtml = struct
  open XHTML.M
  open Xhtmltypes
  include Xhtml__
  let a = (a : ?a:([< Xhtmltypes.a_attrib > `Href ] XHTML.M.attrib list) ->
    ('get, unit, 'b, [< `WithSuffix | `WithoutSuffix ], 'c, unit param_name) 
      service ->
        server_params -> 
          Xhtmltypes.a_content XHTML.M.elt list -> 
            'get -> Xhtmltypes.a XHTML.M.elt
                :> ?a:([< Xhtmltypes.a_attrib > `Href ] XHTML.M.attrib list) ->
                  ('get, unit, 'b, [< `WithSuffix | `WithoutSuffix ], 'c, unit param_name) 
                    service ->
                      server_params -> 
                        Xhtmltypes.a_content XHTML.M.elt list -> 
                          'get -> [> Xhtmltypes.a] XHTML.M.elt)

  let css_link = 
    (css_link : ?a:([< link_attrib > `Href `Rel `Type ] attrib list) ->
      uri -> link elt
          :> ?a:([< link_attrib > `Href `Rel `Type ] attrib list) ->
            uri -> [> link ] elt)

  let js_script = (js_script
                     : ?a:([< script_attrib > `Src ] attrib list) ->
                       uri -> script elt
                           :> ?a:([< script_attrib > `Src ] attrib list) ->
                             uri -> [> script ] elt)

  let get_form = 
    (get_form
       : ?a:([< form_attrib > `Method ] attrib list) ->
         ('get, unit, 'c, 'd, 'getnames, unit param_name) service ->
           server_params -> ('getnames -> form_content elt list) -> form elt
               :> ?a:([< form_attrib > `Method ] attrib list) ->
                 ('get, unit, 'c, 'd, 'getnames, unit param_name) service ->
                   server_params -> ('getnames -> form_content_elt_list) -> [>form] elt)

  let post_form = 
    (post_form
       : ?a:([< form_attrib > `Class `Id `Method ] attrib list) ->
         ('get, 'post, 'c, [< `WithSuffix | `WithoutSuffix ], 'getnames, 'postnames) service ->
           server_params ->
             ('postnames -> form_content elt list) -> 'get -> form elt
                 :> ?a:([< form_attrib > `Class `Id `Method ] attrib list) ->
                   ('get, 'post, 'c, [< `WithSuffix | `WithoutSuffix ], 'getnames, 'postnames) service ->
                     server_params ->
                       ('postnames -> form_content_elt_list) -> 'get -> [>form] elt)

  let int_input = 
    (int_input 
       : ?a:([< input_attrib > `Input_Type `Name `Value ] attrib list ) -> 
         ?value:int ->
           int param_name -> input elt
               :> ?a:([< input_attrib > `Input_Type `Name `Value ] attrib list ) -> 
                 ?value:int ->
                 int param_name -> [> input ] elt)
      
  let float_input = 
    (float_input 
       : ?a:([< input_attrib > `Input_Type `Name `Value ] attrib list ) -> 
         ?value:float ->
         float param_name -> input elt
             :> ?a:([< input_attrib > `Input_Type `Name `Value ] attrib list ) -> 
               ?value:float ->
                 float param_name -> [> input ] elt)

  let user_type_input = 
    (user_type_input
       : ?a:([< input_attrib > `Input_Type `Name `Value ] attrib list ) -> 
         ?value:'a ->
           ('a -> string) ->
             'a param_name -> input elt
                 :> ?a:([< input_attrib > `Input_Type `Name `Value ] attrib list ) -> 
                   ?value:'a ->
                     ('a -> string) ->
                       'a param_name -> [> input ] elt)
      
  let string_input = 
    (string_input 
       : ?a:([< input_attrib > `Input_Type `Name `Value ] attrib list ) ->
         ?value:string -> string param_name -> input elt
           :> ?a:([< input_attrib > `Input_Type `Name `Value ] attrib list ) ->
             ?value:string -> string param_name -> [> input ] elt)

  let int_password_input = 
    (int_password_input 
       : ?a:([< input_attrib > `Input_Type `Name `Value ] attrib list ) -> 
         ?value:int ->
           int param_name -> input elt
               :> ?a:([< input_attrib > `Input_Type `Name `Value ] attrib list ) -> 
                 ?value:int ->
                 int param_name -> [> input ] elt)
      
  let float_password_input = 
    (float_password_input 
       : ?a:([< input_attrib > `Input_Type `Name `Value ] attrib list ) -> 
         ?value:float ->
         float param_name -> input elt
             :> ?a:([< input_attrib > `Input_Type `Name `Value ] attrib list ) -> 
               ?value:float ->
                 float param_name -> [> input ] elt)

  let user_type_password_input = 
    (user_type_password_input
       : ?a:([< input_attrib > `Input_Type `Name `Value ] attrib list ) -> 
         ?value:'a ->
           ('a -> string) ->
             'a param_name -> input elt
                 :> ?a:([< input_attrib > `Input_Type `Name `Value ] attrib list ) -> 
                   ?value:'a ->
                     ('a -> string) ->
                       'a param_name -> [> input ] elt)
      
  let string_password_input = 
    (string_password_input 
       : ?a:([< input_attrib > `Input_Type `Name `Value ] attrib list ) ->
         ?value:string -> string param_name -> input elt
           :> ?a:([< input_attrib > `Input_Type `Name `Value ] attrib list ) ->
             ?value:string -> string param_name -> [> input ] elt)

  let hidden_int_input = 
    (hidden_int_input
       : ?a:([< input_attrib > `Input_Type `Name `Value ] attrib list ) -> 
         int param_name -> int -> input elt
             :> ?a:([< input_attrib > `Input_Type `Name `Value ] attrib list ) -> 
               int param_name -> int -> [> input ] elt)
  let hidden_float_input = 
    (hidden_float_input
       : ?a:([< input_attrib > `Input_Type `Name `Value ] attrib list ) -> 
         float param_name -> float -> input elt
             :> ?a:([< input_attrib > `Input_Type `Name `Value ] attrib list ) -> 
               float param_name -> float -> [> input ] elt)
  let hidden_string_input = 
    (hidden_string_input
       : ?a:([< input_attrib > `Input_Type `Name `Value ] attrib list ) -> 
         string param_name -> string -> input elt
             :> ?a:([< input_attrib > `Input_Type `Name `Value ] attrib list ) -> 
               string param_name -> string -> [> input ] elt)
  let hidden_user_type_input = 
    (hidden_user_type_input
       : ?a:([< input_attrib > `Input_Type `Name `Value ] attrib list ) -> 
         ('a -> string) ->
           'a param_name -> 'a -> input elt
               :> ?a:([< input_attrib > `Input_Type `Name `Value ] attrib list ) -> 
                 ('a -> string) ->
                   'a param_name -> 'a -> [> input ] elt)
      


  let bool_checkbox = 
    (bool_checkbox
       : ?a:([< input_attrib > `Input_Type `Name `Value ] attrib list ) -> 
         ?checked:bool ->
           bool param_name -> input elt
               :> ?a:([< input_attrib > `Input_Type `Name `Value ] attrib list ) -> 
                 ?checked:bool ->
                   bool param_name -> [> input ] elt)

  let string_radio = 
    (string_radio
       : ?a:([< input_attrib > `Input_Type `Name `Value ] attrib list ) -> 
         ?checked:bool ->
            string option param_name -> string -> input elt
             :> ?a:([< input_attrib > `Input_Type `Name `Value ] attrib list ) -> 
               ?checked:bool ->
                  string option param_name -> string -> [> input ] elt)
  let int_radio = 
    (int_radio
       : ?a:([< input_attrib > `Input_Type `Name `Value ] attrib list ) -> 
         ?checked:bool ->
            int option param_name -> int -> input elt
             :> ?a:([< input_attrib > `Input_Type `Name `Value ] attrib list ) -> 
               ?checked:bool ->
                  int option param_name -> int -> [> input ] elt)
  let float_radio = 
    (float_radio
       : ?a:([< input_attrib > `Input_Type `Name `Value ] attrib list ) -> 
         ?checked:bool ->
            float option param_name -> float -> input elt
             :> ?a:([< input_attrib > `Input_Type `Name `Value ] attrib list ) -> 
               ?checked:bool ->
                  float option param_name -> float -> [> input ] elt)
  let user_type_radio = 
    (user_type_radio
       : ?a:([< input_attrib > `Input_Type `Name `Value ] attrib list ) -> 
         ?checked:bool ->
           ('a -> string) ->
             'a option param_name -> 'a -> input elt
                 :> ?a:([< input_attrib > `Input_Type `Name `Value ] attrib list ) -> 
                   ?checked:bool ->
                     ('a -> string) ->
                       'a option param_name -> 'a -> [> input ] elt)

  let textarea = (textarea
                    : ?a:([< textarea_attrib > `Name ] attrib list ) -> 
                      string param_name -> rows:number -> cols:number -> 
                        [ `PCDATA ] XHTML.M.elt ->
                          textarea elt
                            :> ?a:([< textarea_attrib > `Name ] attrib list ) -> 
                              string param_name -> rows:number -> cols:number -> 
                                [ `PCDATA ] XHTML.M.elt ->
                                  [> textarea ] elt)

  let submit_input = (submit_input
                        : ?a:([< input_attrib > `Input_Type `Name `Value ] attrib list ) -> 
                          string -> input elt
                              :> ?a:([< input_attrib > `Input_Type `Name `Value ] attrib list ) -> 
                                string -> [> input ] elt)
                                
  let file_input = (file_input
                          : ?a:([< input_attrib > `Input_Type `Name `Value ] attrib list ) ->
                          ?value:string -> string param_name -> input elt
                              :> ?a:([< input_attrib > `Input_Type `Name `Value ] attrib list ) ->
                                ?value:string -> file_info param_name -> [> input ] elt)

  let action_a = (action_a
                    : ?a:([< a_attrib > `Href ] attrib list) ->
                      ?reload:bool ->
                        ('a,'b) action -> 
                          server_params -> 
                            a_content elt list -> 
                              form elt
                                :> ?a:([< a_attrib > `Href ] attrib list) ->
                                  ?reload:bool ->
                                    ('a,'b) action -> 
                                      server_params -> 
                                        a_content_elt_list -> 
                                          [> form ] elt)

  let action_form = (action_form
                       : ?a:([< form_attrib > `Class `Id `Method ] attrib list) ->
                         ?reload:bool ->
                           ('a, 'b) action ->
                             server_params -> 
                               ('b -> form_content elt list) ->
                                 form elt
                                   :> ?a:([< form_attrib > `Class `Id `Method ] attrib list) ->
                                     ?reload:bool ->
                                       ('a, 'b) action ->
                                         server_params -> 
                                           ('b -> form_content_elt_list) ->
                                             [> form ] elt)

end

(****************************************************************************)
(****************************************************************************)
(****************************************************************************)
(****************************************************************************)

module Text_ = struct
  open XHTML.M
  open Xhtmltypes

  type page = string
  type form_content_elt = string
  type form_content_elt_list = string
  type uri = string
  type a_content_elt = string
  type a_content_elt_list = string
  type div_content_elt = string
  type div_content_elt_list = string

  type a_elt = string
  type a_elt_list = string
  type form_elt = string

  type textarea_elt = string
  type input_elt = string

  type link_elt = string
  type script_elt = string

  type pcdata_elt = string

  let create_sender = Predefined_senders.create_xhtml_sender
  let send = Predefined_senders.send_text_page

  type a_attrib_t = string
  type form_attrib_t = string
  type input_attrib_t = string
  type textarea_attrib_t = string
  type link_attrib_t = string
  type script_attrib_t = string

  type input_type_t = string

  let hidden = "hidden"
  let text = "text"
  let password = "password"
  let checkbox = "checkbox"
  let radio = "radio"
  let submit = "submit"
  let file = "file"

  let make_uri_from_string x = x

  let empty_seq = ""
  let cons_form a l = a^l

  let make_a ?(a="") ~href l : a_elt = 
    "<a href=\""^href^"\""^a^">"^(* List.fold_left (^) "" l *) l^"</a>"

  let make_get_form ?(a="") ~action elt1 elts : form_elt = 
    "<form method=\"get\" action=\""^(make_uri_from_string action)^"\""^a^">"^
    elt1^(*List.fold_left (^) "" elts *) elts^"</form>"

  let make_post_form ?(a="") ~action ?id ?(inline = false) elt1 elts 
      : form_elt = 
    let aa = "enctype=\"multipart/form-data\" "
        (* Always Multipart!!! How to test if there is a file?? *)
      ^(match id with
        None -> a
      | Some i -> " id="^i^" "^a)
    in
    "<form method=\"post\" action=\""^(make_uri_from_string action)^"\""^
    (if inline then "style=\"display: inline\"" else "")^aa^">"^
    elt1^(* List.fold_left (^) "" elts*) elts^"</form>"

  let make_hidden_field content = 
    "<div style=\"display: none\""^content^"</div>"

  let make_div ~classe c =
    "<div class=\""^(List.fold_left (fun a b -> a^" "^b) "" classe)^"\""^
    c^"</div>"
(*    (List.fold_left (^) "" c)^"</div>" *)

  let remove_first l = "",l

  let make_input ?(a="") ?(checked=false) ~typ ?name ?value () = 
    let a2 = match value with
      None -> a
    | Some v -> " value="^v^" "^a
    in
    let a3 = match name with
      None -> a2
    | Some v -> " name="^v^" "^a2
    in
    let a4 = if checked then " checked=\"checked\" "^a3 else a3 in
    "<input type=\""^typ^"\" "^a4^"/>"

  let make_textarea ?(a="") ~name:name ~rows ~cols s = 
    "<textarea name=\""^name^"\" rows=\""^(string_of_int rows)^
    "\" cols=\""^(string_of_int cols)^"\" "^a^">"^s^"</textarea>"

  let make_css_link ?(a="") uri =
    "<link href=\""^uri^" type=\"text/css\" rel=\"stylesheet\" "^a^"/>"
                                                                      
  let make_js_script ?(a="") uri =
    "<script src=\""^uri^" contenttype=\"text/javascript\" "^a^"></script>"

end



(****************************************************************************)
(****************************************************************************)

module Text = Make(Text_)


