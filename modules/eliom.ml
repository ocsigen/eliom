(* Ocsigen
 * http://www.ocsigen.org
 * Module eliom.ml
 * Copyright (C) 2007 Vincent Balat
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
open Eliommod
open Lazy

exception Eliom_Link_too_old = Eliommod.Eliom_Link_too_old
exception Eliom_Session_expired = Eliommod.Eliom_Session_expired

let _ = Random.self_init ()

let get_config () = 
  match global_register_allowed () with
  | Some _ -> !Eliommod.config
  | None -> raise Eliom_function_forbidden_outside_site_loading

type current_url = Extensions.current_url
type url_path = Extensions.url_path
type server_params = Eliommod.server_params

let string_of_url_path = string_of_url_path

let get_user_agent (ri,_,_) = ri.ri_user_agent
let get_full_url (ri,_,_) = ri.ri_path_string^ri.ri_params
let get_ip (ri,_,_) = ri.ri_ip
let get_inet_addr (ri,_,_) = ri.ri_inet_addr
let get_get_params (ri,_,_) = force ri.ri_get_params
let get_get_params_string (ri,_,_) = ri.ri_params
let get_post_params (ri,_,_) = force ri.ri_post_params
let get_current_path_string (ri,_,_) = ri.ri_path_string
let get_current_path (ri,_,_) = ri.ri_path
let get_hostname (ri,_,_) = ri.ri_host
let get_port (ri,_,_) = ri.ri_port
let get_other_get_params (_,si,_) = si.si_other_get_params
let get_suffix (_,_,(_,_,_,_,s)) = s
let get_exn (_,si,_) = si.si_exn
let get_config_file_charset (_,si,_) = si.si_config_file_charset
let get_cookies (ri,_,_) = force ri.ri_cookies
let get_cookie (_,si,_) = !(si.si_cookie)
let get_persistent_cookie (_,si,_) = !(si.si_persistent_cookie)

let get_default_timeout = Eliommod.get_default_timeout
let set_global_timeout_during_session (_, _, (working_dir, _, _, _, _)) s = 
  Eliommod.set_global_timeout working_dir s
let get_global_timeout_during_session (_, _, (working_dir, _, _, _, _)) = 
  Eliommod.find_global_timeout working_dir

let get_default_persistent_timeout = Eliommod.get_default_persistent_timeout
let set_global_persistent_timeout_during_session 
    (_, _, (working_dir, _, _, _, _)) s = 
  Eliommod.set_global_persistent_timeout working_dir s
let get_global_persistent_timeout_during_session
    (_, _, (working_dir, _, _, _, _)) = 
  Eliommod.find_global_persistent_timeout working_dir

let set_global_timeout_during_init s = 
  match global_register_allowed () with
    Some get_current_hostdir ->
      Eliommod.set_global_timeout (snd (get_current_hostdir ())) s
  | _ -> raise Eliom_function_forbidden_outside_site_loading

let get_global_timeout_during_init () = 
  match global_register_allowed () with
    Some get_current_hostdir ->
      Eliommod.find_global_timeout (snd (get_current_hostdir ()))
  | _ -> raise Eliom_function_forbidden_outside_site_loading

let set_global_persistent_timeout_during_init s = 
  match global_register_allowed () with
    Some get_current_hostdir ->
      Eliommod.set_global_persistent_timeout (snd (get_current_hostdir ())) s
  | _ -> raise Eliom_function_forbidden_outside_site_loading

let get_global_persistent_timeout_during_init () = 
  match global_register_allowed () with
    Some get_current_hostdir ->
      Eliommod.find_global_persistent_timeout (snd (get_current_hostdir ()))
  | _ -> raise Eliom_function_forbidden_outside_site_loading

let set_user_timeout (_,_,(_,_,_,(tor,_,_,_),_)) t = tor := Some t
let unset_user_timeout (_,_,(_,_,_,(tor,_,_,_),_)) = tor := None
let get_user_timeout (_,_,(working_dir,_,_,(tor,_,_,_),_)) = 
  match !tor with
    None -> Eliommod.find_global_timeout working_dir
  | Some t -> t

let set_user_expdate (_,_,(_,_,_,(_,exp,_,_),_)) t = exp := t
let get_user_expdate (_,_,(working_dir,_,_,(_,exp,_,_),_)) = !exp

let set_user_persistent_timeout (_,_,(_,_,_,(_,_,tor,_),_)) t = tor := Some t
let unset_user_persistent_timeout (_,_,(_,_,_,(_,_,tor,_),_)) = tor := None
let get_user_persistent_timeout (_,_,(working_dir,_,_,(_,_,tor,_),_)) = 
  match !tor with
    None -> Eliommod.find_global_persistent_timeout working_dir
  | Some t -> t

let set_user_persistent_expdate (_,_,(_,_,_,(_,_,_,exp),_)) t = exp := t
let get_user_persistent_expdate (_,_,(working_dir,_,_,(_,_,_,exp),_)) = !exp

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
  | TESuffix of string list param_name (* 'a = string list *)
  | TESuffixs of string param_name (* 'a = string *)
  | TESuffixu of ('a param_name * (string -> 'a) * ('a -> string)) (* 'a = 'a *)
  | TSuffix of ('a,'tipo,'names) params_type (* 'a = 'a1 *)
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
    (t2 : ('b,[<`WithoutSuffix|`Endsuffix], 'bn) params_type)
    : (('a * 'b),[`WithoutSuffix], 'an * 'bn) params_type =
  Obj.magic (TProd ((Obj.magic t1), (Obj.magic t2)))
let opt (t : ('a,[`WithoutSuffix], 'an) params_type) 
    : ('a option,[`WithoutSuffix], 'an) params_type = 
  Obj.magic (TOption t)
let list (n : string) (t : ('a,[`WithoutSuffix], 'an) params_type) 
    : ('a list,[`WithoutSuffix], 'an listnames) params_type = 
  Obj.magic (TList (n,t))
let ( ** ) = prod

let regexp reg dest n = 
  user_type
    (fun s -> 
      Mutex.lock strlock;
      if Str.string_match reg s 0
      then begin
        let r = Str.replace_matched dest s in
        Mutex.unlock strlock;
        r
      end
      else begin
        Mutex.unlock strlock;
        raise (Failure "Not matching regexp")
      end)
    (fun s -> s)
    n

let all_suffix (n : string) : 
    (string list , [`Endsuffix], string list param_name) params_type = 
  (Obj.magic (TESuffix n))

let all_suffix_string (n : string) : 
    (string, [`Endsuffix], string param_name) params_type = 
  (Obj.magic (TESuffixs n))

let all_suffix_user
    (of_string : string -> 'a) (from_string : 'a -> string) (n : string) :
    ('a, [`Endsuffix], 'a param_name) params_type = 
  (Obj.magic (TESuffixu (n, of_string, from_string)))

let all_suffix_regexp reg dest (n : string) : 
    (string, [`Endsuffix], string param_name) params_type = 
  all_suffix_user
    (fun s -> 
      Mutex.lock strlock;
      if Str.string_match reg s 0
      then begin
        let r = Str.replace_matched dest s in
        Mutex.unlock strlock;
        r
      end
      else begin
        Mutex.unlock strlock;
        raise (Failure "Not matching regexp")
      end)
    (fun s -> s)
    n

let suffix (s : ('s,[<`WithoutSuffix|`Endsuffix],'sn) params_type) : 
    ('s , [`WithSuffix], 'sn) params_type = 
  (Obj.magic (TSuffix s))

let suffix_prod (s : ('s,[<`WithoutSuffix|`Endsuffix],'sn) params_type)
    (t : ('a,[`WithoutSuffix], 'an) params_type) : 
    (('s * 'a), [`WithSuffix], 'sn * 'an) params_type = 
  (Obj.magic (TProd (Obj.magic (TSuffix s), Obj.magic t)))

let contains_suffix = function
    TProd((TSuffix _),_)
  | TSuffix _ -> true
  | _ -> false


(******)
let make_list_suffix i = "["^(string_of_int i)^"]"

let add_to_string s1 sep = function
    "" -> s1
  | s2 -> s1^sep^s2

let concat_strings s1 sep s2 = match s1,s2 with
  _,"" -> s1
| "",_ -> s2
| _ -> s1^sep^s2

(* The following function reconstructs the value of parameters
   from expected type and GET or POST parameters *)
type 'a res_reconstr_param = 
    Res_ of ('a * 
               (string * string) list * 
               (string * file_info) list)
  | Errors_ of ((string * exn) list *
                  (string * string) list * 
                  (string * file_info) list)

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
        | Errors_ (errs, l, f) ->
            (match aa (i+1) l f pref suff with
              Res_ (_,ll,ff) -> Errors_ (errs, ll, ff)
            | Errors_ (errs2, ll, ff) -> Errors_ ((errs@errs2), ll, ff))
      with Not_found -> Res_ ((Obj.magic []),lp,files)
    in 
    aa 0 params files (pref^name^".") suff
  and aux (typ : ('a,[<`WithSuffix|`WithoutSuffix|`Endsuffix],'b) params_type)
      params files pref suff : 'a res_reconstr_param =
    match typ with
      TProd (t1, t2) ->
        (match aux t1 params files pref suff with
          Res_ (v1,l1,f) ->
            (match aux t2 l1 f pref suff with
              Res_ (v2,l2,f2) -> Res_ ((Obj.magic (v1,v2)),l2,f2)
            | err -> err)
        | Errors_ (errs, l, f) ->
            (match aux t2 l f pref suff with
              Res_ (_,ll,ff) -> Errors_ (errs, ll, ff)
            | Errors_ (errs2, ll, ff) -> Errors_ ((errs2@errs), ll, ff)))
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
        with e -> Errors_ ([(pref^name^suff),e], l, files))
    | TFloat name -> 
        let v,l = (list_assoc_remove (pref^name^suff) params) in 
        (try (Res_ ((Obj.magic (float_of_string v)),l,files))
        with e -> Errors_ ([(pref^name^suff),e], l, files))
    | TFile name -> 
        let v,f = list_assoc_remove (pref^name^suff) files in
        Res_ ((Obj.magic v),params,f)
    | TUserType (name, of_string, string_of) ->
        let v,l = (list_assoc_remove (pref^name^suff) params) in 
        (try (Res_ ((Obj.magic (of_string v)),l,files))
        with e -> Errors_ ([(pref^name^suff),e], l, files))
    | TUnit -> Res_ ((Obj.magic ()), params, files)
    | TESuffix n ->
        let v,l = list_assoc_remove n params in
        (* cannot have prefix or suffix *)
        Res_ ((Obj.magic (Neturl.split_path v)), l, files)
    | TESuffixs n ->
        let v,l = list_assoc_remove n params in
        (* cannot have prefix or suffix *)
        Res_ ((Obj.magic v), l, files)
    | TESuffixu (n, of_string, from_string) ->
        let v,l = list_assoc_remove n params in
        (* cannot have prefix or suffix *)
        Res_ ((Obj.magic (of_string v)), l, files)
    | TSuffix _ -> raise (Ocsigen_Internal_Error "Bad use of suffix")
  in
  let aux2 typ params =
    match Obj.magic (aux typ params files "" "") with
      Res_ (v,l,files) -> 
        if (l,files) = ([], [])
        then v
        else raise Eliom_Wrong_parameter
    | Errors_ (errs, l, files) -> 
        if (l,files) = ([], [])
        then raise (Eliom_Typing_Error errs)
        else raise Eliom_Wrong_parameter
  in
  let parse_one typ v =
    match typ with
    | TString _ -> Obj.magic v
    | TInt name -> 
        (try Obj.magic (int_of_string v)
        with e -> raise (Eliom_Typing_Error [("<suffix>", e)]))
    | TFloat name -> 
        (try Obj.magic (float_of_string v)
        with e -> raise (Eliom_Typing_Error [("<suffix>", e)]))
    | TUserType (name, of_string, string_of) ->
        (try Obj.magic (of_string v)
        with e -> raise (Eliom_Typing_Error [("<suffix>", e)]))
    | _ -> raise Eliom_Wrong_parameter
  in
  let rec parse_suffix typ suff =
    match (typ, suff) with
    | (TESuffix _), l -> Obj.magic l
    | (TESuffixs _), l -> Obj.magic (string_of_url_path l)
    | (TESuffixu (_, of_string, from_string)), l -> 
        (try
          Obj.magic (of_string (string_of_url_path l))
        with e -> raise (Eliom_Typing_Error [("<suffix>", e)]))
    | _, [a] -> parse_one typ a
    | (TProd (t1, t2)), a::l -> 
        let b = parse_suffix t2 l in (* First we do parse_suffix to detect
                                        wrong number of parameters *)
        Obj.magic ((parse_one t1 a), b)
    | _ -> raise Eliom_Wrong_parameter
  in
  try 
    match typ with
      (* Each suffixed URL has a version with parameters to be used with 
         forms *)
      TProd((TSuffix s), t) -> 
        if urlsuffix = [""]
          (* no suffix: switching to version with parameters *)
        then 
          (try 
            Obj.magic (aux2 (TProd (s, t)) params)
          with Eliom_Wrong_parameter -> 
            Obj.magic ((parse_suffix s urlsuffix), (aux2 t params)))
        else Obj.magic ((parse_suffix s urlsuffix), (aux2 t params))
    | TSuffix s ->
        if urlsuffix = [""] && params <> [] 
        then
          (try Obj.magic (aux2 s params)
          with Eliom_Wrong_parameter -> 
            Obj.magic (parse_suffix s urlsuffix))
        else Obj.magic (parse_suffix s urlsuffix)
    | _ -> Obj.magic (aux2 typ params)
  with 
  | Not_found -> raise Eliom_Wrong_parameter

(* The following function takes a 'a params_type and a 'a and
   constructs the list of parameters (GET or POST) 
   (This is a marshalling function towards HTTP parameters format) *)
let construct_params_list (typ : ('a, [<`WithSuffix|`WithoutSuffix],'b) params_type)
    (params : 'a) : string list option * (string * string) list =
  let rec aux typ params pref suff l =
    match typ with
      TProd (t1, t2) ->
        let l1 = aux t1 (fst (Obj.magic params)) pref suff l in
        aux t2 (snd (Obj.magic params)) pref suff l1
    | TOption t -> (match ((Obj.magic params) : 'zozo option) with None -> l
      | Some v -> aux t v pref suff l)
    | TBool name -> 
        (if ((Obj.magic params) : bool)
        then ((pref^name^suff), "on")::l
        else l)
    | TList (list_name, t) -> 
        let pref2 = pref^list_name^suff^"." in
        fst 
          (List.fold_left
             (fun (s,i) p -> 
               ((aux t p pref2 (suff^(make_list_suffix i)) s),(i+1)))
             (l,0) (Obj.magic params))
    | TSum (t1, t2) -> (match Obj.magic params with
        Inj1 v -> aux t1 v pref suff l
      | Inj2 v -> aux t2 v pref suff l)
    | TString name -> ((pref^name^suff), (Obj.magic params))::l
    | TInt name -> ((pref^name^suff), (string_of_int (Obj.magic params)))::l
    | TFloat name -> 
        ((pref^name^suff), (string_of_float (Obj.magic params)))::l
    | TFile name -> 
        raise (Failure
                 "Constructing an URL with file parameters not implemented")
    | TUserType (name, of_string, string_of) ->
        ((pref^name^suff), (string_of (Obj.magic params)))::l
    | TUnit -> l
    | TESuffix _
    | TESuffixs _
    | TESuffixu _
    | TSuffix _ -> raise (Ocsigen_Internal_Error "Bad use of suffix")
  in
  let rec make_suffix typ params =
    match typ with
      TProd (t1, t2) ->
        (make_suffix t1 (fst (Obj.magic params)))@
        (make_suffix t2 (snd (Obj.magic params)))
    | TString _ -> [Obj.magic params]
    | TInt _ -> [string_of_int (Obj.magic params)]
    | TFloat _ -> [string_of_float (Obj.magic params)]
    | TUserType (_, of_string, string_of) ->[string_of (Obj.magic params)]
    | TESuffixs _ -> [Obj.magic params]
    | TESuffix _ -> Obj.magic params
    | TESuffixu (_, of_string, string_of) -> [string_of (Obj.magic params)]
    | _ -> raise (Ocsigen_Internal_Error "Bad parameters")
  in
  match typ with
    TProd((TSuffix s), t) ->
   ((Some (make_suffix s (fst (Obj.magic params)))),
   (aux t (snd (Obj.magic params)) "" "" []))
  | TSuffix s -> (Some (make_suffix s (Obj.magic params))), []
  | _ -> None, (aux typ params "" "" [])


(* contruct the string of parameters (& separated) for GET and POST *)
let construct_params_string = function
    [] -> ""
  | (a,b)::l -> 
      List.fold_left
        (fun beg (c,d) -> beg^"&"^c^"="^d)
        (a^"="^b)
        l

let construct_params typ p = 
  let suff, pl = construct_params_list typ p in
  (suff, construct_params_string pl)


(* Add a prefix to parameters *)
let rec add_pref_params pref = function
    TProd (t1, t2) -> TProd ((add_pref_params pref t1),
                             (add_pref_params pref t2))
  | TOption t -> TOption (add_pref_params pref t)
  | TBool name -> TBool (pref^name)
  | TList (list_name, t) -> TList (pref^list_name, t)
  | TSum (t1, t2) -> TSum ((add_pref_params pref t1),
                           (add_pref_params pref t2))
  | TString name -> TString (pref^name)
  | TInt name -> TInt (pref^name)
  | TFloat name -> TFloat (pref^name)
  | TFile name -> TFile (pref^name)
  | TUserType (name, of_string, string_of) -> 
      TUserType (pref^name, of_string, string_of)
  | TUnit -> TUnit
  | TESuffix n -> TESuffix n
  | TESuffixs n -> TESuffixs n
  | TESuffixu a -> TESuffixu a
  | TSuffix s -> TSuffix s

(* Remove all parameters whose name starts with pref *)
let remove_prefixed_param pref l =
  let len = String.length pref in
  let rec aux = function
      [] -> []
    | ((n,v) as a)::l -> 
        try if (String.sub n 0 len) = pref 
        then aux l
        else a::(aux l)
        with _ -> a::(aux l)
  in aux l

(*****************************************************************************)
(* Building href *)
let rec string_of_url_path' = function
    [] -> ""
  | [a] when a = eliom_suffix_internal_name -> ""
  | [a] -> a
  | a::l -> a^"/"^(string_of_url_path' l)

let rec string_of_url_path_suff u = function
    None -> string_of_url_path' u
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
    []
  | [""] -> defaultpagename
  | [a] -> a
  | a::l -> relative_url_path_to_myself l
(*****************************************************************************)



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
     (* unique_id is here only for registering on top of this service *)
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
     (* unique_id is here only for registering on top of this service *)
     unique_id: int;
     pre_applied_parameters: (string * string) list;
     get_params_type: ('get, 'tipo, 'getnames) params_type;
     post_params_type: ('post, [`WithoutSuffix], 'postnames) params_type;
     max_use: int option; (* Max number of use of this service *)
     timeout: float option; (* Timeout for this service (the service will 
          disappear if it has not been used during this amount of seconds) *)
     kind: 'kind; (* < service_kind *)
   }




(*****************************************************************************)
(*****************************************************************************)
(* Page registration, handling of links and forms                            *)
(*****************************************************************************)
(*****************************************************************************)

(** Satic directories **)
let static_dir (_,_,(curdir,_,_,_,_)) =
    {unique_id = counter ();
     pre_applied_parameters = [];
     get_params_type = suffix (all_suffix eliom_suffix_name);
     post_params_type = unit;
     max_use= None;
     timeout= None;
     kind = `Attached
       {url = curdir@[""];
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
  {unique_id = counter ();
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
    ~url
    ~get_params =
  match global_register_allowed () with
    Some get_current_hostdir ->
      let _,curdir = get_current_hostdir () in
      let full_path = remove_middle_slash (curdir@(change_empty_list url)) in
      let u = new_service_aux_aux
          ~url:full_path
          ~kind:(`Internal (`Service, `Get))
          ~get_params
          ~post_params:unit
      in
      add_unregistered (Some full_path, u.unique_id); u
  | None -> raise Eliom_function_forbidden_outside_site_loading

      
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
    ~url
    ~get_params
    () =
  let suffix = contains_suffix get_params in
  new_service_aux 
    ~url:(if suffix then url@[eliom_suffix_internal_name] else url)
    ~get_params

let new_naservice_name () = string_of_int (counter ())

let new_coservice
    ?max_use
    ?timeout
    ~fallback
    ~get_params
    () =
  let c = counter () in
  let `Attached k = fallback.kind in
  (match global_register_allowed () with
    Some _ -> add_unregistered (Some k.url, c);
  | _ -> ());
  {fallback with
   unique_id = c;
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
  let c = counter () in
  (match global_register_allowed () with
    Some _ -> add_unregistered (None, c);
  | _ -> ());
  {unique_id = c;
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
let new_post_service_aux ~fallback ~post_params =
(** Create a main service (not a coservice) internal, post only *)
(* ici faire une vérification "duplicate parameter" ? *) 
  let `Attached k1 = fallback.kind in
  let `Internal (k, _) = k1.att_kind in
  {unique_id = counter ();
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
    
let new_post_service ~fallback ~post_params () = 
  (if post_params = TUnit
  then Messages.warning "Probably error in the module: \
      Creation of a POST service without POST parameters.");
  let `Attached k1 = fallback.kind in
  let `Internal (kind, _) = k1.att_kind in
  let url = Some k1.url in
  let u = new_post_service_aux fallback post_params in
  match global_register_allowed () with
    Some _ ->
      add_unregistered (url, u.unique_id); 
      u
  | None ->
      if kind = `Service
      then raise Eliom_function_forbidden_outside_site_loading
      else u
(* Warning: strange if post_params = unit... *)    
(* if the fallback is a coservice, do we get a coservice or a service? *)    

  
let new_post_coservice ?max_use ?timeout ~fallback ~post_params () = 
  let c = counter () in
  let `Attached k1 = fallback.kind in
  (match global_register_allowed () with
    Some _ -> add_unregistered (Some k1.url, c);
  | _ -> ());
  {fallback with 
   unique_id = c;
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
  let c = counter () in
  (match global_register_allowed () with
    Some _ -> add_unregistered (None, c)
  | _ -> ());
  {unique_id = c;
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
  let c = counter () in
  (match global_register_allowed () with
    Some _ ->
  | _ -> ());
   add_unregistered (None, c);
   {unique_id = c;
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


let preapply service getparams =
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



(****************************************************************************)

module type REGCREATE = 
  sig

    type page

    val send : 
        ?cookies:cookieslist -> 
          ?charset:string ->
            server_params -> page -> result_to_send

  end


module type FORMCREATE = 
  sig
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
    type select_elt
    type input_elt
    type pcdata_elt

    type a_attrib_t
    type form_attrib_t
    type input_attrib_t
    type textarea_attrib_t
    type select_attrib_t
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
     val make_select : ?a:select_attrib_t ->
       name:string ->
       ?selected:((string option * string) option)
         -> (string option * string) -> ((string option * string) list) ->
       select_elt
    val make_div : classe:(string list) -> a_elt -> form_content_elt
    val make_uri_from_string : string -> uri


    val make_css_link : ?a:link_attrib_t -> uri -> link_elt

    val make_js_script : ?a:script_attrib_t -> uri -> script_elt

  end

module type ELIOMFORMSIG =
(* pasted from mli *)
  sig


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
    type select_elt
    type input_elt
    type pcdata_elt
          
    type a_attrib_t
    type form_attrib_t
    type input_attrib_t
    type textarea_attrib_t
    type select_attrib_t
    type link_attrib_t
    type script_attrib_t
    type input_type_t

    val a :
        ?a:a_attrib_t ->
          ('get, unit, [< get_service_kind ], 
           [< suff ], 'gn, unit param_name,
           [< registrable ]) service ->
            server_params -> a_content_elt_list -> 'get -> a_elt
    val get_form :
        ?a:form_attrib_t ->
          ('get, unit, [< get_service_kind ],
           [<suff ], 'gn, unit param_name, 
           [< registrable ]) service ->
             server_params ->
              ('gn -> form_content_elt_list) -> form_elt
    val post_form :
        ?a:form_attrib_t ->
          ('get, 'post, [< post_service_kind ],
           [< suff ], 'gn, 'pn, 
           [< registrable ]) service ->
            server_params ->
              ('pn -> form_content_elt_list) -> 'get -> form_elt
    val make_uri :
        ('get, unit, [< get_service_kind ],
         [< suff ], 'gn, unit param_name, 
         [< registrable ]) service ->
          server_params -> 'get -> uri

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
    val select :
      ?a:select_attrib_t ->
      ?selected:((string option * string) option)
      -> (string option * string) -> ((string option * string) list) ->
      string param_name
      -> select_elt
    val submit_input : ?a:input_attrib_t -> string -> input_elt
    val file_input : ?a:input_attrib_t -> ?value:string -> 
                            file_info param_name-> input_elt


  end


module type ELIOMREGSIG1 =
(* pasted from mli *)
  sig



    type page

    val send : 
        ?cookies:cookieslist -> 
          ?charset:string ->
            server_params -> page -> Eliommod.result_to_send

    val register :
        service:('get, 'post,
                 [< internal_service_kind ],
                 [< suff ], 'gn, 'pn, [ `Registrable ]) service ->
        ?error_handler:(server_params ->
                               (string * exn) list -> page Lwt.t) ->
        (server_params -> 'get -> 'post -> page Lwt.t) ->
          unit
(** Register an service in the global table of the server 
   with the associated generation function.
   [register service t f] will associate the service [service] to the function [f].
   [f] is the function that creates a page. 
   It takes three parameters. The first one has type [server_params]
   and allows to have acces to informations about the request.
   The second and third ones are respectively GET and POST parameters.
   For example if [t] is (int "s"), then ['a] is int.
 *)


    val register_for_session :
        server_params ->
          service:('get, 'post, [< internal_service_kind ],
                   [< suff ], 'gn, 'pn, [ `Registrable ]) service ->
              ?error_handler:(server_params -> (string * exn) list -> 
                page Lwt.t) ->
                  (server_params -> 'get -> 'post -> page Lwt.t) -> unit
(** Registers an service and the associated function in the session table.
   If the same client does a request to this service, this function will be
   used instead of the one from the global table.

   Warning:
   - All service must be registered in the global table during initialisation,
   but never after,
   - You (obviously) can't register an service in a session table 
   when no session is active
 *)


    val register_new_service :
        url:url_path ->
            get_params:('get, [< suff ] as 'tipo, 'gn)
              params_type ->
                ?error_handler:(server_params -> (string * exn) list -> 
                  page Lwt.t) ->
                    (server_params -> 'get -> unit -> page Lwt.t) ->
                      ('get, unit, 
                       [> `Attached of 
                         [> `Internal of [> `Service ] * [> `Get] ] a_s ],
                       'tipo, 'gn, unit param_name, 
                       [> `Registrable ]) service
(** Same as [new_service] followed by [register] *)
                      
    val register_new_coservice :
      ?max_use:int ->
        ?timeout:float ->
        fallback:(unit, unit, 
                  [ `Attached of [ `Internal of [ `Service ] * [`Get]] a_s ],
                   [ `WithoutSuffix ] as 'tipo, 
                   unit param_name, unit param_name, [< registrable ])
        service ->
          get_params: 
            ('get, [`WithoutSuffix], 'gn) params_type ->
              ?error_handler:(server_params -> 
                (string * exn) list -> page Lwt.t) ->
                  (server_params -> 'get -> unit -> page Lwt.t) ->
                    ('get, unit, 
                     [> `Attached of 
                       [> `Internal of [> `Coservice ] * [> `Get]] a_s ], 
                     'tipo, 'gn, unit param_name, 
                     [> `Registrable ])
                      service
(** Same as [new_coservice] followed by [register] *)

    val register_new_coservice' :
      ?max_use:int ->
        ?timeout:float ->
        get_params: 
        ('get, [`WithoutSuffix] as 'tipo, 'gn) params_type ->
          ?error_handler:(server_params -> 
            (string * exn) list -> page Lwt.t) ->
              (server_params -> 'get -> unit -> page Lwt.t) ->
                ('get, unit, 
                 [> `Nonattached of [> `Get] na_s ],
                 'tipo, 'gn, unit param_name, [> `Registrable ])
                  service
(** Same as [new_coservice'] followed by [register] *)

    val register_new_coservice_for_session :
        server_params ->
        ?max_use:int ->
        ?timeout:float ->
          fallback:(unit, unit, 
                    [ `Attached of [ `Internal of [ `Service ] * [`Get]] a_s ],
                    [ `WithoutSuffix ] as 'tipo, 
                    unit param_name, unit param_name, [< registrable ])
            service ->
              get_params: 
                ('get, [`WithoutSuffix] as 'tipo, 'gn) params_type ->
                  ?error_handler:(server_params -> (string * exn) list -> 
                    page Lwt.t) ->
                      (server_params -> 'get -> unit -> page Lwt.t) ->
                        ('get, unit, 
                         [> `Attached of 
                           [> `Internal of [> `Coservice ] * [> `Get] ] a_s ], 
                         'tipo, 'gn, unit param_name, 
                         [> `Registrable ])
                          service
(** Same as [new_coservice] followed by [register_for_session] *)

    val register_new_coservice_for_session' :
        server_params ->
        ?max_use:int ->
        ?timeout:float ->
          get_params: 
            ('get, [`WithoutSuffix] as 'tipo, 'gn) params_type ->
              ?error_handler:(server_params -> (string * exn) list -> 
                page Lwt.t) ->
                  (server_params -> 'get -> unit -> page Lwt.t) ->
                    ('get, unit, [> `Nonattached of [> `Get] na_s ], 
                     'tipo, 'gn, unit param_name, 
                     [> `Registrable ])
                      service
(** Same as [new_coservice'] followed by [register_for_session] *)

    val register_new_post_service :
        fallback:('get, unit, 
                  [ `Attached of [ `Internal of 
                    ([ `Service | `Coservice ] as 'kind) * [`Get] ] a_s ],
                  [< suff ] as 'tipo, 'gn,
                  unit param_name, [< `Registrable ])
        service ->
          post_params:('post, [ `WithoutSuffix ], 'pn) params_type ->
            ?error_handler:(server_params -> (string * exn) list -> 
              page Lwt.t) ->
                (server_params -> 'get -> 'post -> page Lwt.t) ->
                  ('get, 'post, [> `Attached of
                    [> `Internal of 'kind * [> `Post] ] a_s ], 
                   'tipo, 'gn, 'pn, [> `Registrable ])
                    service
(** Same as [new_post_service] followed by [register] *)

    val register_new_post_coservice :
      ?max_use:int ->
        ?timeout:float ->
        fallback:('get, unit , 
                  [ `Attached of 
                    [ `Internal of [< `Service | `Coservice ] * [`Get] ] a_s ],
                   [< suff ] as 'tipo, 
                   'gn, unit param_name, [< `Registrable ])
        service ->
          post_params:('post, [ `WithoutSuffix ], 'pn) params_type ->
            ?error_handler:(server_params -> (string * exn) list -> 
              page Lwt.t) ->
                (server_params -> 'get -> 'post -> page Lwt.t) ->
                  ('get, 'post, 
                   [> `Attached of 
                     [> `Internal of [> `Coservice ] * [> `Post] ] a_s ], 
                     'tipo, 'gn, 'pn, [> `Registrable ])
                    service
(** Same as [new_post_coservice] followed by [register] *)

    val register_new_post_coservice' :
      ?max_use:int ->
        ?timeout:float ->
        post_params:('post, [ `WithoutSuffix ], 'pn) params_type ->
          ?error_handler:(server_params -> (string * exn) list -> 
            page Lwt.t) ->
              (server_params -> unit -> 'post -> page Lwt.t) ->
                (unit, 'post, [> `Nonattached of [> `Post] na_s ], 
                 [ `WithoutSuffix ], unit param_name, 'pn,
                 [> `Registrable ])
                  service
(** Same as [new_post_coservice'] followed by [register] *)

(*
    val register_new_get_post_coservice' :
      ?max_use:int ->
        ?timeout:float ->
        fallback:('get, unit , 
                  [ `Nonattached of [`Get] na_s ],
                   [< suff ] as 'tipo, 
                   'gn, unit param_name, [< `Registrable ])
        service ->
          post_params:('post, [ `WithoutSuffix ], 'pn) params_type ->
            ?error_handler:(server_params -> (string * exn) list -> 
              page Lwt.t) ->
                (server_params -> 'get -> 'post -> page Lwt.t) ->
                  ('get, 'post, [> `Nonattached of [> `Post] na_s ], 
                   [> 'tipo], 'gn, 'pn, [> `Registrable ])
                    service
(* * Same as [new_get_post_coservice'] followed by [register] *)
*)

    val register_new_post_coservice_for_session :
        server_params ->
        ?max_use:int ->
        ?timeout:float ->
          fallback:('get, unit, 
                    [< `Attached of [< `Internal of
                      [< `Service | `Coservice ] * [`Get] ] a_s ],
                    [< suff ] as 'tipo, 
                    'gn, unit param_name, [< `Registrable ])
            service ->
              post_params:('post, [ `WithoutSuffix ], 'pn) params_type ->
                ?error_handler:(server_params -> 
                  (string * exn) list -> page Lwt.t) ->
                    (server_params -> 'get -> 'post -> page Lwt.t) ->
                      ('get, 'post, 
                       [> `Attached of 
                         [> `Internal of [> `Coservice ] * [> `Post]] a_s ], 
                       'tipo, 'gn, 'pn, [> `Registrable ])
                        service
(** Same as [new_post_coservice] followed by [register_for_session] *)

    val register_new_post_coservice_for_session' :
        server_params ->
        ?max_use:int ->
        ?timeout:float ->
          post_params:('post, [ `WithoutSuffix ], 'pn) params_type ->
            ?error_handler:(server_params -> 
              (string * exn) list -> page Lwt.t) ->
                (server_params -> unit -> 'post -> page Lwt.t) ->
                  (unit, 'post, [> `Nonattached of [> `Post] na_s ], 
                   [ `WithoutSuffix ], unit param_name, 'pn, 
                   [> `Registrable ])
                    service
(** Same as [new_post_coservice'] followed by [register_for_session] *)

(*
    val register_new_get_post_coservice_for_session' :
        server_params ->
        ?max_use:int ->
        ?timeout:float ->
          fallback:('get, unit, [ `Nonattached of [`Get] na_s ],
                    [< suff ] as 'tipo, 
                    'gn, unit param_name, [< `Registrable ])
            service ->
              post_params:('post, [ `WithoutSuffix ], 'pn) params_type ->
                ?error_handler:(server_params -> 
                  (string * exn) list -> page Lwt.t) ->
                    (server_params -> 'get -> 'post -> page Lwt.t) ->
                      ('get, 'post, [> `NonAttached of [> `Post] na_s ], 
                       'tipo, 'gn, 'pn, [> `Registrable ])
                        service
(* * Same as [new_get_post_coservice] followed by [register_for_session] *)
*)


    val register_public :
        server_params ->
        coservice:('get, 'post,
                 [< `Attached of 
                   [< `Internal of [< `Coservice ] * getpost ] a_s
                 | `Nonattached of getpost na_s ],
                 [< suff ], 'gn, 'pn, [ `Registrable ]) service ->
        ?error_handler:(server_params ->
                               (string * exn) list -> page Lwt.t) ->
        (server_params -> 'get -> 'post -> page Lwt.t) ->
          unit
(** Register a coservice in the global table after initialization.
    [register] can be used only during the initalization of the module.
    After this phase, use that function, that takes [sp] as parameter.
    Warning: The use of that function is not encouraged for coservices
    without timeout, as such services will be available only until the end
    of the server process!
 *)

    val register_new_public_coservice :
        server_params ->
          ?max_use:int ->
          ?timeout:float ->
            fallback:(unit, unit, 
                      [ `Attached of [ `Internal of [ `Service ] * [`Get]] a_s ],
                      [ `WithoutSuffix ] as 'tipo, 
                      unit param_name, unit param_name, [< registrable ])
              service ->
                get_params: 
                  ('get, [`WithoutSuffix], 'gn) params_type ->
                    ?error_handler:(server_params -> 
                      (string * exn) list -> page Lwt.t) ->
                        (server_params -> 'get -> unit -> page Lwt.t) ->
                          ('get, unit, 
                           [> `Attached of 
                             [> `Internal of [> `Coservice ] * [> `Get]] a_s ], 
                           'tipo, 'gn, unit param_name, 
                           [> `Registrable ])
                            service
(** Same as [new_coservice] followed by [register_public] 
    Warning: The use of that function is not encouraged for coservices
    without timeout, as such services will be available only until the end
    of the server process!
*)

    val register_new_public_coservice' :
        server_params ->
          ?max_use:int ->
          ?timeout:float ->
          get_params: 
              ('get, [`WithoutSuffix] as 'tipo, 'gn) params_type ->
                ?error_handler:(server_params -> 
                  (string * exn) list -> page Lwt.t) ->
                    (server_params -> 'get -> unit -> page Lwt.t) ->
                      ('get, unit, 
                       [> `Nonattached of [> `Get] na_s ],
                       'tipo, 'gn, unit param_name, [> `Registrable ])
                        service
(** Same as [new_coservice'] followed by [register_public] 
    Warning: The use of that function is not encouraged for coservices
    without timeout, as such services will be available only until the end
    of the server process!
*)

    val register_new_post_public_coservice :
        server_params ->
        ?max_use:int ->
        ?timeout:float ->
          fallback:('get, unit, 
                    [< `Attached of [< `Internal of
                      [< `Service | `Coservice ] * [`Get] ] a_s ],
                    [< suff ] as 'tipo, 
                    'gn, unit param_name, [< `Registrable ])
            service ->
              post_params:('post, [ `WithoutSuffix ], 'pn) params_type ->
                ?error_handler:(server_params -> 
                  (string * exn) list -> page Lwt.t) ->
                    (server_params -> 'get -> 'post -> page Lwt.t) ->
                      ('get, 'post, 
                       [> `Attached of 
                         [> `Internal of [> `Coservice ] * [> `Post]] a_s ], 
                       'tipo, 'gn, 'pn, [> `Registrable ])
                        service
(** Same as [new_post_coservice] followed by [register_for_session] 
    Warning: The use of that function is not encouraged for coservices
    without timeout, as such services will be available only until the end
    of the server process!
*)

    val register_new_post_public_coservice' :
        server_params ->
        ?max_use:int ->
        ?timeout:float ->
          post_params:('post, [ `WithoutSuffix ], 'pn) params_type ->
            ?error_handler:(server_params -> 
              (string * exn) list -> page Lwt.t) ->
                (server_params -> unit -> 'post -> page Lwt.t) ->
                  (unit, 'post, [> `Nonattached of [> `Post] na_s ], 
                   [ `WithoutSuffix ], unit param_name, 'pn, 
                   [> `Registrable ])
                    service
(** Same as [new_post_coservice'] followed by [register_for_session]
    Warning: The use of that function is not encouraged for coservices
    without timeout, as such services will be available only until the end
    of the server process!
*)

(*
    val register_new_get_post_public_coservice' :
        server_params ->
        ?max_use:int ->
        ?timeout:float ->
          fallback:('get, unit, [ `Nonattached of [`Get] na_s ],
                    [< suff ] as 'tipo, 
                    'gn, unit param_name, [< `Registrable ])
            service ->
              post_params:('post, [ `WithoutSuffix ], 'pn) params_type ->
                ?error_handler:(server_params -> 
                  (string * exn) list -> page Lwt.t) ->
                    (server_params -> 'get -> 'post -> page Lwt.t) ->
                      ('get, 'post, [> `NonAttached of [> `Post] na_s ], 
                       'tipo, 'gn, 'pn, [> `Registrable ])
                        service
(* * Same as [new_get_post_coservice] followed by [register_for_session] 
    Warning: The use of that function is not encouraged for coservices
    without timeout, as such services will be available only until the end
    of the server process!
*)
*)




  end



module type ELIOMREGSIG =
  sig
    include ELIOMREGSIG1
    module Cookies : ELIOMREGSIG1 
    with type page = page * cookieslist
  end




module type ELIOMSIG = sig
  include ELIOMREGSIG
  include ELIOMFORMSIG
end


module MakeRegister = functor
  (Pages : REGCREATE) ->
    (struct

      type page = Pages.page

      let send = Pages.send

      module Cookies = struct
        
        type page = Pages.page * cookieslist
              
        let send ?(cookies=[]) ?charset sp (p, cl) =
          Pages.send ~cookies:(cookies@cl) ?charset sp p

        let register_aux
            current_dir
            tables
            session (* registering during session *)
            ~service
            ?(error_handler = fun sp l -> raise (Eliom_Typing_Error l))
            page_generator =
          match service.kind with
            `Attached attser ->
              add_service
                tables 
	        current_dir
	        session
	        attser.url
                ({state = (attser.get_state, attser.post_state)},
                 (service.unique_id,
                  (match service.max_use with
                    None -> None
                  | Some i -> Some (ref i)),
                  (match service.timeout with
                    None -> None
                  | Some t -> Some (t, ref (t +. Unix.time ()))),
                  (fun ((ri,_,(_,_,_,_,suff)) as sp) -> 
                    (catch (fun () -> 
                      (force ri.ri_post_params) >>=
                      (fun post_params ->
                        (force ri.ri_files) >>=
                        (fun files ->
                          (page_generator sp
                             (reconstruct_params 
                                service.get_params_type
                                (force ri.ri_get_params)
                                []
                                suff)
                             (reconstruct_params
                                service.post_params_type
                                post_params
                                files
                                [])))))
                       (function
                           Eliom_Typing_Error l -> error_handler sp l
                         | e -> fail e)) >>=
                    (fun (content, cookies_to_set) -> 
                      return (Pages.send 
                                ~cookies:cookies_to_set sp content)))))
          | `Nonattached naser ->
              add_naservice 
	        tables
	        current_dir 
	        session
	        naser.na_name
                ((match service.max_use with
                  None -> None
                | Some i -> Some (ref i)),
                 (match service.timeout with
                  None -> None
                | Some t -> Some (t, ref (t +. Unix.time ()))),
	         (fun ((ri,_,_) as sp) ->
	           (catch
	              (fun () ->
	                (force ri.ri_post_params) >>=
	                (fun post_params ->
		          (force ri.ri_files) >>=
		          (fun files ->
                            (page_generator sp 
                               (reconstruct_params
                                  service.get_params_type
                                  (force ri.ri_get_params)
                                  []
                                  [])
                               (reconstruct_params
                                  service.post_params_type
                                  post_params
                                  files
                                  [])))))
	              (function
                          Eliom_Typing_Error l -> error_handler sp l
                        | e -> fail e)) >>=
                   (fun (content, cookies_to_set) -> 
                     return (Pages.send 
                               ~cookies:cookies_to_set sp content))))


        let register ~service ?error_handler page_gen =
          let url =
            match service.kind with
              `Attached attser -> Some attser.url
            | `Nonattached naser -> None
          in
          match global_register_allowed () with
            Some get_current_hostdir ->
              remove_unregistered (url, service.unique_id);
              let (globtables, _, _), curdir = get_current_hostdir () in
              register_aux 
                curdir
                globtables
                false 
                ~service ?error_handler page_gen
          | _ -> raise Eliom_function_forbidden_outside_site_loading
                (*
              Messages.warning
                ((match url with
                  None -> "Eliom: <Non-attached service>"
                | Some u -> "Eliom: URL .../"^(string_of_url_path u))^
                 " : Public service registration outside <site></site> \
                   or after initialization forbidden! \
                   Please correct your module! (ignored)")
                   *)
(* Warning: not possible to register coservices in public table after init
   with that function, because we do not know working_dir!
 *)

(* WARNING: if we create a new service without registering it,
   we can have a link towards a page that does not exist!!! :-(
   That's why I impose to register all service during init.
   The only other way I see to avoid this is to impose a syntax extension
   like "let rec" for service...
 *)



        let register_for_session
            (ri, si, (curdir, _, sesstab, _, _))
            ~service
            ?error_handler
            page =
          register_aux
            ?error_handler
            curdir
            !sesstab
            true 
            ~service page

        let register_public
            (ri, si, (curdir, (globtables, _, _), _, _, _))
            ~coservice
            ?error_handler page_gen =
          register_aux 
            ?error_handler
            curdir
            globtables
            true
            ~service:coservice
            page_gen


        let register_new_service 
            ~url
            ~get_params
            ?error_handler
            page =
          let u = new_service ~url ~get_params () in
          register ~service:u ?error_handler page;
          u
            
        let register_new_coservice
            ?max_use
            ?timeout
            ~fallback
            ~get_params
            ?error_handler
            page =
          let u = new_coservice ?max_use ?timeout ~fallback ~get_params () in
          register ~service:u ?error_handler page;
          u

        let register_new_coservice'
            ?max_use
            ?timeout
            ~get_params
            ?error_handler
            page =
          let u = new_coservice' ?max_use ?timeout ~get_params () in
          register ~service:u ?error_handler page;
          u

        let register_new_coservice_for_session
            sp
            ?max_use
            ?timeout
            ~fallback
            ~get_params
            ?error_handler
            page =
          let u = new_coservice ?max_use ?timeout ~fallback ~get_params () in
          register_for_session sp ~service:u ?error_handler page;
          u

        let register_new_coservice_for_session'
            sp
            ?max_use
            ?timeout
            ~get_params
            ?error_handler
            page =
          let u = new_coservice' ?max_use ~get_params () in
          register_for_session sp ~service:u ?error_handler page;
          u

        let register_new_public_coservice
            sp
            ?max_use
            ?timeout
            ~fallback
            ~get_params
            ?error_handler
            page =
          let u = new_coservice ?max_use ?timeout ~fallback ~get_params () in
          register_public sp ~coservice:u ?error_handler page;
          u

        let register_new_public_coservice'
            sp
            ?max_use
            ?timeout
            ~get_params
            ?error_handler
            page =
          let u = new_coservice' ?max_use ?timeout ~get_params () in
          register_public sp ~coservice:u ?error_handler page;
          u



        let register_new_post_service 
            ~fallback
            ~post_params
            ?error_handler
            page_gen =
          let u = new_post_service 
              ~fallback:fallback ~post_params:post_params () in
          register ~service:u ?error_handler page_gen;
          u

        let register_new_post_coservice
            ?max_use
            ?timeout
            ~fallback
            ~post_params
            ?error_handler
            page_gen =
          let u = 
            new_post_coservice ?max_use ?timeout ~fallback ~post_params () in
          register ~service:u ?error_handler page_gen;
          u

        let register_new_post_coservice'
            ?max_use
            ?timeout
            ~post_params
            ?error_handler
            page_gen =
          let u = new_post_coservice' ?max_use ?timeout ~post_params () in
          register ~service:u ?error_handler page_gen;
          u

(*
   let register_new_get_post_coservice'
   ?max_use
   ?timeout
   ~fallback
   ~post_params
   ?error_handler
   page_gen =
   let u = new_get_post_coservice'
   ?max_use ?timeout ~fallback ~post_params () in
   register ~service:u ?error_handler page_gen;
   u
 *)

        let register_new_post_coservice_for_session
            sp
            ?max_use
            ?timeout
            ~fallback
            ~post_params
            ?error_handler
            page_gen =
          let u = new_post_coservice 
              ?max_use ?timeout ~fallback ~post_params () in
          register_for_session sp ~service:u ?error_handler page_gen;
          u

        let register_new_post_coservice_for_session'
            sp
            ?max_use
            ?timeout
            ~post_params
            ?error_handler
            page_gen =
          let u = new_post_coservice' ?max_use ?timeout ~post_params () in
          register_for_session sp ~service:u ?error_handler page_gen;
          u

(*
   let register_new_get_post_coservice_for_session'
   sp
   ?max_use
   ?timeout
   ~fallback
   ~post_params
   ?error_handler
   page_gen =
   let u = new_get_post_coservice'
   ?max_use ?timeout ~fallback ~post_params () in
   register_for_session sp ~service:u ?error_handler page_gen;
   u
 *)

        let register_new_post_public_coservice
            sp
            ?max_use
            ?timeout
            ~fallback
            ~post_params
            ?error_handler
            page_gen =
          let u = new_post_coservice
              ?max_use ?timeout ~fallback ~post_params () in
          register_public sp ~coservice:u ?error_handler page_gen;
          u

        let register_new_post_public_coservice'
            sp
            ?max_use
            ?timeout
            ~post_params
            ?error_handler
            page_gen =
          let u = new_post_coservice' ?max_use ?timeout ~post_params () in
          register_public sp ~coservice:u ?error_handler page_gen;
          u

(*
   let register_new_get_post_public_coservice'
   sp
   ?max_use
   ?timeout
   ~fallback
   ~post_params
   ?error_handler
   page_gen =
   let u = new_get_post_coservice' 
   ?max_use ?timeout ~fallback ~post_params () in
   register_public sp ~coservice:u ?error_handler page_gen;
   u
 *)

      end


      let make_error_handler ?error_handler () = 
        match error_handler with
          None -> None
        | Some eh -> Some (fun sp l -> eh sp l >>= (fun r -> return (r,[])))

      let register ~service ?error_handler page_gen =
        Cookies.register
          ~service
          ?error_handler:(make_error_handler ?error_handler ())
          (fun sp g p -> page_gen sp g p >>= (fun r -> return (r,[])))

      let register_for_session
          sp
          ~service
          ?error_handler
          page =
        Cookies.register_for_session
          sp
          ~service
          ?error_handler:(make_error_handler ?error_handler ())
          (fun sp g p -> page sp g p >>= (fun r -> return (r,[])))

      let register_public
          sp
          ~coservice
          ?error_handler
          page =
        Cookies.register_public
          sp
          ~coservice
          ?error_handler:(make_error_handler ?error_handler ())
          (fun sp g p -> page sp g p >>= (fun r -> return (r,[])))


      let register_new_service 
          ~url
          ~get_params
          ?error_handler
          page =
        Cookies.register_new_service 
          ~url
          ~get_params
          ?error_handler:(make_error_handler ?error_handler ())
          (fun sp g p -> page sp g p >>= (fun r -> return (r,[])))
          
      let register_new_coservice
          ?max_use
          ?timeout
          ~fallback
          ~get_params
          ?error_handler
          page =
        Cookies.register_new_coservice
          ?max_use
          ?timeout
          ~fallback
          ~get_params
          ?error_handler:(make_error_handler ?error_handler ())
          (fun sp g p -> page sp g p >>= (fun r -> return (r,[])))

      let register_new_coservice'
          ?max_use
          ?timeout
          ~get_params
          ?error_handler
          page =
        Cookies.register_new_coservice'
          ?max_use
          ?timeout
          ~get_params
          ?error_handler:(make_error_handler ?error_handler ())
          (fun sp g p -> page sp g p >>= (fun r -> return (r,[])))

      let register_new_coservice_for_session
          sp
          ?max_use
          ?timeout
          ~fallback
          ~get_params
          ?error_handler
          page =
      Cookies.register_new_coservice_for_session
          sp
          ?max_use
          ?timeout
          ~fallback
          ~get_params
          ?error_handler:(make_error_handler ?error_handler ())
          (fun sp g p -> page sp g p >>= (fun r -> return (r,[])))

      let register_new_coservice_for_session'
          sp
          ?max_use
          ?timeout
          ~get_params
          ?error_handler
          page =
      Cookies.register_new_coservice_for_session'
          sp
          ?max_use
          ?timeout
          ~get_params
          ?error_handler:(make_error_handler ?error_handler ())
          (fun sp g p -> page sp g p >>= (fun r -> return (r,[])))

      let register_new_public_coservice
          sp
          ?max_use
          ?timeout
          ~fallback
          ~get_params
          ?error_handler
          page =
      Cookies.register_new_public_coservice
          sp
          ?max_use
          ?timeout
          ~fallback
          ~get_params
          ?error_handler:(make_error_handler ?error_handler ())
          (fun sp g p -> page sp g p >>= (fun r -> return (r,[])))

      let register_new_public_coservice'
          sp
          ?max_use
          ?timeout
          ~get_params
          ?error_handler
          page =
      Cookies.register_new_public_coservice'
          sp
          ?max_use
          ?timeout
          ~get_params
          ?error_handler:(make_error_handler ?error_handler ())
          (fun sp g p -> page sp g p >>= (fun r -> return (r,[])))

      let register_new_post_service 
          ~fallback
          ~post_params
          ?error_handler
          page_gen =
      Cookies.register_new_post_service 
          ~fallback
          ~post_params
          ?error_handler:(make_error_handler ?error_handler ())
          (fun sp g p -> page_gen sp g p >>= (fun r -> return (r,[])))

      let register_new_post_coservice
          ?max_use
          ?timeout
          ~fallback
          ~post_params
          ?error_handler
          page_gen =
        Cookies.register_new_post_coservice
          ?max_use
          ?timeout
          ~fallback
          ~post_params
          ?error_handler:(make_error_handler ?error_handler ())
          (fun sp g p -> page_gen sp g p >>= (fun r -> return (r,[])))

      let register_new_post_coservice'
          ?max_use
          ?timeout
          ~post_params
          ?error_handler
          page_gen =
        Cookies.register_new_post_coservice'
          ?max_use
          ?timeout
          ~post_params
          ?error_handler:(make_error_handler ?error_handler ())
          (fun sp g p -> page_gen sp g p >>= (fun r -> return (r,[])))

(*
   let register_new_get_post_coservice'
          ?max_use
          ?timeout
   ~fallback
   ~post_params
   ?error_handler
   page_gen =
   Cookies.register_new_get_post_coservice'
          ?max_use
          ?timeout
   ~fallback
   ~post_params
   ?error_handler:(make_error_handler ?error_handler ())
          (fun sp g p -> page_gen sp g p >>= (fun r -> return (r,[])))

 *)

      let register_new_post_coservice_for_session
          sp
          ?max_use
          ?timeout
          ~fallback
          ~post_params
          ?error_handler
          page_gen =
        Cookies.register_new_post_coservice_for_session
          sp
          ?max_use
          ?timeout
          ~fallback
          ~post_params
          ?error_handler:(make_error_handler ?error_handler ())
          (fun sp g p -> page_gen sp g p >>= (fun r -> return (r,[])))

      let register_new_post_coservice_for_session'
          sp
          ?max_use
          ?timeout
          ~post_params
          ?error_handler
          page_gen =
      Cookies.register_new_post_coservice_for_session'
          sp
          ?max_use
          ?timeout
          ~post_params
          ?error_handler:(make_error_handler ?error_handler ())
          (fun sp g p -> page_gen sp g p >>= (fun r -> return (r,[])))

(*
   let register_new_get_post_coservice_for_session'
   sp
          ?max_use
          ?timeout
   ~fallback
   ~post_params
   ?error_handler
   page_gen =
   Cookies.register_new_get_post_coservice_for_session'
   sp
          ?max_use
          ?timeout
   ~fallback
   ~post_params
   ?error_handler:(make_error_handler ?error_handler ())
          (fun sp g p -> page_gen sp g p >>= (fun r -> return (r,[])))

 *)

      let register_new_post_public_coservice
          sp
          ?max_use
          ?timeout
          ~fallback
          ~post_params
          ?error_handler
          page_gen =
        Cookies.register_new_post_public_coservice
          sp
          ?max_use
          ?timeout
          ~fallback
          ~post_params
          ?error_handler:(make_error_handler ?error_handler ())
          (fun sp g p -> page_gen sp g p >>= (fun r -> return (r,[])))

      let register_new_post_public_coservice'
          sp
          ?max_use
          ?timeout
          ~post_params
          ?error_handler
          page_gen =
      Cookies.register_new_post_public_coservice'
          sp
          ?max_use
          ?timeout
          ~post_params
          ?error_handler:(make_error_handler ?error_handler ())
          (fun sp g p -> page_gen sp g p >>= (fun r -> return (r,[])))

(*
   let register_new_get_post_public_coservice'
   sp
          ?max_use
          ?timeout
   ~fallback
   ~post_params
   ?error_handler
   page_gen =
   Cookies.register_new_get_post_public_coservice'
   sp
          ?max_use
          ?timeout
   ~fallback
   ~post_params
   ?error_handler:(make_error_handler ?error_handler ())
          (fun sp g p -> page_gen sp g p >>= (fun r -> return (r,[])))

 *)

    end : ELIOMREGSIG with 
                 type page = Pages.page)




let make_string_uri
    service
    ((_,si,_) as sp)
    getparams : string =
  match service.kind with
    `Attached attser ->
      begin
        let suff, params_string = 
          construct_params service.get_params_type getparams in
        let preapplied_params = 
          construct_params_string service.pre_applied_parameters in
        let params_string =
          concat_strings preapplied_params "&" params_string in
        let uri = 
          (if attser.att_kind = `External
          then (reconstruct_absolute_url_path
                  (get_current_path sp) attser.url suff)
          else (reconstruct_relative_url_path
                  (get_current_path sp) attser.url suff))
        in
        match attser.get_state with
          None ->
            add_to_string uri "?" params_string
        | Some i -> 
            add_to_string (uri^"?"^
                           get_state_param_name^"="^(string_of_int i))
              "&" params_string
      end
  | `Nonattached naser ->
      let current_get_params =
        (get_other_get_params sp) @ 
        (remove_prefixed_param na_co_param_prefix (get_get_params sp))
      in
      let current_get_params =
        match fst si.si_state_info with
          None -> current_get_params
        | Some i -> (get_state_param_name, 
                     (string_of_int i))::current_get_params
      in
      let _, params_string = 
        construct_params service.get_params_type getparams in
      let preapplied_params = 
        construct_params_string service.pre_applied_parameters in
      let params_string =
        concat_strings preapplied_params "&" params_string in
      let naservice_param = 
        match fst naser.na_name with
          Some n -> naservice_prefix^naservice_name^"="^n
        | _ -> assert false
      in
      let current_get_params_string = 
        construct_params_string current_get_params in
      (("/"^(get_current_path_string sp))^"?"^
       (concat_strings
          current_get_params_string
          "&"
          (naservice_param^"&"^params_string))
      )


module MakeForms = functor
  (Pages : FORMCREATE) ->
    (struct
      
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
      type select_elt = Pages.select_elt
      type input_elt = Pages.input_elt
      type pcdata_elt = Pages.pcdata_elt
            
      type a_attrib_t = Pages.a_attrib_t
      type form_attrib_t = Pages.form_attrib_t
      type input_attrib_t = Pages.input_attrib_t
      type textarea_attrib_t = Pages.textarea_attrib_t
      type select_attrib_t = Pages.select_attrib_t
      type link_attrib_t = Pages.link_attrib_t
      type script_attrib_t = Pages.script_attrib_t
      type input_type_t = Pages.input_type_t


(** Functions to construct web pages: *)

      let a ?a
          service
          ((_,si,_) as sp)
          content
          getparams =
        match service.kind with
          `Attached attser ->
            (let suff, params_string = 
              construct_params service.get_params_type getparams in
            let preapplied_params = 
              construct_params_string service.pre_applied_parameters in
            let params_string =
              concat_strings preapplied_params "&" params_string in
            let uri = 
              (if attser.att_kind = `External
              then 
                (reconstruct_absolute_url_path
                   (get_current_path sp) attser.url suff)
              else 
                (reconstruct_relative_url_path
                   (get_current_path sp) attser.url suff))
            in
            match attser.get_state with
              None ->
                Pages.make_a 
                  ?a ~href:(add_to_string uri "?" params_string) content
            | Some i -> 
                Pages.make_a ?a
                  ~href:(add_to_string 
                           (uri^"?"^get_state_param_name^"="^(string_of_int i))
                           "&" params_string)
                  content)
        | `Nonattached naser ->
            let current_get_params =
              (get_other_get_params sp) @ 
              (remove_prefixed_param na_co_param_prefix (get_get_params sp))
            in
            let current_get_params =
              match fst si.si_state_info with
                None -> current_get_params
              | Some i -> (get_state_param_name, 
                           (string_of_int i))::current_get_params
            in
            let _, params_string = 
              construct_params service.get_params_type getparams in
            let preapplied_params = 
              construct_params_string service.pre_applied_parameters in
            let params_string =
              concat_strings preapplied_params "&" params_string in
            let naservice_param = 
              match fst naser.na_name with
                Some n -> naservice_prefix^naservice_name^"="^n
              | _ -> assert false
            in
            let current_get_params_string = 
              construct_params_string current_get_params in
            Pages.make_a ?a
              ~href:(("/"^(get_current_path_string sp))^"?"^
                     (concat_strings
                        current_get_params_string
                        "&"
                        (naservice_param^"&"^params_string))
                    )
              content

      let make_params_names (params : ('t,'tipo,'n) params_type) : 'n =
        let rec aux prefix suffix = function
            TProd (t1, t2) -> Obj.magic (aux prefix suffix t1, aux prefix suffix t2)
          | TInt name -> Obj.magic (prefix^name^suffix)
          | TFloat name -> Obj.magic (prefix^name^suffix)
          | TString name -> Obj.magic (prefix^name^suffix)
          | TFile name -> Obj.magic (prefix^name^suffix)
          | TUserType (name,o,t) -> Obj.magic (prefix^name^suffix)
          | TUnit -> Obj.magic ("")
          | TESuffix n -> Obj.magic n
          | TESuffixs n -> Obj.magic n
          | TESuffixu (n,_,_) -> Obj.magic n
          | TSuffix t -> Obj.magic (aux prefix suffix t)
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
          service
          ((_,si,_) as sp)
          f =
        match service.kind with
          `Attached attser ->
            let urlname =
              (if attser.att_kind = `External
              then (reconstruct_absolute_url_path
                      (get_current_path sp) attser.url None)
              else (reconstruct_relative_url_path
                      (get_current_path sp) attser.url None)) in
            let state_param =
              (match attser.get_state with
                None -> None
              | Some i -> 
                  let i' = string_of_int i in
                  Some (Pages.make_input ~typ:Pages.hidden
                          ~name:get_state_param_name ~value:i' ()))
            in
            let inside = f (make_params_names service.get_params_type) in
            let inside =
              List.fold_left
                (fun s (n,v) -> 
                  Pages.cons_form
                    (Pages.make_hidden_field
	               (Pages.make_input
                          ~typ:Pages.hidden ~name:n ~value:v ()))
                    s
                )
                inside
                service.pre_applied_parameters
            in
            let i1, i =
              match state_param, inside with
                Some s, i -> (Pages.make_hidden_field s),i
              | None, i -> Pages.remove_first i
            in Pages.make_get_form ?a ~action:urlname i1 i
        | `Nonattached naser ->
            let urlname = "/"^(get_current_path_string sp) in
            let naservice_param_name = naservice_prefix^naservice_name in
            let naservice_param = 
              match fst naser.na_name with
                Some n -> n
              | _ -> assert false
            in
            let naservice_line = 
              Pages.make_hidden_field
	        (Pages.make_input
	           ~typ:Pages.hidden 
                   ~name:naservice_param_name
                   ~value:naservice_param ())
            in
            let current_get_params =
              (get_other_get_params sp) @ 
              (remove_prefixed_param na_co_param_prefix (get_get_params sp))
            in
            let current_get_params =
              match fst si.si_state_info with
                None -> current_get_params
              | Some i -> (get_state_param_name, 
                           (string_of_int i))::current_get_params
            in
            let inside = f (make_params_names service.get_params_type) in
            let all_lines = 
              List.fold_left
                (fun s (n,v) -> 
                  Pages.cons_form
                    (Pages.make_hidden_field
	               (Pages.make_input
                          ~typ:Pages.hidden ~name:n ~value:v ()))
                    s
                )
                inside
                current_get_params
            in
            let all_lines =
              List.fold_left
                (fun s (n,v) -> 
                  Pages.cons_form
                    (Pages.make_hidden_field
	               (Pages.make_input
                          ~typ:Pages.hidden ~name:n ~value:v ()))
                    s
                )
                all_lines
                service.pre_applied_parameters
            in
            Pages.make_get_form ?a ~action:urlname naservice_line all_lines


      let post_form ?a
          service
          sp
          f 
          getparams =
        match service.kind with
          `Attached attser ->
            let suff,params_string = 
              construct_params service.get_params_type getparams in
            let preapplied_params = 
              construct_params_string service.pre_applied_parameters in
            let params_string =
              concat_strings preapplied_params "&" params_string in
            let params_string =
              match attser.get_state with
                None -> params_string
              | Some i -> 
                  add_to_string
                    (get_state_param_name^"="^(string_of_int i))
                    "&"
                    params_string
            in
            let urlname = 
              (if attser.att_kind = `External
              then (reconstruct_absolute_url_path
                      (get_current_path sp) attser.url suff)
              else (reconstruct_relative_url_path
                      (get_current_path sp) attser.url suff))
            in
            let state_param =
              (match  attser.post_state with
                None -> None
              | Some i -> 
                  let i' = string_of_int i in
                  Some (Pages.make_input ~typ:Pages.hidden
                          ~name:post_state_param_name ~value:i' ()))
            in
            let inside = f (make_params_names service.post_params_type) in
            let i1, i =
              match state_param, inside with
                Some s, i -> (Pages.make_hidden_field s),i
              | None, i -> Pages.remove_first i
            in Pages.make_post_form ?a
              ~action:(add_to_string urlname "?" params_string)
              i1 i
        | `Nonattached naser ->
            (* no GET params here for now *)
            let naservice_param_name = naservice_prefix^naservice_name in
            let naservice_param = 
              match snd naser.na_name with
                Some n -> n
              | _ -> assert false
            in
            let naservice_line = 
	      Pages.make_input
	        ~typ:Pages.hidden
                ~name:naservice_param_name
                ~value:naservice_param () 
            in
            let v = "/"^(get_full_url sp) in
            let inside = f (make_params_names service.post_params_type) in
            Pages.make_post_form ?a ~action:v
              (Pages.make_hidden_field naservice_line)
              inside

          



      let make_uri serv sp gp =
        Pages.make_uri_from_string (make_string_uri serv sp gp)
                  
          
          
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

      let select ?a ?selected fp lp (name : string param_name) =
        Pages.make_select ?a ~name:name ?selected fp lp

      let submit_input ?a s =
        Pages.make_input ?a ~typ:Pages.submit ~value:s ()

      let file_input ?a ?value (name : file_info param_name)  = 
        Pages.make_input ?a ~typ:Pages.file ?value ~name:name ()

    end : ELIOMFORMSIG with 
     type form_content_elt = Pages.form_content_elt
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
     and type select_elt = Pages.select_elt
     and type input_elt = Pages.input_elt
     and type pcdata_elt = Pages.pcdata_elt
     and type a_attrib_t = Pages.a_attrib_t
     and type form_attrib_t = Pages.form_attrib_t
     and type input_attrib_t = Pages.input_attrib_t
     and type textarea_attrib_t = Pages.textarea_attrib_t
     and type select_attrib_t = Pages.select_attrib_t
     and type link_attrib_t = Pages.link_attrib_t
     and type script_attrib_t = Pages.script_attrib_t
     and type input_type_t = Pages.input_type_t)


(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)


module Xhtmlreg_ = struct
  open XHTML.M
  open Xhtmltypes

  type page = xhtml elt

   let send ?(cookies=[]) ?charset sp content = 
     EliomResult 
       {res_cookies= cookies;
        res_lastmodified= None;
        res_etag= None;
        res_code= None;
        res_send_page= Predefined_senders.send_xhtml_page ~content:content;
        res_create_sender= Predefined_senders.create_xhtml_sender;
        res_charset= match charset with
          None -> get_config_file_charset sp
        | _ -> charset
      }

end

module Xhtmlforms_ = struct
  open XHTML.M
  open Xhtmltypes

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
  type select_elt = select elt
  type input_elt = input elt

  type link_elt = link elt
  type script_elt = script elt

  type pcdata_elt = pcdata elt

  type a_attrib_t = Xhtmltypes.a_attrib XHTML.M.attrib list
  type form_attrib_t = Xhtmltypes.form_attrib XHTML.M.attrib list
  type input_attrib_t = Xhtmltypes.input_attrib XHTML.M.attrib list
  type textarea_attrib_t = Xhtmltypes.textarea_attrib XHTML.M.attrib list
  type select_attrib_t = Xhtmltypes.select_attrib XHTML.M.attrib list
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

  let make_select ?(a=[]) ~name:name ?(selected=None) fp lp =
    let build_option selec p =
      let lsel = if selec then [a_selected `Selected] else []
      in
        match p with 
        | (None, s) -> option ~a:lsel (pcdata s)
        | (Some v, s) -> option ~a:((a_value v)::lsel) (pcdata s)
    in
      match selected with
      | None -> select ~a:((a_name name)::a) (build_option false fp)
          (List.map (build_option false) lp)
      | Some p -> select ~a:((a_name name)::a) (build_option true p)
          ((build_option false fp)::(List.map (build_option false) lp))
    
  let make_css_link ?(a=[]) uri =
    link ~a:((a_href uri)::
             (a_type "text/css")::(a_rel [`Stylesheet])::a) ()
      
  let make_js_script ?(a=[]) uri =
    script ~a:((a_src uri)::a) ~contenttype:"text/javascript" (pcdata "")

end



(****************************************************************************)
(*****************************************************************************)

module Xhtmlforms = MakeForms(Xhtmlforms_)
module Xhtmlreg = MakeRegister(Xhtmlreg_)

(* As we want -> [> a ] elt and not -> [ a ] elt, we define a new module: *)
module Xhtml = struct
  open XHTML.M
  open Xhtmltypes
  include Xhtmlforms
  include Xhtmlreg
  let a = (a : ?a:([< Xhtmltypes.a_attrib > `Href ] XHTML.M.attrib list) ->
    ('get, unit, 'b, [< `WithSuffix | `WithoutSuffix ], 'c, unit param_name,
     [< registrable ]) service ->
        server_params -> 
          Xhtmltypes.a_content XHTML.M.elt list -> 
            'get -> Xhtmltypes.a XHTML.M.elt
                :> ?a:([< Xhtmltypes.a_attrib > `Href ] XHTML.M.attrib list) ->
                  ('get, unit, 'b, [< `WithSuffix | `WithoutSuffix ], 'c,
                   unit param_name, [< registrable ]) 
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
         ('get, unit, 'c, 'd, 'getnames, unit param_name, 
          [< registrable ]) service ->
           server_params -> ('getnames -> form_content elt list) -> form elt
               :> ?a:([< form_attrib > `Method ] attrib list) ->
                 ('get, unit, 'c, 'd, 'getnames, unit param_name, [< registrable ]) service ->
                   server_params -> ('getnames -> form_content_elt_list) -> [>form] elt)

  let post_form = 
    (post_form
       : ?a:([< form_attrib > `Class `Id `Method ] attrib list) ->
         ('get, 'post, 'c, [< `WithSuffix | `WithoutSuffix ], 'getnames, 'postnames, [< registrable ]) service ->
           server_params ->
             ('postnames -> form_content elt list) -> 'get -> form elt
                 :> ?a:([< form_attrib > `Class `Id `Method ] attrib list) ->
                   ('get, 'post, 'c, [< `WithSuffix | `WithoutSuffix ], 'getnames, 'postnames, [< registrable ]) service ->
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

  let select = (select
                   : ?a:([< select_attrib > `Name ] attrib list ) ->
                 ?selected:((string option * string) option) ->
                 (string option * string) ->
                 ((string option * string) list) ->
                 string param_name ->
                 select elt
                   :> ?a:([< select_attrib > `Name ] attrib list ) ->
                 ?selected:((string option * string) option) ->
                 (string option * string) ->
                 ((string option * string) list) ->
                 string param_name -> 
                 [> select ] elt)

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


end

(****************************************************************************)
(****************************************************************************)
(****************************************************************************)
(****************************************************************************)

module Textreg_ = struct
  open XHTML.M
  open Xhtmltypes

  type page = string

  let send ?(cookies=[]) ?charset sp content = 
    EliomResult
      {res_cookies= cookies;
       res_lastmodified= None;
       res_etag= None;
       res_code= None;
       res_send_page= Predefined_senders.send_text_page ~content:content;
       res_create_sender= Predefined_senders.create_xhtml_sender;
       res_charset= match charset with
          None -> get_config_file_charset sp
        | _ -> charset
     }

end

module Textforms_ = struct
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
  type select_elt = string
  type input_elt = string

  type link_elt = string
  type script_elt = string

  type pcdata_elt = string

  type a_attrib_t = string
  type form_attrib_t = string
  type input_attrib_t = string
  type textarea_attrib_t = string
  type select_attrib_t = string
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

  let make_select ?(a="") ~name:name ?(selected=None) fp lp =
    let build_option selec p =
      let lsel = if selec then " selected=\"selected\"" else ""
      in
        match p with 
        | (None, s) -> "<option"^lsel^">"^s^"</option>"
        | (Some v, s) -> "<option value=\""^v^"\""^lsel^">"^s^"</option>"
    in
      match selected with
      | None -> ("<select name=\""^name^"\" "^a^">")^
          (build_option false fp)^
          (List.fold_left (fun s p -> (build_option false p)^s) "" lp)^
          "</select>"
      | Some p -> ("<select name=\""^name^"\" "^a^">")^
          (build_option true p)^
          ((build_option false fp)^
           (List.fold_left (fun s p -> (build_option false p)^s) "" lp))^
          "</select>"

  let make_css_link ?(a="") uri =
    "<link href=\""^uri^" type=\"text/css\" rel=\"stylesheet\" "^a^"/>"
                                                                      
  let make_js_script ?(a="") uri =
    "<script src=\""^uri^" contenttype=\"text/javascript\" "^a^"></script>"

end



(****************************************************************************)
(****************************************************************************)

module Textforms = MakeForms(Textforms_)
module Textreg = MakeRegister(Textreg_)

module Text = struct
  include Textforms
  include Textreg
end


(****************************************************************************)
(****************************************************************************)

(** Actions are like services, but do not generate any page. The current
   page is reloaded. *)
module Actionreg_ = struct
  open XHTML.M
  open Xhtmltypes

  type page = exn list

  let send ?(cookies=[]) ?charset sp content =
    EliomExn (content, cookies)

end

module Actions = MakeRegister(Actionreg_)


(** Unit services are like services, do not generate any page, and do not
    reload the page. To be used carefully. Probably not usefull at all. *)
module Unitreg_ = struct
  open XHTML.M
  open Xhtmltypes

  type page = unit

  let send ?(cookies=[]) ?charset sp content = 
    EliomResult
      {res_cookies= cookies;
       res_lastmodified= None;
       res_etag= None;
       res_code= None;
       res_send_page= Predefined_senders.send_empty ~content:content;
       res_create_sender= Predefined_senders.create_empty_sender;
       res_charset= None
     }

end


module Unit = MakeRegister(Unitreg_)


(** Redirection services are like services, but send a redirection instead
 of a page. 
   
   The HTTP/1.1 RFC says:
   If the 301 status code is received in response to a request other than GET or HEAD, the user agent MUST NOT automatically redirect the request unless it can be confirmed by the user, since this might change the conditions under which the request was issued.

   Here redirections are done towards services without parameters.
   (possibly preapplied).

 *)
module Redirreg_ = struct
  open XHTML.M
  open Xhtmltypes

  type page = string

  let send ?(cookies=[]) ?charset sp content =
    EliomResult
      {res_cookies= cookies;
       res_lastmodified= None;
       res_etag= None;
       res_code= None;
       res_send_page= 
       (fun ?cookies waiter ?code ?etag ~keep_alive
           ?last_modified ?location ?head ?charset s ->
             Predefined_senders.send_empty
               ~content:() 
               ?cookies
               waiter ~code:301 (* Moved permanently *) 
               ?etag ~keep_alive
               ?last_modified 
               ~location:content
               ?head ?charset s);
       res_create_sender= Predefined_senders.create_empty_sender;
       res_charset= None
     }

end


module Redirections = MakeRegister(Redirreg_)


(* Any is a module allowing to register service that decide themselves
   what they want to send.
 *)
module Anyreg_ = struct
  open XHTML.M
  open Xhtmltypes

  type page = result_to_send

  let send ?(cookies=[]) ?charset sp content = 
    match content with
      EliomResult res ->
        EliomResult
          {res with 
           res_cookies=cookies@res.res_cookies;
           res_charset= match charset with
             None -> res.res_charset
           | _ -> charset
         }
    | EliomExn (e, c) -> 
        EliomExn (e, cookies@c)

end

module Any = MakeRegister(Anyreg_)


(* Files is a module allowing to register services that send files *)
module Filesreg_ = struct
  open XHTML.M
  open Xhtmltypes

  type page = string

  let send ?(cookies=[]) ?charset sp filename = 
    let (filename, stat) =
      (try
        (* That piece of code has been pasted from staticmod.ml *)
        let stat = Unix.LargeFile.stat filename in
        let (filename, stat) = 
          Messages.debug ("Eliom.Files - Testing \""^filename^"\".");
          let path = get_current_path sp in
          if (stat.Unix.LargeFile.st_kind = Unix.S_DIR)
          then 
            if (filename.[(String.length filename) - 1]) = '/'
            then
              let fn2 = filename^"index.html" in
              Messages.debug ("Eliom.Files - Testing \""^fn2^"\".");
              (fn2,(Unix.LargeFile.stat fn2))
            else
              (if (path= []) || (path = [""])
              then 
                let fn2 = filename^"/index.html" in
                Messages.debug ("Eliom.Files - Testing \""^fn2^"\".");
                (fn2,(Unix.LargeFile.stat fn2))
              else (Messages.debug ("Eliom.Files - "^filename^" is a directory");
                    raise Ocsigen_Is_a_directory))
          else (filename, stat)
        in
        Messages.debug ("Eliom.Files - Looking for \""^filename^"\".");
        
        if (stat.Unix.LargeFile.st_kind 
              = Unix.S_REG)
        then begin
          Unix.access filename [Unix.R_OK];
          (filename, stat)
        end
        else raise Ocsigen_404 (* ??? *)
      with
        (Unix.Unix_error (Unix.EACCES,_,_))
      | Ocsigen_Is_a_directory
      | Ocsigen_malformed_url as e -> raise e
      | e -> raise Ocsigen_404)
    in
    EliomResult
      {res_cookies= cookies;
       res_lastmodified= (Some stat.Unix.LargeFile.st_mtime);
       res_etag= (Some (Predefined_senders.File_content.get_etag filename));
       res_code= None;
       res_send_page= Predefined_senders.send_file ~content:filename;
       res_create_sender= Predefined_senders.create_file_sender;
       res_charset= match charset with
         None -> get_config_file_charset sp
       | _ -> charset
     }


end

module Files = MakeRegister(Filesreg_)

(*****************************************************************************)
(** {2 persistent sessions} *)

open Ocsipersist

type 'a persistent_table = (int64 * 'a) Ocsipersist.table

let create_persistent_table = create_persistent_table

let get_persistent_data table sp =
  match (get_persistent_cookie sp) with
  | Some (c, k) -> 
      (catch
         (fun () ->
           find table c >>=
           (fun (k2, v) ->
             if k2 = k
             then return (Some v)
             else begin
               remove table c >>= (* It was an old cookie. I don't trust it! *)
               (fun () -> return None)
             end))
         (fun _ -> return None)) (* ?? If an error occurs, assume no data *)
         (* function 
           | Not_found -> return None
           | e -> fail e) *)
  | None -> return None

let set_persistent_data table sp value =
  create_persistent_cookie sp >>=
  (fun (c, k) -> add table c (k, value))

(*****************************************************************************)
(** {2 session data in memory} *)
type 'a table = 'a Cookies.t

let create_table = create_table

let create_table_during_session = create_table_during_session

let get_session_data table sp =
  match (get_cookie sp) with
  | Some c -> 
      (try
        Some (Cookies.find table c)
      with _ -> None)
  | None -> None

let set_session_data table sp value =
  let c = create_cookie sp in
  Cookies.replace table c value


(*****************************************************************************)
(** Close a session *)
let close_persistent_session (_,si,_) =
  (match !(si.si_persistent_cookie) with
  | Some (c, _) -> 
      catch
        (fun () -> remove_from_all_persistent_tables c)
        (fun _ -> return ())
  | None -> return ()) >>=
  (fun () ->
    si.si_persistent_cookie := None;
    return ())

let close_volatile_session ((_, si, (_,(_,_,_),sesstab,_,_)) as sp) = 
  remove_session sp;
  sesstab := empty_tables ();
  si.si_cookie := None

let close_session sp =
  close_volatile_session sp;
  close_persistent_session sp


(*****************************************************************************)
(* Exploration *)

let number_of_sessions = number_of_sessions

let number_of_tables = Eliommod.number_of_tables

let number_of_table_elements = number_of_table_elements

let number_of_persistent_sessions = number_of_persistent_sessions

let number_of_persistent_tables = number_of_persistent_tables
  (* One table is the main table of sessions *)

let number_of_persistent_table_elements () =
  number_of_persistent_table_elements ()
