(* Ocsigen
 * http://www.ocsigen.org
 * Module eliomparameters.ml
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


open Extensions
open Ocsimisc
open Eliommod

(** Type of names in a formular *)
type 'a param_name = string

type ('a,'b) binsum = Inj1 of 'a | Inj2 of 'b;;

type 'an listnames = 
    {it:'el 'a. ('an -> 'el -> 'a list) -> 'el list -> 'a list -> 'a list}

type coordinates =
    {abscissa: int;
     ordinate: int}

type 'a setoneopt = [ `Set of 'a | `One of 'a | `Opt of 'a ]
type 'a oneopt = [ `One of 'a | `Opt of 'a ]
type 'a setone = [ `Set of 'a | `One of 'a ]

(*****************************************************************************)
(* This is a generalized algebraic datatype *)
(* Use only with constructors from eliom.ml *)
type ('a,+'tipo,+'names) params_type =
    (* 'tipo is [`WithSuffix] or [`WithoutSuffix] *)
  | TProd of (* 'a1 *) ('a,'tipo,'names) params_type * (* 'a2 *) ('a,'tipo,'names) params_type (* 'a = 'a1 * 'a2 ; 'names = 'names1 * 'names2 *)
  | TOption of (* 'a1 *) ('a,'tipo,'names) params_type (* 'a = 'a1 option *)
  | TList of string * (* 'a1 *) ('a,'tipo,'names) params_type (* 'a = 'a1 list *)
  | TSet of ('a,'tipo,'names) params_type (* 'a = 'a1 list *)
  | TSum of (* 'a1 *) ('a,'tipo,'names) params_type * (* 'a2 *) ('a,'tipo,'names) params_type (* 'a = ('a1, 'a2) binsum *)
  | TString of string (* 'a = string *)
  | TInt of string (* 'a = int *)
  | TFloat of string (* 'a = float *)
  | TBool of string (* 'a = bool *)
  | TFile of string (* 'a = file_info *)
  | TUserType of string * (string -> 'a) * ('a -> string) (* 'a = 'a *)
  | TCoord of string (* 'a = 'a1 *)
  | TCoordv of ('a,'tipo,'names) params_type * string
  | TESuffix of string (* 'a = string list *)
  | TESuffixs of string (* 'a = string *)
  | TESuffixu of (string * (string -> 'a) * ('a -> string)) (* 'a = 'a *)
  | TSuffix of ('a,'tipo,'names) params_type (* 'a = 'a1 *)
  | TUnit (* 'a = unit *)
  | TAny (* 'a = (string * string) list *)
;;

type anon_params_type = int

let anonymise_params_type (t : ('a, 'b, 'c) params_type) : anon_params_type = 
  Hashtbl.hash_param 1000 1000 t


(* As GADT are not implemented in OCaml for the while, we define our own
   constructors for params_type *)
let int (n : string) : (int, [`WithoutSuffix], [ `One of int ] param_name) params_type = 
  TInt n

let float (n : string)
    : (float, [`WithoutSuffix], [ `One of float ] param_name) params_type = 
  TFloat n

let bool (n : string)
    : (bool, [`WithoutSuffix], [ `One of bool ] param_name) params_type
    = TBool n

let string (n : string)
    : (string, [`WithoutSuffix], [ `One of string ] param_name) params_type = 
  TString n

let file (n : string)
    : (file_info , [`WithoutSuffix], [ `One of file_info ] param_name) params_type = 
  TFile n

let unit : (unit, [`WithoutSuffix], unit) params_type = TUnit

let user_type
    (of_string : string -> 'a) (to_string : 'a -> string) (n : string)
    : ('a,[`WithoutSuffix], [ `One of 'a ] param_name) params_type =
  Obj.magic (TUserType (n, of_string, to_string))

let sum (t1 : ('a,[`WithoutSuffix], 'an) params_type) 
    (t2 : ('b,[`WithoutSuffix], 'bn) params_type) 
    : (('a,'b) binsum, [`WithoutSuffix], 'an * 'bn ) params_type =
  Obj.magic (TSum (t1, t2))

let prod (t1 : ('a,[`WithoutSuffix], 'an) params_type) 
    (t2 : ('b,[<`WithoutSuffix|`Endsuffix], 'bn) params_type)
    : (('a * 'b),[`WithoutSuffix], 'an * 'bn) params_type =
  Obj.magic (TProd ((Obj.magic t1), (Obj.magic t2)))

let ( ** ) = prod

let coordinates (n : string)
    : (coordinates, [`WithoutSuffix], [ `One of coordinates ] param_name) params_type = 
  TCoord n

let string_coordinates (n : string)
    : (string * coordinates,
       [`WithoutSuffix], 
       [ `One of (string * coordinates) ] param_name) params_type = 
  Obj.magic (TCoordv (string n, n))

let int_coordinates (n : string)
    : (int * coordinates,
       [`WithoutSuffix], 
       [ `One of (int * coordinates) ] param_name) params_type = 
  Obj.magic (TCoordv (int n, n))

let float_coordinates (n : string)
    : (float * coordinates,
       [`WithoutSuffix], 
       [ `One of (float * coordinates) ] param_name) params_type = 
  Obj.magic (TCoordv (float n, n))

let user_type_coordinates
    (of_string : string -> 'a) (to_string : 'a -> string) (n : string)
    : ('a * coordinates,
       [`WithoutSuffix], 
       [ `One of ('a * coordinates) ] param_name) params_type = 
  Obj.magic (TCoordv (user_type of_string to_string n, n))

let opt (t : ('a, [`WithoutSuffix], [ `One of 'an ] param_name) params_type) 
    : ('a option,[`WithoutSuffix], [ `Opt of 'an ] param_name) params_type = 
  Obj.magic (TOption t)

let list (n : string) (t : ('a, [`WithoutSuffix], 'an) params_type) 
    : ('a list,[`WithoutSuffix], 'an listnames) params_type = 
  Obj.magic (TList (n,t))

let set (t : string -> ('a, [`WithoutSuffix], [ `One of 'an ] param_name) params_type) (n : string)
    : ('a list, [`WithoutSuffix], [ `Set of 'an ] param_name) params_type = 
  Obj.magic (TSet (t n))

let any
    : ((string * string) list, [`WithoutSuffix], unit) params_type = 
  TAny

let user_dir_regexp = Netstring_pcre.regexp "(.*)\\$u\\(([^\\)]*)\\)(.*)"
let regexp reg dest n = 
  user_type
    (fun s -> 
      match Netstring_pcre.string_match reg s 0 with
      | Some _ -> 
          begin
            (* hack to get user dirs (same as in staticmod) *)
            let s = Netstring_pcre.global_replace reg dest s in
            match Netstring_pcre.string_match user_dir_regexp dest 0 with
            | None -> s
            | Some result -> 
                let user = Netstring_pcre.matched_group result 2 s in
                try
                  let userdir = (Unix.getpwnam user).Unix.pw_dir in
                  (Netstring_pcre.matched_group result 1 s)^
                  userdir^
                  (Netstring_pcre.matched_group result 3 s)
                with _ -> raise Not_found
          end
      | _ -> raise (Failure "Not matching regexp"))
    (fun s -> s)
    n

let all_suffix (n : string) : 
    (string list , [`Endsuffix], 
     [ `One of string list ] param_name) params_type = 
  (Obj.magic (TESuffix n))

let all_suffix_string (n : string) : 
    (string, [`Endsuffix], [ `One of string ] param_name) params_type = 
  (Obj.magic (TESuffixs n))

let all_suffix_user
    (of_string : string -> 'a) (from_string : 'a -> string) (n : string) :
    ('a, [`Endsuffix], [ `One of 'a ] param_name) params_type = 
  (Obj.magic (TESuffixu (n, of_string, from_string)))

let all_suffix_regexp reg dest (n : string) : 
    (string, [`Endsuffix], [ `One of string ] param_name) params_type = 
  all_suffix_user
    (fun s -> 
      match Netstring_pcre.string_match reg s 0 with
      | Some _ -> Netstring_pcre.global_replace reg dest s
      | _ -> raise (Failure "Not matching regexp"))
    (fun s -> s)
    n

let suffix (s : ('s, [<`WithoutSuffix|`Endsuffix], 'sn) params_type) : 
    ('s , [`WithSuffix], 'sn) params_type = 
  (Obj.magic (TSuffix s))

let suffix_prod (s : ('s, [<`WithoutSuffix|`Endsuffix], 'sn) params_type)
    (t : ('a, [`WithoutSuffix], 'an) params_type) : 
    (('s * 'a), [`WithSuffix], 'sn * 'an) params_type = 
  (Obj.magic (TProd (Obj.magic (TSuffix s), Obj.magic t)))

let contains_suffix = function
  | TProd((TSuffix _),_)
  | TSuffix _ -> true
  | _ -> false












(******************************************************************)
let make_list_suffix i = "["^(string_of_int i)^"]"

(* The following function reconstructs the value of parameters
   from expected type and GET or POST parameters *)
type 'a res_reconstr_param = 
  | Res_ of ('a * 
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
      let rec end_of_list len = function
        | [] -> true
        | (a,_)::_ when 
            (try (String.sub a 0 len) = pref
            with _ -> false) -> false
        | _::l -> end_of_list len l
      in
      if end_of_list (String.length pref) lp
      then Res_ ((Obj.magic []), lp, fl)
      else
        try 
          match aux t lp fl pref (suff^(make_list_suffix i)) with
          | Res_ (v,lp2,f) ->
              (match aa (i+1) lp2 f pref suff with
              | Res_ (v2,lp3,f2) -> Res_ ((Obj.magic (v::v2)),lp3,f2)
              | err -> err)
          | Errors_ (errs, l, f) ->
              (match aa (i+1) l f pref suff with
              | Res_ (_,ll,ff) -> Errors_ (errs, ll, ff)
              | Errors_ (errs2, ll, ff) -> Errors_ ((errs@errs2), ll, ff))
        with Not_found -> Res_ ((Obj.magic []), lp, files)
    in 
    aa 0 params files (pref^name^".") suff
  and aux (typ : ('a,[<`WithSuffix|`WithoutSuffix|`Endsuffix],'b) params_type)
      params files pref suff : 'a res_reconstr_param =
    match typ with
    | TProd (t1, t2) ->
        (match aux t1 params files pref suff with
        | Res_ (v1, l1, f) ->
            (match aux t2 l1 f pref suff with
              Res_ (v2, l2, f2) -> Res_ ((Obj.magic (v1, v2)), l2, f2)
            | err -> err)
        | Errors_ (errs, l, f) ->
            (match aux t2 l f pref suff with
              Res_ (_, ll, ff) -> Errors_ (errs, ll, ff)
            | Errors_ (errs2, ll, ff) -> Errors_ ((errs2@errs), ll, ff)))
    | TOption t -> 
        (try 
          (match aux t params files pref suff with
            Res_ (v, l, f) -> Res_ ((Obj.magic (Some v)), l, f)
          | err -> err)
        with Not_found -> Res_ ((Obj.magic None), params, files))
    | TBool name -> 
        (try 
          let v,l = (list_assoc_remove (pref^name^suff) params) in
          Res_ ((Obj.magic true),l,files)
        with Not_found -> Res_ ((Obj.magic false), params, files))
    | TList (n,t) -> Obj.magic (aux_list t params files n pref suff)
    | TSet t -> 
        let rec aux_set params files =
          try
            match aux t params files pref suff with
            | Res_ (vv, ll, ff) -> 
                (match aux_set ll ff with
                | Res_ (vv2, ll2, ff2) -> 
                    Res_ (Obj.magic (vv::vv2), ll2, ff2)
                | err -> err)
            | Errors_ (errs, ll, ff) ->
                (match aux_set ll ff with
                | Res_ (_, ll2, ff2) -> Errors_ (errs, ll2, ff2)
                | Errors_ (errs2, ll2, ff2) -> Errors_ (errs@errs2, ll2, ff2))
          with Not_found -> Res_ (Obj.magic [], params, files)
        in Obj.magic (aux_set params files)
    | TSum (t1, t2) -> 
        (try 
          match aux t1 params files pref suff with
          | Res_ (v,l,files) -> Res_ ((Obj.magic (Inj1 v)),l,files)
          | err -> err
        with Not_found -> 
          (match aux t2 params files pref suff with
          | Res_ (v,l,files) -> Res_ ((Obj.magic (Inj2 v)),l,files)
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
        Res_ ((Obj.magic v), params, f)
    | TCoord name ->
        let r1 =
          let v, l = (list_assoc_remove (pref^name^suff^".x") params) in
          (try (Res_ ((int_of_string v), l, files))
          with e -> Errors_ ([(pref^name^suff^".x"), e], l, files))
        in
        (match r1 with
        | Res_ (x1, l1, f) ->
            let v, l = (list_assoc_remove (pref^name^suff^".y") l1) in
            (try (Res_ (
                  (Obj.magic
                     {abscissa= x1;
                      ordinate= int_of_string v}), l, f))
            with e -> Errors_ ([(pref^name^suff^".y"), e], l, f))
        | Errors_ (errs, l1, f) ->
            let v, l = (list_assoc_remove (pref^name^suff^".y") l1) in
            (try 
              ignore (int_of_string v); 
              Errors_ (errs, l, f)
            with e -> Errors_ (((pref^name^suff^".y"), e)::errs, l, f)))
    | TCoordv (t, name) ->
        aux (TProd (t, TCoord name)) params files pref suff
    | TUserType (name, of_string, string_of) ->
        let v,l = (list_assoc_remove (pref^name^suff) params) in 
        (try (Res_ ((Obj.magic (of_string v)),l,files))
        with e -> Errors_ ([(pref^name^suff),e], l, files))
    | TUnit -> Res_ ((Obj.magic ()), params, files)
    | TAny -> Res_ ((Obj.magic params), [], files)
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
    | Res_ (v,l,files) -> 
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
    | TProd((TSuffix s), t) -> 
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
let construct_params_list 
    (typ : ('a, [<`WithSuffix|`WithoutSuffix],'b) params_type)
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
    | TSet t ->
        List.fold_left
          (fun l v -> aux t v pref suff l)
          l
          (Obj.magic params)
    | TSum (t1, t2) -> (match Obj.magic params with
      | Inj1 v -> aux t1 v pref suff l
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
    | TCoord name ->
        let coord = Obj.magic params in
        ((pref^name^suff^".x"), string_of_int coord.abscissa)::
        ((pref^name^suff^".y"), string_of_int coord.ordinate)::l
    | TCoordv (t, name) ->
        aux (TProd (t, TCoord name)) params pref suff l
    | TUnit -> l
    | TAny -> l@(Obj.magic params)
    | TESuffix _
    | TESuffixs _
    | TESuffixu _
    | TSuffix _ -> raise (Ocsigen_Internal_Error "Bad use of suffix")
  in
  let rec make_suffix typ params =
    match typ with
    | TProd (t1, t2) ->
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
  | TProd((TSuffix s), t) ->
   ((Some (make_suffix s (fst (Obj.magic params)))),
   (aux t (snd (Obj.magic params)) "" "" []))
  | TSuffix s -> (Some (make_suffix s (Obj.magic params))), []
  | _ -> None, (aux typ params "" "" [])






(* contruct the string of parameters (& separated) for GET and POST *)
let construct_params_string = function
  | [] -> ""
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
  | TProd (t1, t2) -> TProd ((add_pref_params pref t1),
                             (add_pref_params pref t2))
  | TOption t -> TOption (add_pref_params pref t)
  | TBool name -> TBool (pref^name)
  | TList (list_name, t) -> TList (pref^list_name, t)
  | TSet t -> TSet (add_pref_params pref t)
  | TSum (t1, t2) -> TSum ((add_pref_params pref t1),
                           (add_pref_params pref t2))
  | TString name -> TString (pref^name)
  | TInt name -> TInt (pref^name)
  | TFloat name -> TFloat (pref^name)
  | TFile name -> TFile (pref^name)
  | TUserType (name, of_string, string_of) -> 
      TUserType (pref^name, of_string, string_of)
  | TCoord name -> TCoord (pref^name)
  | TCoordv (t, name) -> TCoordv ((add_pref_params pref t), pref^name)
  | TUnit -> TUnit
  | TAny -> TAny
  | TESuffix n -> TESuffix n
  | TESuffixs n -> TESuffixs n
  | TESuffixu a -> TESuffixu a
  | TSuffix s -> TSuffix s






(* Remove all parameters whose name starts with pref *)
let remove_prefixed_param pref l =
  let len = String.length pref in
  let rec aux = function
    | [] -> []
    | ((n,v) as a)::l -> 
        try if (String.sub n 0 len) = pref 
        then aux l
        else a::(aux l)
        with _ -> a::(aux l)
  in aux l

let make_params_names (params : ('t,'tipo,'n) params_type) : 'n =
  let rec aux prefix suffix = function
    | TProd (t1, t2) -> Obj.magic (aux prefix suffix t1, aux prefix suffix t2)
    | TInt name -> Obj.magic (prefix^name^suffix)
    | TFloat name -> Obj.magic (prefix^name^suffix)
    | TString name -> Obj.magic (prefix^name^suffix)
    | TFile name -> Obj.magic (prefix^name^suffix)
    | TUserType (name,o,t) -> Obj.magic (prefix^name^suffix)
    | TCoord name -> Obj.magic (prefix^name^suffix)
    | TCoordv (_, name) -> Obj.magic (prefix^name^suffix)
    | TUnit -> Obj.magic ()
    | TAny -> Obj.magic ()
    | TSet t -> Obj.magic (aux prefix suffix t)
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
    
let string_of_param_name = id
