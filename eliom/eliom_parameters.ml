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


open Ocsigen_extensions
open Ocsigen_lib

(** Type of names in a formular *)
type 'a param_name = string

type ('a,'b) binsum = Inj1 of 'a | Inj2 of 'b;;

type 'an listnames =
    {it:'el 'a. ('an -> 'el -> 'a -> 'a) -> 'el list -> 'a -> 'a}

type coordinates =
    {abscissa: int;
     ordinate: int}

type 'a setoneradio = [ `Set of 'a | `One of 'a | `Radio of 'a ]
type 'a oneradio = [ `One of 'a | `Radio of 'a ]
type 'a setone = [ `Set of 'a | `One of 'a ]

(*****************************************************************************)
(* This is a generalized algebraic datatype *)
(* Use only with constructors from eliom.ml *)
type ('a, +'tipo, +'names) params_type =
    (* 'tipo is [`WithSuffix] or [`WithoutSuffix] *)
  | TProd of (* 'a1 *) ('a,'tipo,'names) params_type * (* 'a2 *) ('a,'tipo,'names) params_type (* 'a = 'a1 * 'a2 ; 'names = 'names1 * 'names2 *)
  | TOption of (* 'a1 *) ('a,'tipo,'names) params_type (* 'a = 'a1 option *)
  | TList of string * (* 'a1 *) ('a,'tipo,'names) params_type (* 'a = 'a1 list *)
  | TSet of ('a,'tipo,'names) params_type (* 'a = 'a1 list *)
  | TSum of (* 'a1 *) ('a,'tipo,'names) params_type * (* 'a2 *) ('a,'tipo,'names) params_type (* 'a = ('a1, 'a2) binsum *)
  | TString of string (* 'a = string *)
  | TInt of string (* 'a = int *)
  | TInt32 of string (* 'a = int32 *)
  | TInt64 of string (* 'a = int64 *)
  | TFloat of string (* 'a = float *)
  | TBool of string (* 'a = bool *)
  | TFile of string (* 'a = file_info *)
  | TUserType of string * (string -> 'a) * ('a -> string) (* 'a = 'a *)
  | TCoord of string (* 'a = 'a1 *)
  | TCoordv of ('a,'tipo,'names) params_type * string
  | TESuffix of string (* 'a = string list *)
  | TESuffixs of string (* 'a = string *)
  | TESuffixu of (string * (string -> 'a) * ('a -> string)) (* 'a = 'a *)
  | TSuffix of (bool * ('a,'tipo, 'names) params_type) (* 'a = 'a1 *)
      (* bool = redirect the version without suffix to the suffix version *)
  | TUnit (* 'a = unit *) 
  | TAny (* 'a = (string * string) list *)
  | TConst of string (* 'a = unit; 'names = unit *)
  | TNLParams of ('a, 'tipo, 'names) non_localized_params

and ('a, 'tipo, +'names) non_localized_params = 
    string *
      bool (* persistent *) *
      ('a option Polytables.key * 'a option Polytables.key) *
      ('a, 'tipo, 'names) params_type



type anon_params_type = int

let anonymise_params_type (t : ('a, 'b, 'c) params_type) : anon_params_type =
  Hashtbl.hash_param 1000 1000 t


(* As GADT are not implemented in OCaml for the while, we define our own
   constructors for params_type *)
let int (n : string) : (int, [`WithoutSuffix], [ `One of int ] param_name) params_type =
  TInt n

let int32 (n : string) : (int32, [`WithoutSuffix], [ `One of int32 ] param_name) params_type =
  TInt32 n

let int64 (n : string) : (int64, [`WithoutSuffix], [ `One of int64 ] param_name) params_type =
  TInt64 n

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
    ~(of_string : string -> 'a) ~(to_string : 'a -> string) (n : string)
    : ('a,[`WithoutSuffix], [ `One of 'a ] param_name) params_type =
  Obj.magic (TUserType (n, of_string, to_string))

let sum (t1 : ('a, [`WithoutSuffix], 'an) params_type)
    (t2 : ('b,[`WithoutSuffix], 'bn) params_type)
    : (('a,'b) binsum, [`WithoutSuffix], 'an * 'bn ) params_type =
  Obj.magic (TSum (Obj.magic t1, Obj.magic t2))

let prod (t1 : ('a, [`WithoutSuffix], 'an) params_type)
    (t2 : ('b, [<`WithoutSuffix|`Endsuffix] as 'e, 'bn) params_type)
    : (('a * 'b),'e, 'an * 'bn) params_type =
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

let int32_coordinates (n : string)
    : (int32 * coordinates,
       [`WithoutSuffix],
       [ `One of (int32 * coordinates) ] param_name) params_type =
  Obj.magic (TCoordv (int32 n, n))

let int64_coordinates (n : string)
    : (int64 * coordinates,
       [`WithoutSuffix],
       [ `One of (int64 * coordinates) ] param_name) params_type =
  Obj.magic (TCoordv (int64 n, n))

let float_coordinates (n : string)
    : (float * coordinates,
       [`WithoutSuffix],
       [ `One of (float * coordinates) ] param_name) params_type =
  Obj.magic (TCoordv (float n, n))

let user_type_coordinates
    ~(of_string : string -> 'a) ~(to_string : 'a -> string) (n : string)
    : ('a * coordinates,
       [`WithoutSuffix],
       [ `One of ('a * coordinates) ] param_name) params_type =
  Obj.magic (TCoordv (user_type of_string to_string n, n))

let opt (t : ('a, [`WithoutSuffix], 'an) params_type)
    : ('a option,[`WithoutSuffix], 'an) params_type =
  Obj.magic (TOption t)

let radio (t : string -> 
            ('a, [`WithoutSuffix], [ `One of 'an ] param_name) params_type)
    (n : string)
    : ('a option, [`WithoutSuffix], [ `Radio of 'an ] param_name) params_type =
  Obj.magic (TOption (t n))

let list (n : string) (t : ('a, [`WithoutSuffix], 'an) params_type)
    : ('a list, [`WithoutSuffix], 'an listnames) params_type =
  Obj.magic (TList (n,t))

let set (t : string -> ('a, [`WithoutSuffix], [ `One of 'an ] param_name) params_type) (n : string)
    : ('a list, [`WithoutSuffix], [ `Set of 'an ] param_name) params_type =
  Obj.magic (TSet (t n))

let any
    : ((string * string) list, [`WithoutSuffix], unit) params_type =
  TAny

let suffix_const (v : string)
    : (unit, [`WithoutSuffix], [ `One of unit ] param_name) params_type =
  TConst v



let regexp reg dest ~to_string n =
  user_type
    (fun s ->
      match Netstring_pcre.string_match reg s 0 with
      | Some _ ->
          begin
            try
              Ocsigen_extensions.replace_user_dir reg 
                (Ocsigen_extensions.parse_user_dir dest) s
            with Ocsigen_extensions.NoSuchUser ->
              raise (Failure "User does not exist")
          end
      | _ -> raise (Failure "Regexp not matching"))
    to_string
    n

let all_suffix (n : string) :
    (string list , [`Endsuffix],
     [ `One of string list ] param_name) params_type =
  (Obj.magic (TESuffix n))

let all_suffix_string (n : string) :
    (string, [`Endsuffix], [ `One of string ] param_name) params_type =
  (Obj.magic (TESuffixs n))

let all_suffix_user
    ~(of_string : string -> 'a) ~(to_string : 'a -> string) (n : string) :
    ('a, [`Endsuffix], [ `One of 'a ] param_name) params_type =
  (Obj.magic (TESuffixu (n, of_string, to_string)))

let all_suffix_regexp reg dest ~(to_string : 'a -> string) (n : string) :
    (string, [`Endsuffix], [ `One of string ] param_name) params_type =
  all_suffix_user
    (fun s ->
      match Netstring_pcre.string_match reg s 0 with
      | Some _ ->
          begin
            try
              Ocsigen_extensions.replace_user_dir reg 
                (Ocsigen_extensions.parse_user_dir dest) s
            with Ocsigen_extensions.NoSuchUser ->
              raise (Failure "User does not exist")
          end
      | _ -> raise (Failure "Regexp not matching"))
    to_string
    n

let suffix ?(redirect_if_not_suffix = true)
    (s : ('s, [<`WithoutSuffix|`Endsuffix], 'sn) params_type) :
    ('s , [`WithSuffix], 'sn) params_type =
  (Obj.magic (TSuffix (redirect_if_not_suffix, s)))

let suffix_prod ?(redirect_if_not_suffix = true)
    (s : ('s, [<`WithoutSuffix|`Endsuffix], 'sn) params_type)
    (t : ('a, [`WithoutSuffix], 'an) params_type) :
    (('s * 'a), [`WithSuffix], 'sn * 'an) params_type =
  (Obj.magic (TProd (Obj.magic (TSuffix (redirect_if_not_suffix, s)), 
                                Obj.magic t)))

let rec contains_suffix = function
  | TProd (a, b) -> 
      (match contains_suffix a with
         | None -> contains_suffix b
         | c -> c)
  | TSuffix (b, _) -> Some b
  | _ -> None






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
    ~sp
    (typ : ('a, [<`WithSuffix|`WithoutSuffix], 'b) params_type)
    params files nosuffixversion urlsuffix : 'a =
  let rec parse_suffix typ suff =
    match (typ, suff) with
      | TAny, l | TESuffix _, l -> Obj.magic l, []
          (*VVV encode=false? *)
      | TESuffixs _, l -> Obj.magic (string_of_url_path ~encode:false l), []
      | TESuffixu (_, of_string, to_string), l ->
          (try
             (*VVV encode=false? *)
             Obj.magic (of_string (string_of_url_path ~encode:false l)), []
           with e -> 
             raise (Eliom_common.Eliom_Typing_Error [("<suffix>", e)]))
      | TOption t, [] -> Obj.magic None, []
      | TOption t, ""::l -> Obj.magic None, l
      | TOption t, l -> 
          let r, ll = parse_suffix t l in
          Obj.magic (Some r), ll
      | TList _, [] | TSet _, [] -> Obj.magic [], []
      | TList (_, t), l | TSet t, l ->
          let b, l = Obj.magic (parse_suffix t l) in
          (match l with
             | [] -> raise (Ocsigen_Is_a_directory sp.Eliom_common.sp_request)
             | [""] -> Obj.magic [b], []
             | _ -> 
                 let c, l = Obj.magic (parse_suffix typ l) in
                 Obj.magic (b::c), l)
      | TProd (TList _, _), _
      | TProd (TSet _, _), _ ->
          failwith "Lists or sets in suffixes must be last parameters"
      | TProd (t1, t2), l ->
          (match parse_suffix t1 l with
             | _, [] -> 
                 raise (Ocsigen_Is_a_directory sp.Eliom_common.sp_request)
             | r, l -> 
                 let rr, ll = parse_suffix t2 l in 
                 Obj.magic (r, rr), ll)
      | TString _, v::l -> Obj.magic v, l
      | TInt name, v::l ->
          (try Obj.magic (int_of_string v), l
           with e -> 
             raise (Eliom_common.Eliom_Typing_Error [("<suffix>", e)]))
      | TInt32 name, v::l ->
          (try Obj.magic (Int32.of_string v), l
           with e -> 
             raise (Eliom_common.Eliom_Typing_Error [("<suffix>", e)]))
      | TInt64 name, v::l ->
          (try Obj.magic (Int64.of_string v), l
           with e -> 
             raise (Eliom_common.Eliom_Typing_Error [("<suffix>", e)]))
      | TFloat name, v::l ->
          (try Obj.magic (float_of_string v), l
           with e -> 
             raise (Eliom_common.Eliom_Typing_Error [("<suffix>", e)]))
      | TUnit, v::l ->
          (if v="" then Obj.magic (), l
           else raise (Eliom_common.Eliom_Wrong_parameter))
      | TBool name, v::l ->
          (try Obj.magic (bool_of_string v), l
           with e -> 
             raise (Eliom_common.Eliom_Typing_Error [("<suffix>", e)]))
      | TUserType (name, of_string, string_of), v::l ->
          (try Obj.magic (of_string v), l
           with e -> 
             raise (Eliom_common.Eliom_Typing_Error [("<suffix>", e)]))
      | TConst value, v::l ->
          if v = value
          then Obj.magic (), l
          else raise Eliom_common.Eliom_Wrong_parameter
      | TSum (t1, t2), l ->
          (try parse_suffix t1 l
           with Eliom_common.Eliom_Wrong_parameter -> parse_suffix t2 l)
      | TCoord _, l ->
          (match Obj.magic (parse_suffix (TInt "") l) with
             | _, [] -> raise Eliom_common.Eliom_Wrong_parameter
             | r, l -> 
                 let rr, ll = Obj.magic (parse_suffix (TInt "") l) in 
                 Obj.magic {abscissa = r; ordinate=rr}, ll)
      | TCoordv (t, _), l ->
          let a, l = parse_suffix t l in
          (match Obj.magic (parse_suffix (TInt "") l) with
             | _, [] -> raise Eliom_common.Eliom_Wrong_parameter
             | r, l -> 
                 let rr, ll = Obj.magic (parse_suffix (TInt "") l) in 
                 Obj.magic (a, {abscissa = r; ordinate=rr}), ll)
      | TNLParams _, _ -> 
          failwith "It is not possible to have non localized parameters in suffix"
      | _ -> raise Eliom_common.Eliom_Wrong_parameter
  in
  let aux2 typ params =
    let rec aux_list t params files name pref suff =
      let rec aa i lp fl pref suff =
        let rec end_of_list len = function
          | [] -> true
          | (a,_)::_ when
              (try (String.sub a 0 len) = pref
               with Invalid_argument _ -> false) -> false
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
    and aux (typ : ('a, [<`WithSuffix|`WithoutSuffix|`Endsuffix], 'b) params_type)
        params files pref suff : 'a res_reconstr_param =
      match typ with
        | TNLParams (_, _, _, t) -> aux t params files pref suff
        | TProd (t1, t2) ->
            (match aux t1 params files pref suff with
               | Res_ (v1, l1, f) ->
                   (match aux t2 l1 f pref suff with
                      | Res_ (v2, l2, f2) -> Res_ ((Obj.magic (v1, v2)), l2, f2)
                      | err -> err)
               | Errors_ (errs, l, f) ->
                   (match aux t2 l f pref suff with
                      | Res_ (_, ll, ff) -> Errors_ (errs, ll, ff)
                      | Errors_ (errs2, ll, ff) -> Errors_ ((errs2@errs), ll, ff)))
        | TOption t ->
            (try
               (match aux t params files pref suff with
                  | Res_ (v, l, f) -> Res_ ((Obj.magic (Some v)), l, f)
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
            (try (Res_ ((Obj.magic (int_of_string v)), l, files))
             with e -> Errors_ ([(pref^name^suff),e], l, files))
        | TInt32 name ->
            let v,l = (list_assoc_remove (pref^name^suff) params) in
            (try (Res_ ((Obj.magic (Int32.of_string v)),l,files))
             with e -> Errors_ ([(pref^name^suff),e], l, files))
        | TInt64 name ->
            let v,l = (list_assoc_remove (pref^name^suff) params) in
            (try (Res_ ((Obj.magic (Int64.of_string v)),l,files))
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
        | TConst _ ->
            Res_ ((Obj.magic ()), params, files)
        | TESuffix n ->
            let v,l = list_assoc_remove n params in
            (* cannot have prefix or suffix *)
            Res_ ((Obj.magic (Neturl.split_path v)), l, files)
        | TESuffixs n ->
            let v,l = list_assoc_remove n params in
            (* cannot have prefix or suffix *)
            Res_ ((Obj.magic v), l, files)
        | TESuffixu (n, of_string, to_string) ->
            let v,l = list_assoc_remove n params in
            (* cannot have prefix or suffix *)
            (try Res_ ((Obj.magic (of_string v)), l, files)
             with e -> Errors_ ([(pref^n^suff), e], l, files))
        | TSuffix (_, s) ->
            match urlsuffix with
              | None ->
                  if nosuffixversion
                    (* the special page name "nosuffix" is present *)
                  then aux s params files pref suff
                  else raise Eliom_common.Eliom_Wrong_parameter
              | Some urlsuffix ->
                  match parse_suffix s urlsuffix with
                    | p, [] -> Res_ (p, params, files)
                    | _ -> raise Eliom_common.Eliom_Wrong_parameter
    in
    match Obj.magic (aux typ params files "" "") with
      | Res_ (v, l, files) ->
          if (l, files) = ([], [])
          then v
          else raise Eliom_common.Eliom_Wrong_parameter
      | Errors_ (errs, l, files) ->
          if (l, files) = ([], [])
          then raise (Eliom_common.Eliom_Typing_Error errs)
          else raise Eliom_common.Eliom_Wrong_parameter
  in
  try Obj.magic (aux2 typ params) with
    | Not_found -> raise Eliom_common.Eliom_Wrong_parameter

(* The following function takes a 'a params_type and a 'a and
   constructs the list of parameters (GET or POST)
   (This is a marshalling function towards HTTP parameters format) *)
let construct_params_list
    nlp
    (typ : ('a, [<`WithSuffix|`WithoutSuffix],'b) params_type)
    (params : 'a) =
  let rec make_suffix typ params =
    match typ with
    | TNLParams (_, _, _, t) -> make_suffix t params
    | TProd (TList _, _)
    | TProd (TSet _, _) ->
        failwith "Lists or sets in suffixes must be last parameters"
    | TProd (t1, t2) ->
        (make_suffix t1 (fst (Obj.magic params)))@
        (make_suffix t2 (snd (Obj.magic params)))
    | TString _ -> [Obj.magic params]
    | TInt _ -> [string_of_int (Obj.magic params)]
    | TInt32 _ -> [Int32.to_string (Obj.magic params)]
    | TInt64 _ -> [Int64.to_string (Obj.magic params)]
    | TFloat _ -> [string_of_float (Obj.magic params)]
    | TBool _ -> [string_of_bool (Obj.magic params)]
    | TUnit -> [""]
    | TConst v -> [Obj.magic v]
    | TOption t -> (match Obj.magic params with
                      | None -> [""]
                      | Some v -> make_suffix t v)
    | TList (_, t) | TSet t -> 
        (match params with
          | [] -> [""]
          | a::l ->
              (make_suffix t (Obj.magic a))@
                (make_suffix typ (Obj.magic l)))
    | TUserType (_, of_string, string_of) ->[string_of (Obj.magic params)]
    | TSum (t1, t2) ->
        (match Obj.magic params with
           | Inj1 p -> make_suffix t1 p
           | Inj2 p -> make_suffix t2 p)
    | TCoord _ ->
        (make_suffix (TInt "") (Obj.magic (Obj.magic params).abscissa))@
        (make_suffix (TInt "") (Obj.magic (Obj.magic params).ordinate))       
    | TCoordv (t, _) ->
        (make_suffix t (fst (Obj.magic params)))@
        ((make_suffix (TInt "") (Obj.magic (snd (Obj.magic params)).abscissa))@
           (make_suffix (TInt "") (Obj.magic (snd (Obj.magic params)).ordinate)))
    | TESuffixs _ -> [Obj.magic params]
    | TAny | TESuffix _ -> (match Obj.magic params with [] -> [""] | p -> p)
    | TESuffixu (_, of_string, string_of) -> [string_of (Obj.magic params)]
    | _ -> raise (Ocsigen_Internal_Error "Bad parameter type in suffix")
  in
  let rec aux typ psuff nlp params pref suff l =
    match typ with
    | TNLParams (name, _, _, t) -> 
        let psuff, nlp, nl = aux t psuff nlp params pref suff [] in
        (psuff, Ocsigen_lib.String_Table.add name nl nlp, l)
    | TProd (t1, t2) ->
        let psuff, nlp, l1 = 
          aux t1 psuff nlp (fst (Obj.magic params)) pref suff l 
        in
        aux t2 psuff nlp (snd (Obj.magic params)) pref suff l1
    | TOption t ->
        (match ((Obj.magic params) : 'zozo option) with
        | None -> psuff, nlp, l
        | Some v -> aux t psuff nlp v pref suff l)
    | TBool name ->
        (if ((Obj.magic params) : bool)
         then psuff, nlp, ((pref^name^suff), "on")::l
         else psuff, nlp, l)
    | TList (list_name, t) ->
        let pref2 = pref^list_name^suff^"." in
        fst
          (List.fold_left
             (fun ((psuff, nlp, s), i) p ->
                ((aux t psuff nlp p pref2 (suff^(make_list_suffix i)) s), (i+1)))
             ((psuff, nlp, l), 0) (Obj.magic params))
    | TSet t ->
        List.fold_left
          (fun (psuff, nlp, l) v -> aux t psuff nlp v pref suff l)
          (psuff, nlp, l)
          (Obj.magic params)
    | TSum (t1, t2) -> (match Obj.magic params with
      | Inj1 v -> aux t1 psuff nlp v pref suff l
      | Inj2 v -> aux t2 psuff nlp v pref suff l)
    | TString name -> psuff, nlp, ((pref^name^suff), (Obj.magic params))::l
    | TInt name -> 
        psuff, nlp, 
        ((pref^name^suff), (string_of_int (Obj.magic params)))::l
    | TInt32 name -> 
        psuff, nlp,
        ((pref^name^suff), (Int32.to_string (Obj.magic params)))::l
    | TInt64 name -> 
        psuff, nlp, 
        ((pref^name^suff), (Int64.to_string (Obj.magic params)))::l
    | TFloat name ->
        psuff, nlp,
        ((pref^name^suff), (string_of_float (Obj.magic params)))::l
    | TFile name ->
        raise (Failure
                 "Constructing an URL with file parameters not implemented")
    | TUserType (name, of_string, string_of) ->
        psuff, nlp,
        ((pref^name^suff), (string_of (Obj.magic params)))::l
    | TCoord name ->
        psuff, nlp,
        let coord = Obj.magic params in
        ((pref^name^suff^".x"), string_of_int coord.abscissa)::
        ((pref^name^suff^".y"), string_of_int coord.ordinate)::l
    | TCoordv (t, name) ->
        aux (TProd (t, TCoord name)) psuff nlp params pref suff l
    | TUnit -> psuff, nlp, l
    | TAny -> psuff, nlp, l@(Obj.magic params)
    | TConst _ -> psuff, nlp, l
    | TESuffix _
    | TESuffixs _
    | TESuffixu _ -> raise (Ocsigen_Internal_Error "Bad use of suffix")
    | TSuffix (_, s) -> Some (make_suffix s (Obj.magic params)), nlp, l
  in
  aux typ None nlp params "" "" []




(* contruct the string of parameters (& separated) for GET and POST *)
let construct_params_string =
  Netencoding.Url.mk_url_encoded_parameters

let construct_params nonlocparams typ p =
  let (suff, nonlocparams, pl) = construct_params_list nonlocparams typ p in
  let nlp = Ocsigen_lib.String_Table.fold
    (fun _ l s -> Ocsigen_lib.concat_strings s "&" (construct_params_string l))
    nonlocparams "" 
  in
  (suff, Ocsigen_lib.concat_strings (construct_params_string pl) "&" nlp)


(* Add a prefix to parameters *)
let rec add_pref_params pref = function
  | TNLParams (a, b, c, t) -> TNLParams (a, b, c, add_pref_params pref t)
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
  | TInt32 name -> TInt32 (pref^name)
  | TInt64 name -> TInt64 (pref^name)
  | TFloat name -> TFloat (pref^name)
  | TFile name -> TFile (pref^name)
  | TUserType (name, of_string, string_of) ->
      TUserType (pref^name, of_string, string_of)
  | TCoord name -> TCoord (pref^name)
  | TCoordv (t, name) -> TCoordv ((add_pref_params pref t), pref^name)
  | TUnit -> TUnit
  | TAny -> TAny
  | TConst v -> TConst v
  | TESuffix n -> TESuffix n
  | TESuffixs n -> TESuffixs n
  | TESuffixu a -> TESuffixu a
  | TSuffix s -> TSuffix s







let make_params_names (params : ('t,'tipo,'n) params_type) : bool * 'n =
  let rec aux issuffix prefix suffix = function
    | TNLParams (_, _, _, t) -> aux issuffix prefix suffix t
    | TProd (t1, t2) -> 
        let issuffix, a = aux issuffix prefix suffix t1 in
        let issuffix, b = aux issuffix prefix suffix t2 in 
        issuffix, Obj.magic (a, b)
    | TInt name
    | TInt32 name
    | TInt64 name
    | TFloat name
    | TString name
    | TBool name
    | TFile name
    | TUserType (name, _, _)
    | TCoord name
    | TCoordv (_, name) -> issuffix, Obj.magic (prefix^name^suffix)
    | TUnit
    | TAny
    | TConst _ -> issuffix, Obj.magic ()
    | TSet t -> Obj.magic (aux issuffix prefix suffix t)
    | TESuffix n
    | TESuffixs n
    | TESuffixu (n, _, _) -> issuffix, Obj.magic n
    | TSuffix (_, t) -> Obj.magic (aux true prefix suffix t)
    | TOption t -> Obj.magic (aux issuffix prefix suffix t)
    | TSum (t1, t2) -> 
        let _, a = aux issuffix prefix suffix t1 in
        let _, b = aux issuffix prefix suffix t2 in 
        issuffix, Obj.magic (a, b)
        (* TSuffix cannot be inside TSum *)
    | TList (name, t1) -> 
        (issuffix,
         Obj.magic
           {it =
               (fun f l init ->
                  let length = List.length l in
                  snd
                    (List.fold_right
                       (fun el (i, l2) ->
                          let i'= i-1 in
                          (i', (f (snd (aux issuffix (prefix^name^".") 
                                          (make_list_suffix i') t1)) el
                                  l2)))
                       l
                       (length, init)))})
          (* TSuffix cannot be inside TList *)
  in aux false "" "" params

let string_of_param_name = id

(* Non localized parameters *)

let make_non_localized_parameters
    ~prefix
    ~name
    ?(persistent = false)
    (p : ('a, [ `WithoutSuffix ], 'b) params_type) 
    : ('a, [ `WithoutSuffix ], 'b) non_localized_params =
  let pr = if persistent then "p_" else "n_" in
  let name = pr^prefix^"-"^name in
  if String.contains name '.'
  then failwith "Non localized parameters names cannot contain dots."
  else
    (name,
     persistent,
     (Polytables.make_key () (* GET *), 
      Polytables.make_key () (* POST *)),
     add_pref_params (Eliom_common.nl_param_prefix^name^".") p)

let get_non_localized_parameters params getorpost ~sp
    (name, _, keys, paramtype) =
  (* non localized parameters are parsed only once, 
     and cached in request_cache *)
  let key = getorpost keys in
  (try 
     (* first, look in cache: *)
     Polytables.get 
       ~table:sp.Eliom_common.sp_request.request_info.ri_request_cache
       ~key
   with Not_found ->
     let p =
       try
         Some 
           (let params = Ocsigen_lib.String_Table.find name params in
            reconstruct_params ~sp paramtype params [] false None)
       with Eliom_common.Eliom_Wrong_parameter | Not_found -> None
     in
     (* add in cache: *)
     Polytables.set 
       ~table:sp.Eliom_common.sp_request.request_info.ri_request_cache
       ~key
       ~value:p;
     p)

let get_non_localized_get_parameters ~sp p =
  let sp = Eliom_sessions.esp_of_sp sp in
  get_non_localized_parameters 
    sp.Eliom_common.sp_si.Eliom_common.si_nl_get_params fst ~sp p

let get_non_localized_post_parameters ~sp p =
  let sp = Eliom_sessions.esp_of_sp sp in
  get_non_localized_parameters
    sp.Eliom_common.sp_si.Eliom_common.si_nl_post_params snd ~sp p

let nl_prod 
    (t : ('a, 'su, 'an) params_type) 
    (s : ('s, [ `WithoutSuffix ], 'sn) non_localized_params) :
    (('a * 's), 'su, 'an * 'sn) params_type =
  (Obj.magic (TProd (Obj.magic t, Obj.magic (TNLParams s))))


(* removes from nlp set the nlp parameters that are present in param
   specification *)
let rec remove_from_nlp nlp = function
    | TNLParams (n, _, _, _) -> Ocsigen_lib.String_Table.remove n nlp
    | TProd (t1, t2) -> 
        let nlp = remove_from_nlp nlp t1 in
        remove_from_nlp nlp t2
    | _ -> nlp

type nl_params_set = (string * string) list Ocsigen_lib.String_Table.t

let empty_nl_params_set = Ocsigen_lib.String_Table.empty

let add_nl_parameter s t v = 
  (fun (_, a, _) -> a) (construct_params_list s (TNLParams t) v)

let table_of_nl_params_set = Ocsigen_lib.id

let get_nl_params_names t = snd (make_params_names (TNLParams t))
