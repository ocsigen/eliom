(* Ocsigen
 * http://www.ocsigen.org
 * Module eliom_parameters.ml
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


open Ocsigen_lib

include Eliom_parameters_cli


type anon_params_type = int

let anonymise_params_type (t : ('a, 'b, 'c) params_type) : anon_params_type =
  Hashtbl.hash_param 1000 1000 t


(*****************************************************************************)
(* types available only on server side (no pcre on browser) *)

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







(******************************************************************)
(* The following function reconstructs the value of parameters
   from expected type and GET or POST parameters *)
type 'a res_reconstr_param =
  | Res_ of ('a *
               (string * string) list *
               (string * Ocsigen_lib.file_info) list)
  | Errors_ of ((string * exn) list *
                  (string * string) list *
                  (string * Ocsigen_lib.file_info) list)

let reconstruct_params_
    req
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
             | [] -> raise (Ocsigen_extensions.Ocsigen_Is_a_directory req)
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
                 raise (Ocsigen_extensions.Ocsigen_Is_a_directory req)
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
(*VVV unsafe unmarshal! *)
      | TMarshal _, v::l -> Marshal.from_string v 0, l
      | _ -> raise Eliom_common.Eliom_Wrong_parameter
  in
  let aux2 typ params =
    let rec aux_list t params files name pref suff =
      let rec aa i lp fl pref =
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
            match aux t lp fl pref (make_list_suffix i) with
              | Res_ (v,lp2,f) ->
                  (match aa (i+1) lp2 f pref with
                     | Res_ (v2,lp3,f2) -> Res_ ((Obj.magic (v::v2)),lp3,f2)
                     | err -> err)
              | Errors_ (errs, l, f) ->
                  (match aa (i+1) l f pref with
                     | Res_ (_,ll,ff) -> Errors_ (errs, ll, ff)
                     | Errors_ (errs2, ll, ff) -> Errors_ ((errs@errs2), ll, ff))
          with Not_found -> Res_ ((Obj.magic []), lp, files)
      in
      aa 0 params files (pref^name^suff^".")
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
            (match urlsuffix with
               | None ->
                   if nosuffixversion
                     (* the special page name "nosuffix" is present *)
                   then aux s params files pref suff
                   else raise Eliom_common.Eliom_Wrong_parameter
               | Some urlsuffix ->
                   (match parse_suffix s urlsuffix with
                      | p, [] -> Res_ (p, params, files)
                      | _ -> raise Eliom_common.Eliom_Wrong_parameter))
        | TMarshal name ->
            let v,l = list_assoc_remove (pref^name^suff) params in
(*VVV unsafe unmarshal! *)
            Res_ ((Marshal.from_string v 0),l,files)
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


let reconstruct_params ~sp = reconstruct_params_ sp.Eliom_common.sp_request


(*****************************************************************************)
(* Non localized parameters *)


let get_non_localized_parameters params getorpost ~sp
    (name, _, keys, paramtype) =
  (* non localized parameters are parsed only once, 
     and cached in request_cache *)
  let key = getorpost keys in
  (try 
     (* first, look in cache: *)
     Polytables.get 
       ~table:sp.Eliom_common.sp_request.Ocsigen_extensions.request_info.Ocsigen_extensions.ri_request_cache
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
       ~table:sp.Eliom_common.sp_request.Ocsigen_extensions.request_info.Ocsigen_extensions.ri_request_cache
       ~key
       ~value:p;
     p)

let get_non_localized_get_parameters p =
  let sp = Eliom_common.get_sp () in
  get_non_localized_parameters 
    sp.Eliom_common.sp_si.Eliom_common.si_nl_get_params fst ~sp p

let get_non_localized_post_parameters p =
  let sp = Eliom_common.get_sp () in
  get_non_localized_parameters
    sp.Eliom_common.sp_si.Eliom_common.si_nl_post_params snd ~sp p

