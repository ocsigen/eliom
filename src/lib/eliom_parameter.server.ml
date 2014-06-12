(* Ocsigen
 * http://www.ocsigen.org
 * Module eliom_parameter.ml
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

open Eliom_lib

include Eliom_parameter_base

open Ocsigen_extensions

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

type params = (string * string) list
type files = (string * file_info) list
type +'a res_reconstr_param =
  | Res_ of 'a * params * files
  | Errors_ of ((string * string * exn) list * params * files)

let reconstruct_params_
    req
    typ
    params files nosuffixversion urlsuffix : 'a =
  let rec parse_suffix : type a c. (a,'b,c) params_type -> string list -> (a * string list) = fun typ suff ->
    match (typ, suff) with
      | TESuffix _, l -> l, []
          (*VVV encode=false? *)
      | TESuffixs _, l -> Url.string_of_url_path ~encode:false l, []
      | TESuffixu (_, ofto), l ->
          (try
             (*VVV encode=false? *)
             ofto.of_string (Url.string_of_url_path ~encode:false l), []
           with e ->
             raise (Eliom_common.Eliom_Typing_Error [("<suffix>", e)]))
      | TOption (t,_), [] -> None, []
      | TOption (t,_), ""::l -> None, l
      | TOption (t,_), l ->
        let r, ll = parse_suffix t l in
        Some r, ll
      | TList _, [] -> [],[]
      | TList (_, t), l ->
        let b, l = parse_suffix t l in
        (match l with
          | [] -> raise Eliom_common.Eliom_Wrong_parameter
          | [""] -> [b], []
          | _ ->
            let c, l = parse_suffix typ l in
            (b::c), l)
      | TSet _, [] -> [], []
      | TSet t, l ->
          let b, l = parse_suffix t l in
          (match l with
            | [] -> raise Eliom_common.Eliom_Wrong_parameter
            | [""] -> [b], []
            | _ ->
              let c, l = parse_suffix typ l in
              (b::c), l)

      | TProd (TList _, _), _ ->
        failwith "Lists or sets in suffixes must be last parameters"
      | TProd (TSet _, _), _ ->
        failwith "Lists or sets in suffixes must be last parameters"
      | TProd (t1, t2), l ->
        (match parse_suffix t1 l with
          | _, [] -> raise Eliom_common.Eliom_Wrong_parameter
          | r, l ->
            let rr, ll = parse_suffix t2 l in
            (r, rr), ll)
      | TAtom (name, t), v::l ->
        (try atom_of_string t v, l
         with e -> raise (Eliom_common.Eliom_Typing_Error [("<suffix>", e)]))
      | TUserType (name, ofto), v::l ->
        (try ofto.of_string v, l
         with e -> raise (Eliom_common.Eliom_Typing_Error [("<suffix>", e)]))
      | TTypeFilter (t, None), _ -> failwith "Type filter without filter"
      | TTypeFilter (t, Some check), l ->
        let (v, _) as a = parse_suffix t l in
        check v;
        a
      | TConst value, v::l ->
          if v = value
          then (), l
          else raise Eliom_common.Eliom_Wrong_parameter
      | TSum (t1, t2), l ->
        (try let x, l = parse_suffix t1 l in Inj1 x,l
         with Eliom_common.Eliom_Wrong_parameter ->
           let x,l = parse_suffix t2 l in Inj2 x,l)
      | TCoord _, l ->
        (match parse_suffix (TAtom ("",TInt)) l with
         | _, [] -> raise Eliom_common.Eliom_Wrong_parameter
         | r, l ->
           let rr, ll = parse_suffix (TAtom ("",TInt)) l in
           {abscissa = r; ordinate=rr}, ll)
      | TCoordv (_, t), l ->
          let a, l = parse_suffix t l in
          (match parse_suffix (TAtom ("",TInt)) l with
           | _, [] -> raise Eliom_common.Eliom_Wrong_parameter
           | r, l ->
             let rr, ll = parse_suffix (TAtom ("",TInt)) l in
             (a, {abscissa = r; ordinate=rr}), ll)
      | TNLParams _, _ ->
          failwith "It is not possible to have non localized parameters in suffix"
      | TJson (_, Some typ), v::l -> Deriving_Json.from_string typ v, l
      | TJson (_, None), v::l -> assert false (* client side only *)
      | TAny, _ -> failwith "It is not possible to use any in suffix. May be try with all_suffix ?"
      | TFile _, _ -> assert false
      | TRaw_post_data, _ -> assert false
      | TUnit, _ -> failwith "It is not possible to use TUnit in suffix."
      | TSuffix _, _ -> failwith "It is not possible to use TSuffix in suffix."
      | _,[]-> raise Eliom_common.Eliom_Wrong_parameter
  in
  let rec aux_list : type a c. (a,'b,c) params_type -> params -> files -> string -> string -> string -> a list res_reconstr_param =
    fun t params files name pref suff ->
      let rec loop_list i lp fl pref =
        let rec end_of_list len = function
          | [] -> true
          | (a, _)::_ when
              (try (String.sub a 0 len) = pref
               with Invalid_argument _ -> false) -> false
          | _::l -> end_of_list len l
        in
        if end_of_list (String.length pref) lp
        then Res_ ([], lp, fl)
        else
          match aux t lp fl pref (make_list_suffix i) with
            | Res_ (v, lp2, f) ->
              (match loop_list (i+1) lp2 f pref with
                | Res_ (v2,lp3,f2) -> Res_ ((v::v2),lp3,f2)
                | err -> err)
            | Errors_ errs -> Errors_ errs
      in
      loop_list 0 params files (pref^name^suff^".")
  and aux : type a c. (a, 'b, c) params_type -> params -> files -> string -> string -> a res_reconstr_param =
    fun typ params files pref suff ->
      match typ with
        | TNLParams {param=t} -> aux t params files pref suff
        | TProd (t1, t2) ->
          (match aux t1 params files pref suff with
            | Res_ (v1, l1, f) ->
              (match aux t2 l1 f pref suff with
                | Res_ (v2, l2, f2) -> Res_ ((v1, v2), l2, f2)
                | Errors_ (err,params,files) -> Errors_ (err,params,files))
            | Errors_ (errs, l, f) ->
              (match aux t2 l f pref suff with
                | Res_ (_, ll, ff) -> Errors_ (errs, ll, ff)
                | Errors_ (errs2, ll, ff) -> Errors_ ((errs2@errs), ll, ff)))
        | TOption (TAtom (_,TString) as t,b) ->
          (try
             (match aux t params files pref suff with
               | Res_ (v, l, f) ->
                 if b && String.length v = 0  (* Is the value an empty string? *)
                 then Res_ (None, l, f)
                 else Res_ (Some v, l, f)
               | Errors_ (errs, ll, ff) when List.for_all (fun (_,s,_) -> s="") errs -> Res_ (None, ll, ff)
               | Errors_ err   -> Errors_ err)
           with Not_found -> Res_ (None, params, files))
        | TOption (t,b) ->
          (try
             (match aux t params files pref suff with
               | Res_ (v, l, f) -> Res_ (Some v, l, f)
               | Errors_ (errs, ll, ff) when List.for_all (fun (_,s,_) -> s="") errs -> Res_ (None, ll, ff)
               | Errors_ err   -> Errors_ err)
           with Not_found -> Res_ (None, params, files))
        | TList (n, t) -> aux_list t params files n pref suff
        | TSet t ->
          let rec aux_set params files =
            try
              match aux t params files pref suff with
                | Res_ (vv, ll, ff) ->
                  (match aux_set ll ff with
                    | Res_ (vv2, ll2, ff2) ->
                      Res_ (vv::vv2, ll2, ff2)
                    | err -> err)
                | Errors_ (errs, ll, ff) ->
                  (match aux_set ll ff with
                    | Res_ (_, ll2, ff2) -> Errors_ (errs, ll2, ff2)
                    | Errors_ (errs2, ll2, ff2) -> Errors_ (errs@errs2, ll2, ff2))
            with Not_found -> Res_ ([], params, files)
          in aux_set params files
        | TSum (t1, t2) ->
          begin
            (* We try to decode both cases, if both succeed,
               we choose the one that consumes parameters (or the 1st one if none consumes) *)
            match aux t1 params files pref suff,aux t2 params files pref suff with
            | Res_ (v,l,files), Errors_ _ -> Res_ (Inj1 v,l,files)
            | Errors_ _, Res_ (v,l,files) -> Res_ (Inj2 v,l,files)
            | Errors_ err, Errors_ _ -> Errors_ err
            | Res_ (v1,l1,files1), Res_ (v2,l2,files2) ->
              if l1 = params
              then
                if l2 = params
                then Res_ (Inj1 v1,l1,files1)
                else Res_ (Inj2 v2,l2,files2)
              else Res_ (Inj1 v1,l1,files1)
          end
        | TAtom (name,TBool) ->
          (try
             let v,l = (List.assoc_remove (pref^name^suff) params) in
             Res_ (true,l,files)
           with Not_found -> Res_ (false, params, files))

        | TAtom (name,a) ->
          let v,l = (List.assoc_remove (pref^name^suff) params) in
          (try (Res_ (atom_of_string a v, l, files))
           with e -> Errors_ ([(pref^name^suff),v,e], l, files))
        | TFile name ->
          (try
             let v,f = List.assoc_remove (pref^name^suff) files in
             Res_ (v, params, f)
           with e ->
             Errors_ ([pref^name^suff,"",e], [], files))
        | TCoord name ->
          let r1 =
            let v, l = (List.assoc_remove (pref^name^suff^".x") params) in
            (try (Res_ ((int_of_string v), l, files))
             with e -> Errors_ ([(pref^name^suff^".x"), v, e], l, files))
          in
          (match r1 with
           | Res_ (x1, l1, f) ->
             let v, l = (List.assoc_remove (pref^name^suff^".y") l1) in
             (try (Res_ (
                  {abscissa= x1;
                   ordinate= int_of_string v}, l, f))
              with e -> Errors_ ([(pref^name^suff^".y"), v, e], l, f))
           | Errors_ (errs, l1, f) ->
             let v, l = (List.assoc_remove (pref^name^suff^".y") l1) in
             (try
                ignore (int_of_string v);
                Errors_ (errs, l, f)
              with e -> Errors_ (((pref^name^suff^".y"), v, e)::errs, l, f)))
        | TCoordv (name, t) ->
            aux (TProd (t, TCoord name)) params files pref suff
        | TUserType (name, ofto) ->
          let v,l = (List.assoc_remove (pref^name^suff) params) in
          (try Res_ (ofto.of_string v,l,files)
           with e -> Errors_ ([(pref^name^suff),v,e], l, files))
        | TTypeFilter (t, None) -> failwith "Type filter without filter"
        | TTypeFilter (t, Some check) ->
          (match aux t params files pref suff with
            | Res_ (v, l, files) as a ->
              (try check v; a
               with e -> Errors_ (["<type_check>","<>",e], l, files))
            | a -> a)
        | TUnit -> Res_ ((), params, files)
        | TAny -> Res_  (params, [], files)
        | TConst _ -> Res_ ((), params, files)
        | TESuffix n ->
          let v,l = List.assoc_remove n params in
          (* cannot have prefix or suffix *)
          Res_ (Neturl.split_path v, l, files)
        | TESuffixs n ->
          let v,l = List.assoc_remove n params in
          (* cannot have prefix or suffix *)
          Res_ (v, l, files)
        | TESuffixu (n, ofto) ->
          let v,l = List.assoc_remove n params in
          (* cannot have prefix or suffix *)
          (try Res_ (ofto.of_string v, l, files)
           with e -> Errors_ ([(pref^n^suff), v, e], l, files))
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
        | TJson (name, Some typ) ->
          let v,l = List.assoc_remove (pref^name^suff) params in
          Res_ ((of_json ~typ v),l,files)
        | TJson (name, None) -> assert false
        (* Never unmarshal server side without type! *)
        | TRaw_post_data -> raise Eliom_common.Eliom_Wrong_parameter
  in
  try
    match aux typ params files "" "" with
      | Res_ (v, l, files) ->
        if (l, files) = ([], [])
        then v
        else raise Eliom_common.Eliom_Wrong_parameter
      | Errors_ (errs, l, files) ->
        if (l, files) = ([], [])
        then raise (Eliom_common.Eliom_Typing_Error (List.map (fun (v,l,e) -> (v,e)) errs))
        else raise Eliom_common.Eliom_Wrong_parameter
  with | Not_found -> raise Eliom_common.Eliom_Wrong_parameter


let reconstruct_params ~sp (type a) (type c) (typ : (a,'b,c) params_type) params files nosuffixversion urlsuffix : a Lwt.t =
  match typ, params, files with
    | TRaw_post_data, None, None ->
      let ri = Eliom_request_info.get_ri_sp sp in
      Lwt.return
        (
           (Ocsigen_request_info.content_type ri,
            (Ocsigen_request_info.http_frame ri).Ocsigen_http_frame.frame_content))
    | typ, None, None ->
      (try
         Lwt.return
           (reconstruct_params_
              sp.Eliom_common.sp_request
              typ [] [] nosuffixversion urlsuffix)
       with e -> Lwt.fail e)
    | typ, Some params, Some files ->
      params >>= fun params ->
      files >>= fun files ->
      (try
         Lwt.return
           (reconstruct_params_
              sp.Eliom_common.sp_request
              typ params files nosuffixversion urlsuffix)
       with e -> Lwt.fail e)
    | _ -> Lwt.fail Eliom_common.Eliom_Wrong_parameter

(*****************************************************************************)
(* Non localized parameters *)


let get_non_localized_parameters params ~getorpost ~sp
    {name;
     get;
     post;
     param = paramtype} =
  (* non localized parameters are parsed only once,
     and cached in request_cache *)
  let key = if getorpost then get else post in
  (try
     (* first, look in cache: *)
     Polytables.get
       ~table:(Ocsigen_request_info.request_cache sp.Eliom_common.sp_request.request_info)
       ~key
   with Not_found ->
     let p =
       try
         Some
           (let params = String.Table.find name params in
            reconstruct_params_
              sp.Eliom_common.sp_request paramtype params [] false None)
       with Eliom_common.Eliom_Wrong_parameter | Not_found -> None
     in
     (* add in cache: *)
     Polytables.set
       ~table:(Ocsigen_request_info.request_cache sp.Eliom_common.sp_request.request_info)
       ~key
       ~value:p;
     p)

let get_non_localized_get_parameters p =
  let sp = Eliom_common.get_sp () in
  get_non_localized_parameters
    sp.Eliom_common.sp_si.Eliom_common.si_nl_get_params ~getorpost:true ~sp p

let get_non_localized_post_parameters p =
  let sp = Eliom_common.get_sp () in
  get_non_localized_parameters
    sp.Eliom_common.sp_si.Eliom_common.si_nl_post_params ~getorpost:false ~sp p
