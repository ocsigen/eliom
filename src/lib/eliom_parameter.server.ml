(* Ocsigen
 * http://www.ocsigen.org
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

type raw_post_data = Eliom_request_info.raw_post_data

open Ocsigen_extensions

(* server-specific constructors *)

let user_type
    ?client_to_and_of
    ~(of_string : string -> 'a)
    ~(to_string : 'a -> string)
    (n : string) =
  TUserType (
    n,
    Eliom_common.To_and_of_shared.create
      ?client_to_and_of
      {of_string ; to_string}
  )

let all_suffix_user
    ?client_to_and_of
    ~(of_string : string -> 'a)
    ~(to_string : 'a -> string)
    (n : string) =
  TESuffixu (
    n,
    Eliom_common.To_and_of_shared.create
      ?client_to_and_of
      {of_string ; to_string}
  )

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

(* Non localized parameters *)

let get_non_localized_parameters params files ~getorpost ~sp
    {name;
     get;
     post;
     param = paramtype} =
  (* non localized parameters are parsed only once,
     and cached in request_cache *)
  let key = match getorpost with `Get  -> get | `Post -> post in
  (try
     (* first, look in cache: *)
     Polytables.get
       ~table:(Ocsigen_request.request_cache sp.Eliom_common.sp_request.request_info)
       ~key
   with Not_found ->
     let p =
       try
         Some
           (let params = try String.Table.find name params with Not_found -> [] in
            let files = try String.Table.find name files with Not_found -> [] in
            reconstruct_params_ paramtype params files false None)
       with Eliom_common.Eliom_Wrong_parameter | Not_found ->
         None
     in
     (* add in cache: *)
     Polytables.set
       ~table:(Ocsigen_request.request_cache sp.Eliom_common.sp_request.request_info)
       ~key
       ~value:p;
     p)

let get_non_localized_get_parameters p =
  let sp = Eliom_common.get_sp () in
  get_non_localized_parameters
    sp.Eliom_common.sp_si.Eliom_common.si_nl_get_params
    sp.Eliom_common.sp_si.Eliom_common.si_nl_file_params
    ~getorpost:`Get ~sp p

let get_non_localized_post_parameters p =
  let sp = Eliom_common.get_sp () in
  get_non_localized_parameters
    sp.Eliom_common.sp_si.Eliom_common.si_nl_post_params
    sp.Eliom_common.sp_si.Eliom_common.si_nl_file_params
    ~getorpost:`Post ~sp p
