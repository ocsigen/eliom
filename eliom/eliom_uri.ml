(* Ocsigen
 * http://www.ocsigen.org
 * Module Eliom_uri
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
open Eliom_parameters
open Eliom_services
open Eliom_sessions


(*****************************************************************************)
(* Building href *)
let rec string_of_url_path' = function
  | [] -> ""
  | [a] when a = Eliom_common.eliom_suffix_internal_name -> ""
  | [a] -> Ocsigen_lib.encode ~plus:false a
  | a::b::l when b = Eliom_common.eliom_suffix_internal_name -> 
      string_of_url_path' (a::l)
  | a::l when a = Eliom_common.eliom_suffix_internal_name ->
      string_of_url_path' l
  | a::l -> (Ocsigen_lib.encode ~plus:false a)^"/"^(string_of_url_path' l)

let rec string_of_url_path_suff u = function
  | None -> string_of_url_path' u
  | Some suff -> 
      let pref = string_of_url_path' u in
      let suf = string_of_url_path' suff in
      if pref = ""
      then suf
      else String.concat "/" [pref; suf]

let reconstruct_absolute_url_path = string_of_url_path_suff

let reconstruct_relative_url_path current_url u =
  let rec drop cururl desturl = match cururl, desturl with
  | a::l, [b] -> l, desturl
  | [a], m -> [], m
  | a::l, b::m when a = b -> drop l m
  | a::l, m -> l, m
  | [], m -> [], m
  in let rec makedotdot = function
    | [] -> []
(*    | [a] -> "" *)
    | _::l -> ".."::(makedotdot l)
  in
  let aremonter, aaller = drop current_url u in 
  (makedotdot aremonter)@aaller

let reconstruct_relative_url_path_string current_url u suff =
  let relurl = reconstruct_relative_url_path current_url u in
  let s = string_of_url_path_suff relurl suff in
  if s = "" 
  then Eliom_common.defaultpagename 
  else if s.[0] = '/'
  then (* possible with optional parameters *) "./"^s
  else s

let rec relative_url_path_to_myself = function
  | []
  | [""] -> Eliom_common.defaultpagename
  | [a] -> a
  | a::l -> relative_url_path_to_myself l

(* make a path by going up when there is a '..' *)
let make_actual_path path =
  let rec aux accu path =
    match (accu, path) with
      | ([], ".."::path') -> aux accu path'
      | (_::accu',  ".."::path') -> aux accu' path'
      | (_,  a::path') -> aux (a::accu) path'
      | (_,  []) -> accu
  in match path with
    | ""::path -> ""::List.rev (aux [] path)
    | _ -> List.rev (aux [] path)

(*****************************************************************************)


let make_proto_prefix
    ?sp
    ?hostname
    ?port
    https
    : string =
  let ssl =
    match sp with
      | Some sp -> Eliom_sessions.get_ssl ~sp
      | None -> false
  in
  let host = match hostname, sp with
    | None, Some sp -> Eliom_sessions.get_hostname ~sp 
    | None, None -> Eliom_sessions.get_default_hostname () 
    | Some h, _ -> h
  in
  let port = 
    match port, sp with
      | Some p, _ -> p
      | None, Some sp ->
          if https = ssl
          then Eliom_sessions.get_server_port ~sp 
          else if https
          then Eliom_sessions.get_default_sslport ~sp ()
          else Eliom_sessions.get_default_port ~sp ()
      | None, None ->
        if https
        then Eliom_sessions.get_default_sslport ()
        else Eliom_sessions.get_default_port ()
  in
  Ocsigen_lib.make_absolute_url https host port "/"



let make_uri_components_ (* does not take into account getparams *)
    ?(absolute = false) (* used to force absolute link *)
    ?(absolute_path = false) (* used to force absolute link without protocol/server/port *)
    ?https
    ~service
    ?sp
    ?hostname
    ?port
    ?fragment
    ?keep_nl_params
    ?(nl_params = Eliom_parameters.empty_nl_params_set) 
    () =

  let ssl =
    match sp with
      | Some sp -> Eliom_sessions.get_ssl ~sp
      | None -> false
  in
  
  let https = 
    (https = Some true) || 
      (Eliom_services.get_https service) ||
      (https = None && ssl)
  in
  let absolute = 
    if absolute || https <> ssl 
    then Some (make_proto_prefix ?sp ?hostname ?port https)
    else if absolute_path
    then Some "/"
    else None 
  in
(*VVV We trust current protocol? *) 



  let nl_params = Eliom_parameters.table_of_nl_params_set nl_params in
  let keep_nl_params = match keep_nl_params with
    | None -> Eliom_services.keep_nl_params service
    | Some b -> b
  in
  (* for preapplied non localized and not non localized: *)
  let preappnlp, preapplied_params = get_pre_applied_parameters_ service in
  let nlp =
    match keep_nl_params, sp with
      | `All, Some sp ->
          (* We replace current nl params by preapplied ones *)
          Ocsigen_lib.String_Table.fold
            (fun key v b -> Ocsigen_lib.String_Table.add key v b)
            preappnlp
            (Eliom_sessions.get_nl_get_params ~sp)
      | `Persistent, Some sp ->
          (* We replace current nl params by preapplied ones *)
          Ocsigen_lib.String_Table.fold
            (fun key v b -> Ocsigen_lib.String_Table.add key v b)
            preappnlp
            (Eliom_sessions.get_persistent_nl_get_params ~sp)
      | `All, None
      | `Persistent, None
      | `None, _ -> preappnlp
  in
  let nlp =
    (* We replace current nl params by nl_params *)
    Ocsigen_lib.String_Table.fold
      (fun key v b -> Ocsigen_lib.String_Table.add key v b)
      nl_params
      nlp
  in

  (* remove in nlp the one present in the service parameters *)
  let getparamstype = get_get_params_type_ service in
  let nlp = Eliom_parameters.remove_from_nlp nlp getparamstype in
  let hiddenparams = 
    Ocsigen_lib.String_Table.fold
      (fun _ l beg -> l@beg)
      nlp preapplied_params
  in


  match get_kind_ service with
    | `Attached attser ->
        begin
          let uri =
            let suff= None in
            if (get_att_kind_ attser) = `External
            then
              (get_prefix_ attser)^
                "/"^  (* we add the "/" even if there is no prefix,
                         because we should do absolute links in that case *)
                (reconstruct_absolute_url_path (get_full_path_ attser) suff)
            else
              match absolute, sp with
                | Some proto_prefix, _ ->
                    proto_prefix^
                      reconstruct_absolute_url_path
                      (get_full_path_ attser) suff
                | None, Some sp ->
                    reconstruct_relative_url_path_string
                      (get_original_full_path sp)
                      (get_full_path_ attser) suff
                | None, None ->
                    reconstruct_relative_url_path_string
                      []
                      (get_full_path_ attser) suff
          in
          match get_get_name_ attser, sp with
            | Eliom_common.SAtt_no, _ ->
                (uri, hiddenparams, fragment)
            | Eliom_common.SAtt_anon s, _ ->
                (uri, 
                 ((Eliom_common.get_numstate_param_name, s)::hiddenparams),
                 fragment)
            | Eliom_common.SAtt_named s, _ ->
                (uri,
                 ((Eliom_common.get_state_param_name, s)::hiddenparams),
                 fragment)
            | Eliom_common.SAtt_csrf_safe csrf_info, Some sp ->
                let s = 
                  Eliom_services.register_delayed_get_or_na_coservice
                    ~sp csrf_info
                in
                (uri, 
                 ((Eliom_common.get_numstate_param_name, s)::hiddenparams),
                 fragment)
            | Eliom_common.SAtt_csrf_safe csrf_info, None ->
              failwith "make_uri_component: not possible on csrf safe service without ~sp parameter"

        end
    | `Nonattached naser ->
        let na_name = get_na_name_ naser in
        let params' =
          let current_get_params =
            match sp with
              | None -> []
              | Some sp ->
                (if na_name = Eliom_common.SNa_void_keep
                 then (Eliom_sessions.get_si sp).Eliom_common.si_all_get_but_nl
                 else Lazy.force 
                    (Eliom_sessions.get_si sp).Eliom_common.si_all_get_but_na_nl)
          in
          (match na_name, sp with
             | Eliom_common.SNa_void_keep, _
             | Eliom_common.SNa_void_dontkeep, _ -> current_get_params
             | Eliom_common.SNa_get' n, _ ->
                 (Eliom_common.naservice_num, n)::current_get_params
             | Eliom_common.SNa_get_ n, _ ->
                 (Eliom_common.naservice_name, n)::current_get_params
             | Eliom_common.SNa_get_csrf_safe csrf_info, Some sp ->
                 let n = 
                   Eliom_services.register_delayed_get_or_na_coservice
                     ~sp csrf_info
                 in
                 (Eliom_common.naservice_num, n)::current_get_params
             | Eliom_common.SNa_get_csrf_safe csrf_info, None ->
               failwith "make_uri_component: not possible on csrf safe service without ~sp parameter"
             | _ -> assert false)
        in
        let params = params'@hiddenparams in
        let beg =
          match absolute, sp with
            | Some proto_prefix, Some sp ->
              proto_prefix^ get_original_full_path_string sp
            | None, Some sp -> 
              relative_url_path_to_myself (get_original_full_path sp)
            | Some proto_prefix, None ->
              proto_prefix
            | None, None -> 
              relative_url_path_to_myself []
        in
        (beg, params, fragment)



let make_uri_components
    ?absolute
    ?absolute_path
    ?https
    ~service
    ?sp
    ?hostname
    ?port
    ?fragment
    ?keep_nl_params
    ?nl_params
    getparams =
  let (uri, pregetparams, fragment) =
    make_uri_components_
      ?absolute
      ?absolute_path
      ?https
      ~service
      ?sp
      ?hostname
      ?port
      ?fragment
      ?keep_nl_params
      ?nl_params
      ()
  in
  (* for getparams and non localized params: *)
  let suff, params =
    construct_params_list
      String_Table.empty
      (get_get_params_type_ service) getparams 
      (* if nl params were already present, they will be replaced
         by new values *)
  in
  let uri =
    match suff with
      | None -> uri
      | Some suff ->
        let suff = string_of_url_path' suff in
        if uri.[String.length uri - 1] = '/'
        then uri ^ suff
        else String.concat "/" [uri; suff]
  in
  let fragment = Ocsigen_lib.apply_option Ocsigen_lib.encode fragment in
  (uri, (params@pregetparams), fragment)





let make_string_uri_from_components (uri, params, fragment) =
  let s = Ocsigen_lib.concat_strings uri "?" (construct_params_string params) in
  match fragment with
    | None -> s
    | Some f -> String.concat "#" [s; f]


let make_string_uri
    ?absolute
    ?absolute_path
    ?https
    ~service
    ?sp
    ?hostname
    ?port
    ?fragment
    ?keep_nl_params
    ?nl_params
    getparams : string =
  make_string_uri_from_components
    (make_uri_components
       ?absolute
       ?absolute_path
       ?https
       ~service
       ?sp
       ?hostname
       ?port
       ?fragment
       ?keep_nl_params
       ?nl_params
       getparams)




let make_post_uri_components_ (* do not take into account postparams *)
    ?(absolute = false)
    ?(absolute_path = false)
    ?https
    ~service
    ~sp
    ?hostname
    ?port
    ?fragment
    ?keep_nl_params
    ?(nl_params = Eliom_parameters.empty_nl_params_set) 
    ?(keep_nl_params : [ `All | `Persistent | `None ] option)
    ?keep_get_na_params
    getparams 
    () =
  match get_kind_ service with
    | `Attached attser ->

        let (uri, getparams, fragment), getname =
          let getname = get_get_name_ attser in
          match getname with
            | Eliom_common.SAtt_csrf_safe csrf_info ->
                (* special case for post-coservices on get csrf safe services:
                   we must register the get service first *)
                let s = 
                  Eliom_common.SAtt_anon 
                    (Eliom_services.register_delayed_get_or_na_coservice
                       ~sp csrf_info)
                in
                (make_uri_components
                   ~absolute
                   ~absolute_path
                   ?https
                   ~service:(Eliom_services.change_get_num service attser s)
                   ~sp
                   ?hostname
                   ?port
                   ?fragment
                   ?keep_nl_params
                   ~nl_params
                   getparams,
                 s)
            | _ -> (make_uri_components
                      ~absolute
                      ~absolute_path
                      ?https
                      ~service
                      ~sp
                      ?hostname
                      ?port
                      ?fragment
                      ?keep_nl_params
                      ~nl_params
                      getparams,
                    getname)
        in


        let postparams =
          match get_post_name_ attser with
            | Eliom_common.SAtt_no -> []
            | Eliom_common.SAtt_anon s ->
                [(Eliom_common.post_numstate_param_name, s)]
            | Eliom_common.SAtt_named s ->
                [(Eliom_common.post_state_param_name, s)]
            | Eliom_common.SAtt_csrf_safe csrf_info ->
                let s =
                  Eliom_services.register_delayed_post_coservice
                    ~sp csrf_info getname
                in
                [(Eliom_common.post_numstate_param_name, s)]
        in
        (uri, getparams, fragment, postparams)


    | `Nonattached naser ->

            let nl_params = Eliom_parameters.table_of_nl_params_set nl_params in
            let keep_nl_params = match keep_nl_params with
              | None -> Eliom_services.keep_nl_params service
              | Some b -> b
            in
            let preappnlp, preapp = get_pre_applied_parameters_ service in
            let nlp =
              match keep_nl_params with
                | `All ->
                    (* We replace current nl params by preapplied ones *)
                    Ocsigen_lib.String_Table.fold
                      (fun key v b -> Ocsigen_lib.String_Table.add key v b)
                      preappnlp
                      (Eliom_sessions.get_nl_get_params ~sp)
                | `Persistent ->
                    (* We replace current nl params by preapplied ones *)
                    Ocsigen_lib.String_Table.fold
                      (fun key v b -> Ocsigen_lib.String_Table.add key v b)
                      preappnlp
                      (Eliom_sessions.get_persistent_nl_get_params ~sp)
                | `None -> preappnlp
            in
            let nlp =
              (* We replace current nl params by nl_params *)
              Ocsigen_lib.String_Table.fold
                (fun key v b -> Ocsigen_lib.String_Table.add key v b)
                nl_params
                nlp
            in


            (* for getparams and non localized params: *)
            let suff, params =
              construct_params_list
                nlp (get_get_params_type_ service) getparams 
                (* if nl params were already present, they will be replaced
                   by new values *)
                (* getparams can be something else than [] 
                   if we have added nl params to the service (?) *)
            in
            let params = params @ preapp in

            let keep_get_na_params =
              match keep_get_na_params with
                | Some b -> b
                | None ->
                    match get_na_kind_ naser with
                      | `Post b -> b
                      | _ -> assert false
            in
            let params =
              params @
              (if keep_get_na_params
               then
                 (Eliom_sessions.get_si sp).Eliom_common.si_all_get_but_nl
               else
                 (Lazy.force
                   (Eliom_sessions.get_si sp).Eliom_common.si_all_get_but_na_nl))
            in


            let ssl = Eliom_sessions.get_ssl ~sp in
            let https = 
              (https = Some true) || 
                (Eliom_services.get_https service) ||
                (https = None && ssl)
            in
            let absolute = 
              if absolute || https <> ssl 
              then Some (make_proto_prefix ~sp ?hostname ?port https)
              else if absolute_path
              then Some "/"
              else None
            in


            (* absolute URL does not work behind a reverse proxy! *)
            let uri =
              match absolute with
                | Some proto_prefix ->
                    proto_prefix^get_original_full_path_string sp
                | None ->
                    relative_url_path_to_myself (get_original_full_path sp)
            in

            let naservice_line =
              match get_na_name_ naser with
               | Eliom_common.SNa_post' n -> (Eliom_common.naservice_num, n)
               | Eliom_common.SNa_post_ n -> (Eliom_common.naservice_name, n)
               | Eliom_common.SNa_post_csrf_safe csrf_info ->
                 let n =
                   Eliom_services.register_delayed_get_or_na_coservice
                     ~sp csrf_info
                 in
                 (Eliom_common.naservice_num, n)
               | _ -> assert false
            in


            let fragment = None (* fragment is not sent to the server *) in

            let postparams = [naservice_line] in
            (uri, params, fragment, postparams)



let make_post_uri_components
    ?absolute
    ?absolute_path
    ?https
    ~service
    ~sp
    ?hostname
    ?port
    ?fragment
    ?keep_nl_params
    ?nl_params
    ?keep_get_na_params
    getparams 
    postparams =

  let (uri, getparams, fragment, prepostparams) =
    make_post_uri_components_
      ?absolute
      ?absolute_path
      ?https
      ~service
      ~sp
      ?hostname
      ?port
      ?fragment
      ?keep_nl_params
      ?nl_params
      ?keep_get_na_params
      getparams 
      () 
  in
  let _, postparams =
    construct_params_list
      Ocsigen_lib.String_Table.empty
      (get_post_params_type_ service) 
      postparams
  in
  (uri, getparams, fragment, postparams@prepostparams)






