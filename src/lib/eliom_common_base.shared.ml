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

exception Eliom_site_information_not_available of string

(******************************************************************)

type scope_hierarchy =
  | User_hier of string
  | Default_ref_hier
  | Default_comet_hier

type user_scope =
  [ `Session_group of scope_hierarchy
  | `Session of scope_hierarchy
  | `Client_process of scope_hierarchy ]

type scope = [`Site | user_scope]
type all_scope = [scope | `Global | `Request]
type global_scope = [`Global]
type site_scope = [`Site]
type session_group_scope = [`Session_group of scope_hierarchy]
type session_scope = [`Session of scope_hierarchy]
type client_process_scope = [`Client_process of scope_hierarchy]
type request_scope = [`Request]

(******************************************************************)
type user_level = [`Session_group | `Session | `Client_process]
type cookie_level = [`Session | `Client_process]

type cookie_scope =
  [`Session of scope_hierarchy | `Client_process of scope_hierarchy]

let level_of_user_scope : [< user_scope] -> [> user_level] = function
  | `Session _ -> `Session
  | `Session_group _ -> `Session_group
  | `Client_process _ -> `Client_process

let cookie_level_of_user_scope : [< user_scope] -> [> cookie_level] = function
  | `Session _ | `Session_group _ -> `Session
  | `Client_process _ -> `Client_process

let cookie_scope_of_user_scope : [< user_scope] -> [> cookie_scope] = function
  | `Session n | `Session_group n -> `Session n
  | `Client_process n -> `Client_process n

let scope_hierarchy_of_user_scope : [< user_scope] -> scope_hierarchy = function
  | `Session n | `Session_group n | `Client_process n -> n

(* The key in the table of states. For cookies scopes, it is also the
   information in the cookie name, without the kind of session, and with the
   scope level (that is not in the cookie name). *)
type full_state_name =
  {user_scope : user_scope; secure : bool; site_dir_str : string}

module Full_state_name_table = Map.Make (struct
  type t = full_state_name

  let compare = compare
end)

(******************************************************************)
(* Service kinds: *)
type att_key_serv =
  | SAtt_no (* regular service *)
  | SAtt_named of string (* named coservice *)
  | SAtt_anon of string (* anonymous coservice *)
  | SAtt_csrf_safe of (int * user_scope * bool option)
  (* CSRF safe anonymous coservice *)
  (* CSRF safe service registration delayed until form/link creation *)
  (* the int is an unique id,
         the user_scope is used for delayed registration
         (if the service is registered in the global table),
         the bool option is the ?secure parameter for delayed registration
         (if the service is registered in the global table) *)
  (* The following three are for non-attached coservices
     that have been attached on a service afterwards *)
  | SAtt_na_named of string
  | SAtt_na_anon of string
  | SAtt_na_csrf_safe of (int * user_scope * bool option)

type na_key_serv =
  | SNa_no (* no na information *)
  | SNa_void_keep (* void coservice that keeps GET na parameters *)
  | SNa_void_dontkeep (* void coservice that does not keep GET na parameters *)
  | SNa_get_ of string (* named *)
  | SNa_post_ of string (* named *)
  | SNa_get' of string (* anonymous *)
  | SNa_post' of string (* anonymous *)
  | SNa_get_csrf_safe of (int * user_scope * bool option)
  (* CSRF safe anonymous coservice *)
  | SNa_post_csrf_safe of (int * user_scope * bool option)
(* CSRF safe anonymous coservice *)

(* the same, for incoming requests: *)
type att_key_req =
  | RAtt_no (* no coservice information *)
  | RAtt_named of string (* named coservice *)
  | RAtt_anon of string
(* anonymous coservice *)

type na_key_req =
  | RNa_no (* no na information *)
  | RNa_get_ of string (* named *)
  | RNa_post_ of string (* named *)
  | RNa_get' of string (* anonymous *)
  | RNa_post' of string
(* anonymous *)

let att_key_serv_of_req = function
  | RAtt_no -> SAtt_no
  | RAtt_named s -> SAtt_named s
  | RAtt_anon s -> SAtt_anon s

let na_key_serv_of_req = function
  | RNa_no -> SNa_no
  | RNa_post' s -> SNa_post' s
  | RNa_get' s -> SNa_get' s
  | RNa_post_ s -> SNa_post_ s
  | RNa_get_ s -> SNa_get_ s

(*****************************************************************************)
let defaultpagename = "./"
(* should be "" but this does not work with firefox.
   "index" works but one page may have two different URLs *)

let eliom_suffix_name = "__eliom_suffix"
let eliom_suffix_internal_name = "__(suffix service)__"
let eliom_nosuffix_page = "__eliom_suffix__"
let naservice_num = "__eliom_na__num"
let naservice_name = "__eliom_na__name"
let get_state_param_name = "__eliom__"
let post_state_param_name = "__eliom_p__"
let get_numstate_param_name = "__eliom_n__"
let post_numstate_param_name = "__eliom_np__"
let co_param_prefix = "__co_eliom_"
let na_co_param_prefix = "__na_eliom_"
let nl_param_prefix = "__nl_"
let pnl_param_prefix = nl_param_prefix ^ "p_"
let npnl_param_prefix = nl_param_prefix ^ "n_"
let eliom_internal_nlp_prefix = "__eliom"
let tab_cookies_param_name = "__eliom_P_tab_cookies"
let to_be_considered_as_get_param_name = "__eliom_P_was_GET"
let appl_name_cookie_name = "__eliom_appl_name"
let appl_name_header_name = "X-Eliom-Application"
let full_xhr_redir_header = "X-Eliom-Location-Full"
let half_xhr_redir_header = "X-Eliom-Location-Half"
let response_url_header = "X-Eliom-Url"
let set_tab_cookies_header_name = "X-Eliom-Set-Process-Cookies"
let tab_cookies_header_name = "X-Eliom-Process-Cookies"

(* Cookie substitutes for iOS WKWebView *)
let cookie_substitutes_header_name = "X-Eliom-Cookie-Substitutes"
let set_cookie_substitutes_header_name = "X-Eliom-Set-Cookie-Substitutes"
let tab_cpi_header_name = "X-Eliom-Process-Info"
let expecting_process_page_name = "X-Eliom-Expecting-Process-Page"
let base_elt_id = "eliom_base_elt"
let nl_is_persistent n = n.[0] = 'p'

(*****************************************************************************)

type client_process_info =
  { cpi_ssl : bool
  ; cpi_hostname : string
  ; cpi_server_port : int
  ; cpi_original_full_path : string list }
[@@deriving json]

type sess_info =
  { si_other_get_params : (string * string) list
  ; si_all_get_params : (string * string) list
  ; si_all_post_params : (string * string) list option
  ; si_all_file_params : (string * file_info) list option
  ; si_service_session_cookies : string Full_state_name_table.t
  ; (* the session service cookies sent by the request *)
    (* the key is the cookie name (or site dir) *)
    si_data_session_cookies : string Full_state_name_table.t
  ; (* the session data cookies sent by the request *)
    (* the key is the cookie name (or site dir) *)
    si_persistent_session_cookies : string Full_state_name_table.t
  ; (* the persistent session cookies sent by the request *)
    (* the key is the cookie name (or site dir) *)
    si_secure_cookie_info :
      string Full_state_name_table.t
      * string Full_state_name_table.t
      * string Full_state_name_table.t
        (* the same, but for secure cookies *)
  ; (* now for tab cookies: *)
    si_service_session_cookies_tab : string Full_state_name_table.t
  ; si_data_session_cookies_tab : string Full_state_name_table.t
  ; si_persistent_session_cookies_tab : string Full_state_name_table.t
  ; si_secure_cookie_info_tab :
      string Full_state_name_table.t
      * string Full_state_name_table.t
      * string Full_state_name_table.t
  ; si_tab_cookies : string Ocsigen_cookie_map.Map_inner.t
  ; si_nonatt_info : na_key_req
  ; si_state_info : att_key_req * att_key_req
  ; si_previous_extension_error : int
        (* HTTP error code sent by previous extension (default: 404) *)
  ; si_na_get_params : (string * string) list Lazy.t
  ; si_nl_get_params : (string * string) list String.Table.t
  ; si_nl_post_params : (string * string) list String.Table.t
  ; si_nl_file_params : (string * file_info) list String.Table.t
  ; si_persistent_nl_get_params : (string * string) list String.Table.t Lazy.t
  ; si_all_get_but_na_nl : (string * string) list Lazy.t
  ; si_all_get_but_nl : (string * string) list
  ; si_ignored_get_params : (string * string) list
  ; si_ignored_post_params : (string * string) list
  ; si_client_process_info : client_process_info option
  ; si_expect_process_data : bool Lazy.t
        (*204FORMS*     si_internal_form: bool; *) }

type eliom_js_page_data =
  { ejs_global_data : (Eliom_runtime.global_data * Eliom_wrap.unwrapper) option
  ; ejs_request_data : Eliom_runtime.request_data
  ; (* Event handlers *)
    ejs_event_handler_table : Eliom_runtime.RawXML.event_handler_table
  ; (* Client Attributes *)
    ejs_client_attrib_table : Eliom_runtime.RawXML.client_attrib_table
  ; (* Session info *)
    ejs_sess_info : sess_info }

(************ unwrapping identifiers *********************)

let tyxml_unwrap_id_int = Eliom_runtime.tyxml_unwrap_id_int
let () = assert (tyxml_unwrap_id_int = 1)
let comet_channel_unwrap_id_int = 2
let react_up_unwrap_id_int = 3
let react_down_unwrap_id_int = 4
let signal_down_unwrap_id_int = 5
let bus_unwrap_id_int = 6
let client_value_unwrap_id_int = Eliom_runtime.client_value_unwrap_id_int
let () = assert (client_value_unwrap_id_int = 7)
let global_data_unwrap_id_int = Eliom_runtime.global_data_unwrap_id_int
let () = assert (global_data_unwrap_id_int = 8)
let server_function_unwrap_id_int = 9

type node_ref = string

(****** *)

(* CCC take care: this must remain of the same syntax as non localised
   non persistent get parameter name *)
let nl_get_appl_parameter = "__nl_n_eliom-process.p"

(* make a path by going up when there is a '..' *)
let make_actual_path path =
  let rec aux accu path =
    match accu, path with
    | [], ".." :: path' -> aux accu path'
    | _ :: accu', ".." :: path' -> aux accu' path'
    | _, a :: path' -> aux (a :: accu) path'
    | _, [] -> accu
  in
  match path with
  | "" :: path -> "" :: List.rev (aux [] path)
  | _ -> List.rev (aux [] path)

let is_client_app = ref false

(* Special version for non localized parameters *)
let prefixlength = String.length nl_param_prefix
let prefixlengthminusone = prefixlength - 1

let split_nl_prefix_param l =
  let rec aux other map = function
    | [] -> map, other
    | ((n, _) as a) :: l ->
        if String.first_diff n nl_param_prefix 0 prefixlengthminusone
           = prefixlength
        then
          try
            let last = String.index_from n prefixlength '.' in
            let nl_param_name =
              String.sub n prefixlength (last - prefixlength)
            in
            let previous =
              try String.Table.find nl_param_name map with Not_found -> []
            in
            aux other (String.Table.add nl_param_name (a :: previous) map) l
          with Invalid_argument _ | Not_found -> aux (a :: other) map l
        else aux (a :: other) map l
  in
  aux [] String.Table.empty l

(* Split parameter list, removing those whose name starts with pref *)
let split_prefix_param pref l =
  let len = String.length pref in
  List.partition
    (fun (n, _) ->
      try String.sub n 0 len = pref with Invalid_argument _ -> false)
    l

(* Remove all parameters whose name starts with pref *)
let remove_prefixed_param pref l =
  let len = String.length pref in
  let rec aux = function
    | [] -> []
    | ((n, _) as a) :: l -> (
      try if String.sub n 0 len = pref then aux l else a :: aux l
      with Invalid_argument _ -> a :: aux l)
  in
  aux l

let remove_na_prefix_params l =
  remove_prefixed_param na_co_param_prefix l
  |> List.remove_assoc naservice_name
  |> List.remove_assoc naservice_num

let filter_na_get_params =
  List.filter @@ fun (s, (_ : string)) ->
  s = naservice_name || s = naservice_num
  || String.sub s 0 (String.length na_co_param_prefix) = na_co_param_prefix

exception Eliom_404

type ('a, 'b) foundornot = Found of 'a | Notfound of 'b

(** Service called with wrong parameter names *)

exception Eliom_Wrong_parameter
exception Eliom_duplicate_registration of string
exception Eliom_page_erasing of string

type 'a dircontent = Vide | Table of 'a direlt ref String.Table.t

and 'a direlt = Dir of 'a dircontent ref | File of 'a ref

let empty_dircontent () = Vide

type meth = [`Get | `Post | `Put | `Delete | `Other]
type page_table_key = {key_state : att_key_serv * att_key_serv; key_meth : meth}
type anon_params_type = int

exception Eliom_Typing_Error of (string * exn) list

type ('params, 'result) service =
  { (* unique_id, computed from parameters type.  must be the same even
     if the actual service reference is different (after reloading the
     site) so that it replaces the former one *)
    s_id : anon_params_type * anon_params_type
  ; mutable s_max_use : int option
  ; s_expire : (float * float ref) option
  ; s_f : bool -> 'params -> 'result Lwt.t }

type 'a to_and_of = {of_string : string -> 'a; to_string : 'a -> string}

(* gets backtrace up until the first slot in the backtrace which mentions
     Lwt, which is usually where the backtrace is no longer informative *)
let backtrace_lwt =
  let lwt_slot_re = Re.Str.regexp "Called from Lwt." in
  fun skip ->
    let stack = Printexc.get_callstack 16 in
    let stack_length = Printexc.raw_backtrace_length stack in
    let rec loop acc i =
      if i >= stack_length
      then acc
      else
        match
          let open Printexc in
          Slot.format i @@ convert_raw_backtrace_slot
          @@ get_raw_backtrace_slot stack i
        with
        | Some s ->
            if Re.Str.string_match lwt_slot_re s 0
            then acc
            else loop (s :: acc) (i + 1)
        | None -> loop acc (i + 1)
    in
    List.rev @@ loop [] skip
