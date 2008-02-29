(* Ocsigen
 * http://www.ocsigen.org
 * Module eliommod.ml
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
(*****************************************************************************)
(*****************************************************************************)
(* Internal functions used by Eliom:                                         *)
(* Cookie and session management                                             *)
(* Tables of services (global and session tables,                            *)
(* persistant and volatile data tables)                                      *)
(* Store and load services                                                   *)
(*****************************************************************************)
(*****************************************************************************)




open Lwt
open Http_frame
open Ocsigen_lib
open Extensions
open Lazy

(** state is a parameter to differenciate coservices
   (several instances of the same URL).
 *)
type internal_state = string

(** Type used for cookies to set. 
    The float option is the timestamp for the expiration date.
    The strings are names and values.
 *)
type cookie = 
  | Set of url_path option * float option * string * string
  | Unset of url_path option * string


type na_key =
  | Na_no
  | Na_get_ of string (* service *)
  | Na_post_ of string (* service *)
  | Na_get' of string (* coservice *)
  | Na_post' of string (* coservice *)


type sess_info =
    {si_other_get_params: (string * string) list;
     si_all_get_params: (string * string) list;
     si_all_post_params: (string * string) list;

     si_service_session_cookies: string Http_frame.Cookievalues.t;
     (* the session service cookies sent by the request *)
     (* the key is the cookie name (or site dir) *)

     si_data_session_cookies: string Http_frame.Cookievalues.t;
     (* the session data cookies sent by the request *)
     (* the key is the cookie name (or site dir) *)

     si_persistent_session_cookies: string Http_frame.Cookievalues.t;
     (* the persistent session cookies sent by the request *)
     (* the key is the cookie name (or site dir) *)

     si_nonatt_info: na_key;
     si_state_info: (internal_state option * internal_state option);
     si_config_file_charset: string;
     si_previous_extension_error: int;
     (* HTTP error code sent by previous extension (default: 404) *)
   }

(* The table of tables for each session. Keys are cookies *)
module SessionCookies = Hashtbl.Make(struct 
  type t = string
  let equal = (=)
  let hash = Hashtbl.hash
end)

type 'a session_cookie =
  | SCNo_data
  | SCData_session_expired
  | SC of 'a

type cookie_exp =
  | CENothing   (** nothing to set *)
  | CEBrowser   (** expires at browser close *)
  | CESome of float (** expiration date *)

type timeout = 
  | TGlobal (** see global setting *)
  | TNone   (** explicitely set no timeout *)
  | TSome of float (** timeout duration in seconds *)



(* cookies information during page generation: *)

type 'a one_service_cookie_info =
    (* service sessions: *)
    {sc_value:string             (* current value *);
     sc_table:'a ref             (* service session table
                                    ref towards cookie table
                                  *);
     sc_timeout:timeout ref      (* user timeout - 
                                    ref towards cookie table
                                  *);
     sc_exp:float option ref     (* expiration date ref
                                    (server side) - 
                                    None = never
                                    ref towards cookie table
                                  *);
     sc_cookie_exp:cookie_exp ref (* cookie expiration date to set *);
     sc_session_group:Eliommod_sessiongroups.sessgrp option ref (* session group *)
   }


type one_data_cookie_info =
    (* in memory data sessions: *)
    {dc_value:string                    (* current value *);
     dc_timeout:timeout ref             (* user timeout - 
                                           ref towards cookie table
                                         *);
     dc_exp:float option ref            (* expiration date ref (server side) - 
                                           None = never
                                           ref towards cookie table
                                         *);
     dc_cookie_exp:cookie_exp ref       (* cookie expiration date to set *);
     dc_session_group: Eliommod_sessiongroups.sessgrp option ref (* session group *)
   }

type one_persistent_cookie_info =
     {pc_value:string                    (* current value *);
      pc_timeout:timeout ref             (* user timeout *); 
      pc_cookie_exp:cookie_exp ref       (* cookie expiration date to set *);
      pc_session_group:Eliommod_sessiongroups.perssessgrp option ref (* session group *)
    }


(*VVV heavy *)
type 'a cookie_info =
    (* service sessions: *)
    (string option            (* value sent by the browser *)
                              (* None = new cookie 
                                 (not sent by the browser) *)
       *
       
       'a one_service_cookie_info session_cookie ref
       (* SCNo_data = the session has been closed
          SCData_session_expired = the cookie has not been found in the table.
          For both of them, ask the browser to remove the cookie.
        *)
    )
      (* This one is not lazy because we must check all service sessions
         at each request to find the services *)
      Http_frame.Cookievalues.t ref (* The key is the full session name *) *
      
    (* in memory data sessions: *)
      (string option            (* value sent by the browser *)
                                (* None = new cookie 
                                   (not sent by the browser) *)
         *
         
         one_data_cookie_info session_cookie ref
         (* SCNo_data = the session has been closed
            SCData_session_expired = the cookie has not been found in the table.
            For both of them, ask the browser to remove the cookie.
          *)
      ) Lazy.t
      (* Lazy because we do not want to ask the browser to unset the cookie 
         if the cookie has not been used, otherwise it is impossible to 
         write a message "Your session has expired" *)
      Http_frame.Cookievalues.t ref (* The key is the full session name *) *
      
      (* persistent sessions: *)
      ((string                  (* value sent by the browser *) *
        timeout                 (* timeout at the beginning of the request *) *
        float option            (* (server side) expdate 
                                   at the beginning of the request
                                   None = no exp *) *
        Eliommod_sessiongroups.perssessgrp option      (* session group at beginning of request *))
         option
                                (* None = new cookie 
                                   (not sent by the browser) *)
         *
         
         one_persistent_cookie_info session_cookie ref
         (* SCNo_data = the session has been closed
            SCData_session_expired = the cookie has not been found in the table.
            For both of them, ask the browser to remove the cookie.
          *)
      ) Lwt.t Lazy.t
      Http_frame.Cookievalues.t ref


(* non persistent cookies for services *)
type 'a servicecookiestablecontent =
    (string              (* session fullsessname *) *
     'a                  (* session table *) * 
     float option ref    (* expiration date by timeout 
                            (server side) *) *
     timeout ref         (* user timeout *) *
     Eliommod_sessiongroups.sessgrp option ref   (* session group *))

type 'a servicecookiestable = 'a servicecookiestablecontent SessionCookies.t
(* the table contains:
   - the table of services
   - the expiration date (by timeout), changed at each access to the table
     (float option) None -> no expiration
   - the timeout for the user (float option option) None -> see global config
     Some None -> no timeout
   - the group to which belongs the session
 *)

(* non persistent cookies for in memory data *)
type datacookiestablecontent = 
    (string                  (* session fullsessname *) *
     float option ref        (* expiration date by timeout 
                                (server side) *) *
     timeout ref             (* user timeout *) *
     Eliommod_sessiongroups.sessgrp option ref   (* session group *))

type datacookiestable = datacookiestablecontent SessionCookies.t

      
type anon_params_type = int

(********)

exception Eliom_Wrong_parameter (** Service called with wrong parameter names *)
exception Eliom_Link_too_old (** The coservice does not exist any more *)
exception Eliom_Session_expired
exception Eliom_Service_session_expired of (string list)
    (** The service session cookies does not exist any more.
        The string lists are the list of names of expired sessions
     *)
exception Eliom_Typing_Error of (string * exn) list

exception Eliom_duplicate_registration of string
exception Eliom_there_are_unregistered_services of
  (string list * string list list * na_key list)
exception Eliom_function_forbidden_outside_site_loading of string
exception Eliom_page_erasing of string
exception Eliom_error_while_loading_site of string

exception Eliom_404

(*****************************************************************************)
type result_to_send = 
  | EliomResult of Http_frame.result
  | EliomExn of (exn list * cookie list)





(*****************************************************************************)
(*****************************************************************************)
(* The table of dynamic pages for each virtual server, and naservices        *)
(* Each node contains either a list of nodes (case directory)
    or a table of "answers" (functions that will generate the page) *)


type page_table_key =
    {key_state: (internal_state option * internal_state option);
     key_kind: Http_frame.Http_header.http_method}
      (* action: server_params -> page *)

      (* module Page_Table = Map.Make(struct type t = page_table_key 
         let compare = compare end) *)

module String_Table = Map.Make(struct 
  type t = string
  let compare = compare 
end)

module NAserv_Table = Map.Make(struct 
  type t = na_key
  let compare = compare 
end)

type server_params = 
    {sp_ri:request_info;
     sp_si:sess_info;
     sp_sitedata:sitedata (* data for the whole site *);
     sp_cookie_info:tables cookie_info;
     sp_suffix:url_path (* suffix *);
     sp_fullsessname:string option (* the name of the session
                                      to which belong the service
                                      that answered
                                      (if it is a session service) *)}

and page_table = 
    (page_table_key * 
       (((anon_params_type * anon_params_type) (* unique_id *) * 
           (int * (* generation (= number of reloads of sites
                     after which that service has been created) *)
              (int ref option (* max_use *) *
                 (float * float ref) option
                 (* timeout and expiration date for the service *) *
                 (server_params -> result_to_send Lwt.t)
	      ))) list)) list
       (* Here, the url_path is the site directory.
          That is, the directory in which we are when we register
          dynamically the pages.
          Each time we load a page, we change to this directory
          (in case the page registers new pages).
        *)

and naservice_table = 
  | AVide 
  | ATable of 
      (int (* generation (= number of reloads of sites
              after which that service has been created) *) *
       int ref option (* max_use *) *
         (float * float ref) option (* timeout and expiration date *) *
         (server_params -> result_to_send Lwt.t)
      )
        NAserv_Table.t

and dircontent = 
  | Vide
  | Table of direlt ref String_Table.t

and direlt = 
  | Dir of dircontent ref
  | File of page_table ref

and tables = 
    dircontent ref * 
    naservice_table ref *
    (* Information for the GC: *)
    bool ref (* true if dircontent contains services with timeout *) *
    bool ref (* true if naservice_table contains services with timeout *)

and sitedata =
  {site_dir: url_path;
   site_dir_string: string;
   mutable servtimeout: (string * float option) list;
   mutable datatimeout: (string * float option) list;
   mutable perstimeout: (string * float option) list;
   global_services: tables; (* global service table *)
   session_services: tables servicecookiestable; (* cookie table for services *)
   session_data: datacookiestable; (* cookie table for in memory session data *)
   mutable remove_session_data: string -> unit;
   mutable not_bound_in_data_tables: string -> bool;
   mutable exn_handler: server_params -> exn -> result_to_send Lwt.t;
   mutable unregistered_services: url_path list;
   mutable unregistered_na_services: na_key list;
   mutable max_volatile_data_sessions_per_group: int option;
   mutable max_service_sessions_per_group: int option;
   mutable max_persistent_data_sessions_per_group: int option;
 }


(* table cookie -> session table *)
let new_service_cookie_table () : tables servicecookiestable = 
  SessionCookies.create 1000

let new_data_cookie_table () : datacookiestable = 
  SessionCookies.create 1000

let empty_page_table () = []
let empty_naservice_table () = AVide
let empty_dircontent () = Vide
let empty_tables () =
  (ref (empty_dircontent ()), 
   ref (empty_naservice_table ()),
   ref false, (* does not contain services with timeout *)
   ref false (* does not contain na_services with timeout *))
    
let service_tables_are_empty (lr,atr,_,_) = 
  (!lr = Vide && !atr = AVide)

let new_service_session_tables = empty_tables


(*****************************************************************************)
    (** Create server parameters record *)
let make_server_params sitedata all_cookie_info ri suffix si fullsessname
    : server_params =
  {sp_ri=ri;
   sp_si=si;
   sp_sitedata=sitedata;
   sp_cookie_info=all_cookie_info;
   sp_suffix=suffix;
   sp_fullsessname= fullsessname}



(*****************************************************************************)
let eliom_suffix_name = "__eliom_suffix"
let eliom_suffix_internal_name = "__eliom_suffix**"
let naservice_num = "__eliom_na__num"
let naservice_name = "__eliom_na__name"
let get_state_param_name = "__eliom__"
let post_state_param_name = "__eliom_p__"
let datacookiename = "eliomdatasession|" 
let servicecookiename = "eliomservicesession|" 
(* must not be a prefix of the following and vice versa (idem for data) *)
let persistentcookiename = "eliompersistentsession|"
let co_param_prefix = "__co_eliom_"
let na_co_param_prefix = "__na_eliom_"

(*VVV Do not forget to change the version number 
  when the internal format change!!! *)
let persistent_cookie_table_version = "_v2" (* v2 introduces session groups *)
let eliom_persistent_cookie_table = 
  "eliom_persist_cookies"^persistent_cookie_table_version


(******************************************************************)
let make_full_cookie_name a b = a^b

let make_fullsessname ~sp = function
  | None -> sp.sp_sitedata.site_dir_string
  | Some s -> sp.sp_sitedata.site_dir_string^"|"^s
(* Warning: do not change this without modifying Eliomsessions.Admin *)

let make_fullsessname2 site_dir_string = function
  | None -> site_dir_string
  | Some s -> site_dir_string^"|"^s
(* Warning: do not change this without modifying Eliomsessions.Admin *)


(*****************************************************************************)
let rng = Cryptokit.Random.device_rng "/dev/urandom"

let make_new_cookie_value =
    let to_hex = Cryptokit.Hexa.encode () in
    fun () ->

  (* Solution by Dario Teixeira: *)
      let random_part =
        Cryptokit.Random.string rng 20
      and sequential_part = 
        Printf.sprintf "%Lx"  (Int64.bits_of_float (Unix.gettimeofday ()))
      in
      (Cryptokit.transform_string
         to_hex
         (Cryptokit.hash_string (Cryptokit.Hash.sha1 ()) random_part))^ 
      sequential_part

(* 
1) The Digest module in the stdlib uses the MD5 algorithm, which is pretty much considered "broken" both in theory and in practice. Consider using at least SHA1 or RIPEMD160 (yes, I know of some theoretical attacks against these, but for the time being they are considered fairly secure). 
 2) Using Unix.times to shuffle the generation of the second 64-bit pseudo-random number is an interesting solution, but it still feels too much like a hack. Besides, you're still relying too much on OCaml's random number generator, which to my knowledge is not crypto-safe. 
 All and all, have you considered using Xavier Leroy's Cryptokit? It provides implementations for a number of digest algorithms, and also has a crypto-safe RNG (which uses /dev/random in Linux systems). It is easier, safer and perhaps even faster to use it. (The disadvantage is of course another external dependency). 
 The code above would be enough to generate a 224 bit session ID (224 bits because SHA1 produces a 160-bit hash, which is then added to the 64 bits from the system time). If you had complete trust in the random number generator, you could even ommit the SHA1 digest, though I would keep it just in case.

Using Cryptokit.Random.secure_rng -- while perhaps the best RNG available -- has the "small" problem that one might exhaust the entropy sources of the system:
Using Cryptokit.Random.device_rng with /dev/urandom or even Cryptokit.Random.pseudo_rng might be a better choice, since they don't suffer from this problem.

Dario Teixeira
*)


  (* Old solution:
  let c1 = Int64.to_string (Random.int64 Int64.max_int) in
  let c2 = Int64.to_string (Int64.add
                              (Random.int64 Int64.max_int) 
                              (Int64.of_float
                                 ((Unix.times ()).Unix.tms_utime *. 10000.))) 
  in
  (Digest.to_hex (Digest.string (c1^c2)))^
  (Printf.sprintf "%Lx"  (Int64.bits_of_float (Unix.gettimeofday ())))
  *)


(* to be called from outside requests *)
let close_data_session2 sitedata fullsessgrp cookie = 
  try
    SessionCookies.remove sitedata.session_data cookie;
    Eliommod_sessiongroups.Data.remove cookie fullsessgrp;
    sitedata.remove_session_data cookie;
  with Not_found -> ()

let close_data_group sitedata fullsessgrp =
  let cooklist = Eliommod_sessiongroups.Data.find fullsessgrp in
  List.iter (close_data_session2 sitedata None) cooklist;
  Eliommod_sessiongroups.Data.remove_group fullsessgrp

(* to be called during a request *)
let close_data_session ?(close_group = false) ?session_name ~sp () = 
  try
    let fullsessname = make_fullsessname ~sp session_name in
    let (_, cookie_info, _) = sp.sp_cookie_info in
    let (_, ior) = 
      Lazy.force (Http_frame.Cookievalues.find fullsessname !cookie_info) 
    in
    match !ior with
    | SC c ->
        if close_group then
          close_data_group sp.sp_sitedata !(c.dc_session_group)
        else
          close_data_session2 sp.sp_sitedata !(c.dc_session_group) c.dc_value;
        ior := SCNo_data
    | _ -> ()
  with Not_found -> ()



let rec new_data_cookie sitedata fullsessgrp fullsessname table = 
  let c = make_new_cookie_value () in
  try
    ignore (SessionCookies.find table c); (* Actually not needed 
                                             for the cookies we use *)
    new_data_cookie sitedata fullsessgrp fullsessname table
  with Not_found ->
    let usertimeout = ref TGlobal (* See global table *) in
    let serverexp = ref (Some 0.) (* None = never. We'll change it later. *) in
    let fullsessgrpref = ref fullsessgrp in
    SessionCookies.replace (* actually it will add the cookie *)
      table 
      c
      (fullsessname,
       serverexp (* exp on server *),
       usertimeout,
       fullsessgrpref);
    List.iter
      (close_data_session2 sitedata None)
      (Eliommod_sessiongroups.Data.add 
         sitedata.max_volatile_data_sessions_per_group c fullsessgrp);
    (* add returns the list of session to close if
       maxsessionspergroup exceded *)
    {dc_value= c;
     dc_timeout= usertimeout;
     dc_exp= serverexp;
     dc_cookie_exp= ref CENothing (* exp on client - nothing to set *);
     dc_session_group= fullsessgrpref
   }






let close_service_session2 sitedata fullsessgrp cookie = 
  SessionCookies.remove sitedata.session_services cookie;
  Eliommod_sessiongroups.Serv.remove cookie fullsessgrp

let close_service_group sitedata fullsessgrp =
  let cooklist = Eliommod_sessiongroups.Serv.find fullsessgrp in
  List.iter (close_service_session2 sitedata None) cooklist;
  Eliommod_sessiongroups.Serv.remove_group fullsessgrp

let close_service_session ?(close_group = false) ?session_name ~sp () = 
  try
    let fullsessname = make_fullsessname ~sp session_name in
    let (cookie_info, _, _) = sp.sp_cookie_info in
    let (_, ior) = Http_frame.Cookievalues.find fullsessname !cookie_info in
    match !ior with
    | SC c ->
        if close_group then
          close_service_group sp.sp_sitedata !(c.sc_session_group)
        else
          close_service_session2 
            sp.sp_sitedata !(c.sc_session_group) c.sc_value;
        ior := SCNo_data
    | _ -> ()
  with Not_found -> ()

let close_volatile_session ?close_group ?session_name ~sp () =
  close_data_session ?close_group ?session_name ~sp ();
  close_service_session ?close_group ?session_name ~sp ()



let rec new_service_cookie sitedata fullsessgrp fullsessname table = 
  let c = make_new_cookie_value () in
  try
    ignore (SessionCookies.find table c); (* Actually not needed 
                                      for the cookies we use *)
    new_service_cookie sitedata fullsessgrp fullsessname table
  with Not_found ->
    let str = ref (new_service_session_tables ()) in
    let usertimeout = ref TGlobal (* See global table *) in
    let serverexp = ref (Some 0.) (* None = never. We'll change it later. *) in
    let fullsessgrpref = ref fullsessgrp in
    SessionCookies.replace (* actually it will add the cookie *)
      table 
      c
      (fullsessname,
       !str, 
       serverexp (* exp on server *),
       usertimeout,
       fullsessgrpref);
    List.iter
      (close_service_session2 sitedata None)
      (Eliommod_sessiongroups.Serv.add 
         sitedata.max_service_sessions_per_group c fullsessgrp);
    (* add returns the list of session to close if
       maxsessionspergroup exceded *)
    {sc_value= c;
     sc_table= str;
     sc_timeout= usertimeout;
     sc_exp= serverexp;
     sc_cookie_exp= ref CENothing (* exp on client - 
                                     nothing to set *);
     sc_session_group= fullsessgrpref
   }


let find_or_create_data_cookie ?session_group ?session_name ~sp () = 
  (* If the cookie does not exist, create it.
     Returns the cookie info for the cookie *)
  let fullsessname = make_fullsessname ~sp session_name in
  let fullsessgrp = 
    Eliommod_sessiongroups.make_full_group_name
      sp.sp_sitedata.site_dir_string session_group 
  in
  let (_, cookie_info, _) = sp.sp_cookie_info in
  try
    let (old, ior) = 
      Lazy.force (Http_frame.Cookievalues.find fullsessname !cookie_info) 
    in
    match !ior with
    | SCData_session_expired 
    | SCNo_data -> 
        let v = 
          new_data_cookie
            sp.sp_sitedata fullsessgrp fullsessname sp.sp_sitedata.session_data
        in
        ior := SC v;
        v
    | SC v -> v;
  with Not_found -> 
    let v = 
      new_data_cookie
        sp.sp_sitedata fullsessgrp fullsessname sp.sp_sitedata.session_data 
    in
    cookie_info := 
      Http_frame.Cookievalues.add
        fullsessname
        (Lazy.lazy_from_val (None, ref (SC v)))
        !cookie_info;
    v
        
let find_data_cookie_only ?session_name ~sp () = 
  (* If the cookie does not exist, do not create it, raise Not_found.
     Returns the cookie info for the cookie *)
  let fullsessname = make_fullsessname ~sp session_name in
  let (_, cookie_info, _) = sp.sp_cookie_info in
  let (_, ior) = 
    Lazy.force (Http_frame.Cookievalues.find fullsessname !cookie_info) 
  in
  match !ior with
  | SCNo_data -> raise Not_found
  | SCData_session_expired -> raise Eliom_Session_expired
  | SC v -> v

let find_or_create_service_cookie ?session_group ?session_name ~sp () = 
  (* If the cookie does not exist, create it.
     Returns the cookie info for the cookie *)
  let fullsessname = make_fullsessname ~sp session_name in
  let fullsessgrp = 
    Eliommod_sessiongroups.make_full_group_name
      sp.sp_sitedata.site_dir_string session_group 
  in
  let (cookie_info, _, _) = sp.sp_cookie_info in
  try
    let (old, ior) = Http_frame.Cookievalues.find fullsessname !cookie_info in
    match !ior with
    | SCData_session_expired 
    | SCNo_data -> 
        let v = 
          new_service_cookie
            sp.sp_sitedata
            fullsessgrp fullsessname sp.sp_sitedata.session_services 
        in
        ior := SC v;
        v
    | SC v -> v
  with Not_found -> 
    let v = 
      new_service_cookie
        sp.sp_sitedata fullsessgrp fullsessname sp.sp_sitedata.session_services 
    in
    cookie_info :=
      Http_frame.Cookievalues.add
        fullsessname
        (None, ref (SC v))
        !cookie_info;
    v


let find_service_cookie_only ?session_name ~sp () = 
  (* If the cookie does not exist, do not create it, raise Not_found.
     Returns the cookie info for the cookie *)
  let fullsessname = make_fullsessname ~sp session_name in
  let (cookie_info, _, _) = sp.sp_cookie_info in
  let (_, ior) = Http_frame.Cookievalues.find fullsessname !cookie_info in
  match !ior with
  | SCNo_data -> raise Not_found
  | SCData_session_expired -> raise Eliom_Session_expired
  | SC v -> v





(*****************************************************************************)
(** Persistent sessions: *)

(* Each persistent table created by sites correspond to a file on the disk.
   We save the names of the currently opened tables in this table: *)

module Perstables = 
  struct 
    let empty = []
    let add v t = v::t
    let fold = List.fold_left
  end

let perstables = ref Perstables.empty

let create_persistent_table name =
  perstables := Perstables.add name !perstables;
  Ocsipersist.open_table name

let persistent_cookies_table = 
  create_persistent_table eliom_persistent_cookie_table
(* Another tables, containing the session info for each cookie *)
(* the table contains:
   - the expiration date (by timeout), changed at each access to the table
     (float option) None -> no expiration
   - the timeout for the user (float option option) None -> see global config
     Some None -> no timeout
 *)

(** removes the entry from all opened tables *)
let remove_from_all_persistent_tables key =
  Perstables.fold (* could be replaced by a parallel map *)
    (fun thr t -> thr >>= fun () ->
      Ocsipersist.remove (Ocsipersist.open_table t) key >>= Lwt_unix.yield)
    (return ())
    !perstables

let number_of_persistent_tables () =
  List.length !perstables

let number_of_persistent_table_elements () = 
  List.fold_left 
    (fun thr t -> 
      thr >>= 
      (fun l -> Ocsipersist.length (Ocsipersist.open_table t) >>=
        (fun e -> return ((t, e)::l)))) (return []) !perstables

(* close a persistent session by cookie *)
let close_persistent_session2 fullsessgrp cookie = 
  catch
    (fun () ->
      Ocsipersist.remove persistent_cookies_table cookie >>= fun () ->
      Eliommod_sessiongroups.Pers.remove cookie fullsessgrp >>= fun () ->
      remove_from_all_persistent_tables cookie
    )
    (function
      | Not_found -> return ()
      | e -> fail e)

let close_persistent_group fullsessgrp =
(*VVV VERIFY concurrent access *)
  Eliommod_sessiongroups.Pers.find fullsessgrp >>= fun cooklist ->
  Lwt_util.iter (close_persistent_session2 None) cooklist
  >>= fun () ->
  Eliommod_sessiongroups.Pers.remove_group fullsessgrp

(* close current persistent session *)
let close_persistent_session ?(close_group = false) ?session_name ~sp () = 
  catch
    (fun () ->
      let fullsessname = make_fullsessname ~sp session_name in
      let (_, _, cookie_info) = sp.sp_cookie_info in
      Lazy.force (Http_frame.Cookievalues.find fullsessname !cookie_info)
      >>= fun (_, ior) ->
      match !ior with
      | SC c ->
          (if close_group then
            close_persistent_group !(c.pc_session_group)
          else
            close_persistent_session2 !(c.pc_session_group) c.pc_value)
          >>= fun () ->
          ior := SCNo_data;
          return ()
      | _ -> return ()
    )
    (function
      | Not_found -> return ()
      | e -> fail e)


let rec new_persistent_cookie sitedata fullsessgrp fullsessname = 
  let c = make_new_cookie_value () in
(*  catch
    (fun () ->
      Ocsipersist.find persistent_cookies_table c >>= (* useless *)
      (fun _ -> new_persistent_cookie sitedata fullsessgrp fullsessname))(* never succeeds *)
    (function
      | Not_found -> 
          begin *)
            let usertimeout = ref TGlobal (* See global table *) in
            Ocsipersist.add 
              persistent_cookies_table c 
              (fullsessname,
               Some 0. (* exp on server - We'll change it later *),
               TGlobal (* timeout - see global config *),
               fullsessgrp)
            >>= fun () -> 
            Eliommod_sessiongroups.Pers.add
              sitedata.max_persistent_data_sessions_per_group
              c fullsessgrp >>= fun l ->
            Lwt_util.iter 
              (close_persistent_session2 None) l >>= fun () ->
            return {pc_value= c;
                    pc_timeout= usertimeout;
                    pc_cookie_exp= ref CENothing (* exp on client *);
                    pc_session_group= ref fullsessgrp
                  }
(*          end
      | e -> fail e) *)


let find_or_create_persistent_cookie ?session_group ?session_name ~sp () =
  (* if it exists, do not create it, but returns its value *)
  let fullsessname = make_fullsessname ~sp session_name in
  let fullsessgrp = 
    Eliommod_sessiongroups.make_persistent_full_group_name
      sp.sp_sitedata.site_dir_string session_group
  in
  let (_, _, cookie_info) = sp.sp_cookie_info in
  catch
    (fun () ->
      Lazy.force (Http_frame.Cookievalues.find fullsessname !cookie_info)
      >>= fun (old, ior) ->
      match !ior with
      | SCData_session_expired 
      | SCNo_data -> 
          new_persistent_cookie 
            sp.sp_sitedata fullsessgrp fullsessname >>= fun v ->
          ior := SC v;
          return v
      | SC v -> return v)
    (function
      | Not_found -> 
          new_persistent_cookie
            sp.sp_sitedata fullsessgrp fullsessname >>= fun v ->
          cookie_info := 
            Http_frame.Cookievalues.add
              fullsessname
              (Lazy.lazy_from_val (return (None, ref (SC v))))
              !cookie_info;
          return v
      | e -> fail e)



let find_persistent_cookie_only ?session_name ~sp () =
  (* If the cookie does not exist, do not create it, raise Not_found.
     Returns the cookie info for the cookie *)
  let fullsessname = make_fullsessname ~sp session_name in
  let (_, _, cookie_info) = sp.sp_cookie_info in
  Lazy.force (Http_frame.Cookievalues.find fullsessname !cookie_info)
  >>= fun (_, ior) ->
  match !ior with
  | SCNo_data -> raise Not_found
  | SCData_session_expired -> raise Eliom_Session_expired
  | SC v -> return v









(*****************************************************************************)
let servicesessiongcfrequency = ref (Some 3600.)
let datasessiongcfrequency = ref (Some 3600.)
let persistentsessiongcfrequency = ref (Some 86400.)
let set_servicesessiongcfrequency i = servicesessiongcfrequency := i
let set_datasessiongcfrequency i = datasessiongcfrequency := i
let get_servicesessiongcfrequency () = !servicesessiongcfrequency
let get_datasessiongcfrequency () = !datasessiongcfrequency
let set_persistentsessiongcfrequency i = persistentsessiongcfrequency := i
let get_persistentsessiongcfrequency () = !persistentsessiongcfrequency


(* garbage collection of timeouted sessions *)
let rec gc_timeouted_services now t = 
  let rec aux k direltr thr = 
    thr >>=
    (fun table ->
      match !direltr with
      | Dir r -> gc_timeouted_services now r >>= 
          (fun () -> match !r with
          | Vide -> return (String_Table.remove k table)
          | Table t -> return table)
      | File ptr ->
          List.fold_right
            (fun (ptk, l) foll -> 
              foll >>=
              (fun foll ->
                let newl =
                  List.fold_right
                    (fun ((i, (_, (_, expdate, _))) as a) foll -> 
                      match expdate with
                      | Some (_, e) when !e < now -> foll
                      | _ -> a::foll
                    )
                    l
                    []
                in
                Lwt_unix.yield () >>=
                (fun () ->
                  match newl with
                  | [] -> return foll
                  | _ -> return ((ptk, newl)::foll))
              )
            )
            !ptr
            (return []) >>=
          (function
            | [] -> return (String_Table.remove k table)
            | r -> ptr := r; return table)
    )
  in
  match !t with
  | Vide -> return ()
  | Table r -> (String_Table.fold aux r (return r)) >>=
      (fun table -> 
        if String_Table.is_empty table
        then begin t := Vide; return () end
        else begin t := Table table; return () end)

let gc_timeouted_naservices now tr = 
  match !tr with
  | AVide -> return ()
  | ATable t -> 
      NAserv_Table.fold
        (fun k (_, _, expdate, _) thr -> 
          thr >>=
          (fun table -> 
            Lwt_unix.yield () >>=
            (fun () ->
              match expdate with
              | Some (_, e) when !e < now -> 
                  return (NAserv_Table.remove k table)
              | _ -> return table)
          ))
        t
        (return t) >>=
      (fun t -> 
        if NAserv_Table.is_empty t
        then tr := AVide
        else tr := ATable t; 
        return ())

        

(* This is a thread that will work for example every hour. *)
let service_session_gc sitedata =
  let (servicetable,
       naservicetable, 
       contains_services_with_timeout, 
       contains_naservices_with_timeout) = sitedata.global_services
  in
  match get_servicesessiongcfrequency () with
  | None -> () (* No garbage collection *)
  | Some t ->
      let rec f () = 
        Lwt_unix.sleep t >>= 
        (fun () ->
          let service_cookie_table = sitedata.session_services in
          let now = Unix.time () in
          Messages.debug2 "--Eliom: GC of service sessions";
          (* public continuation tables: *)
          (if !contains_services_with_timeout
          then gc_timeouted_services now servicetable
          else return ()) >>=
          (fun () -> if !contains_naservices_with_timeout
          then gc_timeouted_naservices now naservicetable
          else return ()) >>=
          (* private continuation tables: *)
          (fun () ->
            (* private continuation tables: *)
            SessionCookies.fold
              (fun k (sessname,
                      ((servicetable,
                        naservicetable, 
                        contains_services_with_timeout, 
                        contains_naservices_with_timeout) as tables),
                      exp, 
                      _,
                      session_group_ref) thr -> 
                        thr >>= fun () ->
                          (match !exp with
                          | Some exp when exp < now -> 
                              close_service_session2 
                                sitedata !session_group_ref k;
                              return ()
                          | _ -> 
                              (if !contains_services_with_timeout
                              then gc_timeouted_services now servicetable
                              else return ()) >>=
                              (fun () -> if !contains_naservices_with_timeout
                              then gc_timeouted_naservices now naservicetable
                              else return ()) >>=
                              (fun () ->
                                if service_tables_are_empty tables
                                then 
                                  close_service_session2 
                                    sitedata !session_group_ref k;
                                return ()
                              )
                          )
                            >>= Lwt_unix.yield
              )
              service_cookie_table
              (return ()))
        )
          >>=
        f
      in ignore (f ())
      
(* This is a thread that will work for example every hour. *)
let data_session_gc sitedata =
  match get_datasessiongcfrequency () with
  | None -> () (* No garbage collection *)
  | Some t ->
      let rec f () = 
        Lwt_unix.sleep t >>= fun () ->
        let data_cookie_table = sitedata.session_data in
        let not_bound_in_data_tables = sitedata.not_bound_in_data_tables in
        let now = Unix.time () in
        Messages.debug2 "--Eliom: GC of session data";
        (* private continuation tables: *)
        SessionCookies.fold
          (fun k (sessname, exp, _, session_group_ref) thr -> 
            thr >>= fun () ->
              (match !exp with
              | Some exp when exp < now -> 
                  close_data_session2 sitedata !session_group_ref k;
                  return ()
              | _ -> 
                  if !session_group_ref = None && not_bound_in_data_tables k
                  then 
                    SessionCookies.remove data_cookie_table k;
                  return ()
              )
                >>= Lwt_unix.yield
          )
          data_cookie_table
          (return ())
          >>=
        f
      in ignore (f ())
      
(* garbage collection of timeouted persistent sessions *)
(* This is a thread that will work every hour/day *)
let persistent_session_gc () =
  match get_persistentsessiongcfrequency () with
  | None -> () (* No garbage collection *)
  | Some t ->
      let rec f () = 
        Lwt_unix.sleep t >>= 
        (fun () ->
          let now = Unix.time () in
          Messages.debug2 "--Eliom: GC of persistent sessions";
          (Ocsipersist.iter_table
             (fun k (_, exp, _, session_group) -> 
               (match exp with
               | Some exp when exp < now -> 
(*VVV ? *)
                   close_persistent_session2 session_group k
                     (*WAS: remove_from_all_persistent_tables k *)
               | _ -> return ())
             )
             persistent_cookies_table))
          >>=
        f
      in ignore (f ())
      

(*****************************************************************************)
(* Exception handler for the site                                            *)

let def_handler sp e = fail e

let handle_site_exn exn (ri, si, _, aci) sitedata =
  sitedata.exn_handler (make_server_params sitedata aci ri [] si None) exn >>=
  (fun r -> return r)



(****************************************************************************)
let default_max_sessions_per_group = Some 20

let new_sitedata =
  (* We want to keep the old site data even if we reload the server *)
  (* To do that, we keep the site data in a table *)
  let module S = Hashtbl.Make(struct 
                                type t = url_path
                                let equal = (=)
                                let hash = Hashtbl.hash
                              end)
  in
  let t = S.create 5 in
  fun site_dir ->
    try
      S.find t site_dir
    with 
      | Not_found ->
          let sitedata =
            {servtimeout = [];
             datatimeout = [];
             perstimeout = [];
             site_dir = site_dir;
             site_dir_string = Ocsigen_lib.string_of_url_path site_dir;
             global_services = empty_tables ();
             session_services = new_service_cookie_table ();
             session_data = new_data_cookie_table ();
             remove_session_data = (fun cookie -> ());
             not_bound_in_data_tables = (fun cookie -> true);
             exn_handler = def_handler;
             unregistered_services = [];
             unregistered_na_services = [];
             max_service_sessions_per_group = 
                default_max_sessions_per_group;
             max_volatile_data_sessions_per_group = 
                default_max_sessions_per_group;
             max_persistent_data_sessions_per_group = 
                default_max_sessions_per_group;
            }
          in
          service_session_gc sitedata;
          data_session_gc sitedata;
          S.add t site_dir sitedata;
          sitedata

(*****************************************************************************)
(* The current registration directory *)
let absolute_change_sitedata, 
  get_current_sitedata, 
  end_current_sitedata =
  let f2 = ref [] in
  let popf2 () =
    match !f2 with
    | _::t -> f2 := t
    | [] -> f2 := [] 
  in
  ((fun sitedata -> f2 := sitedata::!f2) (* absolute_change_sitedata *),
   (fun () ->  match !f2 with
   | [] -> raise (Eliom_function_forbidden_outside_site_loading
                    "get_current_sitedata") 
   | sd::_ -> sd) (* get_current_sitedata *),
   (fun () -> popf2 ()) (* end_current_sitedata *))
(* Warning: these functions are used only during the initialisation
   phase, which is not threaded ... That's why it works, but ...
   it is not really clean ... public registration relies on this
   directory (defined for each site in the config file) 
 *)

let add_unregistered sitedata a = 
  sitedata.unregistered_services <- a::sitedata.unregistered_services

let add_unregistered_na sitedata a = 
  sitedata.unregistered_na_services <- a::sitedata.unregistered_na_services

let remove_unregistered sitedata a = 
  sitedata.unregistered_services <- 
    list_remove_first_if_any a sitedata.unregistered_services

let remove_unregistered_na sitedata a = 
  sitedata.unregistered_na_services <- 
    list_remove_first_if_any a sitedata.unregistered_na_services

let verify_all_registered sitedata =
  match sitedata.unregistered_services, sitedata.unregistered_na_services with
  | [], [] -> () 
  | l1, l2 -> 
      raise (Eliom_there_are_unregistered_services (sitedata.site_dir, l1, l2))


let during_eliom_module_loading, 
  begin_load_eliom_module, 
  end_load_eliom_module =
  let during_eliom_module_loading_ = ref false in
  ((fun () -> !during_eliom_module_loading_),
   (fun () -> during_eliom_module_loading_ := true),
   (fun () -> during_eliom_module_loading_ := false))

let global_register_allowed () =
  if (during_initialisation ()) && (during_eliom_module_loading ())
  then Some get_current_sitedata
  else None



(*****************************************************************************)
(** session data *)

let counttableelements = ref []
(* Here only for exploration functions *)

let create_volatile_table, create_volatile_table_during_session =
  let aux sitedata =
    let t = SessionCookies.create 1000 in
    let old_remove_session_data = sitedata.remove_session_data in
    sitedata.remove_session_data <-
      (fun cookie ->
        old_remove_session_data cookie;
        SessionCookies.remove t cookie
      );
    let old_not_bound_in_data_tables = sitedata.not_bound_in_data_tables in
    sitedata.not_bound_in_data_tables <-
      (fun cookie ->
        old_not_bound_in_data_tables cookie &&
        not (SessionCookies.mem t cookie)
      );
    counttableelements := 
      (fun () -> SessionCookies.length t)::!counttableelements;
    t
  in
  ((fun () ->
    let sitedata = get_current_sitedata () in
    aux sitedata),
   (fun sp -> aux sp.sp_sitedata))



    
(*****************************************************************************)
(* Split parameter list, removing those whose name starts with pref *)
let split_prefix_param pref l =
  let len = String.length pref in
  List.partition (fun (n,_) -> 
    try 
      (String.sub n 0 len) = pref 
    with Invalid_argument _ -> false) l


(*****************************************************************************)
(* cookie manipulation *)
let getcookies cookiename cookies = 
  let length = String.length cookiename in
  let last = length - 1 in
  Http_frame.Cookievalues.fold
    (fun name value beg -> 
      if string_first_diff cookiename name 0 last = length
      then
        Http_frame.Cookievalues.add
          (String.sub name length ((String.length name) - length))
          value
          beg
      else beg
    )
    cookies
    Http_frame.Cookievalues.empty


(** look in table to find if the session cookies sent by the browser
   correspond to existing (and not closed) sessions *)
let get_cookie_info now sitedata
    service_cookies
    data_cookies
    persistent_cookies
    : 'a cookie_info * 'b list =
  
  (* get info about service session cookies: *)
  let (servoktable, servfailedlist) =
    Http_frame.Cookievalues.fold
      (fun name value (oktable, failedlist) ->
        try 
          let fullsessname, ta, expref, timeout_ref, sessgrpref = 
            SessionCookies.find sitedata.session_services value
          in
          Eliommod_sessiongroups.Serv.up value !sessgrpref;
          match !expref with
          | Some t when t < now -> 
              (* session expired by timeout *)
              SessionCookies.remove sitedata.session_services value;
              ((Http_frame.Cookievalues.add
                  name
                  (Some value          (* value sent by the browser *),
                   ref SCData_session_expired (* ask the browser 
                                                 to remove the cookie *))
                  oktable),
               name::failedlist)
          | _ -> ((Http_frame.Cookievalues.add 
                     name
                     (Some value        (* value sent by the browser *),
                      ref 
                        (SC
                           {sc_value= value  (* value *);
                            sc_table= ref ta (* the table of session services *);
                            sc_timeout= timeout_ref (* user timeout ref *);
                            sc_exp= expref  (* expiration date (server side) *);
                            sc_cookie_exp= ref CENothing
                              (* cookie expiration date to send
                                 to the browser *);
                            sc_session_group= sessgrpref}))
                     oktable),
                  failedlist)
        with Not_found ->
          ((Http_frame.Cookievalues.add
              name
              (Some value                 (* value sent by the browser *),
               ref SCData_session_expired (* ask the browser 
                                             to remove the cookie *))
              oktable), 
           name::failedlist)
      )
      service_cookies
      (Http_frame.Cookievalues.empty, [])
  in

  (* get info about "in memory" data session cookies: *)
  let dataoktable =
    Http_frame.Cookievalues.map
      (fun value ->
        lazy
          (try
            let fullsessname, expref, timeout_ref, sessgrpref = 
              SessionCookies.find sitedata.session_data value
            in
            Eliommod_sessiongroups.Serv.up value !sessgrpref;
            match !expref with
            | Some t when t < now -> 
                (* session expired by timeout *)
                sitedata.remove_session_data value;
                SessionCookies.remove sitedata.session_data value;
                (Some value                 (* value sent by the browser *),
                 ref SCData_session_expired (* ask the browser 
                                               to remove the cookie *))
            | _ ->
                (Some value        (* value sent by the browser *),
                 ref 
                   (SC
                      {dc_value= value       (* value *);
                       dc_timeout= timeout_ref (* user timeout ref *);
                       dc_exp= expref      (* expiration date (server side) *);
                       dc_cookie_exp= ref CENothing
                         (* cookie expiration date to send
                            to the browser *);
                       dc_session_group= sessgrpref}
                   )
                )
          with Not_found ->
            (Some value                  (* value sent by the browser *),
             ref SCData_session_expired  (* ask the browser 
                                            to remove the cookie *))))
      data_cookies
  in
  
  
  (* *** get info about persistent session cookies: *)
  let persoktable =
    Http_frame.Cookievalues.map
      (fun value ->
        lazy
          (catch
             (fun () ->
               Ocsipersist.find persistent_cookies_table value >>=
               fun (fullsessname, persexp, perstimeout, sessgrp) ->
                 
                 Eliommod_sessiongroups.Pers.up value sessgrp >>= fun () ->
                 match persexp with
                 | Some t when t < now -> 
                     (* session expired by timeout *)
                     remove_from_all_persistent_tables value >>= fun () -> 
                       return 
                         (Some (value         (* value at the beginning
                                                 of the request *),
                                perstimeout   (* user persistent timeout
                                                 at the beginning 
                                                 of the request *),
                                persexp       (* expiration date (server)
                                                 at the beginning 
                                                 of the request *),
                                sessgrp       (* session group at beginning *)),
                          ref SCData_session_expired (* ask the browser to
                                                        remove the cookie *))
                 | _ -> 
                     return
                       (Some (value        (* value at the beginning
                                              of the request *),
                              perstimeout  (* user persistent timeout
                                              at the beginning 
                                              of the request *),
                              persexp      (* expiration date (server)
                                              at the beginning 
                                              of the request *),
                              sessgrp      (* session group at beginning *)),
                        (ref 
                           (SC
                              {pc_value= value           (* value *);
                               pc_timeout= ref perstimeout
                                 (* user persistent timeout ref *);
                               pc_cookie_exp= ref CENothing
                                 (* persistent cookie expiration
                                    date ref to send to the
                                    browser *);
                               pc_session_group= ref sessgrp
                             })))
             )
             (function
               | Not_found -> 
                   return
                     (Some (value         (* value at the beginning
                                             of the request *),
                            TGlobal       (* user persistent timeout
                                             at the beginning 
                                             of the request *),
                            Some 0.       (* expiration date (server)
                                             at the beginning 
                                             of the request *),
                            None          (* session group at beginning *)),
                      ref SCData_session_expired   (* ask the browser 
                                                      to remove the cookie *))
               | e -> fail e)
          )
      )
      persistent_cookies (* the persistent cookies sent by the request *)
      
  in
  ((ref servoktable, ref dataoktable, ref persoktable), 
   servfailedlist)
      
(*****************************************************************************)
let change_request_info ri charset previous_extension_err =
  force ri.ri_post_params >>=
  (fun post_params -> 
    let get_params = force ri.ri_get_params in
    let get_params0 = get_params in
    let post_params0 = post_params in
    let data_cookies = getcookies datacookiename (force ri.ri_cookies) in
    let service_cookies = getcookies servicecookiename (force ri.ri_cookies) in
    let persistent_cookies = 
      getcookies persistentcookiename (force ri.ri_cookies)
    in
    let naservice_info, 
      (get_state, post_state),
      (get_params, other_get_params), 
      post_params =
      let post_naservice_name, na_post_params = 
        try
          let n, pp =
            list_assoc_remove naservice_num post_params
          in (Na_post' n, pp)
        with Not_found ->
          try
            let n, pp =
              list_assoc_remove naservice_name post_params
            in (Na_post_ n, pp)
          with Not_found -> (Na_no, [])
      in
      match post_naservice_name with
        | Na_post_ _
        | Na_post' _ -> (* POST non attached coservice *)
            (post_naservice_name,
             (None, None), 
             ([], get_params),
             na_post_params)
        | _ ->
            let get_naservice_name, (na_get_params, other_get_params) = 
              try
                let n, gp =
                  list_assoc_remove naservice_num get_params
                in (Na_get' n, (split_prefix_param na_co_param_prefix gp))
              with Not_found ->
                try
                  let n, gp =
                    list_assoc_remove naservice_name get_params
                  in (Na_get_ n, (split_prefix_param na_co_param_prefix gp))
                with Not_found -> (Na_no, ([], get_params))
            in
            match get_naservice_name with
              | Na_get_ _
              | Na_get' _ -> (* GET non attached coservice *)
                  (get_naservice_name,
                   (None, None), 
                   (na_get_params, other_get_params), 
                   [])
                    (* Not possible to have POST parameters 
                       without naservice_num
                       if there is a GET naservice_num
                    *)
              | _ ->
                  let post_state, post_params = 
                    try 
                      let s, pp =
                        list_assoc_remove post_state_param_name post_params
                      in (Some s, pp)
                    with 
                        Not_found -> (None, post_params)
                  in
                  let get_state, (get_params, other_get_params) = 
                    try 
                      let s, gp =
                        list_assoc_remove get_state_param_name get_params
                      in ((Some s), 
                          (split_prefix_param co_param_prefix gp))
                    with Not_found -> (None, (get_params, []))
                  in 
                  (Na_no, 
                   (get_state, post_state), 
                   (get_params, other_get_params), 
                   post_params)
    in

    return 
      ({ri with 
        ri_method = 
        (if ri.ri_method = Http_frame.Http_header.HEAD
        then Http_frame.Http_header.GET
        else ri.ri_method);
        ri_get_params = lazy get_params; 
        ri_post_params = lazy (return post_params)},
       {si_service_session_cookies= service_cookies;
        si_data_session_cookies= data_cookies;
        si_persistent_session_cookies= persistent_cookies;
        si_nonatt_info= naservice_info;
        si_state_info= (get_state, post_state);
        si_other_get_params= other_get_params;
        si_all_get_params= get_params0;
        si_all_post_params= post_params0;
        si_config_file_charset= charset;
        si_previous_extension_error= previous_extension_err}))



(*****************************************************************************)
(* Session service table *)
(** We associate to each service a function server_params -> page *)

type ('a, 'b) foundornot = Found of 'a | Notfound of 'b

let find_page_table 
    now
    (pagetableref : page_table ref)
    fullsessname
    sitedata
    all_cookie_info
    ri
    urlsuffix
    k
    si
    = 
  let sp = 
    make_server_params sitedata all_cookie_info ri urlsuffix si fullsessname 
  in
  let rec aux toremove = function
    | [] -> Lwt.return ((Notfound Eliom_Wrong_parameter), [])
    | (((_, (_, (max_use, expdate, funct))) as a)::l) ->
        match expdate with
        | Some (_, e) when !e < now ->
            (* Service expired. Removing it. *)
            Messages.debug2 "--Eliom: Service expired. I'm removing it";
            aux toremove l >>= 
            (fun (r, toremove) -> Lwt.return (r, a::toremove))
        | _ ->
            catch 
              (fun () ->
                Messages.debug2 "--Eliom: I'm trying a service";
                funct sp
                  >>=
                (* warning: the list ll may change during funct
                   if funct register something on the same URL!! *)
                (fun p -> 
                  Messages.debug2
                    "--Eliom: Page found and generated successfully";

                  (* We update the expiration date *)
                  (match expdate with
                  | Some (timeout, e) -> e := timeout +. now
                  | None -> ());
                  let newtoremove =
                    (match max_use with
                    | Some r -> 
                        if !r = 1
                        then a::toremove
                        else (r := !r - 1; toremove)
                    | _ -> toremove)
                  in
                  Lwt.return (Found p,
                               newtoremove)))
              (function
                | Eliom_Wrong_parameter -> 
                    aux toremove l >>= 
                    (fun (r, toremove) -> Lwt.return (r, toremove))
                | e -> Lwt.return ((Notfound e), toremove))
  in 
  (catch 
     (fun () -> return (List.assoc k !pagetableref))
     (function Not_found -> fail Eliom_404 | e -> fail e)) >>=
  aux [] >>=
  (fun (r, toremove) -> 
    let list, newptr = list_assoc_remove k !pagetableref in
    (* We do it once again because it may have changed! *)
    let newlist = 
      List.fold_left (fun l a -> list_remove_first_if_any_q a l) list toremove 
    in
    (if newlist = []
    then pagetableref := newptr
    else pagetableref := (k, newlist)::newptr);
    match r with
    | Found r -> Lwt.return r
    | Notfound e -> fail e)


let rec insert_as_last_of_generation generation x = function
  | [] -> [x]
  | ((_, (g, _))::l) as ll when g < generation -> x::ll
  | a::l -> a::(insert_as_last_of_generation generation x l)



let add_page_table duringsession url_act t (key, (id, va)) = 
  (* Duplicate registration forbidden in global table with same generation *)
  let generation = Extensions.get_numberofreloads () in
  let v = (id, (generation, va)) in
  try
    let l, newt = list_assoc_remove key t in
    try
      if key.key_state = (None, None)
      then begin
(********* Vérifier ici qu'il n'y a pas qqchose similaire déjà enregistré ?! *)
        let (oldgen, n), oldl = list_assoc_remove id l in
        if not duringsession && (generation = oldgen)
        then
          raise (Eliom_duplicate_registration (string_of_url_path url_act))
        else (key, (insert_as_last_of_generation generation v oldl))::newt 
      end
      else (key, (insert_as_last_of_generation generation v l))::newt
(********* et ici on ne vérifie pas s'il y a déjà l'unique_id ? à rev 20070712 *)
    with Not_found -> 
      (key, (insert_as_last_of_generation generation v l))::newt
  with Not_found -> (key, [v])::t

let add_dircontent dc (key, elt) =
  match dc with
  | Vide -> Table (String_Table.add key elt String_Table.empty)
  | Table t -> Table (String_Table.add key elt t)

let find_dircontent dc k =
  match dc with
  | Vide -> raise Not_found
  | Table t -> String_Table.find k t

let add_naservice_table at (key, elt) = 
  match at with
  | AVide -> ATable (NAserv_Table.add key elt NAserv_Table.empty)
  | ATable t -> ATable (NAserv_Table.add key elt t)

let find_naservice_table at k = 
  match at with
  | AVide -> raise Not_found
  | ATable t -> NAserv_Table.find k t

let remove_naservice_table at k = 
  match at with
  | AVide -> AVide
  | ATable t -> ATable (NAserv_Table.remove k t)

let add_naservice 
    (_, naservicetableref, _, containstimeouts) duringsession name 
    (max_use, expdate, naservice) =
  let generation = Extensions.get_numberofreloads () in
  (if not duringsession
  then
    try
      let (g, _, _, _) = find_naservice_table !naservicetableref name in
      if g = generation then
        match name with
        | Na_no | Na_get' _ | Na_post' _ ->
            raise (Eliom_duplicate_registration "<non-attached coservice>")
        | Na_get_ n ->
            raise (Eliom_duplicate_registration ("GET non-attached service "^n))
        | Na_post_ n ->
            raise (Eliom_duplicate_registration
                     ("POST non-attached service "^n))
    with Not_found -> ());

  (match expdate with
  | Some _ -> containstimeouts := true
  | _ -> ());
  
  naservicetableref :=
    add_naservice_table !naservicetableref
      (name, (generation, max_use, expdate, naservice))

let remove_naservice (_, atr, _, _) name =
  atr := remove_naservice_table !atr name

let find_naservice now ((_, atr, _, _) as str) name =
  let ((_, _, expdate, _) as p) = find_naservice_table !atr name in
  match expdate with
  | Some (_, e) when !e < now ->
      (* Service expired. Removing it. *)
      Messages.debug2 "--Eliom: Non attached service expired. I'm removing it";
      remove_naservice str name;
      raise Not_found
  | _ -> p


let add_service 
    (dircontentref, _, containstimeouts, _)
    duringsession
    url_act
    (page_table_key, 
     ((unique_id1, unique_id2), max_use, expdate, action)) =

  let aux search dircontentref a l =
    try 
      let direltref = find_dircontent !dircontentref a in
      match !direltref with
      | Dir dcr -> search dcr l
      | File ptr -> raise (Eliom_page_erasing a)
            (* Messages.warning ("Eliom page registration: Page "^
               a^" has been replaced by a directory");
               let newdcr = ref (empty_dircontent ()) in
               (direltref := Dir newdcr;
               search newdcr l) *)
    with
    | Not_found -> 
        let newdcr = ref (empty_dircontent ()) in
        (dircontentref := 
          add_dircontent !dircontentref (a, ref (Dir newdcr));
         search newdcr l)
  in 

  let rec search_page_table_ref dircontentref = function
    | [] | [""] -> search_page_table_ref dircontentref [defaultpagename]
    | [a] -> 
        (try 
          let direltref = find_dircontent !dircontentref a in
          (match !direltref with
          | Dir _ -> raise (Eliom_page_erasing a)
                (* Messages.warning ("Eliom page registration: Directory "^
                   a^" has been replaced by a page");
                   let newpagetableref = ref (empty_page_table ()) in
                   (direltref := File newpagetableref;
                   newpagetableref) *)
          | File ptr -> ptr)
        with
        | Not_found ->
            let newpagetableref = ref (empty_page_table ()) in
            (dircontentref := 
              add_dircontent !dircontentref (a, ref (File newpagetableref));
             newpagetableref))
    | ""::l -> search_page_table_ref dircontentref l
    | a::l -> aux search_page_table_ref dircontentref a l
          (* and search_dircontentref dircontentref = function
             [] -> dircontentref
             | ""::l -> search_dircontentref dircontentref l
             | a::l -> aux search_dircontentref a l *)
  in

  (match expdate with
  | Some _ -> containstimeouts := true
  | _ -> ());

  let content = (page_table_key,
                 ((unique_id1, unique_id2), 
                  (max_use, expdate, action))) in

  let page_table_ref = 
    search_page_table_ref (*current_*) dircontentref url_act in
    page_table_ref := 
      add_page_table duringsession url_act !page_table_ref content


      
exception Exn1

let find_service 
    now
    (dircontentref, _, _, _)
    fullsessname
    (sitedata,
     all_cookie_info,
     ri,
     si) =

  let rec search_page_table dircontent =
    let aux a l =
      let aa = match a with
      | None -> defaultpagename
      | Some aa -> aa
      in
      try
        let dc = 
          try !(find_dircontent dircontent aa) 
          with Not_found -> raise Exn1
        in
        (match dc with
        | Dir dircontentref2 -> search_page_table !dircontentref2 l
        | File page_table_ref -> page_table_ref, l)
      with Exn1 -> 
        (match !(find_dircontent dircontent eliom_suffix_internal_name) with
        | Dir _ -> raise Not_found
        | File page_table_ref -> 
            (page_table_ref, (if a = None then [""] else aa::l)))
    in function
      | [] -> raise Ocsigen_Is_a_directory
      | [""] -> aux None []
      | ""::l -> search_page_table dircontent l
      | a::l -> aux (Some a) l
  in
  let page_table_ref, suffix = 
    try search_page_table !dircontentref (change_empty_list ri.ri_sub_path)
    with Not_found -> raise Eliom_404
  in
  find_page_table 
    now
    page_table_ref
    fullsessname
    sitedata
    all_cookie_info
    ri
    suffix
    {key_state = si.si_state_info;
     key_kind = ri.ri_method}
    si






(****************************************************************************)
(****************************************************************************)
(****************************************************************************)


(*****************************************************************************)
(* session administration                                                    *)

(*
(** Iterator on volatile sessions *)
let iter_sessions f =
  
(** Iterator on persistent sessions *)
let iter_persistent_sessions f =

*)

let close_all_service_sessions2 ?(close_group = false) fullsessname sitedata =
  SessionCookies.fold
    (fun k (fullsessname2, table, expref, timeoutref, sessgrpref) thr -> 
      thr >>= fun () ->
      if fullsessname = fullsessname2 && !timeoutref = TGlobal
      then (if close_group then
        close_service_group sitedata !sessgrpref
      else 
        close_service_session2 sitedata !sessgrpref k);
      Lwt_unix.yield ()
    )
    sitedata.session_services
    (return ())
  
(** Close all service sessions for one session name.
    If the optional parameter [?session_name] (session name) is not present,
    only the session with default name is closed.
 *)
let close_all_service_sessions ?close_group ?session_name sitedata =
  let fullsessname = make_fullsessname2 sitedata.site_dir_string session_name in
  close_all_service_sessions2 ?close_group fullsessname sitedata
  
let close_all_data_sessions2 ?(close_group = false) fullsessname sitedata =
  SessionCookies.fold
    (fun k (fullsessname2, expref, timeoutref, sessgrpref) thr -> 
      thr >>= fun () ->
      if fullsessname = fullsessname2 && !timeoutref = TGlobal
      then (if close_group then
        close_data_group sitedata !sessgrpref
      else
        close_data_session2 sitedata !sessgrpref k);
      Lwt_unix.yield ()
    )
    sitedata.session_data
    (return ())
  
(** Close all in memory data sessions for one session name.
    If the optional parameter [?session_name] (session name) is not present,
    only the session with default name is closed.
 *)
let close_all_data_sessions ?close_group ?session_name sitedata =
  let fullsessname = make_fullsessname2 sitedata.site_dir_string session_name in
  close_all_data_sessions2 ?close_group fullsessname sitedata
  

let close_all_persistent_sessions2 ?(close_group = false) fullsessname =
  Ocsipersist.iter_table
    (fun k (fullsessname2, old_exp, old_t, sessiongrp) -> 
      if fullsessname = fullsessname2 && old_t = TGlobal
      then (if close_group then
        close_persistent_group sessiongrp
      else close_persistent_session2 sessiongrp k) >>= Lwt_unix.yield
      else return ()
    )
    persistent_cookies_table

(** Close all persistent sessions for one session name.
    If the optional parameter [?session_name] (session name) is not present,
    only the session with default name is closed.
 *)
let close_all_persistent_sessions ?close_group ?session_name sitedata =
  let fullsessname = make_fullsessname2 sitedata.site_dir_string session_name in
  close_all_persistent_sessions2 ?close_group fullsessname
  


(* Update the expiration date for all service sessions                      *)
let update_serv_exp fullsessname sitedata old_glob_timeout new_glob_timeout =
  Messages.debug2 
    "--Eliom: Updating expiration date for all service sessions";
  match new_glob_timeout with
  | Some t when t <= 0. ->
      (* We close all sessions but those with user defined timeout *)
      close_all_service_sessions2 fullsessname sitedata
  | _ ->
    let now = Unix.time () in
    SessionCookies.fold
      (fun k (fullsessname2, table, expref, timeoutref, sessgrpref) thr ->
        thr >>= fun () ->
        (if fullsessname = fullsessname2 && !timeoutref = TGlobal
        then
          let newexp = match !expref, old_glob_timeout, new_glob_timeout with
          | _, _, None -> None
          | None, _, Some t
          | Some _, None, Some t -> Some (now +. t)
          | Some oldexp, Some oldt, Some t -> Some (oldexp -. oldt +. t)
          in
          match newexp with
          | Some t when t <= now -> 
              close_service_session2 sitedata !sessgrpref k
          | _ -> expref := newexp
        );
        Lwt_unix.yield ()
      )
      sitedata.session_services
      (return ())

(* Update the expiration date for all in memory data sessions                *)
let update_data_exp fullsessname sitedata old_glob_timeout new_glob_timeout =
  Messages.debug2 
    "--Eliom: Updating expiration date for all data sessions";
  match new_glob_timeout with
  | Some t when t <= 0. ->
      (* We close all sessions but those with user defined timeout *)
      close_all_data_sessions2 fullsessname sitedata
  | _ ->
    let now = Unix.time () in
    SessionCookies.fold
      (fun k (fullsessname2, expref, timeoutref, sessgrpref) thr ->
        thr >>= fun () ->
        (if fullsessname = fullsessname2 && !timeoutref = TGlobal
        then
          let newexp = match !expref, old_glob_timeout, new_glob_timeout with
          | _, _, None -> None
          | None, _, Some t
          | Some _, None, Some t -> Some (now +. t)
          | Some oldexp, Some oldt, Some t -> Some (oldexp -. oldt +. t)
          in
          match newexp with
          | Some t when t <= now -> close_data_session2 sitedata !sessgrpref k
          | _ -> expref := newexp
        );
        Lwt_unix.yield ()
      )
      sitedata.session_data
      (return ())


(* Update the expiration date for all sessions                               *)
let update_pers_exp fullsessname old_glob_timeout new_glob_timeout =
  Messages.debug2 
    "--Eliom: Updating expiration date for all persistent sessions";
  match new_glob_timeout with
  | Some t when t <= 0. ->
      (* We close all sessions but those with user defined timeout *)
      close_all_persistent_sessions2 fullsessname
  | _ ->
    let now = Unix.time () in
    Ocsipersist.iter_table
      (fun k (fullsessname2, old_exp, old_t, sessgrp) -> 
        if fullsessname = fullsessname2 && old_t = TGlobal
        then
          let newexp = match old_exp, old_glob_timeout, new_glob_timeout with
          | _, _, None -> None
          | None, _, Some t
          | Some _, None, Some t -> Some (now +. t)
          | Some oldexp, Some oldt, Some t -> Some (oldexp -. oldt +. t)
          in
          match newexp with
          | Some t when t <= now -> close_persistent_session2 sessgrp k
          | _ ->
              Ocsipersist.add
                persistent_cookies_table 
                k
                (fullsessname2, newexp, TGlobal, sessgrp) >>= Lwt_unix.yield
        else return ()
      )
      persistent_cookies_table




(*****************************************************************************)
(* Table of timeouts for sessions *)

(* default timeout = the one set in config file (or here) *)
let (set_default_service_timeout, 
     set_default_data_timeout, 
     set_default_persistent_timeout, 
     get_default_service_timeout, 
     get_default_data_timeout, 
     get_default_persistent_timeout) =

  let service_t = ref (Some 3600.) in (* 1 hour by default *)
  let data_t = ref (Some 3600.) in (* 1 hour by default *)
  let persistent_t = ref (Some 86400.) in (* 1 day by default *)
  ((fun timeout -> service_t := timeout),
   (fun timeout -> data_t := timeout),
   (fun timeout -> persistent_t := timeout),
   (fun () -> !service_t),
   (fun () -> !data_t),
   (fun () -> !persistent_t))

let set_default_volatile_timeout t =
  set_default_data_timeout t;
  set_default_service_timeout t

let add k v l = (k, v)::List.remove_assoc k l

(* global timeout = timeout for the whole site (may be changed dynamically) *)
let (find_global_service_timeout, 
     find_global_data_timeout, 
     find_global_persistent_timeout, 
     set_global_service_timeout2, 
     set_global_data_timeout2, 
     set_global_persistent_timeout2) =

  (
   (* find_global_service_timeout *)
   (fun fullsessname sitedata -> 
     try
       List.assoc fullsessname sitedata.servtimeout
     with Not_found -> get_default_service_timeout ()),

   (* find_global_data_timeout *)
   (fun fullsessname sitedata -> 
     try
       List.assoc fullsessname sitedata.datatimeout
     with Not_found -> get_default_data_timeout ()),

   (* find_global_persistent_timeout *)
   (fun fullsessname sitedata -> 
     try
       List.assoc fullsessname sitedata.perstimeout
     with Not_found -> get_default_persistent_timeout ()),

   (* set_global_service_timeout2 *)
   (fun fullsessname ~recompute_expdates sitedata t -> 
     if recompute_expdates
     then
       let oldt = 
         try
           List.assoc fullsessname sitedata.servtimeout
         with Not_found -> get_default_service_timeout ()
       in
       sitedata.servtimeout <- add fullsessname t sitedata.servtimeout;
       ignore (catch
                 (fun () -> update_serv_exp fullsessname sitedata oldt t)
                 (function e -> 
                   Messages.warning 
                     ("Eliom: Error while updating global service timeouts: "^
                      Ocsigen_lib.string_of_exn e);
                   Lwt.return ())
              )
         (*VVV Check possible exceptions raised *)
     else
       sitedata.servtimeout <- add fullsessname t sitedata.servtimeout
   ),

   (* set_global_data_timeout2 *)
   (fun fullsessname ~recompute_expdates sitedata t -> 
     if recompute_expdates
     then
       let oldt = 
         try
           List.assoc fullsessname sitedata.datatimeout
         with Not_found -> get_default_data_timeout ()
       in
       sitedata.datatimeout <- add fullsessname t sitedata.datatimeout;
       ignore (catch
                 (fun () -> update_data_exp fullsessname sitedata oldt t)
                 (function e -> 
                   Messages.warning 
                     ("Eliom: Error while updating global data timeouts: "^
                      Ocsigen_lib.string_of_exn e);
                   Lwt.return ())
              )
         (*VVV Check possible exceptions raised *)
     else
       sitedata.datatimeout <- add fullsessname t sitedata.datatimeout
   ),

   (* set_global_persistent_timeout *)
   (fun fullsessname ~recompute_expdates sitedata t -> 
     if recompute_expdates
     then
       let oldt = 
         try
           List.assoc fullsessname sitedata.perstimeout
         with Not_found -> get_default_persistent_timeout ()
       in
       sitedata.perstimeout <- add fullsessname t sitedata.perstimeout;
       ignore (catch
                 (fun () -> update_pers_exp fullsessname oldt t)
                 (function e -> 
                   Messages.warning 
                     ("Eliom: Error while updating global persistent timeouts: "^
                      Ocsigen_lib.string_of_exn e);
                   Lwt.return ())
              )
         (*VVV Check possible exceptions raised *)
     else
       sitedata.perstimeout <- add fullsessname t sitedata.perstimeout
   )
  )

let get_global_service_timeout ~session_name sitedata = 
  let fullsessname = 
    make_fullsessname2 sitedata.site_dir_string session_name 
  in
  find_global_service_timeout fullsessname sitedata

let get_global_data_timeout ~session_name sitedata = 
  let fullsessname = 
    make_fullsessname2 sitedata.site_dir_string session_name 
  in
  find_global_data_timeout fullsessname sitedata

let set_global_service_timeout ~session_name ~recompute_expdates sitedata
    timeout = 
  let fullsessname = make_fullsessname2 sitedata.site_dir_string session_name in
  set_global_service_timeout2
    fullsessname ~recompute_expdates sitedata timeout

let set_global_data_timeout ~session_name ~recompute_expdates sitedata
    timeout = 
  let fullsessname = make_fullsessname2 sitedata.site_dir_string session_name in
  set_global_data_timeout2 fullsessname ~recompute_expdates sitedata timeout

let get_global_persistent_timeout ~session_name sitedata =
  let fullsessname = 
    make_fullsessname2 sitedata.site_dir_string session_name 
  in
  find_global_persistent_timeout fullsessname sitedata

let set_global_persistent_timeout
    ~session_name ~recompute_expdates sitedata timeout = 
  let fullsessname = make_fullsessname2 sitedata.site_dir_string session_name in
  set_global_persistent_timeout2
    fullsessname ~recompute_expdates sitedata timeout



(*****************************************************************************)
(** Parsing global configuration for Eliommod: *)
open Simplexmlparser

let rec parse_global_config = function
  | [] -> ()
  | (Element ("timeout", [("value", s)], []))::ll
  | (Element ("volatiletimeout", [("value", s)], []))::ll -> 
      (try
        set_default_volatile_timeout (Some (float_of_string s))
      with Failure _ -> 
        if (s = "infinity")
        then set_default_volatile_timeout None
        else
          raise (Error_in_config_file "Eliom: Wrong value for value attribute of <timeout> or <volatiletimeout> tag"));
      parse_global_config ll
  | (Element ("datatimeout", [("value", s)], []))::ll -> 
      (try
        set_default_data_timeout (Some (float_of_string s))
      with Failure _ -> 
        if (s = "infinity")
        then set_default_data_timeout None
        else
          raise (Error_in_config_file "Eliom: Wrong value for value attribute of <datatimeout> tag"));
      parse_global_config ll
  | (Element ("servicetimeout", [("value", s)], []))::ll -> 
      (try
        set_default_service_timeout (Some (float_of_string s))
      with Failure _ -> 
        if (s = "infinity")
        then set_default_service_timeout None
        else
          raise (Error_in_config_file "Eliom: Wrong value for value attribute of <servicetimeout> tag"));
      parse_global_config ll
  | (Element ("persistenttimeout", [("value", s)], []))::ll -> 
      (try
        set_default_persistent_timeout (Some (float_of_string s))
      with Failure _ -> 
        if (s = "infinity")
        then set_default_persistent_timeout None
        else
          raise (Error_in_config_file "Eliom: Wrong value for value attribute of <persistenttimeout> tag"));
      parse_global_config ll
  | (Element ("sessiongcfrequency", [("value", s)], p))::ll ->
      (try
        let t = float_of_string s in
        set_servicesessiongcfrequency (Some t);
        set_datasessiongcfrequency (Some t)
      with Failure _ -> 
        if s = "infinity"
        then begin
          set_servicesessiongcfrequency None;
          set_datasessiongcfrequency None
        end
        else raise (Error_in_config_file
                      "Eliom: Wrong value for <sessiongcfrequency>"));
      parse_global_config ll
  | (Element ("servicesessiongcfrequency", [("value", s)], p))::ll ->
      (try
        set_servicesessiongcfrequency (Some (float_of_string s))
      with Failure _ -> 
        if s = "infinity"
        then set_servicesessiongcfrequency None
        else raise (Error_in_config_file
                      "Eliom: Wrong value for <servicesessiongcfrequency>"));
      parse_global_config ll
  | (Element ("datasessiongcfrequency", [("value", s)], p))::ll ->
      (try
        set_datasessiongcfrequency (Some (float_of_string s))
      with Failure _ -> 
        if s = "infinity"
        then set_datasessiongcfrequency None
        else raise (Error_in_config_file
                      "Eliom: Wrong value for <datasessiongcfrequency>"));
      parse_global_config ll
  | (Element ("persistentsessiongcfrequency", 
              [("value", s)], p))::ll ->
                (try
                  set_persistentsessiongcfrequency (Some (float_of_string s))
                with Failure _ -> 
                  if s = "infinity"
                  then set_persistentsessiongcfrequency None
                  else raise (Error_in_config_file
                                "Eliom: Wrong value for <persistentsessiongcfrequency>"));
                parse_global_config ll
  | (Element (tag,_,_))::ll -> 
      parse_global_config ll
  | _ -> raise (Error_in_config_file ("Unexpected content inside eliom config"))
        
        
let _ = parse_global_config (Extensions.get_config ())



(*****************************************************************************)
(* Create the table of cookies to send to the browser or to unset            *)
(* (from cookie_info)                                                        *)

let compute_session_cookies_to_send
    sitedata
    (service_cookie_info, 
     data_cookie_info, 
     pers_cookies_info) endlist =
  let getservvexp name (old, newi) =
    return 
      (let newinfo =
        match !newi with
        | SCNo_data
        | SCData_session_expired -> None
        | SC c -> Some (c.sc_value, !(c.sc_cookie_exp))
      in (name, old, newinfo))
  in
  let getdatavexp name v =
    if Lazy.lazy_is_val v
    then 
      return
        (let (old, newi) = Lazy.force v in
        let newinfo =
          match !newi with
          | SCNo_data
          | SCData_session_expired -> None
          | SC c -> Some (c.dc_value, !(c.dc_cookie_exp))
        in (name, old, newinfo))
    else fail Not_found
  in
  let getpersvexp name v =
    if Lazy.lazy_is_val v
    then 
      Lazy.force v >>= fun (old, newi) ->
      return
        (let oldinfo =
          match old with
            | None -> None
            | Some (v, _, _, _) -> Some v
         in
         let newinfo =
           match !newi with
             | SCNo_data
             | SCData_session_expired -> None
             | SC c -> Some (c.pc_value, !(c.pc_cookie_exp)) 
         in (name, oldinfo, newinfo))
    else fail Not_found
  in
  let ch_exp = function
    | CENothing | CEBrowser -> None
    | CESome a -> Some a
  in
  let aux f cookiename tab2 cooktab =
    cooktab >>= fun cooktab ->
    Http_frame.Cookievalues.fold
      (fun name value beg ->
        beg >>= fun beg ->
        catch
          (fun () ->
            f name value >>= fun (name, old, newc) ->
            return
              (match old, newc with
              | None, None -> beg
              | Some _, None ->
                  Http_frame.add_cookie
                    sitedata.site_dir
                    (make_full_cookie_name cookiename name)
                    Http_frame.OUnset
                    beg
                  (* the path is always site_dir because the cookie cannot 
                     have been unset by a service outside
                     this site directory *)
              | None, Some (v, exp) -> 
                  Http_frame.add_cookie
                    sitedata.site_dir
                    (make_full_cookie_name cookiename name)
                    (OSet (ch_exp exp, v))
                    beg
              | Some oldv, Some (newv, exp) -> 
                  if exp = CENothing && oldv = newv
                  then beg
                  else Http_frame.add_cookie
                      sitedata.site_dir
                      (make_full_cookie_name cookiename name)
                      (OSet (ch_exp exp, newv))
                      beg
              )
          )
          (function 
            | Not_found -> return beg
            | e -> fail e)
      )
      tab2
      (return cooktab)
  in
  aux getpersvexp persistentcookiename !pers_cookies_info
    (aux getdatavexp datacookiename !data_cookie_info 
       (aux getservvexp servicecookiename !service_cookie_info 
          (return endlist)))
  

let compute_cookies_to_send = compute_session_cookies_to_send


(* add a list of Eliom's cookies to an Http_frame cookie table *)
let add_cookie_list_to_send sitedata l t =
  let change_pathopt = function
    | None -> sitedata.site_dir 
          (* Not possible to set a cookie for another site (?) *)
    | Some p -> sitedata.site_dir@p
  in
  List.fold_left
    (fun t v -> 
      match v with
      | Set (upo, expo, n, v) ->
          Http_frame.add_cookie (change_pathopt upo) n (OSet (expo, v)) t
      | Unset (upo, n) ->
          Http_frame.add_cookie (change_pathopt upo) n OUnset t
    )
    t
    l

  
let compute_new_ri_cookies'
    now
    ripath
    ricookies
    cookies_set_by_page =

  let prefix upo p = match upo with
    | None -> true
    | Some path -> 
        Ocsigen_lib.list_is_prefix 
          (Ocsigen_lib.remove_slash_at_beginning path)
          (Ocsigen_lib.remove_slash_at_beginning p)
  in
  List.fold_left
    (fun tab v -> 
      match v with
      | Set (upo, Some t, n, v)  when t>now && prefix upo ripath ->
          Http_frame.Cookievalues.add n v tab
      | Set (upo, None, n, v) when prefix upo ripath ->
          Http_frame.Cookievalues.add n v tab
      | Set (upo, _, n, _)
      | Unset (upo, n) when prefix upo ripath ->
          Http_frame.Cookievalues.remove n tab
      | _ -> tab
    )
    ricookies
    cookies_set_by_page


(** Compute new ri.ri_cookies value
    from an old ri.ri_cookies and all_cookie_info 
    as if it had been sent by the browser *)
let compute_new_ri_cookies
    now
    ripath
    ricookies
    (service_cookie_info, data_cookie_info, pers_cookie_info)
    cookies_set_by_page =

  let ric = 
    compute_new_ri_cookies' now ripath ricookies cookies_set_by_page 
  in
  let ric = 
    Http_frame.Cookievalues.fold
      (fun n (_, v) beg -> 
        let n = make_full_cookie_name servicecookiename n in
        match !v with
        | SCData_session_expired
        | SCNo_data -> Http_frame.Cookievalues.remove n beg
        | SC c -> Http_frame.Cookievalues.add n c.sc_value beg
      )
      !service_cookie_info
      ric
  in
  let ric = 
    Http_frame.Cookievalues.fold
      (fun n v beg -> 
        let n = make_full_cookie_name datacookiename n in
        if Lazy.lazy_is_val v
        then 
          let (_, v) = Lazy.force v in
          match !v with
          | SCData_session_expired
          | SCNo_data -> Http_frame.Cookievalues.remove n beg
          | SC c -> Http_frame.Cookievalues.add n c.dc_value beg
        else beg
      )
      !data_cookie_info
      ric
  in
  let ric = 
    Http_frame.Cookievalues.fold
      (fun n v beg -> 
        let n = make_full_cookie_name persistentcookiename n in
        beg >>= fun beg ->
        if Lazy.lazy_is_val v
        then 
          Lazy.force v >>= fun (_, v) ->
          match !v with
          | SCData_session_expired
          | SCNo_data -> return (Http_frame.Cookievalues.remove n beg)
          | SC c -> return (Http_frame.Cookievalues.add n c.pc_value beg)
        else return beg
      )
      !pers_cookie_info
      (return ric)
  in
  ric




(** Compute the exceptions from expired sessions *)
let compute_exn closedservsessions =
  (if closedservsessions = [] 
  then []
  else [Eliom_Service_session_expired closedservsessions])

  
(*****************************************************************************)
(* Generation of the page or naservice 
   + update the cookie tables (value, expiration date and timeout)        *)

let execute
    now
    generate_page 
    ((ri, 
      si, 
      old_cookies_to_set,
      (service_cookies_info, data_cookies_info, pers_cookies_info)) as info)
    sitedata =
  
  catch
    (fun () -> generate_page now info sitedata)
    (fun e -> handle_site_exn e info sitedata) >>=
  (fun result ->
    
    (* Update service expiration date and value *)
    Http_frame.Cookievalues.iter
      
      (fun name (oldvalue, newr) ->
        (* catch fun () -> *)
        match !newr with
        | SCData_session_expired
        | SCNo_data -> () (* The cookie has been removed *)
        | SC newc ->
            newc.sc_exp :=
              match !(newc.sc_timeout) with
              | TGlobal -> 
                  let globaltimeout = 
                    find_global_service_timeout name sitedata 
                  in
                  (match globaltimeout with
                  | None -> None
                  | Some t -> Some (t +. now))
              | TNone -> None
              | TSome t -> Some (t +. now)
      )
      
      !service_cookies_info;
    
    (* Update "in memory data" expiration date and value *)
    Http_frame.Cookievalues.iter
      
      (fun name v ->
        if Lazy.lazy_is_val v (* Only sessions that have been used *)
        then 
          let (oldvalue, newr) = Lazy.force v
          in
          match !newr with
          | SCData_session_expired
          | SCNo_data -> () (* The cookie has been removed *)
          | SC newc ->
              newc.dc_exp :=
                match !(newc.dc_timeout) with
                | TGlobal -> 
                    let globaltimeout = 
                      find_global_data_timeout name sitedata 
                    in
                    (match globaltimeout with
                    | None -> None
                    | Some t -> Some (t +. now))
                | TNone -> None
                | TSome t -> Some (t +. now)
      )
      
      !data_cookies_info;
    
    
    (* Update persistent expiration date, user timeout and value *)
    (* Lwt_util.iter *)
    Http_frame.Cookievalues.fold

      (fun name v thr ->
        let thr2 =
          if Lazy.lazy_is_val v
          then begin
            Lazy.force v >>= fun (oldvalue, newr) ->
              match !newr with
              | SCData_session_expired
              | SCNo_data -> (* The cookie has been removed *)
                  return ()
              | SC newc ->
                  let newexp =
                    match !(newc.pc_timeout) with
                    | TGlobal -> 
                        let globaltimeout = 
                          find_global_persistent_timeout name sitedata 
                        in
                        (match globaltimeout with
                        | None -> None
                        | Some t -> Some (t +. now))
                    | TNone -> None
                    | TSome t -> Some (t +. now)
                  in
                  match oldvalue with
                  | Some (oldv, oldti, oldexp, oldgrp) when
                      (oldexp = newexp && 
                       oldti = !(newc.pc_timeout) &&
                       oldgrp = !(newc.pc_session_group) &&
                       oldv = newc.pc_value) -> return () (* nothing to do *)
                  | Some (oldv, oldti, oldexp, oldgrp) when
                      oldv = newc.pc_value ->
                      catch
                        (fun () ->
                          Ocsipersist.replace_if_exists
                            persistent_cookies_table 
                            newc.pc_value
                            (name, 
                             newexp, 
                             !(newc.pc_timeout), 
                             !(newc.pc_session_group)))
                        (function
                          | Not_found -> return ()
                                (* someone else closed the session *)
                          | e -> fail e)
                  | _ ->
                      Ocsipersist.add
                        persistent_cookies_table 
                        newc.pc_value
                        (name, 
                         newexp, 
                         !(newc.pc_timeout), 
                         !(newc.pc_session_group))

(*VVV Do not forget to change persistent_cookie_table_version
   if you change the type of persistent table data, 
   otherwise the server will crash!!!
 *)
          end
          else return ()
        in thr >>= fun () -> thr2
      )
      
      !pers_cookies_info

      (return ())
      
      
      >>= fun () ->
        
        return result)
    



exception Eliom_retry_with of 
  (request_info * 
     sess_info * 
     cookieset (* user cookies set by previous pages *) *
     tables cookie_info (* current cookie info *)
  ) 

(******************************************************************)
(* attached services                                              *)
let get_page
    now 
    (ri, 
     si,
     cookies_to_set,
     ((service_cookies_info, _, _) as all_cookie_info))
    sitedata
    =

  let find_aux e sci =
    Http_frame.Cookievalues.fold
      (fun fullsessname (_, r) beg -> 
        catch
          (fun () -> beg)
          (function
            | Eliom_404 | Eliom_Wrong_parameter ->
                (match !r with
                | SCData_session_expired
                | SCNo_data (* cookie removed *) -> beg
                | SC c ->
                    find_service
                      now
                      !(c.sc_table)
                      (Some fullsessname)
                      (sitedata,
                       all_cookie_info,
                       ri,
                       si))
            | e -> fail e)
      )
      sci
      (fail Eliom_404)
  in

  (catch
     (fun () -> 
        Messages.debug 
          (fun () ->
            "--Eliom: I'm looking for "^(string_of_url_path ri.ri_sub_path)^
            " in the session table:");
        find_aux Eliom_404 !service_cookies_info
      )
      (function 
        | Eliom_404 | Eliom_Wrong_parameter -> 
            catch (* ensuite dans la table globale *)
              (fun () -> 
                Messages.debug2 "--Eliom: I'm searching in the global table:";
                find_service 
                  now
                  sitedata.global_services
                  None
                  (sitedata,
                   all_cookie_info,
                   ri,
                   si))
              (function
                | Eliom_404 | Eliom_Wrong_parameter as exn -> 
                    (* si pas trouvé avec, on essaie sans l'état *)
                    (match si.si_state_info with
                    | (None, None) -> fail exn
                    | (g, Some _) -> 
                        (* There was a POST state. 
                           We remove it, and remove POST parameters.
                         *)
                        Messages.debug2 
                          "--Eliom: Link to old. I will try without POST parameters:";
                        fail (Eliom_retry_with 
                                ({ri with 
                                  ri_post_params = lazy (return []);
                                  ri_method = Http_frame.Http_header.GET;
                                  ri_extension_info=
                                  Eliom_Link_too_old::ri.ri_extension_info
                                }, 
                                 {si with
                                  si_nonatt_info= Na_no;
                                  si_state_info= (g, None);
                                },
                                 cookies_to_set,
                                 all_cookie_info
                                ))
                    | (Some _, None) -> 
                        (* There was a GET state, but no POST state. 
                           We remove it with its parameters, 
                           and remove POST parameters.
                         *)
                        Messages.debug2 
                          "--Eliom: Link to old. I will try without GET state parameters and POST parameters:";
                        fail (Eliom_retry_with 
                                ({ri with 
                                  ri_get_params = 
                                  lazy si.si_other_get_params;
                                  ri_post_params = lazy (return []);
                                  ri_method = Http_frame.Http_header.GET;
                                  ri_extension_info= 
                                  Eliom_Link_too_old::ri.ri_extension_info
                                },
                                 {si with
                                  si_nonatt_info= Na_no;
                                  si_state_info=(None, None);
                                  si_other_get_params=[];
                                },
                                 cookies_to_set,
                                 all_cookie_info)))
                | e -> fail e)
        | e -> fail e)
  )


(******************************************************************)
(* non attached services                                          *)
let make_naservice
    now 
    (ri,
     si,
     cookies_to_set,
     ((service_cookies_info, _, _) as all_cookie_info))
    sitedata
    =

  let rec find_aux sci =
    match
      Http_frame.Cookievalues.fold
        (fun fullsessname (_, r) beg ->
          match beg with
          | Found _ -> beg
          | Notfound _ ->
              match !r with
              | SCNo_data
              | SCData_session_expired -> beg
              | SC c ->
                  try
                    Found
                      ((find_naservice
                          now !(c.sc_table) si.si_nonatt_info),
                       !(c.sc_table), 
                       Some fullsessname)
                  with Not_found -> beg
        )
        sci
        (Notfound ())
    with
    | Found v -> v
    | Notfound _ ->
        (find_naservice now sitedata.global_services si.si_nonatt_info,
         sitedata.global_services,
         None)
  in
  (try
    (* look in the session service tables corresponding to cookies sent
       and then in the global table to find the service *)
    return (find_aux !service_cookies_info)
  with
  | Not_found ->
      (* The non-attached service has not been found.
         We call the same URL without non-attached parameters.
       *)
      match si.si_nonatt_info with
      | Na_no -> assert false
      | Na_post_ _
      | Na_post' _ -> 
(*VVV (Some, Some) or (_, Some)? *)
          Messages.debug2 
            "--Eliom: Link too old to a non-attached POST coservice. I will try without POST parameters:";
          change_request_info
            {ri with 
               ri_get_params = lazy si.si_other_get_params;
               ri_post_params = lazy (return []);
               ri_method = Http_frame.Http_header.GET;
               ri_extension_info= Eliom_Link_too_old::ri.ri_extension_info
            } 
            si.si_config_file_charset
            si.si_previous_extension_error
          >>=
            (fun (ri', si') -> 
               fail (Eliom_retry_with (ri', si',
                                       cookies_to_set,
                                       all_cookie_info)))

      | Na_get_ _
      | Na_get' _ ->
          Messages.debug2 
            "--Eliom: Link too old. I will try without non-attached parameters:";
          change_request_info
            {ri with 
             ri_get_params = lazy si.si_other_get_params;
             ri_post_params = lazy (return []);
             ri_method = Http_frame.Http_header.GET;
             ri_extension_info= Eliom_Link_too_old::ri.ri_extension_info
           } 
            si.si_config_file_charset
            si.si_previous_extension_error
            >>=
          (fun (ri', si') -> 
            fail (Eliom_retry_with (ri', si',
                                    cookies_to_set,
                                    all_cookie_info)))
  ) >>=
  (fun ((_, max_use, expdate, naservice), 
        tablewhereithasbeenfound,
        fullsessname) ->
    (naservice
       (make_server_params 
          sitedata
          all_cookie_info
          ri
          []
          si
          fullsessname)) >>=
    (fun r -> 
      Messages.debug2
        "--Eliom: Non attached page found and generated successfully";
      (match expdate with
      | Some (timeout, e) -> e := timeout +. now
      | None -> ());
      (match max_use with
      | None -> ()
      | Some r -> 
          if !r = 1
          then remove_naservice tablewhereithasbeenfound si.si_nonatt_info
          else r := !r - 1);
      return r))





let gen sitedata charset = function
| Extensions.Req_found (_, r) -> Lwt.return (Extensions.Ext_found r)
| Extensions.Req_not_found (previous_extension_err, ri) ->
  let now = Unix.time () in
  let rec gen_aux ((ri, si, old_cookies_to_set, all_cookie_info) as info) =
    let genfun = 
      match si.si_nonatt_info with
      | Na_no ->
          
          (* page generation *)
          get_page
            
      | _ ->
          
          (* anonymous service *)
          make_naservice
    in
    
    catch 
      (fun () ->
        execute 
          now
          genfun
          info
          sitedata >>= fun result_to_send ->
          
          match result_to_send with
          | EliomExn (exnlist, cookies_set_by_page) -> 
                     (* It is an action, we reload the page.
                        To do that, we retry without POST params.
                        If no post param at all, we retry
                        without GET non_att info.
                        If no GET non_att info, we retry without
                        GET state.
                        If no GET state,
                        we do not reload, otherwise it will loop.
                      *)
(* be very carefull while re-reading this *)
              let all_user_cookies =
                add_cookie_list_to_send
                  sitedata
                  cookies_set_by_page
                  old_cookies_to_set
              in

              (match si.si_nonatt_info, si.si_state_info, ri.ri_method with
              | Na_no, (None, None), Http_frame.Http_header.GET ->
                  compute_cookies_to_send 
                    sitedata
                    all_cookie_info
                    all_user_cookies
                  >>= fun all_new_cookies ->
                  let empty_result = Http_frame.empty_result () in
                  return
                    (Ext_found
                       (fun () ->
                         Lwt.return
                           {empty_result with
                            res_cookies= all_new_cookies}))
                    
              | _ ->
                  
                  compute_new_ri_cookies 
                    now 
                    ri.ri_sub_path
                    (Lazy.force ri.ri_cookies)
                    all_cookie_info
                    cookies_set_by_page
(*VVV old_cookies_to_set already are in ri_cookies, right? *)
                  >>= fun ric ->
                    
                  compute_cookies_to_send 
                    sitedata 
                    all_cookie_info
                    all_user_cookies
                  >>= fun all_new_cookies ->

                  (match 
                    si.si_nonatt_info, si.si_state_info, ri.ri_method 
                  with
                  | Na_get_ _, (_, None), Http_frame.Http_header.GET
                  | Na_get' _, (_, None), Http_frame.Http_header.GET ->
                      (* no post params, GET na coservice *)
                      
                      return
                        (Ext_retry_with
                           ({ri with
                             ri_get_params = lazy si.si_other_get_params;
                             ri_cookies= lazy ric;
                             ri_extension_info= exnlist
(* @ri.ri_extension_info *)
(*VVV I do not keep the old exceptions any more, 
  otherwise no way to remove them. *)
                           },
                            all_new_cookies
                           )
                        )
                        
                  | Na_no, (_, None), Http_frame.Http_header.GET ->
                      (* no post params, GET attached coservice *)
                      
                      return
                        (* Ext_retry_with, not Eliom_retry_with *)
                        (Ext_retry_with
                           ({ri with
                             ri_get_params = lazy si.si_other_get_params;
                             ri_cookies= lazy ric;
                             ri_extension_info= exnlist
(* @ri.ri_extension_info *)
(*VVV I do not keep the old exceptions any more, 
  otherwise no way to remove them. *)
                           },
                            all_new_cookies
                           ))
                        
                  | Na_post_ _, (_, _), _
                  | Na_post' _, (_, _), _ ->
                      (* POST na coservice *)
                      (* retry without POST params *)
                        
                      return
                        (* Ext_retry_with, not Eliom_retry_with *)
                        (Ext_retry_with
                           ({ri with
                             ri_get_params = lazy si.si_other_get_params;
(*VVV 31/12/2007 <-
  do we keep GET na_name ?
  Here, yes.
*)
                             ri_post_params = lazy (return []);
                             ri_method = Http_frame.Http_header.GET;
                             ri_cookies= lazy ric;
                             ri_extension_info= exnlist
(* @ri.ri_extension_info *)
(*VVV I do not keep the old exceptions any more, 
  otherwise no way to remove them. *)
                           },
                            all_new_cookies
                           ))
                        
                  | _ ->
                      (* retry without POST params *)
                      
                      return
                        (* Ext_retry_with, not Eliom_retry_with *)
                        (Ext_retry_with
                           ({ri with
                             ri_post_params = lazy (return []);
                             ri_method = Http_frame.Http_header.GET;
                             ri_cookies= lazy ric;
                             ri_extension_info= exnlist
(* @ri.ri_extension_info *)
(*VVV I do not keep the old exceptions any more, 
  otherwise no way to remove them. *)
                           },
                            all_new_cookies
                           ))
                  )
              )

          | EliomResult res ->

              let all_user_cookies =
                Http_frame.add_cookies
                  res.res_cookies
                  old_cookies_to_set
              in

              compute_cookies_to_send 
                sitedata 
                all_cookie_info
                all_user_cookies

              >>= fun all_new_cookies ->

              return 
                (Ext_found 
                   (fun () ->
                     Lwt.return
                       {res with
                        res_cookies= all_new_cookies}))
      )
      (function
        | Eliom_Typing_Error l -> 
            Predefined_senders.Xhtml_content.result_of_content
              (Error_pages.page_error_param_type l) >>= fun r ->
            return (Ext_found
                      (fun () ->
                        Lwt.return
                          {r with
                           res_cookies = old_cookies_to_set;
                           res_code= 500;
                         }))
	| Eliom_Wrong_parameter -> 
            force ri.ri_post_params >>= fun ripp ->
            Predefined_senders.Xhtml_content.result_of_content
                (Error_pages.page_bad_param (List.map fst ripp)) >>= fun r ->
            return (Ext_found 
                      (fun () ->
                        Lwt.return
                          {r with
                           res_cookies= old_cookies_to_set;
                           res_code= 500;
                         }))
	| Eliom_404 -> return (Ext_next previous_extension_err)
        | Eliom_retry_with a -> gen_aux a
	| e -> fail e)

  in
  change_request_info ri charset previous_extension_err >>= fun (ri, si) ->
  let (all_cookie_info, closedsessions) =
    get_cookie_info now
      sitedata 
      si.si_service_session_cookies
      si.si_data_session_cookies
      si.si_persistent_session_cookies 
  in
  let exn = compute_exn closedsessions in
  gen_aux ({ri with ri_extension_info= exn@ri.ri_extension_info}, 
           si,
           Http_frame.Cookies.empty, 
           all_cookie_info)



(*****************************************************************************)
(** Module loading *)
let config = ref []

let load_eliom_module sitedata cmo content =
  let preload () =
    config := content;
    begin_load_eliom_module ()
  in
  let postload () =
    end_load_eliom_module ();
    config := []
  in
  try
    Ocsigen_loader.loadfiles preload postload true cmo
  with Ocsigen_loader.Dynlink_error _ as e ->
    raise (Eliom_error_while_loading_site
             (Printf.sprintf "(eliom extension) %s"
                (Ocsigen_loader.error_message e)))



(*****************************************************************************)
(** Parsing of config file for each site: *)
let parse_config site_dir charset = 
(*--- if we put the following line here: *)
  let sitedata = new_sitedata site_dir in
(*--- then there is one service tree for each <site> *)
(*--- (mutatis mutandis for the following line:) *)
  absolute_change_sitedata sitedata;
  let rec parse_module_attrs file = function
    | [] -> (match file with
        None -> 
          raise (Error_in_config_file
                   ("Missing module attribute in <eliom>"))
      | Some s -> s)
    | ("module", s)::suite ->
        (match file with
          None -> parse_module_attrs (Some [s]) suite
        | _ -> raise (Error_in_config_file
                        ("Duplicate attribute file in <eliom>")))
    | ("findlib-package", s)::suite ->
        begin match file with
          | None ->
              begin try
                parse_module_attrs (Some (Ocsigen_loader.findfiles s)) suite
              with Ocsigen_loader.Findlib_error _ as e ->
                raise (Error_in_config_file
                         (Printf.sprintf "Findlib error: %s"
                            (Ocsigen_loader.error_message e)))
              end
          | _ -> raise (Error_in_config_file
                          ("Duplicate attribute file in <eliom>"))
        end
    | (s, _)::_ ->
        raise
          (Error_in_config_file ("Wrong attribute for <eliom>: "^s))
  in fun _ parse_site -> function
    | Element ("eliom", atts, content) -> 
(*--- if we put the line "new_sitedata" here, then there is 
  one service table for each <eliom> tag ...
  I think the other one is the best, because it corresponds to the way
  browsers manage cookies (one cookie for one site).
  Thus we can have one site in several cmo (with one session).
 *)
        let file = parse_module_attrs None atts in
        load_eliom_module sitedata file content;
        gen sitedata charset
    | Element (t, _, _) -> 
        raise (Extensions.Bad_config_tag_for_extension t)
    | _ -> raise (Error_in_config_file "(Eliommod extension)")


(*****************************************************************************)
(** Function to be called at the beginning of the initialisation phase *)
let start_init () = ()

(** Function to be called at the end of the initialisation phase *)
let end_init () =
  verify_all_registered (get_current_sitedata ());
  end_current_sitedata ()

(** Function that will handle exceptions during the initialisation phase *)
let handle_init_exn = function
  | Eliom_duplicate_registration s -> 
      ("Fatal - Eliom: Duplicate registration of url \""^s^
       "\". Please correct the module.")
  | Eliom_there_are_unregistered_services (s, l1, l2) ->
      ("Fatal - Eliom: in site \""^
       (Ocsigen_lib.string_of_url_path s)^"\" - "^
       (match l1 with
       | [] -> ""
       | [a] -> "One service or coservice has not been registered on URL \""
           ^(Ocsigen_lib.string_of_url_path a)^"\". "
       | a::ll -> 
           let string_of = Ocsigen_lib.string_of_url_path in
           "Some services or coservices have not been registered \
             on URLs: "^
             (List.fold_left
                (fun beg v -> beg^", "^(string_of v))
                (string_of a)
                ll
             )^". ")^
       (match l2 with
       | [] -> ""
       | [Na_get' _] -> "One non-attached GET coservice has not been registered."
       | [Na_post' _] -> "One non-attached POST coservice has not been registered."
       | [Na_get_ a] -> "The non-attached GET service \""
           ^a^
           "\" has not been registered."
       | [Na_post_ a] -> "The non-attached POST service \""
           ^a^
           "\" has not been registered."
       | a::ll -> 
           let string_of = function
             | Na_no -> "<no>"
             | Na_get' _ -> "<GET coservice>"
             | Na_get_ n -> n^" (GET)"
             | Na_post' _ -> "<POST coservice>"
             | Na_post_ n -> n^" (POST)"
           in
           "Some non-attached services or coservices have not been registered: "^
             (List.fold_left
                (fun beg v -> beg^", "^(string_of v))
                (string_of a)
                ll
             )^".")^
         "\nPlease correct your modules and make sure you have linked in all the modules...")
  | Eliom_function_forbidden_outside_site_loading f ->
      ("Fatal - Eliom: Bad use of function \""^f^
         "\" outside site loading. \
         (for some functions, you must add the ~sp parameter \
               to use them after initialization. \
               Creation or registration of public service for example)")
  | Eliom_page_erasing s ->
      ("Fatal - Eliom: You cannot create a page or directory here. "^s^
       " already exists. Please correct your modules.")
  | Eliom_error_while_loading_site s ->
      ("Fatal - Eliom: Error while loading site: "^s)
  | e -> raise e


(*****************************************************************************)
(** extension registration *)
let _ = register_extension
  (fun hostpattern -> parse_config)
  Extensions.void_extension
  start_init
  end_init
  handle_init_exn

let _ = persistent_session_gc ()



(*****************************************************************************)
(* Iterators or sessions *)

  (** Iterator on service sessions *)
let iter_service_sessions sitedata f =
  SessionCookies.fold
    (fun k v thr -> 
      thr >>= fun () ->
        f (k, v, sitedata) >>=
        Lwt_unix.yield
    )
    sitedata.session_services
    (return ())
      
  
    (** Iterator on data sessions *)
let iter_data_sessions sitedata f =
  SessionCookies.fold
    (fun k v thr -> 
      thr >>= fun () ->
        f (k, v, sitedata) >>=
        Lwt_unix.yield
    )
    sitedata.session_data
    (return ())
    
    (** Iterator on persistent sessions *)
let iter_persistent_sessions f =
  Ocsipersist.iter_table
    (fun k v -> 
      f (k, v) >>=
      Lwt_unix.yield
    )
    persistent_cookies_table
    
    
    (** Iterator on service sessions *)
let fold_service_sessions sitedata f beg =
  SessionCookies.fold
    (fun k v thr -> 
      thr >>= fun res1 ->
        f (k, v, sitedata) res1 >>= fun res ->
          Lwt_unix.yield () >>= fun () ->
            return res
    )
    sitedata.session_services
    (return beg)
    
    
    (** Iterator on data sessions *)
let fold_data_sessions sitedata f beg =
  SessionCookies.fold
    (fun k v thr -> 
      thr >>= fun res1 ->
        f (k, v, sitedata) res1 >>= fun res ->
          Lwt_unix.yield () >>= fun () ->
            return res
    )
    sitedata.session_data
    (return beg)
    
    (** Iterator on persistent sessions *)
let fold_persistent_sessions f beg =
  Ocsipersist.fold_table
    (fun k v beg -> 
      f (k, v) beg >>= fun res ->
        Lwt_unix.yield () >>= fun () ->
          return res
    )
    persistent_cookies_table
    beg

(*****************************************************************************)
(* Exploration *)

let number_of_service_sessions ~sp = 
  SessionCookies.length sp.sp_sitedata.session_services

let number_of_data_sessions ~sp = 
  SessionCookies.length sp.sp_sitedata.session_data

let number_of_tables () =
  List.length !counttableelements

let number_of_table_elements () =
  List.map (fun f -> f ()) !counttableelements

let number_of_persistent_sessions () = 
  Ocsipersist.length persistent_cookies_table


