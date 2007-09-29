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
open Ocsimisc
open Extensions
open Lazy

(** state is a parameter to differenciate coservices
   (several instances of the same URL).
 *)
type internal_state = string

type sess_info =
    {si_other_get_params: (string * string) list;
     si_all_get_params: (string * string) list;
     si_all_post_params: (string * string) list;

     si_service_session_cookies: (string (* cookie name (or site dir) *) * 
                                    string (* value *)) list;
     (* the session service cookies sent by the request *)

     si_data_session_cookies: (string (* cookie name (or site dir) *) * 
                                 string (* value *)) list;
     (* the session data cookies sent by the request *)

     si_persistent_session_cookies: (string (* cookie name (or site dir) *) *
                                       string (* value *)) list;
     (* the persistent session cookies sent by the request *)

     si_nonatt_info: (string option * string option);
     si_state_info: (internal_state option * internal_state option);
     si_exn: exn list;
     si_config_file_charset: string option}

(* The table of tables for each session. Keys are cookies *)
module Cookies = Hashtbl.Make(struct 
  type t = string
  let equal = (=)
  let hash = Hashtbl.hash
end)

type 'a session_cookie =
  | SCNo_data
  | SCData_session_expired
  | SC of 'a

type 'a one_service_cookie_info =
    (* service sessions: *)
    (string                   (* current value *) *
     'a ref                   (* service session table
                                 ref towards cookie table
                               *) *
     float option option ref  (* user timeout - 
                                 None = see global config
                                 Some None = no timeout
                                 ref towards cookie table
                               *) * 
     float option ref         (* expiration date ref (server side) - 
                                 None = never
                                 ref towards cookie table
                               *) * 
     float option option ref  (* cookie expiration date to set
                                 None = nothing to set
                                 Some None = set expiration = browser close
                                 Some Some = send expiration date
                               *)
    )


type one_data_cookie_info =
    (* in memory data sessions: *)
    (string                   (* current value *) *
     float option option ref  (* user timeout - 
                                 None = see global config
                                 Some None = no timeout
                                 ref towards cookie table
                               *) * 
     float option ref         (* expiration date ref (server side) - 
                                 None = never
                                 ref towards cookie table
                               *) * 
     float option option ref  (* cookie expiration date to set
                                 None = nothing to set
                                 Some None = set expiration = browser close
                                 Some Some = send expiration date
                               *)
    )

type one_persistent_cookie_info =
     (string                   (* current value *) *
      float option option ref  (* user timeout - 
                                  None = see global config
                                  Some None = no timeout
                                *) * 
      float option option ref  (* cookie expiration date to set
                                  None = nothing to set
                                  Some None = set expiration = browser close
                                  Some Some = send expiration date
                                *)

     )


type 'a cookie_info =
    (* service sessions: *)
    (string                    (* cookie fullsessname *) 
       * 

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
    )
      list ref *
      
    (* in memory data sessions: *)
    (string                    (* cookie fullsessname *) 
       * 

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
    )
      list ref *
      
      (* persistent sessions: *)
    (string                    (* cookie fullsessname *) 
       *

     ((string                  (* value sent by the browser *) *
       float option option     (* timeout at the beginning of the request *) *
       float option            (* (server side) expdate 
                                  at the beginning of the request
                                  None = no exp *))
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
    )
      list ref


(* non persistent cookies for services *)
type 'a servicecookiestablecontent =
    (string                  (* session fullsessname *) *
     'a                      (* session table *) * 
     float option ref        (* expiration date by timeout 
                                (server side) *) *
     float option option ref (* user timeout *))

type 'a servicecookiestable = 'a servicecookiestablecontent Cookies.t
(* the table contains:
   - the table of services
   - the expiration date (by timeout), changed at each access to the table
     (float option) None -> no expiration
   - the timeout for the user (float option option) None -> see global config
     Some None -> no timeout
 *)

(* non persistent cookies for in memory data *)
type datacookiestablecontent = 
    (string                  (* session fullsessname *) *
     float option ref        (* expiration date by timeout 
                                (server side) *) *
     float option option ref (* user timeout *))

type datacookiestable = datacookiestablecontent Cookies.t


type 'a server_params1 = 
    {sp_ri:request_info;
     sp_si:sess_info;
     sp_site_dir:url_path (* main directory of the site *);
     sp_site_dir_string:string (* the same, but string *);
     sp_global_table:'a (* global table *);
     sp_cookie_service_table: 'a servicecookiestable (* cookies table for volatile service sessions *);
     sp_cookie_data_table:datacookiestable (* cookies table for volatile data sessions *);
     sp_remove_sess_data:(string -> unit) ref (* remove_session_data *);
     sp_data_tables_are_empty:(string -> bool) ref (* are_empty_session_tables *);
     sp_cookie_info:'a cookie_info;
     sp_suffix:url_path (* suffix *);
     sp_fullsessname:string option (* the name of the session to which belong the service that answered (if it is a session service) *)}
      
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
exception Eliom_there_are_unregistered_services of string list
exception Eliom_function_forbidden_outside_site_loading of string
exception Eliom_page_erasing of string
exception Eliom_error_while_loading_site of string


(*****************************************************************************)
type result_to_send = 
  | EliomResult of Extensions.result
  | EliomExn of (exn list * cookieslist)





(*****************************************************************************)
(*****************************************************************************)
(* The table of dynamic pages for each virtual server, and naservices        *)
(* Each node contains either a list of nodes (case directory)
    or a table of "answers" (functions that will generate the page) *)


type page_table_key =
    {key_state: (internal_state option * internal_state option);
     key_kind: Http_frame.Http_header.http_method}
      (* action: tables server_params1 -> page *)

      (* module Page_Table = Map.Make(struct type t = page_table_key 
         let compare = compare end) *)

module String_Table = Map.Make(struct 
  type t = string
  let compare = compare 
end)

module NAserv_Table = Map.Make(struct 
  type t = string option * string option
  let compare = compare 
end)

type page_table = 
    (page_table_key * 
       (((anon_params_type * anon_params_type) (* unique_id *) * 
           (int * (* generation (= number of reloads of sites
                     that after which that service has been created) *)
              (int ref option (* max_use *) *
                 (float * float ref) option
                 (* timeout and expiration date for the service *) *
                 (tables server_params1 -> result_to_send Lwt.t)
	         * url_path))) list)) list
       (* Here, the url_path is the site directory.
          That is, the directory in which we are when we register
          dynamically the pages.
          Each time we load a page, we change to this directory
          (in case the page registers new pages).
        *)

and naservice_table = 
  | AVide 
  | ATable of 
      (int ref option (* max_use *) *
         (float * float ref) option (* timeout and expiration date *) *
         (tables server_params1 -> result_to_send Lwt.t)
	 * url_path)
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

(* table cookie -> session table *)
let new_service_cookie_table () : tables servicecookiestable = 
  Cookies.create 100

let new_data_cookie_table () : datacookiestable = 
  Cookies.create 100



type pages_tree = 
    tables (* global table of continuations/naservices *)
      * tables servicecookiestable (* service session tables *)
      * datacookiestable (* session data tables *)
      * ((string -> unit) ref (* remove_session_data *) *
           (string -> bool) ref (* not_bound_in_data_tables *))

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

let new_pages_tree () =
  ((empty_tables ()),             (* global service table *)
   (new_service_cookie_table ()), (* cookie table for services *)
   (new_data_cookie_table ()),    (* cookie table for in memory session data *)
   ((ref (fun cookie -> ())),     (* remove_session_data *)
      ref (fun cookie -> true))   (* not_bound_in_data_tables *))

(****************************************************************************)

type service_session_table = tables

(** Type of http parameters *)
type server_params = service_session_table server_params1

let new_service_session_tables = empty_tables

(*****************************************************************************)
(* The current registration directory *)
let absolute_change_hostdir, get_current_hostdir, 
  begin_current_host_dir, end_current_hostdir =
  let current_dir : ((unit -> pages_tree) * url_path) ref = 
    ref ((fun () ->
      raise (Ocsigen_Internal_Error "No pages tree available")), []) 
  in
  let f1' (pagetree, dir) = current_dir := ((fun () -> pagetree), dir) in
  let f2' () = let (cd1, cd2) = !current_dir in (cd1 (), cd2) in
  let f1 = ref f1' in
  let f2 = ref f2' in
  let exn1 _ = 
    raise (Eliom_function_forbidden_outside_site_loading 
             "absolute_change_hostdir") in
  let exn2 () = 
    raise (Eliom_function_forbidden_outside_site_loading
             "get_current_hostdir") in
  ((fun hostdir -> !f1 hostdir) (* absolute_change_hostdir *),
   (fun () -> !f2 ()) (* get_current_hostdir *),
   (fun () -> f1 := f1'; f2 := f2') (* begin_current_host_dir *),
   (fun () -> f1 := exn1; f2 := exn2) (* end_current_hostdir *))
(* Warning: these functions are used only during the initialisation
   phase, which is not threaded ... That's why it works, but ...
   it is not really clean ... public registration relies on this
   directory (defined for each site in the config file) 
 *)

let add_unregistered, remove_unregistered, verify_all_registered =
  let l = ref [] in
  ((fun a -> l := a::!l),
   (fun a -> l := list_remove_first_if_any a !l),
   (fun () -> 
     match !l with
     | [] -> () 
     | l -> 
         raise (Eliom_there_are_unregistered_services 
                  (List.map
                     (function
                       | None -> "<Non-attached service>"
                       | Some a -> string_of_url_path a)
                     l))))

let during_eliom_module_loading, 
  begin_load_eliom_module, 
  end_load_eliom_module =
  let during_eliom_module_loading_ = ref false in
  ((fun () -> !during_eliom_module_loading_),
   (fun () -> during_eliom_module_loading_ := true),
   (fun () -> during_eliom_module_loading_ := false))

let global_register_allowed () =
  if (during_initialisation ()) && (during_eliom_module_loading ())
  then Some get_current_hostdir
  else None


(*****************************************************************************)
let eliom_suffix_name = "__eliom_suffix"
let eliom_suffix_internal_name = "__eliom_suffix**"
let naservice_name = "__eliom_na__name"
let get_state_param_name = "__eliom__"
let post_state_param_name = "__eliom_p__"
let datacookiename = "eliomdatasession|" 
let servicecookiename = "eliomservicesession|" 
(* must not be a prefix of the following and vice versa (idem for data) *)
let persistentcookiename = "eliompersistentsession|"
let co_param_prefix = "__co_eliom_"
let na_co_param_prefix = "__na_eliom_"
let eliom_persistent_cookie_table = "eliom_persist_cookies"

(******************************************************************)
let make_full_cookie_name a b = a^b

let make_fullsessname ~sp = function
  | None -> sp.sp_site_dir_string
  | Some s -> sp.sp_site_dir_string^"|"^s
(* Warning: do not change this without modifying Eliomsessions.Admin *)

let make_fullsessname2 site_dir_string = function
  | None -> site_dir_string
  | Some s -> site_dir_string^"|"^s
(* Warning: do not change this without modifying Eliomsessions.Admin *)


let make_new_cookie_value () =

  (* Solution by Dario Teixeira: *)
  let random_part =
    let rng = Cryptokit.Random.device_rng "/dev/urandom/" in
    Cryptokit.Random.string rng 20
  and sequential_part = 
    Printf.sprintf "%Lx"  (Int64.bits_of_float (Unix.gettimeofday ()))
  in
  (Cryptokit.hash_string (Cryptokit.Hash.sha1 ()) random_part) ^ 
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


let rec new_data_cookie fullsessname table = 
  let c = make_new_cookie_value () in
  try
    ignore (Cookies.find table c); (* Actually not needed 
                                      for the cookies we use *)
    new_data_cookie fullsessname table
  with Not_found ->
    let usertimeout = ref None (* None = See global table *) in
    let serverexp = ref (Some 0.) (* None = never. We'll change it later. *) in
    Cookies.replace (* actually it will add the cookie *)
      table 
      c
      (fullsessname,
       serverexp (* exp on server *),
       usertimeout);
    (c, usertimeout, serverexp, ref None (* exp on client - 
                                            None = nothing to set *))

let rec new_service_cookie fullsessname table = 
  let c = make_new_cookie_value () in
  try
    ignore (Cookies.find table c); (* Actually not needed 
                                      for the cookies we use *)
    new_service_cookie fullsessname table
  with Not_found ->
    let str = ref (new_service_session_tables ()) in
    let usertimeout = ref None (* None = See global table *) in
    let serverexp = ref (Some 0.) (* None = never. We'll change it later. *) in
    Cookies.replace (* actually it will add the cookie *)
      table 
      c
      (fullsessname,
       !str, 
       serverexp (* exp on server *),
       usertimeout);
    (c, str, usertimeout, serverexp, ref None (* exp on client - 
                                                 None = nothing to set *))


let find_or_create_data_cookie ?session_name ~sp () = 
  (* If the cookie does not exist, create it.
     Returns the cookie info for the cookie *)
  let fullsessname = make_fullsessname ~sp session_name in
  let (_, cookie_info, _) = sp.sp_cookie_info in
  try
    let (old, ior) = Lazy.force (List.assoc fullsessname !cookie_info) in
    match !ior with
    | SCData_session_expired 
    | SCNo_data -> 
        let v = new_data_cookie fullsessname sp.sp_cookie_data_table in
        ior := SC v;
        v
    | SC v -> v;
  with Not_found -> 
    let v = new_data_cookie fullsessname sp.sp_cookie_data_table in
    cookie_info := 
      (fullsessname, 
       Lazy.lazy_from_val (None, ref (SC v)))::
      !cookie_info;
    v
        
let find_data_cookie_only ?session_name ~sp () = 
  (* If the cookie does not exist, do not create it, raise Not_found.
     Returns the cookie info for the cookie *)
  let fullsessname = make_fullsessname ~sp session_name in
  let (_, cookie_info, _) = sp.sp_cookie_info in
  let (_, ior) = Lazy.force (List.assoc fullsessname !cookie_info) in
  match !ior with
  | SCNo_data -> raise Not_found
  | SCData_session_expired -> raise Eliom_Session_expired
  | SC v -> v

let find_or_create_service_cookie ?session_name ~sp () = 
  (* If the cookie does not exist, create it.
     Returns the cookie info for the cookie *)
  let fullsessname = make_fullsessname ~sp session_name in
  let (cookie_info, _, _) = sp.sp_cookie_info in
  try
    let (old, ior) = List.assoc fullsessname !cookie_info in
    match !ior with
    | SCData_session_expired 
    | SCNo_data -> 
        let v = new_service_cookie fullsessname sp.sp_cookie_service_table in
        ior := SC v;
        v
    | SC v -> v
  with Not_found -> 
    let v = new_service_cookie fullsessname sp.sp_cookie_service_table in
    cookie_info :=
      (fullsessname,
       (None, ref (SC v)))::
      !cookie_info;
    v


let find_service_cookie_only ?session_name ~sp () = 
  (* If the cookie does not exist, do not create it, raise Not_found.
     Returns the cookie info for the cookie *)
  let fullsessname = make_fullsessname ~sp session_name in
  let (cookie_info, _, _) = sp.sp_cookie_info in
  let (_, ior) = List.assoc fullsessname !cookie_info in
  match !ior with
  | SCNo_data -> raise Not_found
  | SCData_session_expired -> raise Eliom_Session_expired
  | SC v -> v


(*****************************************************************************)
(** session data *)

let counttableelements = ref []
(* Here only for exploration functions *)

let create_table, create_table_during_session =
  let aux remove_session_data not_bound_in_data_tables =
    let t = Cookies.create 100 in
    let old_remove_session_data = !remove_session_data in
    remove_session_data := 
      (fun cookie ->
        old_remove_session_data cookie;
        Cookies.remove t cookie
      );
    let old_not_bound_in_data_tables = !not_bound_in_data_tables in
    not_bound_in_data_tables :=
      (fun cookie ->
        old_not_bound_in_data_tables cookie &&
        not (Cookies.mem t cookie)
      );
    counttableelements := 
      (fun () -> Cookies.length t)::!counttableelements;
    t
  in
  ((fun () ->
    let (_, _, _, (remove_session_data, not_bound_in_data_tables)), _ = 
      get_current_hostdir () 
    in
    aux remove_session_data not_bound_in_data_tables),
   (fun sp -> aux sp.sp_remove_sess_data sp.sp_data_tables_are_empty))



(* to be called from outside requests *)
let close_data_session2 remove_session_data cookie_table cookie = 
  try
    Cookies.remove cookie_table cookie;
    remove_session_data cookie;
  with Not_found -> ()

(* to be called during a request *)
let close_data_session ?session_name ~sp () = 
  try
    let fullsessname = make_fullsessname ~sp session_name in
    let (_, cookie_info, _) = sp.sp_cookie_info in
    let (_, ior) = Lazy.force (List.assoc fullsessname !cookie_info) in
    match !ior with
    | SC (c, _, _, _) ->
        close_data_session2 
          !(sp.sp_remove_sess_data)
          sp.sp_cookie_data_table c;
        ior := SCNo_data
    | _ -> ()
  with Not_found -> ()



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

let rec new_persistent_cookie fullsessname = 
  let c = make_new_cookie_value () in
  catch
    (fun () ->
      Ocsipersist.find persistent_cookies_table c >>= (* useless *)
      (fun _ -> new_persistent_cookie fullsessname)) (* never succeeds *)
    (function
      | Not_found -> 
          begin
            let deprecated = Int64.zero in (* for compatibility with
                                              old versions *)
            let usertimeout = ref None (* None = See global table *) in
            Ocsipersist.add persistent_cookies_table c 
              (fullsessname,
               Some 0. (* exp on server - We'll change it later *),
               None (* timeout - None = see global config *),
               deprecated)
              >>= fun () -> 
            return (c, usertimeout, ref None (* exp on client *))
          end
      | e -> fail e)


let find_or_create_persistent_cookie ?session_name ~sp () =
  (* if it exists, do not create it, but returns its value *)
  let fullsessname = make_fullsessname ~sp session_name in
  let (_, _, cookie_info) = sp.sp_cookie_info in
  catch
    (fun () ->
      Lazy.force (List.assoc fullsessname !cookie_info) >>= fun (old, ior) ->
      match !ior with
      | SCData_session_expired 
      | SCNo_data -> 
          new_persistent_cookie fullsessname >>= fun v ->
          ior := SC v;
          return v
      | SC v -> return v)
    (function
      | Not_found -> 
          new_persistent_cookie fullsessname >>= fun v ->
          cookie_info := 
            (fullsessname, 
             Lazy.lazy_from_val (return (None, ref (SC v))))::
            !cookie_info;
          return v
      | e -> fail e)



let find_persistent_cookie_only ?session_name ~sp () =
  (* If the cookie does not exist, do not create it, raise Not_found.
     Returns the cookie info for the cookie *)
  let fullsessname = make_fullsessname ~sp session_name in
  let (_, _, cookie_info) = sp.sp_cookie_info in
  Lazy.force (List.assoc fullsessname !cookie_info) >>= fun (_, ior) ->
  match !ior with
  | SCNo_data -> raise Not_found
  | SCData_session_expired -> raise Eliom_Session_expired
  | SC v -> return v





(* close a persistent session by cookie *)
let close_persistent_session2 cookie = 
  catch
    (fun () ->
      Ocsipersist.remove persistent_cookies_table cookie >>= fun () ->
      remove_from_all_persistent_tables cookie
    )
    (function
      | Not_found -> return ()
      | e -> fail e)

(* close current persistent session *)
let close_persistent_session ?session_name ~sp () = 
  catch
    (fun () ->
      let fullsessname = make_fullsessname ~sp session_name in
      let (_, _, cookie_info) = sp.sp_cookie_info in
      Lazy.force (List.assoc fullsessname !cookie_info) >>= fun (_, ior) ->
      match !ior with
      | SC (c, _, _) ->
          close_persistent_session2 c >>= fun () ->
          ior := SCNo_data;
          return ()
      | _ -> return ()
    )
    (function
      | Not_found -> return ()
      | e -> fail e)



    
(*****************************************************************************)
(* Split parameter list, removing those whose name starts with pref *)
let split_prefix_param pref l =
  let len = String.length pref in
  List.partition (fun (n,_) -> 
    try 
      (String.sub n 0 len) = pref 
    with _ -> false) l


(*****************************************************************************)
(* cookie manipulation *)
let getcookies cookiename cookies = 
  let length = String.length cookiename in
  let last = length - 1 in
  List.fold_left
    (fun beg (name, value) -> 
      if string_first_diff cookiename name 0 last = length
      then (String.sub name length ((String.length name) - length), 
            value)::beg
      else beg
    )
    []
    cookies


(** look in table to find if the session cookies sent by the browser
   correspond to existing (and not closed) sessions *)
let get_cookie_info now
    (_, service_cookie_table, data_cookie_table, (remove_session_data, _))
    service_cookies data_cookies persistent_cookies
    : 'a cookie_info * 'b list =
  
  (* get info about "in memory" data session cookies: *)
  let (servoklist, servfailedlist) =
    List.fold_left
      (fun (oklist, failedlist) (name, value) ->
        try 
          let fullsessname, ta, expref, timeout_ref = 
            Cookies.find service_cookie_table value
          in
          match !expref with
          | Some t when t < now -> 
              (* session expired by timeout *)
              Cookies.remove service_cookie_table value;
              ((name, 
                (Some value          (* value sent by the browser *),
                 ref SCData_session_expired (* ask the browser 
                                               to remove the cookie *)))::
               oklist, 
               name::failedlist)
          | _ -> ((name, 
                   (Some value        (* value sent by the browser *),
                    ref 
                      (SC
                         (value       (* value *),
                          ref ta      (* the table of session services *), 
                          timeout_ref (* user timeout ref *),
                          expref      (* expiration date (server side) *),
                          ref None    (* cookie expiration date to send
                                         to the browser *)))))::
                  oklist,
                  failedlist)
        with Not_found ->
          ((name, 
            (Some value                 (* value sent by the browser *),
             ref SCData_session_expired (* ask the browser 
                                           to remove the cookie *)))::
           oklist, 
           name::failedlist)
      )
      ([], [])
      service_cookies
  in

  (* get info about "in memory" data session cookies: *)
  let dataoklist =
    List.map
      (fun (name, value) ->
        (name,
         lazy
           (try
             let fullsessname, expref, timeout_ref = 
               Cookies.find data_cookie_table value
             in
             match !expref with
             | Some t when t < now -> 
                 (* session expired by timeout *)
                 !remove_session_data value;
                 Cookies.remove data_cookie_table value;
                 (Some value                 (* value sent by the browser *),
                  ref SCData_session_expired (* ask the browser 
                                                to remove the cookie *))
             | _ ->
                 (Some value        (* value sent by the browser *),
                  ref 
                    (SC
                       (value       (* value *),
                        timeout_ref (* user timeout ref *),
                        expref      (* expiration date (server side) *),
                        ref None    (* cookie expiration date to send
                                       to the browser *))))
           with Not_found ->
             (Some value                  (* value sent by the browser *),
              ref SCData_session_expired  (* ask the browser 
                                             to remove the cookie *))))
      )
      data_cookies
  in
  
  
  (* *** get info about persistent session cookies: *)
  let persoklist =
    List.map
      (fun (name, value) ->
        (name, 
         lazy
           (catch
              (fun () ->
                Ocsipersist.find persistent_cookies_table value >>=
                fun (fullsessname, persexp, perstimeout, _) ->
                  
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
                                                  of the request *)),
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
                                               of the request *)),
                         (ref 
                            (SC
                               (value           (* value *),
                                ref perstimeout (* user persistent timeout 
                                                   ref *),
                                ref None        (* persistent cookie expiration
                                                   date ref to send to the
                                                   browser *)
                               ))))
              )
              (function
                | Not_found -> 
                    return
                      (Some (value         (* value at the beginning
                                              of the request *),
                             None          (* user persistent timeout
                                              at the beginning 
                                              of the request *),
                             Some 0.       (* expiration date (server)
                                              at the beginning 
                                              of the request *)),
                       ref SCData_session_expired   (* ask the browser 
                                                       to remove the cookie *))
                | e -> fail e)
           )
        )
      )
      persistent_cookies (* the persistent cookies sent by the request *)
      
  in
  ((ref servoklist, ref dataoklist, ref persoklist), 
   servfailedlist)
      
(*****************************************************************************)
let change_request_info ri charset =
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
    let (naservice_info, (get_state, post_state),
      (get_params, other_get_params), post_params) =
      let post_naservice_name, na_post_params = 
        try
          let n, pp =
            list_assoc_remove naservice_name post_params
          in (Some n, pp)
        with Not_found -> (None, []) 
        (* Not possible to have POST parameters without naservice_name
           if there is a GET naservice_name
         *)
      in
      let get_naservice_name, (na_get_params, other_get_params) = 
        try
          let n, gp =
            list_assoc_remove naservice_name get_params
          in (Some n, (split_prefix_param na_co_param_prefix gp))
        with Not_found -> (None, ([], get_params))
      in
      match get_naservice_name, post_naservice_name with
      | _, Some _
      | Some _, None -> (* non attached coservice *)
          ((get_naservice_name, post_naservice_name),
           (None, None), (na_get_params, other_get_params), na_post_params)
      | None, None ->
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
          in ((None, None), (get_state, post_state), 
          (get_params, other_get_params), post_params)
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
        si_nonatt_info=naservice_info;
        si_state_info=(get_state, post_state);
        si_other_get_params=other_get_params;
        si_all_get_params= get_params0;
        si_all_post_params= post_params0;
        si_exn=[];
        si_config_file_charset=charset}))



(*****************************************************************************)
(* Session service table *)
(** We associate to each service a function server_params -> page *)

    (** Create server parameters record *)
let make_server_params 
    dir (gt, cst, cdt, (f1, f2)) all_cookie_info ri suffix si fullsessname
    : 'a server_params1 =
  {sp_ri=ri;
   sp_si=si;
   sp_site_dir=dir;
   sp_site_dir_string=string_of_url_path dir;
   sp_global_table=gt;
   sp_cookie_service_table=cst;
   sp_cookie_data_table=cdt;
   sp_remove_sess_data=f1;
   sp_data_tables_are_empty=f2;
   sp_cookie_info=all_cookie_info;
   sp_suffix=suffix;
   sp_fullsessname= fullsessname}


type ('a, 'b) foundornot = Found of 'a | Notfound of 'b

let find_page_table 
    now
    (pagetableref : page_table ref)
    fullsessname
    tables
    all_cookie_info
    ri
    urlsuffix
    k
    si
    = 
  let sp = 
    make_server_params [] tables all_cookie_info ri urlsuffix si fullsessname 
  in
  let rec aux toremove = function
    | [] -> Lwt.return ((Notfound Eliom_Wrong_parameter), [])
    | (((_, (_, (max_use, expdate, funct, site_dir))) as a)::l) ->
        match expdate with
        | Some (_, e) when !e < now ->
            (* Service expired. Removing it. *)
            Messages.debug "--Eliom: Service expired. I'm removing it";
            aux toremove l >>= 
            (fun (r, toremove) -> Lwt.return (r, a::toremove))
        | _ ->
            catch 
              (fun () ->
                Messages.debug "--Eliom: I'm trying a service";
                funct {sp with
                       sp_site_dir=site_dir;
                       sp_site_dir_string=string_of_url_path site_dir}
                  >>=
                (* warning: the list ll may change during funct
                   if funct register something on the same URL!! *)
                (fun p -> 
                  Messages.debug
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
                  Lwt.return (Found (p, site_dir),
                               newtoremove)))
              (function
                | Eliom_Wrong_parameter -> 
                    aux toremove l >>= 
                    (fun (r, toremove) -> Lwt.return (r, toremove))
                | e -> Lwt.return ((Notfound e), toremove))
  in 
  (catch 
     (fun () -> return (List.assoc k !pagetableref))
     (function Not_found -> fail Ocsigen_404 | e -> fail e)) >>=
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
  with Not_found -> (key,[v])::t

let add_dircontent dc (key,elt) =
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
  try
    match at with
    | AVide -> AVide
    | ATable t -> ATable (NAserv_Table.remove k t)
  with _ -> at

let add_naservice 
    (_, naservicetableref, _, containstimeouts) current_dir duringsession name 
    (max_use, expdate, naservice) =
  (if not duringsession
  then
    try
      ignore (find_naservice_table !naservicetableref name);
      raise (Eliom_duplicate_registration "<non-attached coservice>")
    with Not_found -> ());

  (match expdate with
  | Some _ -> containstimeouts := true
  | _ -> ());
  
  naservicetableref :=
    add_naservice_table !naservicetableref
      (name, (max_use, expdate, naservice, current_dir))

let remove_naservice (_,atr,_,_) name =
  atr := remove_naservice_table !atr name

let find_naservice now ((_,atr,_,_) as str) name =
  let ((_, expdate, _, _) as p) = find_naservice_table !atr name in
  match expdate with
  | Some (_, e) when !e < now ->
      (* Service expired. Removing it. *)
      Messages.debug "--Eliom: Non attached service expired. I'm removing it";
      remove_naservice str name;
      raise Not_found
  | _ -> p


let add_service 
    (dircontentref,_,containstimeouts,_)
    current_dir
    duringsession
    url_act
    (page_table_key, 
     ((unique_id1, unique_id2), max_use, expdate, action)) =

  let aux search dircontentref a l =
    try 
      let direltref = find_dircontent !dircontentref a in
      match !direltref with
      | Dir dcr -> search dcr l
      | File ptr -> raise (Eliom_page_erasing 
                             ((string_of_url_path current_dir)^"/"^a))
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
                  (max_use, expdate, action, current_dir))) in
  (* let current_dircontentref = 
     search_dircontentref dircontentref current_dir) in *)
  let page_table_ref = 
    search_page_table_ref (*current_*) dircontentref url_act in
    page_table_ref := 
      add_page_table duringsession url_act !page_table_ref content


      
exception Exn1

let find_service 
    now
    (dircontentref, _, _, _)
    fullsessname
    (tables,
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
    try search_page_table !dircontentref (change_empty_list ri.ri_path)
    with Not_found -> raise Ocsigen_404
  in
  find_page_table 
    now
    page_table_ref
    fullsessname
    tables
    all_cookie_info
    ri
    suffix
    {key_state = si.si_state_info;
     key_kind = ri.ri_method}
    si





let close_service_session2 cookie_table cookie = 
  Cookies.remove cookie_table cookie

let close_service_session ?session_name ~sp () = 
  try
    let fullsessname = make_fullsessname ~sp session_name in
    let (cookie_info, _, _) = sp.sp_cookie_info in
    let (_, ior) = List.assoc fullsessname !cookie_info in
    match !ior with
    | SC (c, _, _, _, _) ->
        close_service_session2 sp.sp_cookie_service_table c;
        ior := SCNo_data
    | _ -> ()
  with Not_found -> ()


let close_volatile_session ?session_name ~sp () =
  close_data_session ?session_name ~sp ();
  close_service_session ?session_name ~sp ()


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

let close_all_service_sessions2 fullsessname cookie_table =
  Cookies.fold
    (fun k (fullsessname2, table, expref, timeoutref) thr -> 
      thr >>= fun () ->
      if fullsessname = fullsessname2 && !timeoutref = None
      then close_service_session2 cookie_table k;
      Lwt_unix.yield ()
    )
    cookie_table
    (return ())
  
(** Close all service sessions for one session name.
    If the optional parameter [?session_name] (session name) is not present,
    only the session with default name is closed.
 *)
let close_all_service_sessions ?session_name cookie_table wd =
  let fullsessname = make_fullsessname2 (string_of_url_path wd) session_name in
  close_all_service_sessions2 fullsessname cookie_table
  
let close_all_data_sessions2 fullsessname remove_session_data cookie_table =
  Cookies.fold
    (fun k (fullsessname2, expref, timeoutref) thr -> 
      thr >>= fun () ->
      if fullsessname = fullsessname2 && !timeoutref = None
      then close_data_session2 remove_session_data cookie_table k;
      Lwt_unix.yield ()
    )
    cookie_table
    (return ())
  
(** Close all in memory data sessions for one session name.
    If the optional parameter [?session_name] (session name) is not present,
    only the session with default name is closed.
 *)
let close_all_data_sessions ?session_name 
    remove_session_data cookie_table wd =
  let fullsessname = make_fullsessname2 (string_of_url_path wd) session_name in
  close_all_data_sessions2 fullsessname remove_session_data cookie_table
  

let close_all_persistent_sessions2 fullsessname =
  Ocsipersist.iter_table
    (fun k (fullsessname2, old_exp, old_t, rk) -> 
      if fullsessname = fullsessname2 && old_t = None
      then close_persistent_session2 k >>= Lwt_unix.yield
      else return ()
    )
    persistent_cookies_table

(** Close all persistent sessions for one session name.
    If the optional parameter [?session_name] (session name) is not present,
    only the session with default name is closed.
 *)
let close_all_persistent_sessions ?session_name wd =
  let fullsessname = make_fullsessname2 (string_of_url_path wd) session_name in
  close_all_persistent_sessions2 fullsessname
  


(* Update the expiration date for all service sessions                      *)
let update_serv_exp fullsessname cookie_table 
    old_glob_timeout new_glob_timeout =
  Messages.debug 
    "--Eliom: Updating expiration date for all service sessions";
  match new_glob_timeout with
  | Some t when t <= 0.->
      (* We close all sessions but those with user defined timeout *)
      close_all_service_sessions2 fullsessname cookie_table
  | _ ->
    let now = Unix.time () in
    Cookies.fold
      (fun k (fullsessname2, table, expref, timeoutref) thr ->
        thr >>= fun () ->
        (if fullsessname = fullsessname2 && !timeoutref = None
        then
          let newexp = match !expref, old_glob_timeout, new_glob_timeout with
          | _, _, None -> None
          | None, _, Some t
          | Some _, None, Some t -> Some (now +. t)
          | Some oldexp, Some oldt, Some t -> Some (oldexp -. oldt +. t)
          in
          match newexp with
          | Some t when t <= now ->
              close_service_session2 cookie_table k
          | _ -> expref := newexp
        );
        Lwt_unix.yield ()
      )
      cookie_table
      (return ())

(* Update the expiration date for all in memory data sessions                *)
let update_data_exp
    fullsessname remove_session_data cookie_table 
    old_glob_timeout new_glob_timeout =
  Messages.debug 
    "--Eliom: Updating expiration date for all data sessions";
  match new_glob_timeout with
  | Some t when t <= 0.->
      (* We close all sessions but those with user defined timeout *)
      close_all_data_sessions2 fullsessname remove_session_data cookie_table
  | _ ->
    let now = Unix.time () in
    Cookies.fold
      (fun k (fullsessname2, expref, timeoutref) thr ->
        thr >>= fun () ->
        (if fullsessname = fullsessname2 && !timeoutref = None
        then
          let newexp = match !expref, old_glob_timeout, new_glob_timeout with
          | _, _, None -> None
          | None, _, Some t
          | Some _, None, Some t -> Some (now +. t)
          | Some oldexp, Some oldt, Some t -> Some (oldexp -. oldt +. t)
          in
          match newexp with
          | Some t when t <= now ->
              close_data_session2 remove_session_data cookie_table k
          | _ -> expref := newexp
        );
        Lwt_unix.yield ()
      )
      cookie_table
      (return ())


(* Update the expiration date for all sessions                               *)
let update_pers_exp fullsessname old_glob_timeout new_glob_timeout =
  Messages.debug 
    "--Eliom: Updating expiration date for all persistent sessions";
  match new_glob_timeout with
  | Some t when t <= 0.->
      (* We close all sessions but those with user defined timeout *)
      close_all_persistent_sessions2 fullsessname
  | _ ->
    let now = Unix.time () in
    Ocsipersist.iter_table
      (fun k (fullsessname2, old_exp, old_t, rk) -> 
        if fullsessname = fullsessname2 && old_t = None
        then
          let newexp = match old_exp, old_glob_timeout, new_glob_timeout with
          | _, _, None -> None
          | None, _, Some t
          | Some _, None, Some t -> Some (now +. t)
          | Some oldexp, Some oldt, Some t -> Some (oldexp -. oldt +. t)
          in
          match newexp with
          | Some t when t <= now ->
              close_persistent_session2 k
          | _ ->
              Ocsipersist.add
                persistent_cookies_table 
                k
                (fullsessname2, newexp, None, rk) >>= Lwt_unix.yield
        else return ()
      )
      persistent_cookies_table




(*****************************************************************************)
(* Table of timeouts for sessions *)

(* default timeout = the set in config file (or here) *)
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

type timeout_table =
    Tt of ((string * float option) list (* session name -> timeout *) *
             (string * timeout_table) list)

(* global timeout = timeout for the whole site (may be changed dynamically) *)
let (find_global_service_timeout, 
     find_global_data_timeout, 
     find_global_persistent_timeout, 
     set_global_service_timeout2, 
     set_global_data_timeout2, 
     set_global_persistent_timeout2) =

  let servtable = ref (Tt ([], [])) in
  let datatable = ref (Tt ([], [])) in
  let perstable = ref (Tt ([], [])) in

  let rec find fullsessname = function
    | (Tt (l, _), []) -> 
        (try
          Some (List.assoc fullsessname l)
        with Not_found -> None)
    | (ta, ""::l) -> find fullsessname (ta, l)
    | (Tt (_, r), a::l) ->
        (try
          find fullsessname ((List.assoc a r), l)
        with Not_found -> None)
  in
  let rec add fullsessname timeout = function
    | (Tt (tl, l), []) -> 
        (try
          let ol = List.remove_assoc fullsessname tl in
          Tt ((fullsessname, timeout)::ol, l)
        with Not_found ->
          Tt ((fullsessname, timeout)::tl, l))
    | (ta, ""::l) -> add fullsessname timeout (ta, l)
    | (Tt (tl, r), a::l) ->
        (try
          let (Tt (tt, ll), rr) = list_assoc_remove a r in
          Tt (tl, (a, add fullsessname timeout (Tt (tt, ll), l))::rr)
        with Not_found -> 
          Tt (tl, (a, (add fullsessname timeout (Tt ([], []), l)))::r))
  in
  (
   (* find_global_service_timeout *)
   (fun fullsessname wd -> 
     match find fullsessname (!servtable, wd) with
     | None -> get_default_service_timeout ()
     | Some t -> t),

   (* find_global_data_timeout *)
   (fun fullsessname wd -> 
     match find fullsessname (!datatable, wd) with
     | None -> get_default_data_timeout ()
     | Some t -> t),

   (* find_global_persistent_timeout *)
   (fun fullsessname wd -> 
     match find fullsessname (!perstable, wd) with
     | None -> get_default_persistent_timeout ()
     | Some t -> t),

   (* set_global_service_timeout2 *)
   (fun fullsessname ~recompute_expdates site_dir cookie_table t -> 
     if recompute_expdates
     then
       let oldt = 
         match find fullsessname (!servtable, site_dir) with
         | None -> get_default_service_timeout ()
         | Some oldt -> oldt
       in
       servtable := add fullsessname t (!servtable, site_dir);
       update_serv_exp fullsessname cookie_table oldt t
     else begin
       servtable := add fullsessname t (!servtable, site_dir);
       return ()
     end),

   (* set_global_data_timeout2 *)
   (fun fullsessname ~recompute_expdates 
       site_dir remove_session_data cookie_table t -> 
     if recompute_expdates
     then
       let oldt = 
         match find fullsessname (!datatable, site_dir) with
         | None -> get_default_data_timeout ()
         | Some oldt -> oldt
       in
       datatable := add fullsessname t (!datatable, site_dir);
       update_data_exp fullsessname remove_session_data cookie_table oldt t
     else begin
       datatable := add fullsessname t (!datatable, site_dir);
       return ()
     end),

   (* set_global_persistent_timeout *)
   (fun fullsessname ~recompute_expdates site_dir t -> 
     if recompute_expdates
     then
       let oldt = 
         match find fullsessname (!perstable, site_dir) with
         | None -> get_default_persistent_timeout ()
         | Some t -> t
       in
       perstable := add fullsessname t (!perstable, site_dir);
       update_pers_exp fullsessname oldt t
     else begin
       perstable := add fullsessname t (!perstable, site_dir);
       return ()
     end)
  )

let get_global_service_timeout ~session_name site_dir = 
  let fullsessname = 
    make_fullsessname2 (string_of_url_path site_dir) session_name 
  in
  find_global_service_timeout fullsessname site_dir

let get_global_data_timeout ~session_name site_dir = 
  let fullsessname = 
    make_fullsessname2 (string_of_url_path site_dir) session_name 
  in
  find_global_data_timeout fullsessname site_dir

let set_global_service_timeout ~session_name ~recompute_expdates site_dir
    cookie_table timeout = 
  let site_dir_string = string_of_url_path site_dir in
  let fullsessname = make_fullsessname2 site_dir_string session_name in
  set_global_service_timeout2
    fullsessname ~recompute_expdates site_dir cookie_table timeout

let set_global_data_timeout ~session_name ~recompute_expdates site_dir
    remove_session_data cookie_table timeout = 
  let site_dir_string = string_of_url_path site_dir in
  let fullsessname = make_fullsessname2 site_dir_string session_name in
  set_global_data_timeout2
    fullsessname ~recompute_expdates 
    site_dir remove_session_data cookie_table timeout

let get_global_persistent_timeout ~session_name site_dir =
  let fullsessname = 
    make_fullsessname2 (string_of_url_path site_dir) session_name 
  in
  find_global_persistent_timeout fullsessname site_dir

let set_global_persistent_timeout
    ~session_name ~recompute_expdates site_dir timeout = 
  let site_dir_string = string_of_url_path site_dir in
  let fullsessname = make_fullsessname2 site_dir_string session_name in
  set_global_persistent_timeout2
    fullsessname ~recompute_expdates site_dir timeout



(*****************************************************************************)
(** Parsing global configuration for Eliommod: *)
open Simplexmlparser

let servicesessiongcfrequency = ref (Some 3600.)
let datasessiongcfrequency = ref (Some 3600.)
let persistentsessiongcfrequency = ref (Some 86400.)
let set_servicesessiongcfrequency i = servicesessiongcfrequency := i
let set_datasessiongcfrequency i = datasessiongcfrequency := i
let get_servicesessiongcfrequency () = !servicesessiongcfrequency
let get_datasessiongcfrequency () = !datasessiongcfrequency
let set_persistentsessiongcfrequency i = persistentsessiongcfrequency := i
let get_persistentsessiongcfrequency () = !persistentsessiongcfrequency

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
(* Exception handler for the site                                            *)
type handler_tree =
    Exntree of
      ((server_params -> exn -> result_to_send Lwt.t) option * 
         ((string * handler_tree) list))

let def_handler sp e = fail e

let handle_site_exn, set_site_handler, init_site_handler =
  let tree = ref (Exntree ((Some def_handler), [])) in
  ((fun exn (ri, si, _, aci) tables ->
    let rec find_handler h current_dir site_dir tree path = 
      match tree, path with
      | (Exntree (Some h, _)), [] -> (h, site_dir)
      | (Exntree (None, _)), [] -> (h, site_dir)
      | (Exntree (Some h, hl)), a::l -> 
          (try
            let tree2 = List.assoc a hl in
            find_handler h (current_dir@[a]) current_dir tree2 l
          with Not_found -> (h, current_dir))
      | (Exntree (None, hl)), a::l -> 
          (try
            let tree2 = List.assoc a hl in
            find_handler h (current_dir@[a]) site_dir tree2 l
          with Not_found -> (h, site_dir))
    in 
    let h, wd = find_handler def_handler [] [] !tree ri.ri_path in
    h (make_server_params wd tables aci ri [] si None) exn >>=
    (fun r ->
      return (r,
              wd))),
   (fun dir handler ->
     let rec add = function
       | [] -> Exntree (Some handler, [])
       | a::l -> Exntree (None, [(a, add l)])
     in
     let rec aux = function
       | (Exntree (h, hl), []) -> Exntree ((Some handler), hl)
       | (Exntree (h, hl)), a::l -> 
           try
             let ht,ll = list_assoc_remove a hl in
             Exntree (h, (a, aux (ht, l))::ll)
           with Not_found -> Exntree (h, (a, add l)::hl)
     in tree := aux (!tree, dir)
   ),
   (fun dir -> 
     let rec add = function
       | [] -> Exntree (Some def_handler, [])
       | a::l -> Exntree (None, [(a, add l)])
     in
     let rec aux = function
       | (Exntree (None, hl), []) -> Exntree ((Some def_handler), hl)
       | ((Exntree (h, hl)) as i, []) -> i
       | (Exntree (h, hl)), a::l -> 
           try
             let ht,ll = list_assoc_remove a hl in
             Exntree (h, (a, aux (ht, l))::ll)
           with Not_found -> Exntree (h, (a, add l)::hl)
     in tree := aux (!tree, dir)
   ))


(*****************************************************************************)
(* Create the list of cookies to send to the browser or to unset             *)
(* (from cookie_info)                                                        *)

let compute_session_cookies_to_send
    (service_cookie_info, 
     data_cookie_info, 
     pers_cookies_info) site_dir endlist =
  let getservvexp (name, (old, newi)) =
    return 
      (let newinfo =
        match !newi with
        | SCNo_data
        | SCData_session_expired -> None
        | SC (v, _, _, _, exp) -> Some (v, !exp)
      in (name, old, newinfo))
  in
  let getdatavexp (name, v) =
    if Lazy.lazy_is_val v
    then 
      return
        (let (old, newi) = Lazy.force v in
        let newinfo =
          match !newi with
          | SCNo_data
          | SCData_session_expired -> None
          | SC (v, _, _, exp) -> Some (v, !exp)
        in (name, old, newinfo))
    else fail Not_found
  in
  let getpersvexp (name, v) =
    if Lazy.lazy_is_val v
    then 
      Lazy.force v >>= fun (old, newi) ->
      return
        (let oldinfo =
          match old with
          | None -> None
          | Some (v, _, _) -> Some v
        in
        let newinfo =
          match !newi with
          | SCNo_data
          | SCData_session_expired -> None
          | SC (v, _, exp) -> Some (v, !exp)
        in (name, oldinfo, newinfo))
    else fail Not_found
  in
  let ch_exp = function
    | None | Some None -> None
    | Some a -> a
  in
  let aux f cookiename l endlist =
    endlist >>= fun endlist ->
    Lwt_util.fold_left
      (fun beg v ->
        catch
          (fun () ->
            f v >>= fun (name, old, newc) ->
            return
              (match old, newc with
              | None, None -> beg
              | Some _, None ->
                  (Unset (Some site_dir, 
                          [make_full_cookie_name cookiename name]))::beg
                  (* the path is always site_dir because the cookie cannot 
                     have been unset by a service outside this site directory *)
              | None, Some (v, exp) -> 
                  (Set (Some site_dir, (ch_exp exp), 
                        [make_full_cookie_name cookiename name, v]))::beg
              | Some oldv, Some (newv, exp) -> 
                  if exp = None && oldv = newv
                  then beg
                  else (Set (Some site_dir, (ch_exp exp),
                             [make_full_cookie_name cookiename name, newv]))
                    ::beg
              )
          )
          (function 
            | Not_found -> return beg
            | e -> fail e)
      )
      endlist
      l
  in
  aux getpersvexp persistentcookiename !pers_cookies_info
    (aux getdatavexp datacookiename !data_cookie_info 
       (aux getservvexp servicecookiename !service_cookie_info 
          (return endlist)))
  


let compute_cookies_to_send site_dir all_cookie_info cookies_set_by_page =

  (* We change the cookies set by user: *)
  let cookies_set_by_page =
    let change_pathopt = function
      | None -> Some site_dir 
            (* Not possible to set a cookie for another site (?) *)
      | Some p -> Some (site_dir@p)
    in
    List.map 
      (function
        | Set (pathopt, expopt, cl) -> 
            Set (change_pathopt pathopt, expopt, cl)
        | Unset (pathopt, cl) -> 
            Unset (change_pathopt pathopt, cl)
      )
      cookies_set_by_page
  in

  compute_session_cookies_to_send all_cookie_info site_dir cookies_set_by_page
  

(** Compute new ri.ri_cookies value
    from an old ri.ri_cookies and all_cookie_info 
    as if it had been sent by the browser *)
let compute_new_cookies
    now
    ricookies
    (service_cookie_info, data_cookie_info, pers_cookie_info)
    endlist =
  let rec remove_list f l from =
    match l with
    | [] -> from
    | a::l -> remove_list f l (List.remove_assoc (f a) from)
  in
  let endlist =
    List.fold_left
      (fun beg v ->
        match v with
        | Set (_, None, l)  ->
            l@(remove_list fst l beg)
        | Set (_, Some t, l) when t>now ->
            l@(remove_list fst l beg)
        | Unset (_, l) -> remove_list id l beg
        | Set (_, _, l) -> remove_list fst l beg
      )
      ricookies
      endlist
  in
  let ric = 
    List.fold_left
      (fun beg (n, (_, v)) -> match !v with
      | SCData_session_expired
      | SCNo_data -> List.remove_assoc n beg
      | SC (v, _, _, _, _) -> (n, v)::(List.remove_assoc n beg)
      )
      endlist
      !service_cookie_info
  in
  let ric = 
    List.fold_left
      (fun beg (n, v) -> 
        if Lazy.lazy_is_val v
        then 
          let (_, v) = Lazy.force v in
          match !v with
          | SCData_session_expired
          | SCNo_data -> List.remove_assoc n beg
          | SC (v, _, _, _) -> (n, v)::(List.remove_assoc n beg)
        else beg
      )
      ric
      !data_cookie_info
  in
  let ric = 
    List.fold_left
      (fun beg (n, v) -> 
        beg >>= fun beg ->
        if Lazy.lazy_is_val v
        then 
          Lazy.force v >>= fun (_, v) ->
          match !v with
          | SCData_session_expired
          | SCNo_data -> return (List.remove_assoc n beg)
          | SC (v, _, _) -> return ((n, v)::(List.remove_assoc n beg))
        else return beg
      )
      (return ric)
      !pers_cookie_info
  in
  ric


(* removes one cookielist form another *)
let cookie_remove_list l from =

  let rec find_names cont = function
    | [] -> cont
    | Set (_, _, l)::ll -> find_names (List.map fst l) ll
    | Unset (_, l)::ll -> find_names cont ll
  in

  let rec cookie_remove_name from name =
    List.fold_left
      (fun beg v ->
        match v with
        | Set (a, b, l) ->
            let newl = list_remove_all_assoc name l in
            if newl = []
            then beg
            else (Set (a, b, newl))::beg
        | Unset (a, l) ->
            let newl = list_remove_all name l in
            if newl = []
            then beg
            else Unset (a, newl)::beg
      )
      []
      from
  in

  let names = find_names [] l in
  List.fold_left cookie_remove_name from names





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
    tables =
  
  catch
    (fun () -> generate_page now info tables)
    (fun e -> handle_site_exn e info tables) >>=
  (fun ((result, site_dir) as res) ->
    
    (* Update service expiration date and value *)
    List.iter
      
      (fun (name, (oldvalue, newr)) ->
        (* catch fun () -> *)
        match !newr with
        | SCData_session_expired
        | SCNo_data -> () (* The cookie has been removed *)
        | SC (newvalue, newsesstable, newtimeout, newexp, newcookieexp) ->
            newexp :=
              match !newtimeout with
              | None -> 
                  let globaltimeout = 
                    find_global_service_timeout name site_dir 
                  in
                  (match globaltimeout with
                  | None -> None
                  | Some t -> Some (t +. now))
              | Some None -> None
              | Some (Some t) -> Some (t +. now)
      )
      
      !service_cookies_info;
    
    (* Update "in memory data" expiration date and value *)
    List.iter
      
      (fun (name, v) ->
        if Lazy.lazy_is_val v (* Only sessions that have been used *)
        then 
          let (oldvalue, newr) = Lazy.force v
          in
          match !newr with
          | SCData_session_expired
          | SCNo_data -> () (* The cookie has been removed *)
          | SC (newvalue, newtimeout, newexp, newcookieexp) ->
              newexp :=
                match !newtimeout with
                | None -> 
                    let globaltimeout = 
                      find_global_data_timeout name site_dir 
                    in
                    (match globaltimeout with
                    | None -> None
                    | Some t -> Some (t +. now))
                | Some None -> None
                | Some (Some t) -> Some (t +. now)
      )
      
      !data_cookies_info;
    
    
    (* Update persistent expiration date, user timeout and value *)
    Lwt_util.iter

      (fun (name, v) ->
        if Lazy.lazy_is_val v
        then begin
          Lazy.force v >>= fun (oldvalue, newr) ->
          match !newr with
          | SCData_session_expired
          | SCNo_data -> (* The cookie has been removed *)
              return ()
          | SC (newvalue, newtimeout, newcookieexp) ->
              let newexp =
                match !newtimeout with
                | None -> 
                    let globaltimeout = 
                      find_global_persistent_timeout name site_dir 
                    in
                    (match globaltimeout with
                    | None -> None
                    | Some t -> Some (t +. now))
                | Some None -> None
                | Some (Some t) -> Some (t +. now)
              in
              match oldvalue with
              | Some (oldv, oldti, oldexp) when
                  (oldexp = newexp && 
                   oldti = !newtimeout &&
                   oldv = newvalue) -> return () (* nothing to do *)
              | Some (oldv, oldti, oldexp) when oldv = newvalue ->
                  catch
                    (fun () ->
                      Ocsipersist.replace_if_exists
                        persistent_cookies_table 
                        newvalue
                        (name, newexp, !newtimeout, Int64.zero))
                    (function
                      | Not_found -> return ()
                            (* someone else closed the session *)
                      | e -> fail e)
              | _ ->
                  Ocsipersist.add
                    persistent_cookies_table 
                    newvalue
                    (name, newexp, !newtimeout, Int64.zero)
        end
        else return ()
      )
      
      !pers_cookies_info
      
      
      >>= fun () ->
        
        return res)
    



exception Eliom_retry_with of 
  (request_info * 
     sess_info * 
     cookieslist (* user cookies set by previous pages *) *
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
    ((global_tables, _, _, _) as tables)
    =
  let rec find_aux e = function
    | [] -> fail e
    | (fullsessname, (_, r))::l -> 
        match !r with
        | SCData_session_expired
        | SCNo_data (* cookie removed *) -> find_aux e l
        | SC (_, service_session_tables_ref, _, _, _) ->
            catch
              (fun () ->
                find_service
                  now
                  !service_session_tables_ref
                  (Some fullsessname)
                  (tables,
                   all_cookie_info,
                   ri,
                   si))
              (function
                | Ocsigen_404 | Eliom_Wrong_parameter as e -> find_aux e l
                | e -> fail e)
  in

  (catch
     (fun () -> 
        Messages.debug 
          ("--Eliom: I'm looking for "^(string_of_url_path ri.ri_path)^
           " in the session table:");
        find_aux Ocsigen_404 !service_cookies_info
      )
      (function 
        | Ocsigen_404 | Eliom_Wrong_parameter -> 
            catch (* ensuite dans la table globale *)
              (fun () -> 
                Messages.debug "--Eliom: I'm searching in the global table:";
                find_service 
                  now
                  global_tables
                  None
                  (tables,
                   all_cookie_info,
                   ri,
                   si))
              (function
                | Ocsigen_404 | Eliom_Wrong_parameter as exn -> 
                    (* si pas trouvé avec, on essaie sans l'état *)
                    (match si.si_state_info with
                    | (None, None) -> fail exn
                    | (g, Some _) -> 
                        (* There was a POST state. 
                           We remove it, and remove POST parameters.
                         *)
                        Messages.debug 
                          "--Eliom: Link to old. I will try without POST parameters:";
                        fail (Eliom_retry_with 
                                ({ri with 
                                  ri_post_params = lazy (return []);
                                  ri_method = Http_frame.Http_header.GET
                                }, 
                                 {si with
                                  si_nonatt_info= (None, None);
                                  si_state_info= (g, None);
                                  si_exn= Eliom_Link_too_old::si.si_exn
                                },
                                 cookies_to_set,
                                 all_cookie_info
                                ))
                    | (Some _, None) -> 
                        (* There was a GET state, but no POST state. 
                           We remove it with its parameters, 
                           and remove POST parameters.
                         *)
                        Messages.debug 
                          "--Eliom: Link to old. I will try without GET state parameters and POST parameters:";
                        fail (Eliom_retry_with 
                                ({ri with 
                                  ri_get_params = 
                                  lazy si.si_other_get_params;
                                  ri_post_params = lazy (return []);
                                  ri_method = Http_frame.Http_header.GET
                                },
                                 {si with
                                  si_nonatt_info=(None, None);
                                  si_state_info=(None, None);
                                  si_other_get_params=[];
                                  si_exn= Eliom_Link_too_old::si.si_exn
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
    ((global_tables, _, _, _) as tables) 
    =
  
  let rec find_aux = function
    | [] -> (find_naservice now global_tables si.si_nonatt_info,
             global_tables,
             None)
    | (fullsessname, (_, r))::l -> 
        match !r with
        | SCNo_data
        | SCData_session_expired -> find_aux l
        | SC (_, service_session_tables_ref, _, _, _) ->
            try
              ((find_naservice
                  now !service_session_tables_ref si.si_nonatt_info),
               !service_session_tables_ref, 
               Some fullsessname)
            with Not_found -> find_aux l
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
      | None, None -> assert false
      | Some _ as g, Some _ -> (* (Some, Some) or (_, Some) ? *)
          Messages.debug 
            "--Eliom: Link too old to a non-attached POST coservice. I will try without POST parameters:";
          fail (Eliom_retry_with
                  ({ri with 
                    ri_post_params = lazy (return []);
                    ri_method = Http_frame.Http_header.GET
                  },
                   {si with
                    si_nonatt_info=(g, None);
                    si_state_info=(None, None);
                    si_exn= Eliom_Link_too_old::si.si_exn
                  },
                   cookies_to_set,
                   all_cookie_info))
      | _ ->
          Messages.debug 
            "--Eliom: Link too old. I will try without non-attached parameters:";
          change_request_info
            {ri with 
             ri_get_params = lazy si.si_other_get_params;
             ri_post_params = lazy (return []);
             ri_method = Http_frame.Http_header.GET
           } 
            si.si_config_file_charset
            >>=
          (fun (ri', si') -> 
            fail (Eliom_retry_with 
                    (ri',
                     {si' with
                      si_exn= Eliom_Link_too_old::si.si_exn (* not si' *)
                    },
                     cookies_to_set,
                     all_cookie_info)))
  ) >>=
  (fun ((max_use, expdate, naservice, site_dir), 
        tablewhereithasbeenfound,
        fullsessname) ->
    (naservice
       (make_server_params 
          site_dir
          tables
          all_cookie_info
          ri
          []
          si
          fullsessname)) >>=
    (fun r -> 
      Messages.debug
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
      return (r, site_dir)))





let gen page_tree charset ri =
  let now = Unix.time () in
  let rec gen_aux ((ri, si, old_cookies_to_set, all_cookie_info) as info) =
    let genfun = 
      match si.si_nonatt_info with
      | None, None ->
          
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
          page_tree >>= fun (result_to_send, site_dir) ->
          
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
                cookies_set_by_page@
                (cookie_remove_list 
                   cookies_set_by_page old_cookies_to_set)
              in

              force ri.ri_post_params >>=
              (fun ripp ->
                (match si.si_nonatt_info, si.si_state_info, ri.ri_method with
                | (None, None), (None, None), Http_frame.Http_header.GET ->
                    compute_cookies_to_send 
                      site_dir
                      all_cookie_info
                      all_user_cookies
                    >>= fun all_new_cookies ->
                    return
                      (Ext_found
                         {res_cookies= all_new_cookies;
                          res_send_page=
                          Predefined_senders.send_empty ~content:();
                          res_headers=[];
                          res_code=Some 204; (* No content *)
                          res_lastmodified=None; 
                          (* No date => proxies use etag *)
                          res_etag=None;
                          res_charset=None;
                          res_filter=None})
                      
                | _ ->

                    compute_new_cookies 
                      now 
                      (Lazy.force ri.ri_cookies)
                      all_cookie_info all_user_cookies
                    >>= fun ric ->
                    
                    (match 
                      si.si_nonatt_info, si.si_state_info, ri.ri_method 
                    with
                    | (Some _, None), (_, None), Http_frame.Http_header.GET ->
                             (* no post params, GET na coservice *)
                        change_request_info
                          {ri with
                           ri_get_params = lazy si.si_other_get_params;
                           ri_cookies= lazy ric
                         }
                          si.si_config_file_charset >>=
                        (fun (ri', si') ->
                          fail
                            (* Ext_retry_with or Eliom_retry_with? *)
                            (Eliom_retry_with 
                               (ri',
                                {si_other_get_params= si'.si_other_get_params;
                                 si_all_get_params= si.si_all_get_params;
                                 si_all_post_params= si.si_all_post_params;
                                 si_service_session_cookies= si'.si_service_session_cookies;
                                 si_data_session_cookies= si'.si_data_session_cookies;
                                 si_persistent_session_cookies= 
                                 si'.si_persistent_session_cookies;
                                 si_nonatt_info= (None, None);
                                 si_state_info= si'.si_state_info;
                                 si_exn = exnlist@si.si_exn; (* not si' *)
                                 si_config_file_charset = 
                                 si.si_config_file_charset}, (* verifier *)
                                all_user_cookies,
                                all_cookie_info)))
                    | (None, None), (_, None), Http_frame.Http_header.GET ->
                        (* no post params, GET attached coservice *)
                        fail
                          (* Ext_retry_with or Eliom_retry_with? *)
                          (Eliom_retry_with 
                             ({ri with
                               ri_get_params = lazy si.si_other_get_params;
                               ri_cookies= lazy ric
                             },
                              {si_other_get_params= [];
                               si_all_get_params= si.si_all_get_params;
                               si_all_post_params= si.si_all_post_params;
                               si_service_session_cookies= 
                               getcookies servicecookiename ric;
                               si_data_session_cookies= 
                               getcookies datacookiename ric;
                               si_persistent_session_cookies= 
                               getcookies persistentcookiename ric;
                               si_nonatt_info= si.si_nonatt_info;
                               si_state_info= (None, None);
                               si_exn = exnlist@si.si_exn;
                               si_config_file_charset = 
                               si.si_config_file_charset},
                              all_user_cookies,
                              all_cookie_info))
                    | (_, Some _), (_, _), _ ->
                        (* retry without POST params *)
                        change_request_info
                          {ri with
                           ri_get_params = lazy si.si_other_get_params;
                           ri_post_params = lazy (return []);
                           ri_method = Http_frame.Http_header.GET;
                           ri_cookies= lazy ric
                         }
                          si.si_config_file_charset >>=
                        (fun (ri', si') ->
                          fail
                            (* Ext_retry_with or Eliom_retry_with? *)
                            (Eliom_retry_with 
                               (ri',
                                {si_other_get_params= si'.si_other_get_params;
                                 si_all_get_params= si.si_all_get_params;
                                 si_all_post_params= si.si_all_post_params;
                                 si_service_session_cookies= 
                                 si'.si_service_session_cookies;
                                 si_data_session_cookies= 
                                 si'.si_data_session_cookies;
                                 si_persistent_session_cookies= 
                                 si'.si_persistent_session_cookies;
                                 si_nonatt_info= si'.si_nonatt_info;
                                 si_state_info= si'.si_state_info;
                                 si_exn = exnlist@si.si_exn; (* not si' *)
                                 si_config_file_charset = 
                                 si.si_config_file_charset}, (* verifier *)
                                all_user_cookies,
                                all_cookie_info)))
                    | _ ->
                        (* retry without POST params *)
                        fail
                          (* Ext_retry_with or Eliom_retry_with? *)
                          (Eliom_retry_with 
                             ({ri with
                               ri_post_params = lazy (return []);
                               ri_method = Http_frame.Http_header.GET;
                               ri_cookies= lazy ric
                             },
                              {si_other_get_params= si.si_other_get_params;
                               si_all_get_params= si.si_all_get_params;
                               si_all_post_params= si.si_all_post_params;
                               si_service_session_cookies= 
                               getcookies servicecookiename ric;
                               si_data_session_cookies= 
                               getcookies datacookiename ric;
                               si_persistent_session_cookies= 
                               getcookies persistentcookiename ric;
                               si_nonatt_info= ((fst si.si_nonatt_info), None);
                               si_state_info= ((fst si.si_state_info), None);
                               si_exn = exnlist@si.si_exn;
                               si_config_file_charset = 
                               si.si_config_file_charset},
                              all_user_cookies,
                              all_cookie_info)))))

          | EliomResult res ->

              compute_cookies_to_send 
                site_dir 
                all_cookie_info
                (res.res_cookies@
                 (cookie_remove_list res.res_cookies old_cookies_to_set))
              >>= fun all_new_cookies ->

              return 
                (Ext_found 
                   {res with
                    res_cookies= all_new_cookies})
      )
      (function
        | Eliom_Typing_Error l -> 
            return (Ext_found
                      {res_cookies= old_cookies_to_set;
                       res_send_page=
                       (Predefined_senders.send_xhtml_page 
                          ~content:(Error_pages.page_error_param_type l));
                       res_headers=
                       Predefined_senders.dyn_headers;
                       res_code=None;
                       res_lastmodified=None;
                       res_etag=None;
                       res_charset= Error_pages.charset;
                       res_filter=None})
	| Eliom_Wrong_parameter -> 
            (force ri.ri_post_params) >>=
            (fun ripp ->
	      return (Ext_found 
                        {res_cookies= old_cookies_to_set;
                         res_send_page=
                         (Predefined_senders.send_xhtml_page 
                            ~content:(Error_pages.page_bad_param 
                                      (List.map fst ripp)));
                         res_headers=
                         Predefined_senders.dyn_headers;
                         res_code=None;
                         res_lastmodified=None;
                         res_etag=None;
                         res_charset= Error_pages.charset;
                         res_filter=None}))
	| Ocsigen_404 -> return (Ext_not_found Ocsigen_404)
        | Eliom_retry_with a -> gen_aux a
	| e -> fail e)

  in
  change_request_info ri charset >>= fun (ri, si) ->
  let (all_cookie_info, closedsessions) =
    get_cookie_info now
      page_tree 
      si.si_service_session_cookies
      si.si_data_session_cookies
      si.si_persistent_session_cookies 
  in
  let exn = compute_exn closedsessions in
  gen_aux (ri, {si with si_exn=exn}, [], all_cookie_info)


(*****************************************************************************)
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
                    (fun ((i, (_, (_, expdate, _, _))) as a) foll -> 
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
        (fun k (_, expdate,_,_) thr -> 
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
let service_session_gc ((servicetable,
                         naservicetable, 
                         contains_services_with_timeout, 
                         contains_naservices_with_timeout), 
                        service_cookie_table,
                        _,
                        _) =
  match get_servicesessiongcfrequency () with
  | None -> () (* No garbage collection *)
  | Some t ->
      let rec f () = 
        Lwt_unix.sleep t >>= 
        (fun () ->
          let now = Unix.time () in
          Messages.debug "--Eliom: GC of service sessions";
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
            Cookies.fold
              (fun k (sessname,
                      ((servicetable,
                        naservicetable, 
                        contains_services_with_timeout, 
                        contains_naservices_with_timeout) as tables), 
                      exp, _) thr -> 
                        thr >>= fun () ->
                          (match !exp with
                          | Some exp when exp < now -> 
                              Cookies.remove service_cookie_table k;
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
                                  Cookies.remove service_cookie_table k;
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
let data_session_gc (_,
                     _,
                     data_cookie_table,
                     (remove_session_data,
                      not_bound_in_data_tables)) =
  match get_datasessiongcfrequency () with
  | None -> () (* No garbage collection *)
  | Some t ->
      let rec f () = 
        Lwt_unix.sleep t >>= fun () ->
        let now = Unix.time () in
        Messages.debug "--Eliom: GC of session data";
        (* private continuation tables: *)
        Cookies.fold
          (fun k (sessname, exp, _) thr -> 
            thr >>= fun () ->
              (match !exp with
              | Some exp when exp < now -> 
                  Cookies.remove data_cookie_table k;
                  !remove_session_data k;
                  return ()
              | _ -> 
                  if !not_bound_in_data_tables k
                  then 
                    Cookies.remove data_cookie_table k;
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
          Messages.debug "--Eliom: GC of persistent sessions";
          (Ocsipersist.iter_table
             (fun k (_, exp, _, _) -> 
               (match exp with
               | Some exp when exp < now -> 
                   remove_from_all_persistent_tables k
               | _ -> return ())
             )
             persistent_cookies_table))
          >>=
        f
      in ignore (f ())
      


(*****************************************************************************)
(** Module loading *)
let config = ref []

let load_eliom_module pages_tree path cmo content =
  config := content;
  begin_load_eliom_module ();
  absolute_change_hostdir (pages_tree, path);
  init_site_handler path;
  (try
    Dynlink.loadfile cmo
  with Dynlink.Error e -> 
    end_load_eliom_module ();
    raise (Eliom_error_while_loading_site
             ("(eliommod extension) "^cmo^": "^
              (Dynlink.error_message e))));
  (* absolute_change_hostdir save_current_dir; *)
  end_load_eliom_module ();
  config := []



(*****************************************************************************)
(** Parsing of config file for each site: *)
let parse_config page_tree path = 
  let rec parse_module_attrs file = function
    | [] -> (match file with
        None -> 
          raise (Error_in_config_file
                   ("Missing module attribute in <eliom>"))
      | Some s -> s)
    | ("module", s)::suite ->
        (match file with
          None -> parse_module_attrs (Some s) suite
        | _ -> raise (Error_in_config_file
                        ("Duplicate attribute file in <eliom>")))
    | (s, _)::_ ->
        raise
          (Error_in_config_file ("Wrong attribute for <eliom>: "^s))
  in function
    | Element ("eliom", atts, content) -> 
          let file = parse_module_attrs None atts in
          load_eliom_module page_tree path file content
    | Element (t, _, _) -> 
        raise (Extensions.Bad_config_tag_for_extension t)
    | _ -> raise (Error_in_config_file "(Eliommod extension)")


(*****************************************************************************)
(** Function to be called at the beginning of the initialisation phase *)
let start_init () =
  begin_current_host_dir ()

(** Function to be called at the end of the initialisation phase *)
let end_init () =
  end_current_hostdir ();
  verify_all_registered ()                                

(** Function that will handle exceptions during the initialisation phase *)
let handle_init_exn = function
  | Eliom_duplicate_registration s -> 
      ("Fatal - Eliom: Duplicate registration of url \""^s^
       "\". Please correct the module.")
  | Eliom_there_are_unregistered_services l ->
      ("Fatal - Eliom: "^
       (match l with
       | [] -> "<none(??)>"
       | [a] -> "One service or coservice has not been registered on URL \""
           ^a^"\"."
       | a::ll -> "Some services or coservices have not been registered \
             on URLs: "^
             (List.fold_left
                (fun beg v -> beg^", "^v)
                a
                ll
             )^".")^
         "\nPlease correct your modules.")
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
(** table of page trees *)

let page_tree_table = ref []

let find k = List.assoc k !page_tree_table

let add k a = page_tree_table := (k,a)::!page_tree_table

(*****************************************************************************)
(** extension registration *)
let _ = R.register_extension
    ((fun hostpattern -> 
      let page_tree = 
        try 
          find hostpattern
        with Not_found ->
          let n = new_pages_tree () in
          add hostpattern n;
          service_session_gc n;
          data_session_gc n;
          n
      in
      (gen page_tree, 
       parse_config page_tree)),
     start_init,
     end_init,
     handle_init_exn)

let _ = persistent_session_gc ()



(*****************************************************************************)
(* Iterators or sessions *)

  (** Iterator on service sessions *)
  let iter_service_sessions cookie_table hostdir f =
    Cookies.fold
      (fun k v thr -> 
        thr >>= fun () ->
        f (k, v, hostdir) >>=
        Lwt_unix.yield
      )
      cookie_table
      (return ())
      
  
  (** Iterator on data sessions *)
  let iter_data_sessions cookie_table hostdir f =
    Cookies.fold
      (fun k v thr -> 
        thr >>= fun () ->
          f (k, v, hostdir) >>=
          Lwt_unix.yield
      )
      cookie_table
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
  let fold_service_sessions cookie_table hostdir f beg =
    Cookies.fold
      (fun k v thr -> 
        thr >>= fun res1 ->
        f (k, v, hostdir) res1 >>= fun res ->
        Lwt_unix.yield () >>= fun () ->
        return res
      )
      cookie_table
      (return beg)
      
  
  (** Iterator on data sessions *)
  let fold_data_sessions cookie_table hostdir f beg =
    Cookies.fold
      (fun k v thr -> 
        thr >>= fun res1 ->
        f (k, v, hostdir) res1 >>= fun res ->
        Lwt_unix.yield () >>= fun () ->
        return res
      )
      cookie_table
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
  Cookies.length sp.sp_cookie_service_table

let number_of_data_sessions ~sp = 
  Cookies.length sp.sp_cookie_data_table

let number_of_tables () =
  List.length !counttableelements

let number_of_table_elements () =
  List.map (fun f -> f ()) !counttableelements

let number_of_persistent_sessions () = 
  Ocsipersist.length persistent_cookies_table


