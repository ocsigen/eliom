(* Ocsigen
 * http://www.ocsigen.org
 * Module ocsipersist.ml
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


(** Module Ocsipersist: persistent data *)

open Ocsidbm
open Ocsidbmtypes
open Lwt

(** Data are divided into stores. 
   Create one store for your project, where you will save all your data.
 *)
type store = string

exception Ocsipersist_error

let socketname = "socket"

(*****************************************************************************)
(** Internal functions: storage directory *)

open Simplexmlparser
(** getting the directory from config file *)
let rec parse_global_config d = function
      [] -> d
    | (Element ("store", [("dir", s)], []))::ll -> 
        (match d with
        | None, dbm -> parse_global_config ((Some s), dbm) ll
        | (Some _), _ -> raise (Extensions.Error_in_config_file 
                                  ("Ocsipersist: Duplicate <store> tag")))
    | (Element ("ocsidbm", [("name", s)], []))::ll -> 
        (match d with
        | a, None -> parse_global_config (a, (Some s)) ll
        | _, Some _ -> raise (Extensions.Error_in_config_file 
                             ("Ocsipersist: Duplicate <ocsidbm> tag")))
    | (Element (tag,_,_))::ll -> parse_global_config d ll
    | _ -> raise (Extensions.Error_in_config_file ("Unexpected content inside Ocsipersist config"))

let (directory, ocsidbm) = 
  let (store, ocsidbm) =
    parse_global_config (None, None) (Extensions.get_config ()) 
  in
  ((match store with
    None -> (Ocsiconfig.get_datadir ())^"/ocsipersist"
  | Some d -> d),
   (match ocsidbm with
     None -> (Ocsiconfig.get_bindir ())^"/ocsidbm"
   | Some d -> d))


(*****************************************************************************)
(** Communication with the DB server *)

let try_connect sname socket =
  catch
    (fun () ->
      Lwt_unix.connect (Lwt_unix.Plain socket) (Unix.ADDR_UNIX sname)
    )
    (fun _ ->
      if Unix.fork () = 0 
      then begin
        Messages.warning ("Launching a new Ocsidbm process: "^ocsidbm^" on directory "^directory^".");
        Unix.execv ocsidbm 
          (match Ocsiconfig.get_pidfile () with
            None -> [|"ocsidbm"; directory|]
          | Some p -> [|"ocsidbm"; directory; p|]);
        exit 0
      end
      else Lwt_unix.sleep 0.5 >>= 
        (fun () -> Lwt_unix.connect
            (Lwt_unix.Plain socket) (Unix.ADDR_UNIX sname))
    ) >>=
  (fun () -> return (Lwt_unix.Plain socket))

let indescr =
  catch
    (fun () -> 
      Lwt_unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 >>=
      try_connect (directory^"/"^socketname))
    (fun e -> Messages.errlog ("Cannot connect to Ocsidbm. Will continue without Persistent session support. Error message is: "^(Printexc.to_string e));
      fail e)

let inch = indescr >>= (fun r -> return (Lwt_unix.in_channel_of_descr r))
let outch = indescr >>= (fun r -> return (Lwt_unix.out_channel_of_descr r))

let send =
  let previous = ref (return Ok) in
  fun v ->
    catch
      (fun () -> !previous)
      (fun _ -> return Ok) >>=
    (fun _ ->
      inch >>= 
      (fun inch ->
        outch >>= 
        (fun outch ->
          previous :=
            (Lwt_unix.output_value outch v >>=
             (fun () -> 
               Lwt_unix.flush outch >>=
               (fun () -> Lwt_unix.input_value inch)));
          !previous)))

let db_get (store, name) =
  send (Get (store, name)) >>=
  (function 
    | Value v -> return v
    | Dbm_not_found -> fail Not_found
    | _ -> fail Ocsipersist_error)

let db_remove (store, name) =
  send (Remove (store, name)) >>=
  (function 
    | Ok -> return ()
    | _ -> fail Ocsipersist_error)

let db_replace (store, name) value = 
  send (Replace (store, name, value)) >>=
  (function 
    | Ok -> return ()
    | _ -> fail Ocsipersist_error)






(*****************************************************************************)
(** Public functions: *)

(** Type of persistent data *)
type 'a t = store * string
      
let open_store name : store = name

let make_persistent_lazy ~store ~name ~default =
  let pvname = (store, name) in
  (catch
     (fun () -> db_get pvname >>= (fun _ -> return ()))
     (function
       | Not_found -> 
           let def = Marshal.to_string (default ()) []
           in db_replace pvname def
       | e -> fail e)) >>=
   (fun () -> return pvname)
      
let make_persistent ~store ~name ~default = 
  make_persistent_lazy ~store ~name ~default:(fun () -> default)
    
let get (pvname : 'a t) : 'a =
  db_get pvname >>=
  (fun r -> return (Marshal.from_string r 0))
    
let set pvname v =
  let data = Marshal.to_string v [] in
  db_replace pvname data



(** Type of persistent tables *)
type 'value table = string

module Tableoftables = 
  struct 
    let empty = []
    let add v t = v::t
    let fold = List.fold_left
  end

let tableoftables = ref Tableoftables.empty

let open_table name = 
  tableoftables := Tableoftables.add name !tableoftables;
  name
    
let find table key =
  db_get (table, key) >>=
  (fun v -> return (Marshal.from_string v 0))

let add table key value =
  let data = Marshal.to_string value [] in
  db_replace (table, key) data

let remove table key =
  db_remove (table, key)

let remove_from_all_tables key =
  Tableoftables.fold
    (fun thr t -> thr >>= (fun () -> db_remove (t, key) >>= Lwt_unix.yield))
    (return ())
    !tableoftables

  
(* iterator: we must use a separate connexion *)
exception Exn1
let iter_table =
  fun f table ->
    let first = Marshal.to_string (Firstkey table) [] in
    let firstl = String.length first in
    let next = Marshal.to_string (Nextkey table) [] in
    let nextl = String.length next in
    (Lwt_unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 >>=
     (fun socket ->
       Lwt_unix.connect 
         (Lwt_unix.Plain socket)
         (Unix.ADDR_UNIX (directory^"/"^socketname)) >>=
       (fun () -> return (Lwt_unix.Plain socket)) >>=
       (fun indescr ->
         let inch = Lwt_unix.in_channel_of_descr indescr in
         let nextkey next nextl =
           Lwt_unix.write indescr next 0 nextl >>=
           (fun l2 -> if l2 <> nextl 
           then fail Ocsipersist_error
           else (Lwt_unix.input_line inch >>=
                 fun answ -> return (Marshal.from_string answ 0)))
         in
         let rec aux n l =
           nextkey n l >>=
           (function
             | End -> return ()
             | Key k -> find table k >>= f k
             | _ -> fail Ocsipersist_error) >>=
           (fun () -> aux next nextl)
         in
         catch
           (fun () ->
             aux first firstl >>=
             (fun () -> Unix.close socket; return ()))
           (fun e -> Unix.close socket; fail e))))




