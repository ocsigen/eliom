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
  | [] -> d
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
  | None -> (Ocsiconfig.get_datadir ())^"/ocsipersist"
  | Some d -> d),
   (match ocsidbm with
   | None -> (Ocsiconfig.get_bindir ())^"/ocsidbm"
   | Some d -> d))


(*****************************************************************************)
(** Communication with the DB server *)

let rec try_connect sname =
  catch
    (fun () ->
      Lwt_unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 >>=
      (fun socket ->
        Lwt_unix.connect socket (Unix.ADDR_UNIX sname) >>=
        (fun () -> return socket)))
    (fun _ ->
      Messages.warning ("Launching a new Ocsidbm process: "^ocsidbm^" on directory "^directory^".");
      let param = [|ocsidbm; directory|] in
      let child () = 
        Unix.dup2 !(snd Messages.error) Unix.stderr; 
        Unix.close !(snd Messages.error);
        Unix.close !(snd Messages.access);
        Unix.close !(snd Messages.warningfile);
        let devnull = Unix.openfile "/dev/null" [Unix.O_WRONLY] 0 in
        Unix.dup2 devnull Unix.stdout;
        Unix.close devnull;
        Unix.close Unix.stdin;
        Unix.execv ocsidbm param 
      in
      let pid = Unix.fork () in
      if pid = 0
      then begin (* double fork *)
        if Unix.fork () = 0
        then begin
          child ()
        end
        else exit 0
      end
      else 
        Lwt_unix.waitpid [] pid >>=
        (fun _ ->  Lwt_unix.sleep 1.1 >>=
          (fun () ->
            Lwt_unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 >>=
            (fun socket ->
              Lwt_unix.connect socket (Unix.ADDR_UNIX sname) >>=
              (fun () -> return socket)))))
    
let rec get_indescr i =
  (catch
     (fun () -> try_connect (directory^"/"^socketname))
     (fun e -> 
       if i = 0 
       then begin
         Messages.errlog ("Cannot connect to Ocsidbm. Will continue \
                            without persistent session support. \
                            Error message is: "^
                            (match e with
                            | Unix.Unix_error (a,b,c) -> 
                                (Unix.error_message a)^" in "^b^"("^c^")"
                            | _ -> Printexc.to_string e)^
                            ". Have a look at the logs to see if there is an \
                            error message from the Ocsidbm process.");
         fail e
       end
       else (Lwt_unix.sleep 2.1) >>= (fun () -> get_indescr (i-1))))

let indescr = get_indescr 2

let inch = indescr >>= (fun r -> return (Lwt_unix.in_channel_of_descr r))

let outch = indescr >>= (fun r -> return (Lwt_unix.out_channel_of_descr r))

let send =
  let previous = ref (return Ok) in
  fun v ->
    catch
      (fun () -> !previous)
      (fun _ -> return Ok) >>=
    (fun _ -> 
      inch >>= fun inch ->
      outch >>= fun outch ->
      previous :=
        (Lwt_chan.output_value outch v >>= fun () -> 
         Lwt_chan.flush outch >>= fun () -> 
         Lwt_chan.input_value inch);
      !previous)

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

let db_replace_if_exists (store, name) value = 
  send (Replace_if_exists (store, name, value)) >>=
  (function 
    | Ok -> return ()
    | Dbm_not_found -> fail Not_found
    | _ -> fail Ocsipersist_error)

let db_firstkey store = 
  send (Firstkey store) >>=
  (function 
    | Key k -> return (Some k)
    | _ -> return None)

let db_nextkey store = 
  send (Nextkey store) >>=
  (function 
    | Key k -> return (Some k)
    | _ -> return None)

let db_length store = 
  send (Length store) >>=
  (function 
    | Value v -> return (Marshal.from_string v 0)
    | Dbm_not_found -> return 0
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

let open_table name = name
    
let find table key =
  db_get (table, key) >>=
  (fun v -> return (Marshal.from_string v 0))

let add table key value =
  let data = Marshal.to_string value [] in
  db_replace (table, key) data

let replace_if_exists table key value =
  let data = Marshal.to_string value [] in
  db_replace_if_exists (table, key) data

let remove table key =
  db_remove (table, key)
  
let iter_table f table =
  let rec aux nextkey =
    nextkey table >>=
    (function
      | None -> return ()
      | Some k -> find table k >>= f k >>= (fun () -> aux db_nextkey))
  in
  aux db_firstkey

let iter_step = iter_table
  
let fold_table f table beg =
  let rec aux nextkey beg =
    nextkey table >>=
    (function
      | None -> return beg
      | Some k -> find table k >>= fun r ->
          f k r beg >>= (fun res -> aux db_nextkey res))
  in
  aux db_firstkey beg

let fold_step = fold_table

let iter_block a b = failwith "iter_block not implemented for DBM. Please use Ocsipersist with sqlite"

(* iterator: with a separate connexion:
exception Exn1
let iter_table f table =
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

*)

let length table = 
  db_length table
(* Because of Dbm implementation, the result may be less thann the expected
   result in some case (with a version of ocsipersist based on Dbm) *)

