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
let rec parse_global_config (store, ocsidbm, delayloading as d) = function
  | [] -> d
  | Element ("delayloading", [("val", ("true" | "1"))], []) :: ll ->
       parse_global_config (store, ocsidbm, true) ll

  | Element ("store", [("dir", s)], []) :: ll ->
      if store = None then
        parse_global_config ((Some s), ocsidbm, delayloading) ll
      else
        Ocsigen_extensions.badconfig "Ocsipersist: Duplicate <store> tag"

  | Element ("ocsidbm", [("name", s)], []) :: ll ->
      if ocsidbm = None then
        parse_global_config (store, (Some s), delayloading) ll
      else
        Ocsigen_extensions.badconfig "Ocsipersist: Duplicate <ocsidbm> tag"

  | (Element (s,_,_))::ll -> Ocsigen_extensions.badconfig "Bad tag %s" s

  | _ -> Ocsigen_extensions.badconfig
      "Unexpected content inside Ocsipersist config"

let (directory, ocsidbm) =
  (ref ((Ocsigen_config.get_datadir ())^"/ocsipersist"),
   ref ((Ocsigen_config.get_extralibdir ())^"/extensions/ocsidbm"))


(*****************************************************************************)
(** Communication with the DB server *)

let rec try_connect sname =
  catch
    (fun () ->
      let socket = Lwt_unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
      Lwt_unix.connect socket (Unix.ADDR_UNIX sname) >>= fun () ->
      return socket)
    (fun _ ->
      Ocsigen_messages.warning ("Launching a new Ocsidbm process: "^(!ocsidbm)^
                        " on directory "^(!directory)^".");
      let param = [|!ocsidbm; !directory|] in
      let child () =
        let err = !(Ocsigen_lib.thd3 Ocsigen_messages.error) in
        Unix.dup2 err Unix.stderr;
        Unix.close !(Ocsigen_lib.thd3 Ocsigen_messages.error);
        Unix.close !(Ocsigen_lib.thd3 Ocsigen_messages.access);
        Unix.close !(Ocsigen_lib.thd3 Ocsigen_messages.warningfile);
        let devnull = Unix.openfile "/dev/null" [Unix.O_WRONLY] 0 in
        Unix.dup2 devnull Unix.stdout;
        Unix.close devnull;
        Unix.close Unix.stdin;
        Unix.execv !ocsidbm param
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
            let socket = Lwt_unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
            Lwt_unix.connect socket (Unix.ADDR_UNIX sname) >>= fun () ->
            return socket)))

let rec get_indescr i =
  (catch
     (fun () -> try_connect (!directory^"/"^socketname))
     (fun e ->
       if i = 0
       then begin
         Ocsigen_messages.errlog ("Cannot connect to Ocsidbm. Will continue \
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

let inch = ref (Lwt.fail (Failure "Ocsipersist not initalised"))
let outch = ref (Lwt.fail (Failure "Ocsipersist not initalised"))

let init_fun config =
  let (store, ocsidbmconf, delay_loading) =
    parse_global_config (None, None, false) config
  in
  (match store with
     | None -> ()
     | Some d -> directory := d);
  (match ocsidbmconf with
     | None -> ()
     | Some d -> ocsidbm := d);

  Ocsigen_messages.warning
    (if delay_loading then
       "Asynchronuous initialization of Ocsipersist-dbm (may fail later)"
     else
       "Initializing Ocsipersist-dbm...");
  let indescr = get_indescr 2 in
  if delay_loading then (
    inch  := (indescr >>= fun r -> return (Lwt_chan.in_channel_of_descr r));
    outch := (indescr >>= fun r -> return (Lwt_chan.out_channel_of_descr r));
  ) else (
    let r = Lwt_unix.run indescr in
    inch  := return (Lwt_chan.in_channel_of_descr r);
    outch := return (Lwt_chan.out_channel_of_descr r);
    Ocsigen_messages.warning "...Initialization of Ocsipersist-dbm complete";
  )


let send =
  let previous = ref (return Ok) in
  fun v ->
    catch
      (fun () -> !previous)
      (fun _ -> return Ok) >>=
    (fun _ ->
      !inch >>= fun inch ->
      !outch >>= fun outch ->
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
    | Error e -> fail e
    | _ -> fail Ocsipersist_error)

let db_remove (store, name) =
  send (Remove (store, name)) >>=
  (function
    | Ok -> return ()
    | Error e -> fail e
    | _ -> fail Ocsipersist_error)

let db_replace (store, name) value =
  send (Replace (store, name, value)) >>=
  (function
    | Ok -> return ()
    | Error e -> fail e
    | _ -> fail Ocsipersist_error)

let db_replace_if_exists (store, name) value =
  send (Replace_if_exists (store, name, value)) >>=
  (function
    | Ok -> return ()
    | Dbm_not_found -> fail Not_found
    | Error e -> fail e
    | _ -> fail Ocsipersist_error)

let db_firstkey store =
  send (Firstkey store) >>=
  (function
    | Key k -> return (Some k)
    | Error e -> fail e
    | _ -> return None)

let db_nextkey store =
  send (Nextkey store) >>=
  (function
    | Key k -> return (Some k)
    | Error e -> fail e
    | _ -> return None)

let db_length store =
  send (Length store) >>=
  (function
    | Value v -> return (Marshal.from_string v 0)
    | Dbm_not_found -> return 0
    | Error e -> fail e
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

let table_name n = Lwt.return n

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
           | Error e -> fail e
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

let _ = Ocsigen_extensions.register_extension ~name:"ocsipersist" ~init_fun ()
