(* Ocsigen
 * Copyright (C) 2005 Vincent Balat
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

let access = "access.log",ref Unix.stdout
let warning = "warnings.log",ref Unix.stderr
let error = "errors.log",ref Unix.stderr

(* Several processes will access the same files, but if I am right,
   it is not a problem when opening with O_APPEND
 *)
let open_files =
  let opened = ref false in
  let openlog f =
    Unix.openfile
      ((Ocsiconfig.get_logdir ())^"/"^f)
      [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_APPEND] 0o640
  in
  fun () ->
    if !opened
    then begin
      Unix.close !(snd access);
      Unix.close !(snd warning);
      Unix.close !(snd error)
    end;
    opened := true;
    snd access := openlog (fst access);
    snd warning := openlog (fst warning);
    snd error := openlog (fst error)

let log_aux file console_print s =
  let date = 
    let t = Unix.localtime (Unix.time ()) in
    Printf.sprintf 
      "%02d-%02d-%04d %02d:%02d:%02d" 
      t.Unix.tm_mday 
      (t.Unix.tm_mon + 1)
      (1900 + t.Unix.tm_year)
      t.Unix.tm_hour
      t.Unix.tm_min
      t.Unix.tm_sec 
  in
  let s = date^" - "^s^"\n" in
  if console_print then prerr_endline ("["^(fst file)^"] "^s);
  ignore (Unix.write !(snd file) s 0 (String.length s))
      
let lwtbip i = 
  if Ocsiconfig.get_veryverbose () then
    let s = "bip"^(string_of_int i)^"\n" in
    ignore (Unix.write Unix.stderr s 0 (String.length s))

let accesslog s =
  log_aux access (Ocsiconfig.get_verbose ()) s

let errlog s =
  log_aux error (not (Ocsiconfig.get_silent ())) s

let warning s =
  log_aux warning (Ocsiconfig.get_verbose ()) s

(*
let lwtlog = 
  fun s ->
    let s = s^"\n" in
    let syslog = Syslog.openlog ~facility:`LOG_DAEMON ~logpath:????
           ~flags:[ `LOG_CONS ] "ocsigen" in
    Syslog.syslog syslog `LOG_NOTICE s;
    Syslog.closelog syslog
*)


let debug_noel s =
  if Ocsiconfig.get_veryverbose () then
    prerr_string s

let debug s =
  if Ocsiconfig.get_veryverbose () then
    prerr_endline s

let bip i = 
  if Ocsiconfig.get_veryverbose () then
    prerr_endline ("bip"^(string_of_int i))

let console s =
  if (not (Ocsiconfig.get_silent ()))
  then print_endline s
      
