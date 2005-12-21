(* Ocsigen
 * Copyright (C) 2005 Vincent Balat
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

let log_aux f s =
  let logfile = (Ocsiconfig.get_logdir ())^"/"^f in
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
  if Ocsiconfig.get_verbose () then prerr_endline ("["^f^"] "^s);
  let file = 
    Unix.openfile 
      logfile [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_APPEND] 0o640 in
  ignore(Unix.write file s 0 (String.length s));
  Unix.close file
      
let lwtbip i = 
  if Ocsiconfig.get_veryverbose () then
    let s = "bip"^(string_of_int i)^"\n" in
    ignore (Unix.write Unix.stderr s 0 (String.length s))

let accesslog s =
  log_aux "access.log" s

let errlog s =
  log_aux "errors.log" s

let warning s =
  log_aux "warnings.log" s

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


