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

let lwtbip i = 
  let s = "bip"^(string_of_int i)^"\n" in
  Lwt_unix.write Unix.stderr s 0 (String.length s)

let lwtwarning s = 
  let s = s^"\n" in
  Lwt_unix.write Unix.stderr s 0 (String.length s)

let lwtlog = 
  let logfile = Config.get_var "logfile" in
  fun s ->
    let s = s^"\n" in
    let file = 
      Unix.openfile 
	logfile [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_APPEND] 0o640 in
    Lwt_unix.write file s 0 (String.length s);
    Unix.close file

let warning s = prerr_endline s

let bip i = prerr_endline ("bip"^(string_of_int i))

