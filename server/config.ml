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


exception Config_file_error of string

let port = ref 80
let logdir = ref "/var/log/ocsigen/"
let staticpages = ref "/var/www/ocsigen"
let config_file = ref "/etc/ocsigen/ocsigen.conf"

let set_port i = port := i
let set_logdir s = logdir := s
let set_staticpages s = staticpages := s
let set_configfile s = config_file := s
let get_port () = !port
let get_logdir () = !logdir
let get_staticpages () = !staticpages
let get_config_file () = !config_file

let print_location loc =
  Printf.sprintf "%d-%d" (fst loc).Lexing.pos_cnum (snd loc).Lexing.pos_cnum


let _ = Arg.parse
    [("-c", Arg.String set_configfile, 
      "Alternate config file (default /etc/ocsigen.conf)")]
    (fun _ -> ())
    "usage: ocsigen [-c configfile]"

let config = 
  try
    Simplexmlparser.xmlparser (get_config_file ())
  with
    Stdpp.Exc_located (fl,exn) -> 
      Printf.eprintf "%s" 
	("Syntax error in config file at location : "^(print_location fl));
      raise exn





