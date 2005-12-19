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

type static_dir = Static_dir of string option * (string * static_dir) list

let port = ref 80
let logdir = ref "/var/log/ocsigen/"
let config_file = ref "/etc/ocsigen/ocsigen.conf"
let verbose = ref false
let veryverbose = ref false
let static_tree = ref (Static_dir (Some "/var/www/ocsigen", []))
let user = ref "ocsigen"
let group = ref "ocsigen"

let set_port i = port := i
let set_logdir s = logdir := s
let set_configfile s = config_file := s
let set_verbose () = verbose := true
let set_veryverbose () = verbose := true; veryverbose := true
let set_user s = user := s
let set_group s = group := s
let get_port () = !port
let get_logdir () = !logdir
let get_config_file () = !config_file
let get_verbose () = !verbose
let get_veryverbose () = !veryverbose
let get_static_tree () = !static_tree
let get_user () = !user
let get_group () = !group

let set_static_dir s path =
  let rec assoc_and_remove a = function
      [] -> raise Not_found
    | (b,v)::l when a = b -> (v,l)
    | e::l -> let v,ll = assoc_and_remove a l
	  in v,(e::ll)
  in
  let rec add_path = function
      [] -> Static_dir (Some s,[])
    | a::l -> Static_dir (None, [(a,add_path l)])
  in
  let rec aux (Static_dir (s1,l1)) = function
      [] -> Static_dir (Some s,l1)
    | a::l -> 
	try
	  let sd1,l2 = assoc_and_remove a l1 in
	  let sd = aux sd1 l in
	  Static_dir (s1,(a,sd)::l2)
	with Not_found -> Static_dir (s1,(a,(add_path l))::l1)
  in static_tree := aux !static_tree path

let set_staticpages s = set_static_dir s []


let print_location loc =
  Printf.sprintf "%d-%d" (fst loc).Lexing.pos_cnum (snd loc).Lexing.pos_cnum


let _ = Arg.parse
    [("-c", Arg.String set_configfile, 
      "Alternate config file (default /etc/ocsigen.conf)");
     ("-v", Arg.Unit set_verbose, "Verbose mode");
     ("-vv", Arg.Unit set_veryverbose, "Very verbose mode (debug)")
   ]
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





