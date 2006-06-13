(* Ocsigen
 * http://www.ocsigen.org
 * Module parseconfig.ml
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

(******************************************************************)
(* Config file parsing *)

open Simplexmlparser
open ExpoOrPatt
open Ocsiconfig

(* I put the parser here and not in config.ml because of cyclic dependancies *)

(* My xml parser is not really adapted to this.
   It is the parser for the syntax extension.
   But it works.
 *)

type modules = Cmo of string | Mod of string list * string

let rec parser_config = 
  let rec verify_empty = function
      PLEmpty -> ()
    | PLCons ((EPcomment _), l) -> verify_empty l
    | PLCons ((EPwhitespace _), l) -> verify_empty l
    | _ -> raise (Config_file_error "Don't know what to do with tailing data")
  in
  let rec parse_string = function
      PLEmpty -> ""
    | PLCons ((EPpcdata s), l) -> s^(parse_string l)
    | PLCons ((EPwhitespace s), l) -> s^(parse_string l)
    | PLCons ((EPcomment _), l) -> parse_string l
    | _ -> raise (Config_file_error "string expected")
  in let rec parse_site2 (cmo,stat) = function
      PLCons ((EPanytag ("module", PLEmpty, s)), l) -> 
	(match cmo with
	  None -> parse_site2 (Some (parse_string s),stat) l
	| _ -> raise 
	      (Config_file_error "Only one <module> tag allowed inside <url>"))
    | PLCons ((EPanytag ("staticdir", PLEmpty, s)), l) -> 
	(match stat with
	  None -> parse_site2 (cmo, Some (parse_string s)) l
	| _ -> raise 
	      (Config_file_error 
		 "Only one <staticdir> tag allowed inside <url>"))
    | PLCons ((EPcomment _), l) -> parse_site2 (cmo,stat) l
    | PLCons ((EPwhitespace _), l) -> parse_site2 (cmo,stat) l
    | PLEmpty -> 
	(match (cmo,stat) with
	  None, None -> raise (Config_file_error "<module> or <staticdir> tag expected inside <site>")
	| _ -> (cmo,stat))
    | _ -> raise 
	  (Config_file_error "Only <module> or <staticdir> tag expected inside <site>")
  in
  let rec parse_site = function
      PLCons ((EPanytag ("url", PLEmpty, s)), l) -> 
	let path = Neturl.split_path (parse_string s) in
	let cmo,static = parse_site2 (None, None) l in
	(match static with
	  None -> ()
	| Some s -> Ocsiconfig.set_static_dir s path);
	(match cmo with
	  None -> []
	| Some cmo -> [Mod (path,cmo)])
    | PLCons ((EPcomment _), l) -> parse_site l
    | PLCons ((EPwhitespace _), l) -> parse_site l
    | _ -> raise (Config_file_error "<url> tag expected inside <site>")
  in
  let rec parse_ocsigen = function
      PLEmpty -> []
    | PLCons ((EPanytag ("port", PLEmpty, p)), ll) -> 
	set_port (int_of_string (parse_string p));
	parse_ocsigen ll
    | PLCons ((EPanytag ("logdir", PLEmpty, p)), ll) -> 
	set_logdir (parse_string p);
	parse_ocsigen ll
    | PLCons ((EPanytag ("staticdir", PLEmpty, p)), ll) -> 
	set_staticpages (parse_string p);
	parse_ocsigen ll
    | PLCons ((EPanytag ("user", PLEmpty, p)), ll) -> 
	set_user (parse_string p);
	parse_ocsigen ll
    | PLCons ((EPanytag ("group", PLEmpty, p)), ll) -> 
	set_group (parse_string p);
	parse_ocsigen ll
    | PLCons ((EPanytag ("maxconnected", PLEmpty, p)), ll) -> 
	set_max_number_of_connections (int_of_string (parse_string p));
	parse_ocsigen ll
    | PLCons ((EPanytag ("timeout", PLEmpty, p)), ll) -> 
	set_connect_time_max (float_of_string (parse_string p));
	parse_ocsigen ll
    | PLCons ((EPanytag ("dynlink", PLEmpty,l)), ll) -> 
	(Cmo (parse_string l))::parse_ocsigen ll
    | PLCons ((EPanytag ("site", PLEmpty, l)), ll) -> 
	(parse_site l)@(parse_ocsigen ll)
    | PLCons ((EPcomment _), ll) -> parse_ocsigen ll
    | PLCons ((EPwhitespace _), ll) -> parse_ocsigen ll
    | PLCons ((EPanytag (tag, PLEmpty, l)), ll) -> 
	raise (Config_file_error ("tag "^tag^" unexpected inside <ocsigen>"))
    | _ ->
	raise (Config_file_error "Syntax error")
  in function 
      PLCons ((EPanytag ("ocsigen", PLEmpty, l)), ll) -> 
	verify_empty ll; 
	parse_ocsigen l
    | PLCons ((EPcomment _), ll) -> parser_config ll
    | PLCons ((EPwhitespace _), ll) -> parser_config ll
    | _ -> raise (Config_file_error "<ocsigen> tag expected")



let parse_config () = parser_config Ocsiconfig.config

(******************************************************************)
