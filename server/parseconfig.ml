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
  let rec parse_site n = function
      PLCons ((EPanytag ("url", PLEmpty, s)), l) -> 
	let path = Neturl.split_path (parse_string s) in
	let cmo,static = parse_site2 (None, None) l in
	(match static with
	  None -> ()
	| Some s -> Ocsiconfig.set_static_dir n s path);
	(match cmo with
	  None -> []
	| Some cmo -> [Mod (path,cmo)])
    | PLCons ((EPcomment _), l) -> parse_site n l
    | PLCons ((EPwhitespace _), l) -> parse_site n l
    | _ -> raise (Config_file_error "<url> tag expected inside <site>")
  in
  let rec parse_ocsigen n = function
      PLEmpty -> []
    | PLCons ((EPanytag ("port", PLEmpty, p)), ll) -> 
	set_port n (int_of_string (parse_string p));
	parse_ocsigen n ll
    | PLCons ((EPanytag ("ssl", PLEmpty, p)), ll) ->
    	(match parse_string p with
	| "on" -> set_ssl n true
	| "off" -> set_ssl n false
	| _ -> raise (Config_file_error "wrong value for <ssl> tag"));
	parse_ocsigen n ll
    | PLCons ((EPanytag ("privatekey", PLEmpty, p)), ll) ->
    	set_key n (parse_string p);
	parse_ocsigen n ll
    | PLCons ((EPanytag ("certificate", PLEmpty, p)), ll) ->
        set_certificate n (parse_string p);
	parse_ocsigen n ll
    | PLCons ((EPanytag ("uploaddir", PLEmpty, p)), ll) ->
    	set_uploaddir n (parse_string p);
	parse_ocsigen n ll
    | PLCons ((EPanytag ("logdir", PLEmpty, p)), ll) -> 
	set_logdir n (parse_string p);
	parse_ocsigen n ll
    | PLCons ((EPanytag ("staticdir", PLEmpty, p)), ll) -> 
	set_staticpages n (parse_string p);
	parse_ocsigen n ll
    | PLCons ((EPanytag ("user", PLEmpty, p)), ll) -> 
	set_user n (parse_string p);
	parse_ocsigen n ll
    | PLCons ((EPanytag ("group", PLEmpty, p)), ll) -> 
	set_group n (parse_string p);
	parse_ocsigen n ll
    | PLCons ((EPanytag ("maxconnected", PLEmpty, p)), ll) -> 
	set_max_number_of_connections n (int_of_string (parse_string p));
	parse_ocsigen n ll
    | PLCons ((EPanytag ("timeout", PLEmpty, p)), ll) -> 
	set_connect_time_max n (float_of_string (parse_string p));
	parse_ocsigen n ll
    | PLCons ((EPanytag ("dynlink", PLEmpty,l)), ll) -> 
	(Cmo (parse_string l))::parse_ocsigen n ll
    | PLCons ((EPanytag ("site", PLEmpty, l)), ll) -> 
	(parse_site n l)@(parse_ocsigen n ll)
    | PLCons ((EPcomment _), ll) -> parse_ocsigen n ll
    | PLCons ((EPwhitespace _), ll) -> parse_ocsigen n ll
    | PLCons ((EPanytag (tag, PLEmpty, l)), ll) -> 
	raise (Config_file_error ("tag "^tag^" unexpected inside <server>"))
    | _ ->
	raise (Config_file_error "Syntax error")
  in let rec parse_servers n = function
      PLEmpty -> if n > 0 then [] else raise(Config_file_error ("<server> tag expected"))
    | PLCons((EPanytag ("server", PLEmpty, p)), ll) ->
    	if n >= Ocsiconfig.max_servers then raise (Config_file_error ("too many servers"))
    	else incr Ocsiconfig.number_of_servers; 
	     ((parse_ocsigen n p) @ (parse_servers (n + 1) ll))
    | PLCons ((EPcomment _), ll) -> parse_servers n ll
    | PLCons ((EPwhitespace _), ll) -> parse_servers n ll
    | _ -> raise (Config_file_error ("syntax error inside <ocsigen>"))
  in function 
      PLCons ((EPanytag ("ocsigen", PLEmpty, l)), ll) -> 
	verify_empty ll; 
	parse_servers 0 l
    | PLCons ((EPcomment _), ll) -> parser_config ll
    | PLCons ((EPwhitespace _), ll) -> parser_config ll
    | _ -> raise (Config_file_error "<ocsigen> tag expected")



let parse_config () = parser_config Ocsiconfig.config

(******************************************************************)
