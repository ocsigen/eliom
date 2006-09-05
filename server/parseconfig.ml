(* Ocsigen
 * http://www.ocsigen.org
 * Module parseconfig.ml
 * Copyright (C) 2005 Vincent Balat, Nataliya Guts
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
(* 2006 07 : Add multiple servers -- Nataliya *)

open Simplexmlparser
open ExpoOrPatt
open Ocsiconfig

(* I put the parser here and not in config.ml because of cyclic dependancies *)

(* My xml parser is not really adapted to this.
   It is the parser for the syntax extension.
   But it works.
 *)

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
  in let rec parse_site2 (cmo,stat,mime) = function
      PLCons ((EPanytag ("module", PLEmpty, s)), l) -> 
	(match cmo with
	  None -> parse_site2 (Some (parse_string s),stat,mime) l
	| _ -> raise 
	      (Config_file_error "Only one <module> tag allowed inside <site>"))
    | PLCons ((EPanytag ("staticdir", PLEmpty, s)), l) -> 
	(match stat with
	  None -> parse_site2 (cmo, Some (parse_string s), mime) l
	| _ -> raise 
	      (Config_file_error 
		 "Only one <staticdir> tag allowed inside <site>"))
    | PLCons ((EPanytag ("mimefile", PLEmpty, p)), l) ->
        (match mime with
	  None -> parse_site2 (cmo, stat, Some (parse_string p)) l
	| _ -> raise (Config_file_error "Only one <mimefile> tag allowed inside <site>"))
    | PLCons ((EPcomment _), l) -> parse_site2 (cmo,stat,mime) l
    | PLCons ((EPwhitespace _), l) -> parse_site2 (cmo,stat,mime) l
    | PLEmpty -> 
	(match (cmo,stat,mime) with
	  None, None, _ -> raise (Config_file_error "<module> or <staticdir> tag expected inside <site>")
	| _ -> (cmo,stat,mime))
    | _ -> raise 
	  (Config_file_error "Unexpected tag inside <site>")
  in
  let rec parse_site n host = function
      PLCons ((EPanytag ("url", PLEmpty, s)), l) ->
      	let path = Neturl.split_path (host^(parse_string s)) in
	let cmo,static,mime = parse_site2 (None, None, None) l in
	(match static with
	  None -> ()
	| Some s -> Ocsiconfig.set_static_dir n s path);
	(match mime with
	  None -> ()
	| Some m -> Ocsiconfig.set_mimefile n m);
	(match cmo with
	  None -> []
	| Some cmo -> [Mod (path,cmo)])
    | PLCons ((EPcomment _), l) -> parse_site n host l
    | PLCons ((EPwhitespace _), l) -> parse_site n host l
    | _ -> raise (Config_file_error "<url> tag expected inside <site>")
  in
  let rec parse_ssl n = function
      PLEmpty -> ()
    | PLCons ((EPanytag ("certificate", PLEmpty, p)), l) ->
        set_certificate n (parse_string p);
	parse_ssl n l 
    | PLCons ((EPanytag ("privatekey", PLEmpty, p)), l) ->
    	set_key n (parse_string p);
	parse_ssl n l
    | PLCons ((EPcomment _), l) -> parse_ssl n l
    | PLCons ((EPwhitespace _), l) -> parse_ssl n l
    | PLCons ((EPanytag (tag,_,_)),l) -> raise (Config_file_error ("<"^tag^"> tag unexpected inside <ssl>"))		  
    | _ -> raise (Config_file_error ("Unexpected content inside <ssl>"))		  
  in
  let rec parse_ocsigen n = function
      PLEmpty -> []
    | PLCons ((EPanytag ("port", PLEmpty, p)), ll) -> 
	(try
	  set_port n (int_of_string (parse_string p))
	with _ -> raise (Config_file_error "wrong value for <port> tag"));
	parse_ocsigen n ll
    | PLCons ((EPanytag ("virtual", PLEmpty, p)), ll) ->
    	 (match parse_string p with
         | "on" -> set_virtual n true
         | "off" -> set_virtual n false
         | _ -> raise (Config_file_error "wrong value for <virtual> tag"));
         parse_ocsigen n ll			 
    | PLCons ((EPanytag ("ssl", PLEmpty, p)), ll) ->
    	set_ssl n true;
	parse_ssl n p;
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
    | PLCons ((EPanytag ("minthreads", PLEmpty, p)), ll) ->
    	set_minthreads n (int_of_string (parse_string p));
	parse_ocsigen n ll
    | PLCons ((EPanytag ("maxthreads", PLEmpty, p)), ll) ->
    	set_maxthreads n (int_of_string (parse_string p));
	parse_ocsigen n ll
    | PLCons ((EPanytag ("maxdetachedcomputationsqueued", PLEmpty, p)), ll) ->
    	set_max_number_of_threads_queued n (int_of_string (parse_string p));
	parse_ocsigen n ll
    | PLCons ((EPanytag ("maxconnected", PLEmpty, p)), ll) -> 
	set_max_number_of_connections n (int_of_string (parse_string p));
	parse_ocsigen n ll
    | PLCons ((EPanytag ("timeout", PLEmpty, p)), ll) -> 
	set_connect_time_max n (float_of_string (parse_string p));
	parse_ocsigen n ll
    | PLCons ((EPanytag ("dynlink", PLEmpty,l)), ll) -> 
	(Cmo (parse_string l))::parse_ocsigen n ll
    | PLCons ((EPanytag ("site", atts, l)), ll) ->
       let host = match atts with
        | PLEmpty -> ""
        | PLCons ((EPanyattr (EPVstr("host"), EPVstr(s))), PLEmpty) -> 
	   if get_virtual_n n then (s^"/") else ""
        | _ -> raise (Config_file_error "Wrong attribute for <site>") in
	(parse_site n host l)@(parse_ocsigen n ll)
    | PLCons ((EPcomment _), ll) -> parse_ocsigen n ll
    | PLCons ((EPwhitespace _), ll) -> parse_ocsigen n ll
    | PLCons ((EPanytag (tag, PLEmpty, l)), ll) -> 
	raise (Config_file_error ("tag "^tag^" unexpected inside <server>"))
    | _ ->
	raise (Config_file_error "Syntax error")
  in let rec parse_servers n = function
      PLEmpty -> (match n with [] -> 
	raise(Config_file_error ("<server> tag expected"))
      | _ -> n)
    | PLCons ((EPanytag ("server", PLEmpty, p)), ll) ->
    	let nouveau = Ocsiconfig.init_config () in
    	incr Ocsiconfig.number_of_servers;
	set_modules nouveau (parse_ocsigen nouveau p);
	parse_servers (nouveau :: n) ll
    | PLCons ((EPcomment _), ll) -> parse_servers n ll
    | PLCons ((EPwhitespace _), ll) -> parse_servers n ll
    | _ -> raise (Config_file_error ("syntax error inside <ocsigen>"))
  in function 
      PLCons ((EPanytag ("ocsigen", PLEmpty, l)), ll) -> 
	verify_empty ll; 
	cfgs := parse_servers [] l (* : config list TODO*)
    | PLCons ((EPcomment _), ll) -> parser_config ll
    | PLCons ((EPwhitespace _), ll) -> parser_config ll
    | _ -> raise (Config_file_error "<ocsigen> tag expected")



let parse_config () = parser_config Ocsiconfig.config

(******************************************************************)
