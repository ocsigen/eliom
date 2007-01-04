(* Ocsigen
 * http://www.ocsigen.org
 * Module parseconfig.ml
 * Copyright (C) 2005 Vincent Balat, Nataliya Guts
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

(******************************************************************)
(** Config file parsing *)
(* 2006 07 : Add multiple servers -- Nataliya *)
(* 2006 12 : Changes for extensions -- Vincent *)

open Simplexmlparser
open ExprOrPatt
open Ocsiconfig


(*****************************************************************************)
let parse_size =
  let kilo = Int64.of_int 1000 in
  let mega = Int64.of_int 1000000 in
  let giga = Int64.mul kilo mega in
  let tera = Int64.mul mega mega in
  let kibi = Int64.of_int 1024 in
  let mebi = Int64.of_int 1048576 in
  let gibi = Int64.mul kibi mebi in
  let tebi = Int64.mul mebi mebi in
  fun s ->
    let v l =
      Int64.of_string (String.sub s 0 l)
    in
    let o l =
      let l1 = l-1 in
      if l1>0
      then
        let c1 = s.[l1] in
        if (c1 = 'o') || (c1 = 'B')
        then v l1
        else v l
      else v l
    in
    if (s = "") || (s = "infinity")
    then None
    else Some
        (let l = String.length s in
        let l2 = l-2 in
        if l2>0
        then
          let c2 = String.sub s l2 2 in
          if (c2 = "To") || (c2 = "TB")
          then Int64.mul tera (v l2)
          else
            if (c2 = "Go") || (c2 = "GB")
            then Int64.mul giga (v l2)
            else
              if (c2 = "Mo") || (c2 = "MB")
              then Int64.mul mega (v l2)
              else
                if (c2 = "ko") || (c2 = "kB")
                then Int64.mul kilo (v l2)
                else       
                  let l3 = l-3 in
                  if l3>0
                  then
                    let c3 = String.sub s l3 3 in
                    if (c3 = "Tio") || (c3 = "TiB")
                    then Int64.mul tebi (v l3)
                    else
                      if (c3 = "Gio") || (c3 = "GiB")
                      then Int64.mul gibi (v l3)
                      else
                        if (c3 = "Mio") || (c3 = "MiB")
                        then Int64.mul mebi (v l3)
                        else
                          if (c3 = "kio") || (c3 = "kiB")
                          then Int64.mul kibi (v l3)
                          else o l
                else o l
        else o l)

(* My xml parser is not really adapted to this.
   It is the parser for the syntax extension.
   But it works.
 *)

let rec parse_string = function
    PLEmpty -> ""
  | PLCons ((EPpcdata s), l) -> s^(parse_string l)
  | PLCons ((EPwhitespace s), l) -> s^(parse_string l)
  | PLCons ((EPcomment _), l) -> parse_string l
  | _ -> raise (Config_file_error "string expected")

let rec parser_config = 
  let rec verify_empty = function
      PLEmpty -> ()
    | PLCons ((EPcomment _), l) -> verify_empty l
    | PLCons ((EPwhitespace _), l) -> verify_empty l
    | _ -> raise (Config_file_error "Don't know what to do with tailing data")
  in let rec parse_servers n = function
      PLEmpty -> (match n with [] -> 
        raise(Config_file_error ("<server> tag expected"))
      | _ -> n)
    | PLCons ((EPanytag ("server", PLEmpty, nouveau)), ll) ->
        parse_servers (n@[nouveau]) ll
        (* nouveau at the end *)
    | PLCons ((EPcomment _), ll) -> parse_servers n ll
    | PLCons ((EPwhitespace _), ll) -> parse_servers n ll
    | _ -> raise (Config_file_error ("syntax error inside <ocsigen>"))
  in function 
      PLCons ((EPanytag ("ocsigen", PLEmpty, l)), ll) -> 
        verify_empty ll; 
        parse_servers [] l
    | PLCons ((EPcomment _), ll) -> parser_config ll
    | PLCons ((EPwhitespace _), ll) -> parser_config ll
    | _ -> raise (Config_file_error "<ocsigen> tag expected")


(* Config file is parsed twice. 
   This is the second parsing (site loading) 
 *)
let parse_server c =
  let rec parse_server_aux =
    let rec parse_site parse_site_function path = function
      | PLCons ((EPcomment _), l) -> parse_site parse_site_function path l
      | PLCons ((EPwhitespace _), l) -> parse_site parse_site_function path l
      | PLEmpty -> ()
      | PLCons (xml, l) -> 
          (try
            parse_site_function path xml
          with Extensions.Bad_config_tag_for_extension t -> 
            raise (Config_file_error ("Unexpected tag <"^t
                                      ^"> inside <site dir=\""^
				      (Ocsimisc.string_of_url_path path)
				      ^"\">")));
          parse_site parse_site_function path l
    in
    let rec parse_host psf = function
	PLEmpty -> ()
      | PLCons ((EPanytag ("site", atts, l)), ll) ->
          let dir = match atts with
          | PLEmpty -> 
              raise (Config_file_error "dir attribute expected for <site>")
          | PLCons ((EPanyattr (EPVstr("dir"), EPVstr(s))), PLEmpty) -> s
          | _ -> raise (Config_file_error "Wrong attribute for <site>") 
          in
          let path = Ocsimisc.remove_slash (Neturl.split_path dir) in
          parse_site psf path l;
          parse_host psf ll
      | PLCons ((EPcomment _), l) -> parse_host psf l
      | PLCons ((EPwhitespace _), l) -> parse_host psf l
      | PLCons ((EPanytag (tag,_,_)),l) -> 
          raise (Config_file_error ("<"^tag^"> tag unexpected inside <host>"))
      | _ -> raise (Config_file_error ("Unexpected content inside <host>"))
    in function
	PLEmpty -> []
      | PLCons ((EPanytag ("port", atts, p)), ll) ->
          parse_server_aux ll
      | PLCons ((EPanytag ("ssl", PLEmpty, p)), ll) ->
          parse_server_aux ll
      | PLCons ((EPanytag ("user", PLEmpty, p)), ll) -> 
          parse_server_aux ll
      | PLCons ((EPanytag ("group", PLEmpty, p)), ll) -> 
          parse_server_aux ll
      | PLCons ((EPanytag ("uploaddir", PLEmpty, p)), ll) ->
          set_uploaddir (Some (parse_string p));
          parse_server_aux ll
      | PLCons ((EPanytag ("logdir", PLEmpty, p)), ll) -> 
          set_logdir (parse_string p);
          parse_server_aux ll
      | PLCons ((EPanytag ("staticdir", PLEmpty, p)), ll) -> 
          set_default_static_dir (parse_string p);
          parse_server_aux ll
      | PLCons ((EPanytag ("minthreads", PLEmpty, p)), ll) ->
          set_minthreads (int_of_string (parse_string p));
          parse_server_aux ll
      | PLCons ((EPanytag ("maxthreads", PLEmpty, p)), ll) ->
          set_maxthreads (int_of_string (parse_string p));
          parse_server_aux ll
      | PLCons ((EPanytag ("maxdetachedcomputationsqueued", PLEmpty, p)), ll) ->
          set_max_number_of_threads_queued (int_of_string (parse_string p));
          parse_server_aux ll
      | PLCons ((EPanytag ("maxconnected", PLEmpty, p)), ll) -> 
          set_max_number_of_connections (int_of_string (parse_string p));
          parse_server_aux ll
      | PLCons ((EPanytag ("mimefile", PLEmpty, p)), ll) ->
          Ocsiconfig.set_mimefile (parse_string p);
          parse_server_aux ll
      | PLCons ((EPanytag ("timeout", PLEmpty, p)), ll) -> 
          set_connect_time_max (float_of_string (parse_string p));
          parse_server_aux ll
      | PLCons ((EPanytag ("keepalivetimeout", PLEmpty, p)), ll) -> 
          set_keepalive_timeout (float_of_string (parse_string p));
          parse_server_aux ll
      | PLCons ((EPanytag ("netbuffersize", PLEmpty, p)), ll) -> 
          set_netbuffersize (int_of_string (parse_string p));
          parse_server_aux ll
      | PLCons ((EPanytag ("filebuffersize", PLEmpty, p)), ll) -> 
          set_filebuffersize (int_of_string (parse_string p));
          parse_server_aux ll
      | PLCons ((EPanytag ("maxrequestbodysize", PLEmpty, p)), ll) -> 
          set_maxrequestbodysize (parse_size (parse_string p));
          parse_server_aux ll
      | PLCons ((EPanytag ("maxuploadfilesize", PLEmpty, p)), ll) -> 
          set_maxuploadfilesize (parse_size (parse_string p));
          parse_server_aux ll
      | PLCons ((EPanytag ("dynlink", PLEmpty,l)), ll) -> 
          Dynlink.loadfile (parse_string l);
          parse_server_aux ll
      | PLCons ((EPanytag ("host", atts, l)), ll) ->
	  let host = match atts with
          | PLEmpty -> [[Ocsimisc.Wildcard],None] (* default = "*:*" *)
          | PLCons ((EPanyattr (EPVstr("name"), EPVstr(s))), PLEmpty) -> 
              List.map
		(fun ss ->
                  let host, port = 
                    try
                      let dppos = String.index ss ':' in
                      let len = String.length ss in
                      ((String.sub ss 0 dppos),
                       match String.sub ss (dppos+1) ((len - dppos) - 1) with
			 "*" -> None
                       | p -> Some (int_of_string p))
                    with 
                      Not_found -> ss, None
                    | Failure _ ->
			raise (Config_file_error "bad port number")
                  in
                  ((List.map
                      (function
                          Netstring_str.Delim _ -> Ocsimisc.Wildcard
			| Netstring_str.Text t -> 
                            Ocsimisc.Text (t, String.length t))
                      (Netstring_str.full_split (Netstring_str.regexp "[*]+") 
			 host)),
                   port))
		(Netstring_str.split (Netstring_str.regexp "[ \t]+") s)
          | _ -> raise (Config_file_error "Wrong attribute for <host>") 
	  in 
	  let (gen_page, parse_site_function) = Extensions.create_virthost host in
	  parse_host parse_site_function l;
	  (host,gen_page)::(parse_server_aux ll)
      | PLCons ((EPcomment _), ll) -> parse_server_aux ll
      | PLCons ((EPwhitespace _), ll) -> parse_server_aux ll
      | PLCons ((EPanytag (tag, _, _)), _) -> 
          raise (Config_file_error ("tag <"^tag^"> unexpected inside <server>"))
      | _ ->
          raise (Config_file_error "Syntax error")
  in Extensions.set_virthosts (parse_server_aux c)

(* First parsing of config file *)
let extract_info c =
  let rec parse_ssl certificate privatekey = function
      PLEmpty -> Some (certificate,privatekey)
    | PLCons ((EPanytag ("certificate", PLEmpty, p)), l) ->
        (match certificate with
          None ->
            parse_ssl (Some (parse_string p)) privatekey l 
        | _ -> raise (Config_file_error 
                        "Two certificates inside <ssl>"))
    | PLCons ((EPanytag ("privatekey", PLEmpty, p)), l) ->
        (match certificate with
          None ->
            parse_ssl certificate (Some (parse_string p)) l 
        | _ -> raise (Config_file_error 
                        "Two private keys inside <ssl>"))
    | PLCons ((EPcomment _), l) -> parse_ssl certificate privatekey l
    | PLCons ((EPwhitespace _), l) -> parse_ssl certificate privatekey l
    | PLCons ((EPanytag (tag,_,_)),l) -> 
        raise (Config_file_error ("<"^tag^"> tag unexpected inside <ssl>"))
    | _ -> raise (Config_file_error ("Unexpected content inside <ssl>"))
  in
  let rec aux user group ssl ports sslports = function
      PLEmpty -> ((user,group),(ssl,ports,sslports))
    | PLCons ((EPanytag ("port", atts, p)), ll) ->
        (match atts with
          PLEmpty
        | PLCons 
            ((EPanyattr
                (EPVstr("protocol"), 
                 EPVstr "HTTP")), PLEmpty) -> 
                   let po = try
                     int_of_string (parse_string p)
                   with Failure _ -> 
                     raise (Config_file_error "Wrong value for <port> tag")
                   in aux user group ssl (po::ports) sslports ll
        | PLCons
            ((EPanyattr 
                (EPVstr("protocol"), 
                 EPVstr "HTTPS")), PLEmpty) ->
                   let po = try
                     int_of_string (parse_string p)
                   with Failure _ -> 
                     raise (Config_file_error "Wrong value for <port> tag")
                   in
                   aux user group ssl ports (po::sslports) ll
        | _ -> raise (Config_file_error "Wrong attribute for <port>"))
    | PLCons ((EPanytag ("ssl", PLEmpty, p)), ll) ->
        (match ssl with
          None ->
            aux user group (parse_ssl None None p) ports sslports ll
        | _ -> 
            raise 
              (Config_file_error
                 "Only one ssl certificate for each server supported for now"))
    | PLCons ((EPanytag ("user", PLEmpty, p)), ll) -> 
        (match user with
          None -> 
            aux (Some (parse_string p)) group ssl ports sslports ll
        | _ -> raise (Config_file_error
                        "Only one <user> tag for each server allowed"))
    | PLCons ((EPanytag ("group", PLEmpty, p)), ll) -> 
        (match group with
          None -> 
            aux user (Some (parse_string p)) ssl ports sslports ll
        | _ -> raise (Config_file_error 
                        "Only one <group> tag for each server allowed"))
    | PLCons ((EPcomment _), ll) -> aux user group ssl ports sslports ll
    | PLCons ((EPwhitespace _), ll) -> aux user group ssl ports sslports ll
    | PLCons ((EPanytag (tag, _, _)), ll) -> 
        aux user group ssl ports sslports ll
    | _ ->
        raise (Config_file_error "Syntax error")
  in 
  let (user,group),si = aux None None None [] [] c in
  let user = match user with
    None -> get_default_user ()
  | Some u -> u
  in
  let group = match group with
    None -> get_default_group ()
  | Some g -> g
  in
  ((user,group),si)

let parse_config () = parser_config Ocsiconfig.config

(******************************************************************)
