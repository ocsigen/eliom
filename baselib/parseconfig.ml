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
(* Config file parsing *)
(* 2006 07 : Add multiple servers -- Nataliya *)

open Simplexmlparser
open ExpoOrPatt
open Ocsiconfig

(* I put the parser here and not in config.ml because of cyclic dependancies *)

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
  in let rec parse_site n (cmos,stat) = function
      PLCons ((EPanytag ("module", PLEmpty, s)), l) -> 
        parse_site n (cmos@[parse_string s],stat) l
    | PLCons ((EPanytag ("staticdir", PLEmpty, s)), l) -> 
        (match stat with
          None -> parse_site n (cmos, Some (parse_string s)) l
        | _ -> raise 
              (Config_file_error 
                 "Only one <staticdir> tag allowed inside <site>"))
    | PLCons ((EPcomment _), l) -> parse_site n (cmos,stat) l
    | PLCons ((EPwhitespace _), l) -> parse_site n (cmos,stat) l
    | PLEmpty -> 
        (match (cmos,stat) with
          [], None -> 
            raise (Config_file_error 
                     "<module> or <staticdir> tag expected inside <site>")
        | _ -> (cmos,stat))
    | _ -> raise (Config_file_error "Unexpected tag inside <site>")
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
    | PLCons ((EPanytag (tag,_,_)),l) -> 
        raise (Config_file_error ("<"^tag^"> tag unexpected inside <ssl>"))
    | _ -> raise (Config_file_error ("Unexpected content inside <ssl>"))
  in
  let rec parse_host n = function
      PLEmpty -> []
    | PLCons ((EPanytag ("site", atts, l)), ll) ->
        let dir = match atts with
        | PLEmpty -> 
            raise (Config_file_error "dir attribute expected for <site>")
        | PLCons ((EPanyattr (EPVstr("dir"), EPVstr(s))), PLEmpty) -> s
        | _ -> raise (Config_file_error "Wrong attribute for <site>") 
        in
              let path = Neturl.split_path dir in
        (path, parse_site n ([],None) l)::(parse_host n ll)
    | PLCons ((EPcomment _), l) -> parse_host n l
    | PLCons ((EPwhitespace _), l) -> parse_host n l
    | PLCons ((EPanytag (tag,_,_)),l) -> 
        raise (Config_file_error ("<"^tag^"> tag unexpected inside <host>"))
    | _ -> raise (Config_file_error ("Unexpected content inside <host>"))
  in
  let rec parse_ocsigen n = function
      PLEmpty -> []
    | PLCons ((EPanytag ("port", atts, p)), ll) ->
        (match atts with
          PLEmpty
        | PLCons 
            ((EPanyattr
                (EPVstr("protocol"), 
                 EPVstr "HTTP")), PLEmpty) -> 
                   (try
                     add_port n (int_of_string (parse_string p))
                   with _ -> 
                     raise (Config_file_error "wrong value for <port> tag"))
        | PLCons
            ((EPanyattr 
                (EPVstr("protocol"), 
                 EPVstr "HTTPS")), PLEmpty) ->
                   (try
                     add_sslport n (int_of_string (parse_string p))
                   with _ -> 
                     raise (Config_file_error "wrong value for <port> tag"))
        | _ -> raise (Config_file_error "Wrong attribute for <port>"));
        parse_ocsigen n ll
    | PLCons ((EPanytag ("ssl", PLEmpty, p)), ll) ->
        parse_ssl n p;
        parse_ocsigen n ll
    | PLCons ((EPanytag ("uploaddir", PLEmpty, p)), ll) ->
            set_uploaddir n (Some (parse_string p));
        parse_ocsigen n ll
    | PLCons ((EPanytag ("logdir", PLEmpty, p)), ll) -> 
        set_logdir n (parse_string p);
        parse_ocsigen n ll
    | PLCons ((EPanytag ("staticdir", PLEmpty, p)), ll) -> 
        set_default_static_dir n (parse_string p);
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
    | PLCons ((EPanytag ("mimefile", PLEmpty, p)), ll) ->
        Ocsiconfig.set_mimefile n (parse_string p);
        parse_ocsigen n ll
    | PLCons ((EPanytag ("timeout", PLEmpty, p)), ll) -> 
        set_connect_time_max n (float_of_string (parse_string p));
        parse_ocsigen n ll
    | PLCons ((EPanytag ("keepalivetimeout", PLEmpty, p)), ll) -> 
        set_keepalive_timeout n (float_of_string (parse_string p));
        parse_ocsigen n ll
    | PLCons ((EPanytag ("netbuffersize", PLEmpty, p)), ll) -> 
        set_netbuffersize n (int_of_string (parse_string p));
        parse_ocsigen n ll
    | PLCons ((EPanytag ("filebuffersize", PLEmpty, p)), ll) -> 
        set_filebuffersize n (int_of_string (parse_string p));
        parse_ocsigen n ll
    | PLCons ((EPanytag ("maxrequestbodysize", PLEmpty, p)), ll) -> 
        set_maxrequestbodysize n (parse_size (parse_string p));
        parse_ocsigen n ll
    | PLCons ((EPanytag ("maxuploadfilesize", PLEmpty, p)), ll) -> 
        set_maxuploadfilesize n (parse_size (parse_string p));
        parse_ocsigen n ll
    | PLCons ((EPanytag ("dynlink", PLEmpty,l)), ll) -> 
        (Cmo (parse_string l))::parse_ocsigen n ll
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
       in (Host (host, parse_host n l))::(parse_ocsigen n ll)
    | PLCons ((EPcomment _), ll) -> parse_ocsigen n ll
    | PLCons ((EPwhitespace _), ll) -> parse_ocsigen n ll
    | PLCons ((EPanytag (tag, _, _)), _) -> 
        raise (Config_file_error ("tag <"^tag^"> unexpected inside <server>"))
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
        parse_servers (n@[nouveau]) ll
        (* nouveau at the end *)
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
