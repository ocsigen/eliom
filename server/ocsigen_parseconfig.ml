(* Ocsigen
 * http://www.ocsigen.org
 * Module ocsigen_parseconfig.ml
 * Copyright (C) 2005-2008 Vincent Balat, Nataliya Guts, Stéphane Glondu
 * Laboratoire PPS - CNRS Université Paris Diderot
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

open Simplexmlparser
open Ocsigen_config

let int_of_string tag s =
  try
    int_of_string (Ocsigen_lib.remove_spaces s 0 ((String.length s) -1))
  with Failure _ -> raise (Ocsigen_config.Config_file_error
                             ("While parsing <"^tag^"> - "^s^
                                " is not a valid integer."))

(*****************************************************************************)
let default_hostname =
  let hostname = Unix.gethostname () in
  try
(*VVV Is it ok? Is it reliable? *)
    (List.hd
       (Unix.getaddrinfo hostname "www" 
          [Unix.AI_CANONNAME; 
           Unix.AI_SOCKTYPE Unix.SOCK_STREAM])).Unix.ai_canonname
  with Failure _ ->
    let warning =
      "Cannot determine default host name. Will use \""^hostname^
        "\" to create absolute links or redirections dynamically if you do not set <host defaulthostname=\"...\" ...> in config file."
    in
    Ocsigen_messages.warning warning;
(*VVV Is it the right behaviour? *)
    hostname
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
    let l = String.length s in
    let s = Ocsigen_lib.remove_spaces s 0 (l-1) in
    let v l =
      try
        Int64.of_string (String.sub s 0 l)
      with Failure _ -> failwith "Ocsigen_parseconfig.parse_size"
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
         let l1 = l-1 in
         if l1>0
         then
           let c1 = String.sub s l1 1 in
           if (c1 = "T")
           then Int64.mul tebi (v l1)
           else
             if (c1 = "G")
             then Int64.mul gibi (v l1)
             else
               if (c1 = "M")
               then Int64.mul mebi (v l1)
               else
                 if (c1 = "k")
                 then Int64.mul kibi (v l1)
                 else       
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
                   else o l
         else o l)


let parse_size_tag tag s = 
  try
    parse_size s 
  with Failure _ -> 
    raise
      (Ocsigen_config.Config_file_error 
         ("While parsing <"^tag^"> - "^s^" is not a valid size."))






(* My xml parser is not really adapted to this.
   It is the parser for the syntax extension.
   But it works.
 *)


let rec parse_string = function
  | [] -> ""
  | (PCData s)::l -> s^(parse_string l)
  | _ -> failwith "ocsigen_parseconfig.parse_string"

let parse_string_tag tag s = 
  try
    parse_string s 
  with Failure _ -> 
    raise
      (Ocsigen_config.Config_file_error 
         ("While parsing <"^tag^"> - String expected."))


let rec parser_config =
  let rec verify_empty = function
    | [] -> ()
    | _ -> raise (Config_file_error "Don't know what to do with trailing data")
  in let rec parse_servers n = function
    | [] -> (match n with
      | [] -> raise (Config_file_error ("<server> tag expected"))
      | _ -> n)
    | (Element ("server", [], nouveau))::ll ->
        (match ll with
        | [] -> ()
        | _ ->
            ignore (Ocsigen_messages.warning
                      "At most one <server> tag possible in config file. \
                      Ignoring trailing data."));
        parse_servers (n@[nouveau]) [] (* ll *)
        (* Multiple server not supported any more *)
        (* nouveau at the end *)
    | _ -> raise (Config_file_error ("syntax error inside <ocsigen>"))
  in function
    | (Element ("ocsigen", [], l))::ll ->
        verify_empty ll;
        parse_servers [] l
    | _ -> raise (Config_file_error "<ocsigen> tag expected")


let parse_ext file =
  parser_config (Simplexmlparser.xmlparser_file file)


let preloadfile config () = Ocsigen_extensions.set_config config
let postloadfile () = Ocsigen_extensions.set_config []


(* Config file is parsed twice.
   This is the second parsing (site loading)
 *)
let parse_server isreloading c =
  let rec parse_server_aux = function
      | [] -> []
      | (Element ("port", atts, p))::ll ->
          parse_server_aux ll
      | (Element ("charset" as st, atts, p))::ll ->
          set_default_charset (Some (parse_string_tag st p));
          parse_server_aux ll
      | (Element ("logdir", [], p))::ll ->
          parse_server_aux ll
      | (Element ("ssl", [], p))::ll ->
          parse_server_aux ll
      | (Element ("user", [], p))::ll ->
          parse_server_aux ll
      | (Element ("group", [], p))::ll ->
          parse_server_aux ll
      | (Element ("uploaddir" as st, [], p))::ll ->
          set_uploaddir (Some (parse_string_tag st p));
          parse_server_aux ll
      | (Element ("datadir" as st, [], p))::ll ->
          set_datadir (parse_string_tag st p);
          parse_server_aux ll
      | (Element ("minthreads" as st, [], p))::ll ->
          set_minthreads (int_of_string st (parse_string_tag st p));
          parse_server_aux ll
      | (Element ("maxthreads" as st, [], p))::ll ->
          set_maxthreads (int_of_string st (parse_string_tag st p));
          parse_server_aux ll
      | (Element ("maxdetachedcomputationsqueued" as st, [], p))::ll ->
          set_max_number_of_threads_queued (int_of_string st (parse_string_tag st p));
          parse_server_aux ll
      | (Element ("maxconnected" as st, [], p))::ll ->
          set_max_number_of_connections (int_of_string st (parse_string_tag st p));
          parse_server_aux ll
      | (Element ("mimefile" as st, [], p))::ll ->
          Ocsigen_config.set_mimefile (parse_string_tag st p);
          parse_server_aux ll
      | (Element ("maxretries" as st, [], p))::ll ->
          set_maxretries (int_of_string st (parse_string_tag st p));
          parse_server_aux ll
      | (Element ("timeout" as st, [], p))::ll
(*VVV timeout: backward compatibility with <= 0.99.4 *)
      | (Element ("clienttimeout" as st, [], p))::ll ->
          set_client_timeout (int_of_string st (parse_string_tag st p));
          parse_server_aux ll
      | (Element ("servertimeout" as st, [], p))::ll ->
          set_server_timeout (int_of_string st (parse_string_tag st p));
          parse_server_aux ll
(*VVV For now we use silentservertimeout and silentclienttimeout also
  for keep alive :-(
      | (Element ("keepalivetimeout" as st, [], p))::ll ->
          set_keepalive_timeout (int_of_string st (parse_string_tag st p));
          parse_server_aux ll
      | (Element ("keepopentimeout" as st, [], p))::ll ->
          set_keepopen_timeout (int_of_string st (parse_string_tag st p));
          parse_server_aux ll
*)
      | (Element ("netbuffersize" as st, [], p))::ll ->
          set_netbuffersize (int_of_string st (parse_string_tag st p));
          parse_server_aux ll
      | (Element ("filebuffersize" as st, [], p))::ll ->
          set_filebuffersize (int_of_string st (parse_string_tag st p));
          parse_server_aux ll
      | (Element ("maxrequestbodysize" as st, [], p))::ll ->
          set_maxrequestbodysize (parse_size_tag st (parse_string_tag st p));
          parse_server_aux ll
      | (Element ("maxuploadfilesize" as st, [], p))::ll ->
          set_maxuploadfilesize (parse_size_tag st
                                   (parse_string_tag st p));
          parse_server_aux ll
      | (Element ("commandpipe" as st, [], p))::ll ->
          set_command_pipe (parse_string_tag st p);
          parse_server_aux ll
      | (Element ("debugmode", [], []))::ll ->
          set_debugmode true;
          parse_server_aux ll
      | (Element ("usedefaulthostname", [], []))::ll ->
          set_usedefaulthostname true;
          parse_server_aux ll
      | (Element ("disablepartialrequests", [], []))::ll ->
          set_disablepartialrequests true;
          parse_server_aux ll
      | (Element ("respectpipeline", [], []))::ll ->
          set_respect_pipeline ();
          parse_server_aux ll
      | (Element ("findlib", ["path",p], []))::ll ->
          Ocsigen_loader.add_ocamlpath p;
          parse_server_aux ll
      | (Element ("require", atts, l))::ll
      | (Element ("extension", atts, l))::ll ->
          (* We do not reload extensions *)
          let modules = match atts with
            | [] ->
                raise
                  (Config_file_error "missing module, name or findlib-package attribute in <extension>")
            | [("name", s)] -> `Name s
            | [("module", s)] -> `Files [s]
            | [("findlib-package", s)] -> `Files (Ocsigen_loader.findfiles s)
            | _ ->
                raise (Config_file_error "Wrong attribute for <extension>")
          in begin
            match modules with
              | `Files modules ->
                  Ocsigen_loader.loadfiles (preloadfile l) postloadfile false modules;
              | `Name name ->
                  Ocsigen_loader.init_module (preloadfile l) postloadfile false name
          end;
          parse_server_aux ll
      | (Element ("library", atts, l))::ll ->
          let modules = match atts with
            | [] ->
                raise
                  (Config_file_error "missing module or findlib-package attribute in <library>")
            | [("name", s)] -> `Name s
            | [("module", s)] -> `Files [s]
            | [("findlib-package", s)] -> `Files (Ocsigen_loader.findfiles s)
            | _ -> raise (Config_file_error "Wrong attribute for <library>")
          in begin
            match modules with
              | `Files modules ->
                  Ocsigen_loader.loadfiles (preloadfile l) postloadfile true modules;
              | `Name name ->
                  Ocsigen_loader.init_module (preloadfile l) postloadfile true name
          end;
          parse_server_aux ll
      | (Element ("host", atts, l))::ll ->
          let rec parse_attrs ((name, 
                                charset, 
                                defaulthostname, 
                                defaulthttpport, 
                                defaulthttpsport) as r) = function
            | [] -> r
            | ("name", s)::suite (*VVV deprecated!! remove it in 1.2 *)
            | ("hostfilter", s)::suite
            | ("aliases", s)::suite (*VVV deprecated!! remove it in 1.3 *) ->
                (match name with
                | None -> parse_attrs ((Some s), charset, 
                                       defaulthostname, 
                                       defaulthttpport, 
                                       defaulthttpsport) suite
                | _ -> raise (Ocsigen_config.Config_file_error
                                ("Duplicate attribute name in <host>")))
            | ("charset", s)::suite ->
                (match charset with
                | None -> parse_attrs (name, Some s, 
                                       defaulthostname, 
                                       defaulthttpport, 
                                       defaulthttpsport) suite
                | _ -> raise (Ocsigen_config.Config_file_error
                                ("Duplicate attribute charset in <host>")))
            | ("defaulthostname", s)::suite
            | ("hostname", s)::suite (*VVV deprecated!! remove it in 1.3 *) ->
                (match defaulthostname with
                | None -> parse_attrs (name, charset, 
                                       (Some s), 
                                       defaulthttpport, 
                                       defaulthttpsport) suite
                | _ -> raise (Ocsigen_config.Config_file_error
                                ("Duplicate attribute defaulthostname in <host>")))
            | ("defaulthttpport", s)::suite ->
                (match defaulthttpport with
                | None -> parse_attrs (name, charset, 
                                       defaulthostname, 
                                       (Some s), 
                                       defaulthttpsport) suite
                | _ -> raise (Ocsigen_config.Config_file_error
                                ("Duplicate attribute defaulthttpport in <host>")))
            | ("defaulthttpsport", s)::suite ->
                (match defaulthttpsport with
                | None -> parse_attrs (name, charset, 
                                       defaulthostname, 
                                       defaulthttpport, 
                                       Some s) suite
                | _ -> raise (Ocsigen_config.Config_file_error
                                ("Duplicate attribute defaulthttpsport in <host>")))
            | (s, _)::_ ->
                raise (Ocsigen_config.Config_file_error
                         ("Wrong attribute for <host>: "^s))
          in
          let host, charset, defaulthostname, defaulthttpport,defaulthttpsport =
            parse_attrs (None, None, None, None, None) atts
          in
          let host = match host with
          | None -> [[Ocsigen_extensions.Wildcard], None] (* default = "*:*" *)
          | Some s ->
              List.map
                (fun ss ->
                  let host, port =
                    try
                      let dppos = String.index ss ':' in
                      let len = String.length ss in
                      ((String.sub ss 0 dppos),
                       match String.sub ss (dppos+1) ((len - dppos) - 1) with
                         | "*" -> None
                         | p -> Some (int_of_string "host" p))
                    with
                      | Not_found -> ss, None
                      | Failure _ ->
                          raise (Config_file_error "bad port number")
                  in
                  ((List.map
                      (function
                         | Netstring_str.Delim _ -> Ocsigen_extensions.Wildcard
                         | Netstring_str.Text t ->
                            Ocsigen_extensions.Text (t, String.length t))
                      (Netstring_str.full_split (Netstring_str.regexp "[*]+")
                         host)),
                   port))
                (Netstring_str.split (Netstring_str.regexp "[ \t]+") s)
          in
          let charset =
            match charset, Ocsigen_config.get_default_charset () with
              | Some charset, _
              | None, Some charset -> charset
              | None, None -> "utf-8"
          in
          let defaultdefaulthostname = default_hostname in
          let defaulthostname = match defaulthostname with
            | Some d -> d
            | None ->
                try
                  (match
                    fst
                      (List.find 
                         (* We look for a hostname without wildcard *)
                         (*VVV one could do sthg more clever *)
                         (function
                            | ([Ocsigen_extensions.Text (t, _)], None) -> true
                            | _ -> false
                         ) 
                         host)
                   with
                     | [Ocsigen_extensions.Text (t, _)] -> 
                         Ocsigen_messages.warning
                           ("While parsing config file, tag <host>: Assuming defaulthostname is \""^t^"\"");
                         t
                     | _ -> 
                         defaultdefaulthostname)
                with Not_found -> 
                  Ocsigen_messages.warning
                    ("While parsing config file, tag <host>: Assuming defaulthostname is \""^defaultdefaulthostname^"\"");
                  defaultdefaulthostname
          in
          let defaulthttpport = match defaulthttpport with
            | None ->
                (try snd (List.hd (Ocsigen_config.get_ports ())) 
                with Failure _ -> 80)
            | Some p -> int_of_string "host" p
          in
          let defaulthttpsport = match defaulthttpsport with
            | None ->
                (try snd (List.hd (Ocsigen_config.get_sslports ())) 
                with Failure _ -> 443)
            | Some p -> int_of_string "host" p
          in
          let parse_host = Ocsigen_extensions.parse_config_item host in
          let conf = {
            Ocsigen_extensions.default_hostname = defaulthostname;
            default_httpport = defaulthttpport;
            default_httpsport = defaulthttpsport;
            mime_assoc = Ocsigen_charset_mime.default_mime_assoc ();
            charset_assoc = Ocsigen_charset_mime.empty_charset_assoc
              ~default:charset ();
            default_directory_index = ["index.html"];
            list_directory_content = false;
            follow_symlinks = Ocsigen_extensions.FollowSymlinksIfOwnerMatch;
            do_not_serve_404 = [];
            do_not_serve_403 = [];
          }
          in
          let parse_config =
            Ocsigen_extensions.make_parse_config [] parse_host
          in
          (* default site for host *)
          (host, conf, parse_config l)::(parse_server_aux ll)
      | (Element ("extconf", [("dir", dir)], []))::ll ->
          let one =
            try
              let files = Sys.readdir dir in
              Array.sort compare files;
              Array.fold_left
                (fun l s ->
                  if Filename.check_suffix s "conf" then
                    let filename = dir^"/"^s in
                    let filecont =
                      try
                        Ocsigen_messages.debug (fun () -> "Parsing configuration file "^
                          filename);
                        parse_ext filename
                      with e ->
                        Ocsigen_messages.errlog
                          ("Error while loading configuration file "^filename^
                           ": "^(Ocsigen_lib.string_of_exn e)^" (ignored)");
                        []
                    in
                    (match filecont with
                    | [] -> l
                    | s::_ -> l@(parse_server_aux s)
                    )
                  else l
                )
                []
                files
            with
            | Sys_error _ as e ->
                Ocsigen_messages.errlog
                  ("Error while loading configuration file: "^
                   ": "^(Ocsigen_lib.string_of_exn e)^" (ignored)");
                []
          in
          one@(parse_server_aux ll)
      | (Element (tag, _, _))::_ ->
          raise (Config_file_error
                   ("tag <"^tag^"> unexpected inside <server>"))
      | _ ->
          raise (Config_file_error "Syntax error")
  in Ocsigen_extensions.set_hosts (parse_server_aux c)


(* Parsing <port> tags *)
let parse_port =
  let all_ipv6 = Netstring_pcre.regexp "^\\[::\\]:([0-9]+)$" in
  let all_ipv4 = Netstring_pcre.regexp "^\\*:([0-9]+)$" in
  let single_ipv6 = Netstring_pcre.regexp "^\\[([0-9A-Fa-f.:]+)\\]:([0-9]+)$" in
  let single_ipv4 = Netstring_pcre.regexp "^([0-9.]+):([0-9]+)$" in
  fun s ->
    let do_match r = Netstring_pcre.string_match r s 0 in
    let get x i = Netstring_pcre.matched_group x i s in
    match do_match all_ipv6 with
      | Some r -> Some (Unix.inet6_addr_any), int_of_string "port" (get r 1)
      | None -> match do_match all_ipv4 with
      | Some r -> Some (Unix.inet_addr_any), int_of_string "port" (get r 1)
      | None -> match do_match single_ipv6 with
      | Some r -> Some (Unix.inet_addr_of_string (get r 1)), 
          int_of_string "port" (get r 2)
      | None -> match do_match single_ipv4 with
      | Some r -> Some (Unix.inet_addr_of_string (get r 1)), 
          int_of_string "port" (get r 2)
      | None -> None, int_of_string "port" s


(* First parsing of config file *)
let extract_info c =
  let rec parse_ssl certificate privatekey = function
      [] -> Some (certificate,privatekey)
    | (Element ("certificate" as st, [], p))::l ->
        (match certificate with
          None ->
            parse_ssl (Some (parse_string_tag st p)) privatekey l
        | _ -> raise (Config_file_error
                        "Two certificates inside <ssl>"))
    | (Element ("privatekey" as st, [], p))::l ->
        (match privatekey with
          None ->
            parse_ssl certificate (Some (parse_string_tag st p)) l
        | _ -> raise (Config_file_error
                        "Two private keys inside <ssl>"))
    | (Element (tag,_,_))::l ->
        raise (Config_file_error ("<"^tag^"> tag unexpected inside <ssl>"))
    | _ -> raise (Config_file_error ("Unexpected content inside <ssl>"))
  in
  let rec aux user group ssl ports sslports minthreads maxthreads = function
      [] -> ((user, group), (ssl, ports,sslports), (minthreads, maxthreads))
    | (Element ("logdir" as st, [], p))::ll ->
        set_logdir (parse_string_tag st p);
        aux user group ssl ports sslports minthreads maxthreads ll
    | (Element ("port" as st, atts, p))::ll ->
        (match atts with
          []
        | [("protocol", "HTTP")] ->
            let po = try
              parse_port (parse_string_tag st p)
            with Failure _ ->
              raise (Config_file_error "Wrong value for <port> tag")
            in aux user group ssl (po::ports) sslports minthreads maxthreads ll
        | [("protocol", "HTTPS")] ->
            let po = try
              parse_port (parse_string_tag st p)
            with Failure _ ->
              raise (Config_file_error "Wrong value for <port> tag")
            in
            aux user group ssl ports (po::sslports) minthreads maxthreads ll
        | _ -> raise (Config_file_error "Wrong attribute for <port>"))
    | (Element ("minthreads" as st, [], p))::ll ->
        aux user group ssl ports sslports
          (Some (int_of_string st (parse_string_tag st p))) maxthreads ll
    | (Element ("maxthreads" as st, [], p))::ll ->
        aux user group ssl ports sslports minthreads
          (Some (int_of_string st (parse_string_tag st p))) ll
    | (Element ("ssl", [], p))::ll ->
        (match ssl with
          None ->
            aux user group (parse_ssl None None p) ports sslports
              minthreads maxthreads ll
        | _ ->
            raise
              (Config_file_error
                 "Only one ssl certificate for each server supported for now"))
    | (Element ("user" as st, [], p))::ll ->
        (match user with
          None ->
            aux (Some (parse_string_tag st p)) group ssl ports sslports
              minthreads maxthreads ll
        | _ -> raise (Config_file_error
                        "Only one <user> tag for each server allowed"))
    | (Element ("group" as st, [], p))::ll ->
        (match group with
          None ->
            aux user (Some (parse_string_tag st p)) ssl ports sslports
              minthreads maxthreads ll
        | _ -> raise (Config_file_error
                        "Only one <group> tag for each server allowed"))
    | (Element ("commandpipe" as st, [], p))::ll ->
        set_command_pipe (parse_string_tag st p);
        aux user group ssl ports sslports minthreads maxthreads ll
    | (Element (tag, _, _))::ll ->
        aux user group ssl ports sslports minthreads maxthreads ll
    | _ ->
        raise (Config_file_error "Syntax error")
  in
  let (user, group), si, (mint, maxt) = aux None None None [] [] None None c in
  let user = match user with
    None -> None (* Some (get_default_user ()) *)
  | Some s -> if s = "" then None else Some s
  in
  let group = match group with
    None -> None (* Some (get_default_group ()) *)
  | Some s -> if s = "" then None else Some s
  in
  let mint = match mint with
  | Some t -> t
  | None -> get_minthreads ()
  in
  let maxt = match maxt with
  | Some t -> t
  | None -> get_maxthreads ()
  in
  ((user, group), si, (mint, maxt))

let parse_config () =
    parser_config (Ocsigen_config.config ())

(******************************************************************)
