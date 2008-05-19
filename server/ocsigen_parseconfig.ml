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
(* 2006 07 : Add multiple servers -- Nataliya *)
(* 2006 12 : Changes for extensions -- Vincent *)

open Simplexmlparser
open Ocsigen_config

let int_of_string s =
  try
    int_of_string (Ocsigen_lib.remove_spaces s 0 ((String.length s) -1))
  with Failure _ -> raise (Ocsigen_config.Config_file_error
                             (s^" is not a valid integer."))

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
      with Failure _ -> raise (Ocsigen_config.Config_file_error (s^" is not a valid size."))
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
  | [] -> ""
  | (PCData s)::l -> s^(parse_string l)
  | _ -> raise (Config_file_error "string expected")

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
  parser_config (Simplexmlparser.xmlparser file)


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
      | (Element ("charset", atts, p))::ll ->
          set_default_charset (Some (parse_string p));
          parse_server_aux ll
      | (Element ("logdir", [], p))::ll ->
          parse_server_aux ll
      | (Element ("ssl", [], p))::ll ->
          parse_server_aux ll
      | (Element ("user", [], p))::ll ->
          parse_server_aux ll
      | (Element ("group", [], p))::ll ->
          parse_server_aux ll
      | (Element ("uploaddir", [], p))::ll ->
          set_uploaddir (Some (parse_string p));
          parse_server_aux ll
      | (Element ("datadir", [], p))::ll ->
          set_datadir (parse_string p);
          parse_server_aux ll
      | (Element ("minthreads", [], p))::ll ->
          set_minthreads (int_of_string (parse_string p));
          parse_server_aux ll
      | (Element ("maxthreads", [], p))::ll ->
          set_maxthreads (int_of_string (parse_string p));
          parse_server_aux ll
      | (Element ("maxdetachedcomputationsqueued", [], p))::ll ->
          set_max_number_of_threads_queued (int_of_string (parse_string p));
          parse_server_aux ll
      | (Element ("maxconnected", [], p))::ll ->
          set_max_number_of_connections (int_of_string (parse_string p));
          parse_server_aux ll
      | (Element ("mimefile", [], p))::ll ->
          Ocsigen_config.set_mimefile (parse_string p);
          parse_server_aux ll
      | (Element ("timeout", [], p))::ll
(*VVV timeout: backward compatibility with <= 0.99.4 *)
      | (Element ("clienttimeout", [], p))::ll ->
          set_client_timeout (int_of_string (parse_string p));
          parse_server_aux ll
      | (Element ("servertimeout", [], p))::ll ->
          set_server_timeout (int_of_string (parse_string p));
          parse_server_aux ll
(*VVV For now we use silentservertimeout and silentclienttimeout also
  for keep alive :-(
      | (Element ("keepalivetimeout", [], p))::ll ->
          set_keepalive_timeout (int_of_string (parse_string p));
          parse_server_aux ll
      | (Element ("keepopentimeout", [], p))::ll ->
          set_keepopen_timeout (int_of_string (parse_string p));
          parse_server_aux ll
*)
      | (Element ("netbuffersize", [], p))::ll ->
          set_netbuffersize (int_of_string (parse_string p));
          parse_server_aux ll
      | (Element ("filebuffersize", [], p))::ll ->
          set_filebuffersize (int_of_string (parse_string p));
          parse_server_aux ll
      | (Element ("maxrequestbodysize", [], p))::ll ->
          set_maxrequestbodysize (parse_size (parse_string p));
          parse_server_aux ll
      | (Element ("maxuploadfilesize", [], p))::ll ->
          set_maxuploadfilesize (parse_size (parse_string p));
          parse_server_aux ll
      | (Element ("commandpipe", [], p))::ll ->
          set_command_pipe (parse_string p);
          parse_server_aux ll
      | (Element ("debugmode", [], []))::ll ->
          set_debugmode true;
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
                  (Config_file_error "missing module or findlib-package attribute in <extension>")
            | [("module", s)] -> [s]
            | [("findlib-package", s)] -> Ocsigen_loader.findfiles s
            | _ ->
                raise (Config_file_error "Wrong attribute for <extension>")
          in
          Ocsigen_loader.loadfiles (preloadfile l) postloadfile false modules;
          parse_server_aux ll
      | (Element ("library", atts, l))::ll ->
          let modules = match atts with
            | [] ->
                raise
                  (Config_file_error "missing module or findlib-package attribute in <library>")
            | [("module", s)] -> [s]
            | [("findlib-package", s)] -> Ocsigen_loader.findfiles s
            | _ -> raise (Config_file_error "Wrong attribute for <library>")
          in
          Ocsigen_loader.loadfiles (preloadfile l) postloadfile true modules;
          parse_server_aux ll
      | (Element ("host", atts, l))::ll ->
          let rec parse_attrs (name, charset) = function
            | [] -> (name, charset)
            | ("name", s)::suite ->
                (match name with
                | None -> parse_attrs ((Some s), charset) suite
                | _ -> raise (Ocsigen_config.Config_file_error
                                ("Duplicate attribute name in <host>")))
            | ("charset", s)::suite ->
                (match charset with
                | None -> parse_attrs (name, Some s) suite
                | _ -> raise (Ocsigen_config.Config_file_error
                                ("Duplicate attribute charset in <host>")))
            | (s, _)::_ ->
                raise (Ocsigen_config.Config_file_error
                         ("Wrong attribute for <host>: "^s))
          in
          let host, charset = parse_attrs (None, None) atts in
          let host = match host with
          | None -> [[Ocsigen_extensions.Wildcard],None] (* default = "*:*" *)
          | Some s ->
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
                          Netstring_str.Delim _ -> Ocsigen_extensions.Wildcard
                        | Netstring_str.Text t ->
                            Ocsigen_extensions.Text (t, String.length t))
                      (Netstring_str.full_split (Netstring_str.regexp "[*]+")
                         host)),
                   port))
                (Netstring_str.split (Netstring_str.regexp "[ \t]+") s)
          in
          let charset = match charset with
          | None -> Ocsigen_config.get_default_charset ()
          | Some charset -> Some charset
          in
          let charset = match charset with
          | None -> "utf-8"
          | Some charset -> charset
          in
          let parse_host = Ocsigen_extensions.parse_site_item host in
          let parse_site = Ocsigen_extensions.make_parse_site [] charset parse_host in
          (* default site for host *)
          (host, parse_site l)::(parse_server_aux ll)
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
      | Some r -> Some (Unix.inet6_addr_any), int_of_string (get r 1)
      | None -> match do_match all_ipv4 with
      | Some r -> Some (Unix.inet_addr_any), int_of_string (get r 1)
      | None -> match do_match single_ipv6 with
      | Some r -> Some (Unix.inet_addr_of_string (get r 1)), int_of_string (get r 2)
      | None -> match do_match single_ipv4 with
      | Some r -> Some (Unix.inet_addr_of_string (get r 1)), int_of_string (get r 2)
      | None -> None, int_of_string s


(* First parsing of config file *)
let extract_info c =
  let rec parse_ssl certificate privatekey = function
      [] -> Some (certificate,privatekey)
    | (Element ("certificate", [], p))::l ->
        (match certificate with
          None ->
            parse_ssl (Some (parse_string p)) privatekey l
        | _ -> raise (Config_file_error
                        "Two certificates inside <ssl>"))
    | (Element ("privatekey", [], p))::l ->
        (match privatekey with
          None ->
            parse_ssl certificate (Some (parse_string p)) l
        | _ -> raise (Config_file_error
                        "Two private keys inside <ssl>"))
    | (Element (tag,_,_))::l ->
        raise (Config_file_error ("<"^tag^"> tag unexpected inside <ssl>"))
    | _ -> raise (Config_file_error ("Unexpected content inside <ssl>"))
  in
  let rec aux user group ssl ports sslports minthreads maxthreads = function
      [] -> ((user, group), (ssl, ports,sslports), (minthreads, maxthreads))
    | (Element ("logdir", [], p))::ll ->
        set_logdir (parse_string p);
        aux user group ssl ports sslports minthreads maxthreads ll
    | (Element ("port", atts, p))::ll ->
        (match atts with
          []
        | [("protocol", "HTTP")] ->
            let po = try
              parse_port (parse_string p)
            with Failure _ ->
              raise (Config_file_error "Wrong value for <port> tag")
            in aux user group ssl (po::ports) sslports minthreads maxthreads ll
        | [("protocol", "HTTPS")] ->
            let po = try
              parse_port (parse_string p)
            with Failure _ ->
              raise (Config_file_error "Wrong value for <port> tag")
            in
            aux user group ssl ports (po::sslports) minthreads maxthreads ll
        | _ -> raise (Config_file_error "Wrong attribute for <port>"))
    | (Element ("minthreads", [], p))::ll ->
        aux user group ssl ports sslports
          (Some (int_of_string (parse_string p))) maxthreads ll
    | (Element ("maxthreads", [], p))::ll ->
        aux user group ssl ports sslports minthreads
          (Some (int_of_string (parse_string p))) ll
    | (Element ("ssl", [], p))::ll ->
        (match ssl with
          None ->
            aux user group (parse_ssl None None p) ports sslports
              minthreads maxthreads ll
        | _ ->
            raise
              (Config_file_error
                 "Only one ssl certificate for each server supported for now"))
    | (Element ("user", [], p))::ll ->
        (match user with
          None ->
            aux (Some (parse_string p)) group ssl ports sslports
              minthreads maxthreads ll
        | _ -> raise (Config_file_error
                        "Only one <user> tag for each server allowed"))
    | (Element ("group", [], p))::ll ->
        (match group with
          None ->
            aux user (Some (parse_string p)) ssl ports sslports
              minthreads maxthreads ll
        | _ -> raise (Config_file_error
                        "Only one <group> tag for each server allowed"))
    | (Element ("commandpipe", [], p))::ll ->
        set_command_pipe (parse_string p);
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
