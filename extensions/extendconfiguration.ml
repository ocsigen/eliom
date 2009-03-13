(* Ocsigen
 * http://www.ocsigen.org
 * Module rewritemod.ml
 * Copyright (C) 2008 Boris Yakobowski
 * CNRS - Université Paris Diderot Paris 7
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
open Lwt
open Ocsigen_extensions
open Simplexmlparser
open Ocsigen_charset_mime


let bad_config s = raise (Error_in_config_file s)

let gen configfun = function
  | Ocsigen_extensions.Req_found _ ->
      Lwt.return Ocsigen_extensions.Ext_do_nothing

  | Ocsigen_extensions.Req_not_found (err, request) ->
      Ocsigen_messages.debug2 "--Updating configuration";
      let updated_request = { request with request_config =
          configfun request.request_config }
      in
      Lwt.return
        (Ocsigen_extensions.Ext_continue_with
           (updated_request,
            Ocsigen_http_frame.Cookies.empty,
            err
           ))


let gather_do_not_serve_files tag =
  let rec aux (regexps, files, extensions) = function
    | [] -> {
        Ocsigen_extensions.do_not_serve_regexps = regexps;
        do_not_serve_files = files;
        do_not_serve_extensions = extensions
      }

    | Element ("regexp", ["regexp", f], []) :: q ->
        aux (f :: regexps, files, extensions) q
    | Element ("file", ["file", f], []) :: q ->
        aux (regexps, f :: files, extensions) q
    | Element ("extension", ["ext", f], []) :: q ->
        aux (regexps, files, f :: extensions) q

    | _ :: q -> bad_config ("invalid options in tag " ^ tag)
  in aux ([], [], [])



exception Bad_regexp of string

let check_regexp_list =
  let hashtbl = Hashtbl.create 17 in
  let aux r =
    try Hashtbl.find hashtbl r
    with Not_found ->
      try
        ignore (Netstring_pcre.regexp r);
        Hashtbl.add hashtbl r ()
      with _ -> raise (Bad_regexp r)
  in
  (fun l -> List.iter aux l)


let update_config usermode = function
  | Element ("listdirs", ["value", "true"], []) ->
      gen (fun config -> { config with list_directory_content = true })
  | Element ("listdirs", ["value", "false"], []) ->
      gen (fun config -> { config with list_directory_content = false })
  | Element ("listdirs" as s, _, _) -> badconfig "Bad syntax for tag %s" s


  | Element ("followsymlinks", ["value", s], []) ->
      let v = match s with
        | "never" -> DoNotFollowSymlinks
        | "always" ->
            if usermode = false then
              AlwaysFollowSymlinks
            else
              raise (Error_in_user_config_file
                       "Cannot specify value 'always' for option \
                        'followsymlinks' in userconf files")
        | "ownermatch" -> FollowSymlinksIfOwnerMatch
        | _ ->
            bad_config ("Wrong value \""^s^"\" for option \"followsymlinks\"")
      in
      gen (fun config -> { config with follow_symlinks = v })
  | Element ("followsymlinks" as s, _, _) -> badconfig "Bad syntax for tag %s" s


  | Element ("charset", attrs, exts) ->
      let rec aux charset_assoc = function
        | [] -> charset_assoc
        | Element ("extension", ["ext", extension; "value", charset], []) :: q ->
            aux (update_charset_ext charset_assoc extension charset) q
        | Element ("file", ["file", file; "value", charset], []) :: q ->
            aux (update_charset_file charset_assoc file charset) q
        | Element ("regexp", ["regexp", regexp; "value", charset], []) :: q ->
            (try
               let r = Netstring_pcre.regexp regexp in
               aux (update_charset_regexp charset_assoc r charset) q
             with _ -> bad_config "invalid regexp '%s' in <extension regexp ...>")
        | _ :: q -> bad_config "invalid subtag in option charset"
      in
      gen (fun config ->
             let config = match attrs with
               | ["default", s] ->
                   { config with charset_assoc =
                       set_default_charset config.charset_assoc s }
               | [] -> config
               | _ -> bad_config "Only attribute \"default\" is permitted \
                           for option \"charset\""
             in
             { config with charset_assoc = aux config.charset_assoc exts })


  | Element ("contenttype", attrs, exts) ->
      let rec aux mime_assoc = function
        | [] -> mime_assoc
        | Element ("extension", ["ext", extension; "value", mime], []) :: q ->
            aux (update_mime_ext mime_assoc extension mime) q
        | Element ("file", ["file", file; "value", mime], []) :: q ->
            aux (update_mime_file mime_assoc file mime) q
        | Element ("regexp", ["regexp", regexp; "value", mime], []) :: q ->
            (try
               let r = Netstring_pcre.regexp regexp in
               aux (update_mime_regexp mime_assoc r mime) q
             with _ -> bad_config "invalid regexp '%s' in <extension regexp ...>")
        | _ :: q -> bad_config "invalid subtag in option mime"
      in
      gen (fun config ->
             let config = match attrs with
               | ["default", s] ->
                   { config with mime_assoc =
                       set_default_mime config.mime_assoc s }
               | [] -> config
               | _ -> bad_config "Only attribute \"default\" is permitted \
                           for option \"contenttype\""
             in
             { config with mime_assoc = aux config.mime_assoc exts })


  | Element ("defaultindex", [], l) ->
      let rec aux indexes = function
        | [] -> List.rev indexes
        | Element ("index", [], [PCData f]) :: q ->
            aux (f :: indexes) q
        | _ :: q -> bad_config "subtags must be of the form \
                      <index>...</index> \
                      in option defaultindex"
      in
      gen (fun config ->
             { config with default_directory_index = aux [] l })
  | Element ("defaultindex" as s, _, _) -> badconfig "Bad syntax for tag %s" s

  | Element ("hidefile", [], l) ->
      let do_not_serve = gather_do_not_serve_files "hidefile" l in
      (try
         check_regexp_list do_not_serve.do_not_serve_regexps;
         gen (fun config ->
                { config with do_not_serve_404 =
                    join_do_not_serve do_not_serve config.do_not_serve_404 })
       with Bad_regexp r ->
         badconfig "Invalid regexp %s in %s" r "hidefile")
  | Element ("hidefile" as s, _, _) -> badconfig "Bad syntax for tag %s" s

  | Element ("forbidfile", [], l) ->
      let do_not_serve = gather_do_not_serve_files "forbidfile" l in
      (try
         check_regexp_list do_not_serve.do_not_serve_regexps;
         gen (fun config ->
                { config with do_not_serve_403 =
                    join_do_not_serve do_not_serve config.do_not_serve_403 })
       with Bad_regexp r ->
         badconfig "Invalid regexp %s in %s" r "forbidfile")
  | Element ("forbidfile" as s, _, _) -> badconfig "Bad syntax for tag %s" s

  | Element (t, _, _) -> raise (Bad_config_tag_for_extension t)
  | _ ->
      raise (Error_in_config_file "Unexpected data in config file")


let parse_config usermode : parse_config_aux = fun _ _ _ xml ->
  update_config usermode xml


let () = register_extension
  ~name:"extendconfiguration"
  ~fun_site:(fun _ -> parse_config false)
  ~user_fun_site:(fun path _ -> parse_config true)
  ()
