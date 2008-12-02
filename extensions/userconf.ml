(* Ocsigen
 * http://www.ocsigen.org
 * Module userconf.ml
 * Copyright (C) 2007 Vincent Balat
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
(*****************************************************************************)
(*****************************************************************************)
(* Ocsigen module to allow local (users) config files                        *)
(*****************************************************************************)
(*****************************************************************************)


open Lwt
open Ocsigen_lib
open Ocsigen_extensions

exception NoConfFile


(*****************************************************************************)

let gen hostpattern sitepath (regexp, conf, url, prefix, localpath) req_state =
  match req_state with
  | Ocsigen_extensions.Req_found _ -> 
      Lwt.return Ocsigen_extensions.Ext_do_nothing
(*VVV not possible to set a filter for now *)
  | Ocsigen_extensions.Req_not_found (previous_extension_err, req) ->
      let path = req.request_info.ri_sub_path_string in
      match Netstring_pcre.string_match regexp path 0 with
      | None -> Lwt.return (Ext_next previous_extension_err)
      | Some _ -> (* Matching regexp found! *)
          try
            Ocsigen_messages.debug2 "--Userconf: Using user configuration";
            let conf = Ocsigen_extensions.replace_user_dir regexp conf path in
            let url = Netstring_pcre.global_replace regexp url path in
            let prefix = Netstring_pcre.global_replace regexp prefix path in
            let userconf_options = {
              Ocsigen_extensions.localfiles_root =
                Ocsigen_extensions.replace_user_dir regexp localpath path
            } in
            let user_parse_host =
              Ocsigen_extensions.parse_user_site_item userconf_options hostpattern in
            let user_parse_site =
              Ocsigen_extensions.make_parse_config
                (sitepath@[prefix]) user_parse_host
            in
            let xmllist =
              try Simplexmlparser.xmlparser_file conf
              with Sys_error _ -> raise NoConfFile
            in
            Lwt.return
              (Ext_sub_result
                 (fun awake cookies_to_set rs ->
                   let path =
                     Ocsigen_lib.remove_slash_at_beginning
                       (Ocsigen_lib.remove_dotdot (Neturl.split_path url))
                   in
                   user_parse_site
                     xmllist
                     awake
                     cookies_to_set
                     (* The [request_config] field of the request is kept
                        in the enclosing site *)
                     (Ocsigen_extensions.Req_not_found
                        (previous_extension_err,
                         {req with
                            request_info =
                             { req.request_info with
                                 ri_sub_path = path;
                                 ri_sub_path_string = url}}))
(*VVV Do we want to continue to search in the same <site>
      if the page has not been found (???) *)
(*VVV Filters wouldn't be applied. *)
                   >>=
                   let rec aux ((answer, cts) as r) =
                     match answer with
                     | Ext_sub_result sr ->
                         sr awake cookies_to_set req_state
                         >>= aux
                     | Ext_continue_with (newreq, c, e) ->
                         (* We keep config information outside userconf! *)
                         Lwt.return 
                           ((Ext_continue_with ({req with 
                                                  request_config = 
                                                   newreq.request_config
                                               }, c, e)), cts)
                     | Ext_found_continue_with r ->
                         (* We keep config information outside userconf! *)
                         Lwt.return 
                           (Ext_found_continue_with
                              (fun () -> 
                                 r () >>= fun (r, newreq) ->
                                 Lwt.return 
                                   (r, {req with 
                                          request_config = 
                                        newreq.request_config
                                       })),
                            cts)
                     | _ -> Lwt.return r
                   in aux
 (*VVV ^ ^ ^ *)
                 )
              )
          with
          | Ocsigen_extensions.NoSuchUser
          | NoConfFile
          | Unix.Unix_error (Unix.EACCES,_,_)
          | Unix.Unix_error (Unix.ENOENT, _, _) ->
              Lwt.return (Ocsigen_extensions.Ext_next previous_extension_err)
          | Ocsigen_extensions.Error_in_user_config_file s ->
              Ocsigen_messages.errlog
                (Printf.sprintf "Userconf error for url %s: %s" url s);
              Lwt.return
                (Ocsigen_extensions.Ext_stop_site
                   (Ocsigen_http_frame.Cookies.empty, 500))



(*****************************************************************************)
(** Parsing of config file *)
open Simplexmlparser

let parse_config hostpattern path =
  fun _ _ ->
    let rec parse_attrs_local ((regexp, conf, url, prefix, path) as res) = function
      | [] -> res
      | ("regexp", s)::l when regexp = None ->
          (try
            parse_attrs_local
              (Some (Netstring_pcre.regexp ("^"^s^"$")), conf, url, prefix, path)
              l
          with Failure _ ->
            raise (Error_in_config_file "Bad regexp in <userconf regexp=\"...\" />"))
      | ("conf", s)::l when conf = None ->
          parse_attrs_local
            (regexp, Some (Ocsigen_extensions.parse_user_dir s), url, prefix, path)
            l
      | ("url", s)::l when url = None ->
          parse_attrs_local
            (regexp, conf, Some s, prefix, path)
            l
      | ("prefix", s)::l when prefix = None ->
          parse_attrs_local
            (regexp, conf, url, Some s, path)
            l
      | ("localpath", s) :: l when path = None ->
          parse_attrs_local
            (regexp, conf, url, prefix, Some (Ocsigen_extensions.parse_user_dir s))
            l
      | _ -> raise (Error_in_config_file "Wrong attribute for <userconf>")
    in
    function
      | Element ("userconf", atts, []) ->
          let info =
            match parse_attrs_local (None, None, None, None, None) atts  with
            | (Some r, Some t, Some u, Some p, Some p') -> (r, t, u, p, p')
            | _ -> raise (Error_in_config_file
                            "Missing attributes for <userconf>")
          in
          gen hostpattern path info
      | Element (t, _, _) -> raise (Bad_config_tag_for_extension t)
      | _ -> raise (Error_in_config_file "(userconf extension) Bad data")




(*****************************************************************************)
(** extension registration *)
let _ = register_extension
  ~fun_site:parse_config
  ()
