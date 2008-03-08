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
open Extensions


(*****************************************************************************)

exception Failed_404

let gen hostpattern sitepath charset (regexp, conf, url, prefix) req_state = 
  match req_state with
  | Extensions.Req_found (_, r) -> Lwt.return (Extensions.Ext_found r)
(*VVV not possible to set a filter for now *)
  | Extensions.Req_not_found (previous_extension_err, ri) ->
      let path = ri.ri_sub_path_string in
      match Netstring_pcre.string_match regexp path 0 with
      | None -> Lwt.return (Ext_next previous_extension_err)
      | Some _ -> (* Matching regexp found! *)
          try
            Ocsigen_messages.debug2 "--Userconf: Using user configuration";
            let conf = 
              try
                Extensions.replace_user_dir regexp conf path 
              with Not_found -> raise Failed_404
            in
            let url = Netstring_pcre.global_replace regexp url path in
            let prefix = Netstring_pcre.global_replace regexp prefix path in
            ignore (Unix.stat conf);
            let user_parse_host = Extensions.parse_user_site_item hostpattern in
            let user_parse_site = 
              Extensions.make_parse_site
                (sitepath@[prefix]) charset user_parse_host 
            in
            let xmllist = Simplexmlparser.xmlparser conf in
            Lwt.return 
              (Ext_sub_result
                 (fun awake cookies_to_set rs ->
                   let path =
                     Ocsigen_lib.remove_slash_at_end
                       (Ocsigen_lib.remove_slash_at_beginning 
                          (Ocsigen_lib.remove_dotdot (Neturl.split_path url))) 
                   in
                   user_parse_site 
                     xmllist
                     awake
                     cookies_to_set
                     (Extensions.Req_not_found 
                        (previous_extension_err, 
                         {ri with
                          ri_sub_path = path; 
                          ri_sub_path_string = url}))
(*VVV We do not want to continue to search if the page has not been found *)
(*VVV Filters won't be applied. Is it the right behaviour? *)
(*VVV v v v *)
                   >>= 
                   let rec aux ((answer, cts) as r) =
                     match answer with
                     | Ext_found _ 
                     | Ext_stop_all _
                     | Ext_retry_with _ -> Lwt.return r
                     | Ext_next err -> 
                         Lwt.return
                           (Ext_stop_all (Http_frame.Cookies.empty, err),
                            cts)
                     | Ext_continue_with (_, cts2, err)
                     | Ext_stop_site (cts2, err)
                     | Ext_stop_host (cts2, err) -> 
                         Lwt.return (Ext_stop_all (cts2, err), cts)
                     | Ext_sub_result sr ->
                         sr awake cookies_to_set req_state
                         >>= aux
                   in aux
 (*VVV ^ ^ ^ *)
                 )
              )
          with
          | Unix.Unix_error (Unix.EACCES,_,_)
          | Unix.Unix_error (Unix.ENOENT, _, _) ->
              Lwt.return (Extensions.Ext_next previous_extension_err)
          | Failed_404 ->
              Lwt.return (Extensions.Ext_next 404)

          

(*****************************************************************************)
(** Parsing of config file *)
open Simplexmlparser

let parse_config hostpattern path charset =
  fun _ _ ->
    let rec parse_attrs_local ((regexp, conf, url, prefix) as res) = function
      | [] -> res
      | ("regexp", s)::l when regexp = None ->
          (try
            parse_attrs_local
              (Some (Netstring_pcre.regexp ("^"^s^"$")), conf, url, prefix)
              l
          with Failure _ ->
            raise (Error_in_config_file "Bad regexp in <userconf regexp=\"...\" />"))
      | ("conf", s)::l when conf = None ->
          parse_attrs_local
            (regexp, Some (Extensions.parse_user_dir s), url, prefix)
            l
      | ("url", s)::l when url = None ->
          parse_attrs_local
            (regexp, conf, Some s, prefix)
            l
      | ("prefix", s)::l when prefix = None ->
          parse_attrs_local
            (regexp, conf, url, Some s)
            l
      | _ -> raise (Error_in_config_file "Wrong attribute for <userconf>")
    in
    function
      | Element ("userconf", atts, []) -> 
          let info = 
            match parse_attrs_local (None, None, None, None) atts  with
            | (Some r, Some t, Some u, Some p) -> (r, t, u, p)
            | _ -> raise (Error_in_config_file 
                            "Missing attributes for <userconf>")
          in
          gen hostpattern path charset info
      | Element (t, _, _) -> raise (Bad_config_tag_for_extension t)
      | _ -> raise (Error_in_config_file "(userconf extension) Bad data")


(*****************************************************************************)
(** Function to be called at the beginning of the initialisation phase *)
let start_init () =
  ()

(** Function to be called at the end of the initialisation phase *)
let end_init () =
  ()



(*****************************************************************************)
(** extension registration *)
let _ = register_extension
    parse_config
    Extensions.void_extension
  (*fun hostpattern -> 
    parse_config (Extensions.parse_user_site_item hostpattern)*)
  start_init
  end_init
  raise

