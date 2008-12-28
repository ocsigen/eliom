(* Ocsigen
 * http://www.ocsigen.org
 * Module outputfilter.ml
 * Copyright (C) 2008 Vincent Balat
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
(* This module allows to rewrite the output sent by the server               *)
(*****************************************************************************)
(*****************************************************************************)

open Lwt
open Ocsigen_extensions
open Simplexmlparser
open Ocsigen_headers


let gen (header, regexp, dest) = function
  | Req_not_found (code,_) -> return (Ext_next code)
  | Req_found (ri, res) ->
      try
        let header_values =
          Http_headers.find_all header res.Ocsigen_http_frame.res_headers
        in
        let h =
          Http_headers.replace_opt header None res.Ocsigen_http_frame.res_headers
        in
        let new_headers =
          List.fold_left
            (fun h value ->

              Http_headers.add
                header
                (Netstring_pcre.global_replace regexp dest value)
                h
            )
            h
            header_values
        in
        Lwt.return
          (Ocsigen_extensions.Ext_found
             (fun () ->
               Lwt.return
                 {res with Ocsigen_http_frame.res_headers = new_headers}))
      with Not_found ->
        Lwt.return (Ocsigen_extensions.Ext_found (fun () -> Lwt.return res))



(*****************************************************************************)
(** Extensions may define new tags for configuring each site.
    These tags are inside <site ...>...</site> in the config file.

   For example:
   <site dir="">
     <extensiontemplate module=".../mymodule.cmo" />
   </site>

   Each extension will set its own configuration options, for example:
   <site dir="">
     <extensiontemplate module=".../mymodule.cmo" />
     <eliom module=".../myeliommodule.cmo" />
     <static dir="/var/www/plop" />
   </site>

 *)

let parse_config path _ _ = function
  | Element ("outputfilter", atts, []) ->
      let rec parse_attrs ((h, r, d) as res) = function
        | [] -> res
        | ("header", header)::l when h = None ->
            parse_attrs (Some header, r, d) l
        | ("regexp", regexp)::l when r = None ->
            parse_attrs (h, Some (Netstring_pcre.regexp regexp), d) l
        | ("dest", dest)::l when d = None ->
            parse_attrs (h, r, Some dest) l
        | _ -> raise (Error_in_config_file "Wrong attribute for <outputfilter header=... regexp=... dest=... />")
      in
      (match parse_attrs (None, None, None) atts with
      | (Some h, Some r, Some d) ->
          gen (Http_headers.name h, r, d)
      | _ ->
          raise
            (Error_in_config_file
               "Missing attributes for <outputfilter header=... regexp=... dest=... />"))
  | Element (t, _, _) -> raise (Bad_config_tag_for_extension t)
  | _ ->
      raise (Error_in_config_file "Unexpected data in config file")




(*****************************************************************************)
let site_creator hostpattern = parse_config
   (* hostpattern has type Ocsigen_extensions.virtual_hosts
      and represents the name of the virtual host *)


(*****************************************************************************)
(** Registration of the extension *)
let () = register_extension
  ~name:"outputfilter"
  ~fun_site:site_creator
  ~user_fun_site:(fun _ -> site_creator)
  ()
