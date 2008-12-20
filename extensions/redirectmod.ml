(* Ocsigen
 * http://www.ocsigen.org
 * Module redirectmod.ml
 * Copyright (C) 2007 Vincent Balat
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
(*****************************************************************************)
(*****************************************************************************)
(* Ocsigen extension for defining page redirections                          *)
(* in the configuration file                                                 *)
(*****************************************************************************)
(*****************************************************************************)

(* To compile it:
ocamlfind ocamlc  -thread -package netstring,ocsigen -c extensiontemplate.ml

Then load it dynamically from Ocsigen's config file:
   <extension module=".../redirectmod.cmo"/>

*)

open Lwt
open Ocsigen_extensions
open Simplexmlparser



(*****************************************************************************)
(* The table of redirections for each virtual server                         *)
type assockind =
  | Regexp of Netstring_pcre.regexp * string
      * Ocsigen_lib.yesnomaybe (* full url *) 
      * bool (* temporary *)




(*****************************************************************************)
let rec parse_global_config = function
  | [] -> ()
  | _ -> raise (Error_in_config_file
                  ("Unexpected content inside redirectmod config"))

let _ = parse_global_config (Ocsigen_extensions.get_config ())










(*****************************************************************************)
(** The function that will generate the pages from the request. *)
let gen dir = function
  | Ocsigen_extensions.Req_found _ -> 
      Lwt.return Ocsigen_extensions.Ext_do_nothing
  | Ocsigen_extensions.Req_not_found (err, ri) ->
      catch
        (* Is it a redirection? *)
        (fun () ->
           Ocsigen_messages.debug2 "--Redirectmod: Is it a redirection?";
           let Regexp (regexp, dest, full, temp) = dir in
           let redir =
             let fi full =
               Ocsigen_extensions.find_redirection
                 regexp
                 full
                 dest
                 ri.request_info.ri_ssl
                 ri.request_info.ri_host
                 ri.request_info.ri_server_port
                 ri.request_info.ri_get_params_string
                 ri.request_info.ri_sub_path_string
                 ri.request_info.ri_full_path_string
             in
             match full with
               | Ocsigen_lib.Yes -> fi true
               | Ocsigen_lib.No -> fi false
               | Ocsigen_lib.Maybe -> 
                   try fi false 
                   with Ocsigen_extensions.Not_concerned -> fi true
           in
           Ocsigen_messages.debug
             (fun () ->
                "--Redirectmod: YES! "^
                  (if temp then "Temporary " else "Permanent ")^
                  "redirection to: "^redir);
           let empty_result = Ocsigen_http_frame.empty_result () in
           return
             (Ext_found
                (fun () ->
                   Lwt.return
                     {empty_result with
                        Ocsigen_http_frame.res_location = Some redir;
                        Ocsigen_http_frame.res_code= 
                         if temp then 302 else 301}))
        )
        (function
           | Ocsigen_extensions.Not_concerned -> return (Ext_next err)
           | e -> fail e)




(*****************************************************************************)
(** Configuration for each site.
    These tags are inside <site ...>...</site> in the config file.

   For example:
   <site dir="">
     <redirect regexp="" dest="" />
   </extension>

 *)

let parse_config path _ parse_site = function
  | Element ("redirect", atts, []) ->
      let rec parse_attrs ((r, f, d, pipeline) as res) = function
        | [] -> res
        | ("regexp", regexp)::l when r = None -> (* deprecated *)
            parse_attrs
              (Some (Netstring_pcre.regexp ("^"^regexp^"$")), Ocsigen_lib.Maybe,
               d, pipeline)
              l
        | ("fullurl", regexp)::l when r = None ->
            parse_attrs
              (Some (Netstring_pcre.regexp ("^"^regexp^"$")), Ocsigen_lib.Yes,
               d, pipeline)
              l
        | ("suburl", regexp)::l when r = None ->
            parse_attrs
              (Some (Netstring_pcre.regexp ("^"^regexp^"$")), Ocsigen_lib.No,
               d, pipeline)
              l
        | ("dest", dest)::l when d = None ->
            parse_attrs
              (r, f, Some dest, pipeline)
              l
        | ("nopipeline", "nopipeline")::l ->
            parse_attrs
              (r, f, d, false)
              l
        | _ -> raise (Error_in_config_file "Wrong attribute for <redirect>")
        in
        let dir =
          match parse_attrs (None, Ocsigen_lib.Yes, None, true) atts with
          | (None, _, _, _) -> 
              raise (Error_in_config_file
                       "Missing attribute regexp for <redirect>")
          | (_, _, None, _) -> 
              raise (Error_in_config_file
                       "Missing attribute dest for <redirect>>")
          | (Some r, full, Some d, pipeline) ->
              Regexp (r, d, full, pipeline)
        in
        gen dir
  | Element (t, _, _) ->
      raise (Bad_config_tag_for_extension t)
  | _ -> raise (Error_in_config_file "(redirectmod extension) Bad data")




(*****************************************************************************)
(** Registration of the extension *)
let _ = register_extension
  ~fun_site:(fun _ -> parse_config)
  ~user_fun_site:(fun _ _ -> parse_config)
  ()
