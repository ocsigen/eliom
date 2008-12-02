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


exception Not_concerned


(*****************************************************************************)
(* The table of redirections for each virtual server                         *)
type assockind =
  | Regexp of Netstring_pcre.regexp * string * bool (* temporary *)




(*****************************************************************************)
let rec parse_global_config = function
  | [] -> ()
  | _ -> raise (Error_in_config_file
                  ("Unexpected content inside redirectmod config"))

let _ = parse_global_config (Ocsigen_extensions.get_config ())





(*****************************************************************************)
(* Finding redirections *)

let find_redirection (Regexp (regexp, dest, temp)) https host port 
    get_params_string
    sub_path_string
    full_path_string
    =
  let path =
    match get_params_string with
      | None -> sub_path_string
      | Some g -> sub_path_string ^ "?" ^ g
  in
  match Netstring_pcre.string_match regexp path 0 with
  | None -> 
      (match host with
        | None -> raise Not_concerned
        | Some host ->
            let path =
              match get_params_string with
                | None -> full_path_string
                | Some g -> full_path_string ^ "?" ^ g
            in
            let path =
              if (not https) && port = 80
              then "http://"^host^"/"^path
              else if https && port = 443
              then "https://"^host^"/"^path
              else if https
              then "https://"^host^":"^(string_of_int port)^"/"^path
              else "http://"^host^":"^(string_of_int port)^"/"^path
            in
            (match Netstring_pcre.string_match regexp path 0 with
               | None -> raise Not_concerned
               | Some _ -> (* Matching regexp found! *)
                   (Netstring_pcre.global_replace regexp dest path, temp)
            ))
  | Some _ -> (* Matching regexp found! *)
      (Netstring_pcre.global_replace regexp dest path, temp)






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
      let (redir, temp) =
        find_redirection dir
          ri.request_info.ri_ssl
          ri.request_info.ri_host
          ri.request_info.ri_server_port
          ri.request_info.ri_get_params_string
          ri.request_info.ri_sub_path_string
          ri.request_info.ri_full_path_string
      in
      Ocsigen_messages.debug (fun () ->
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
                   Ocsigen_http_frame.res_code= if temp then 302 else 301}))
    )
    (function
      | Not_concerned -> return (Ext_next err)
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
      let dir = match atts with
      | [] ->
          raise (Error_in_config_file
                   "regexp attribute expected for <redirect>")
      | [("regexp", s);("dest",t)] ->
          Regexp ((Netstring_pcre.regexp ("^"^s^"$")), t, false)
      | [("temporary", "temporary");("regexp", s);("dest",t)] ->
          Regexp ((Netstring_pcre.regexp ("^"^s^"$")), t, true)
      | _ -> raise (Error_in_config_file "Wrong attribute for <redirect>")
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
