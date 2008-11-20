(* Ocsigen
 * http://www.ocsigen.org
 * Module rewritemod.ml
 * Copyright (C) 2008 Vincent Balat
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
(* Ocsigen extension for rewriteing URLs                                     *)
(* in the configuration file                                                 *)
(*****************************************************************************)
(*****************************************************************************)

(* IMPORTANT WARNING 
   It is really basic for now:
    - rewrites only subpaths (and do not change get parameters)
    - changes only ri_sub_path and ri_sub_path_tring
   not ri_full_path and ri_full_path_string and ri_url_string and ri_url
   This is probably NOT what we want ...
*)



(* To compile it:
ocamlfind ocamlc  -thread -package netstring,ocsigen -c extensiontemplate.ml

Then load it dynamically from Ocsigen's config file:
   <extension module=".../rewritemod.cmo"/>

*)

open Lwt
open Ocsigen_extensions
open Simplexmlparser


exception Not_concerned


(*****************************************************************************)
(* The table of rewrites for each virtual server                         *)
type assockind =
  | Regexp of Netstring_pcre.regexp * string




(*****************************************************************************)
let rec parse_global_config = function
  | [] -> ()
  | _ -> raise (Error_in_config_file
                  ("Unexpected content inside rewritemod config"))

let _ = parse_global_config (Ocsigen_extensions.get_config ())





(*****************************************************************************)
(* Finding rewrites *)

let find_rewrite (Regexp (regexp, dest)) suburl =
  match Netstring_pcre.string_match regexp suburl 0 with
  | None -> raise Not_concerned
  | Some _ -> (* Matching regexp found! *)
      Netstring_pcre.global_replace regexp dest suburl






(*****************************************************************************)
(** The function that will generate the pages from the request. *)
let gen dir charset = function
| Ocsigen_extensions.Req_found (_, r) -> 
    Lwt.return (Ocsigen_extensions.Ext_found r)
| Ocsigen_extensions.Req_not_found (err, ri) ->
  catch
    (* Is it a rewrite? *)
    (fun () ->
      Ocsigen_messages.debug2 "--Rewritemod: Is it a rewrite?";
      let redir =
        Netencoding.Url.encode
          (find_rewrite dir 
             (match ri.ri_get_params_string with
                | None -> ri.ri_sub_path_string
                | Some g -> ri.ri_sub_path_string ^ "?" ^ g))
      in
      Ocsigen_messages.debug (fun () ->
        "--Rewritemod: YES! rewrite to: "^redir);
      return
        (Ext_retry_with
           (Ocsigen_extensions.ri_of_url redir ri,
            Ocsigen_http_frame.Cookies.empty)
        )
    )
    (function
      | Not_concerned -> return (Ext_next err)
      | e -> fail e)




(*****************************************************************************)
(** Configuration for each site.
    These tags are inside <site ...>...</site> in the config file.

   For example:
   <site dir="">
     <rewrite regexp="" dest="" />
   </extension>

 *)

let parse_config path charset _ parse_site = function
  | Element ("rewrite", atts, []) ->
      let dir = match atts with
      | [] ->
          raise (Error_in_config_file
                   "regexp attribute expected for <rewrite>")
      | [("regexp", s); ("url", t)] ->
          Regexp ((Netstring_pcre.regexp ("^"^s^"$")), t)
      | _ -> raise (Error_in_config_file "Wrong attribute for <rewrite>")
      in
      gen dir charset
  | Element (t, _, _) ->
      raise (Bad_config_tag_for_extension t)
  | _ -> raise (Error_in_config_file "(rewritemod extension) Bad data")




(*****************************************************************************)
(** Registration of the extension *)
let _ = register_extension
  ~fun_site:(fun _ -> parse_config)
  ~user_fun_site:(fun _ _ -> parse_config)
  ()
