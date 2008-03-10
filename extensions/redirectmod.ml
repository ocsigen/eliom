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

let find_redirection (Regexp (regexp, dest, temp)) path =
  match Netstring_pcre.string_match regexp path 0 with
  | None -> raise Not_concerned
  | Some _ -> (* Matching regexp found! *)
      (Netstring_pcre.global_replace regexp dest path, temp)
  






(*****************************************************************************)
(** Function to be called at the beginning of the initialisation phase 
    of the server (actually each time the config file is reloaded) *)
let start_init () =
  ()

(** Function to be called at the end of the initialisation phase *)
let end_init () =
  ()



(*****************************************************************************)
(** The function that will generate the pages from the request. *)
let gen dir charset = function
| Ocsigen_extensions.Req_found (_, r) -> Lwt.return (Ocsigen_extensions.Ext_found r)
| Ocsigen_extensions.Req_not_found (err, ri) ->
  catch
    (* Is it a redirection? *)
    (fun () ->
      Ocsigen_messages.debug2 "--Redirectmod: Is it a redirection?";
      let (redir, temp) = 
        find_redirection dir ri.ri_sub_path_string 
      in
      Ocsigen_messages.debug (fun () ->
        "--Redirectmod: YES! "^
        (if temp then "Temporary " else "Permanent ")^
        "redirection to: "^redir);      
      let empty_result = Http_frame.empty_result () in
      return
        (Ext_found
           (fun () ->
              Lwt.return 
                {empty_result with
                   Http_frame.res_location = Some redir;
	           Http_frame.res_code= if temp then 302 else 301}))
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

let parse_config path charset _ parse_site = function
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
      gen dir charset
  | Element (t, _, _) -> 
      raise (Bad_config_tag_for_extension t)
  | _ -> raise (Error_in_config_file "(redirectmod extension) Bad data")





(*****************************************************************************)
(** A function that will be called for each virtual host,
   generating two functions: 
    - one that will be called to generate the pages
    - one to parse the configuration file. *)
let virtual_host_creator hostpattern = (gen, parse_config)
   (* hostpattern has type Ocsigen_extensions.virtual_hosts
      and represents the name of the virtual host *)
   

(*****************************************************************************)
(** Registration of the extension *)
let _ = register_extension (* takes a quadruple *)
    (fun hostpattern -> parse_config)
    (fun hostpattern -> parse_config)
    start_init
    end_init
    raise

