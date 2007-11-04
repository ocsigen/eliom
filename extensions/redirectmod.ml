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
open Extensions
open Simplexmlparser



(*****************************************************************************)
(* The table of redirections for each virtual server                         *)
type assockind = 
  | Regexp of Netstring_pcre.regexp * string * bool (* temporary *)




(*****************************************************************************)
let rec parse_global_config = function
  | [] -> ()
  | _ -> raise (Error_in_config_file 
                  ("Unexpected content inside redirectmod config"))

let _ = parse_global_config (Extensions.get_config ())





(*****************************************************************************)
(* Finding redirections *)

let find_redirection (Regexp (regexp, dest, temp)) path =
  let path = Ocsimisc.string_of_url_path path in
  match Netstring_pcre.string_match regexp path 0 with
  | None -> raise Ocsigen_404
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
let gen dir charset ri =
  catch
    (* Is it a redirection? *)
    (fun () ->
      Messages.debug ("--Redirectmod: Is it a redirection?");
      let (redir, temp) = find_redirection dir ri.ri_sub_path in
      Messages.debug ("--Redirectmod: YES! "^
                      (if temp then "Temporary " else "Permanent ")^
                      "redirection to: "^redir);      
      return
        (Ext_found
           {res_cookies=[];
	    res_send_page=
            (fun ?filter ?cookies waiter ~clientproto ?mode 
                ?code ?etag ~keep_alive
                ?last_modified ?location ~head ?headers ?charset s ->
                  Predefined_senders.send_empty
                    ~content:()
                    ?filter
                    ?cookies
                    waiter 
                    ~clientproto
                    ?mode
                    ?code
                    ?etag ~keep_alive
                    ?last_modified 
                    ~location:redir
                    ~head ?headers ?charset s);
	    res_headers=Http_headers.empty;
	    res_code= Some (if temp then 302 else 301);
	    res_lastmodified= None;
	    res_etag= None;
	    res_charset= None;
            res_filter=None})
    )
    (function 
      | Extensions.Ocsigen_404 -> return (Ext_not_found Ocsigen_404)
      | e -> fail e)




(*****************************************************************************)
(** Configuration for each site.
    These tags are inside <site ...>...</site> in the config file.
        
   For example:
   <site dir="">
     <redirect regexp="" dest="" />
   </extension>

 *)

let parse_config path = function
  | Element ("redirect", atts, []) -> 
        let dir = match atts with
        | [] -> 
            raise (Error_in_config_file
                     "regexp attribute expected for <redirect>")
        | [("regexp", s);("dest",t)] -> 
            Regexp ((Netstring_pcre.regexp ("/"^s)), t, false)
        | [("temporary", "temporary");("regexp", s);("dest",t)] -> 
            Regexp ((Netstring_pcre.regexp ("/"^s)), t, true)
        | _ -> raise (Error_in_config_file "Wrong attribute for <redirect>")
        in
        Page_gen (gen dir)
  | Element (t, _, _) -> 
      raise (Bad_config_tag_for_extension t)
  | _ -> raise (Error_in_config_file "(redirectmod extension) Bad data")





(*****************************************************************************)
(** A function that will be called for each virtual host,
   generating two functions: 
    - one that will be called to generate the pages
    - one to parse the configuration file. *)
let virtual_host_creator hostpattern = (gen, parse_config)
   (* hostpattern has type Extensions.virtual_hosts
      and represents the name of the virtual host *)
   

(*****************************************************************************)
(** Registration of the extension *)
let _ = register_extension (* takes a quadruple *)
    ((fun hostpattern path charset -> parse_config path),
     start_init,
     end_init,
     raise)

