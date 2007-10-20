(* Ocsigen
 * http://www.ocsigen.org
 * Module extensiontemplate.ml
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
(* This is an example of extension for Ocsigen                               *)
(* Take this as a template for writing your own extensions to the Web server *)
(*****************************************************************************)
(*****************************************************************************)

(* If you want to create an extension to filter the output of the server
   (for ex: compression), have a look at deflatemod.ml as an example.
   It is very similar to this example, but using 
   Extensions.register_output_filter
   instead of Extensions.register_extension.
 *)

(* To compile it:
ocamlfind ocamlc  -thread -package netstring,ocsigen -c extensiontemplate.ml

Then load it dynamically from Ocsigen's config file:
   <extension module=".../extensiontemplate.cmo"/>

*)

open Lwt
open Extensions
open Simplexmlparser



(*****************************************************************************)
(** Extensions may take some options from the config file. 
    These options are written in xml inside the <extension> tag.
   For example:
   <extension module=".../extensiontemplate.cmo">
     <myoption myattr="hello">
        ...
     </myoption>
   </extension>
 *)

let rec parse_global_config = function
  | [] -> ()
  | (Element ("myoption", [("myattr", s)], []))::ll -> ()
  | _ -> raise (Error_in_config_file 
                  ("Unexpected content inside extensiontemplate config"))

let _ = parse_global_config (Extensions.get_config ())



(*****************************************************************************)
(** Extensions may define new tags for configuring each site.
    These tags are inside <site ...>...</site> in the config file.
        
   For example:
   <site dir="">
     <extensiontemplate module=".../mymodule.cmo" />
   </extension>

   Each extension will set its own configuration options, for example:
   <site dir="">
     <extensiontemplate module=".../mymodule.cmo" />
     <eliom module=".../myeliommodule.cmo" />
     <static dir="/var/www/plop" />
   </extension>

 *)

let parse_config path = function
  | Element ("extensiontemplate", atts, []) ->  ()
  | Element (t, _, _) -> raise (Bad_config_tag_for_extension t)
  | _ -> 
      raise (Error_in_config_file "Unexpected data in config file")




(*****************************************************************************)
(** Function to be called at the beginning of the initialisation phase 
    of the server (actually each time the config file is reloaded) *)
let start_init () =
  ()

(** Function to be called at the end of the initialisation phase *)
let end_init () =
  ()



(*****************************************************************************)
(** A function that will create an error message from the exceptions
    that may be raised during the initialisation phase, and raise again 
    all other exceptions. That function has type exn -> string. Use the 
   raise function if you don't need any. *)
let exn_handler = raise



(*****************************************************************************)
(** The function that will generate the pages from the request. 
   It has type 
   string option -> Extensions.request_info -> Extensions.answer Lwt.t, 
   where the string option is the encoding for characters possibly specified 
   in the configuration file,
   request_info is the request
   answer

   You must define a sender for the type you want to send (Here text).
   Some are predefined in module Predefined_senders (for text, xhtml typed
   with XHTML.M, files, empty content).
   To define a new sender, see for example eliomduce.ml in 
   Ocsigen's source code.
 *)
let gen charset ri =
  let content = "Extensiontemplate page" in
  return
    (Ext_found
       {res_cookies= [];
        res_send_page= 
        Predefined_senders.send_text_page 
          ~contenttype:"text/plain" ~content:content;
        res_headers= Predefined_senders.dyn_headers;
        res_code= None; (* 200 by default *)
        res_lastmodified= None;
        res_etag= None;
        res_charset= None;
        res_filter=None})



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
let _ = R.register_extension (* takes a quadruple *)
    (virtual_host_creator,
     start_init,
     end_init,
     exn_handler)

