(* Ocsigen
 * http://www.ocsigen.org
 * Module deflatemod.ml
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
(* This module allows to compress output sent by the server                  *)
(*****************************************************************************)
(*****************************************************************************)

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
(*  | (Element ("myoption", [("myattr", s)], []))::ll -> () *)
  | _ -> raise (Error_in_config_file 
                  ("Unexpected content inside deflatemod config"))

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
(*  | Element ("deflate", atts, []) -> () 
   Ici il faut créer un arbre de répertoires en se souvenant les options
   de compression de chaque répertoire.
   cf staticmod par exemple pour page_tree
 *)
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
(** The filter function (here deflate) *)
(* We implement Content-Encoding, not Transfer-Encoding *)

let stream_filter contenttype (len, etag, stream, finalize) = 
  (* Ici remplacer les deux lignes par le truc de compression *)
  (* We generate a new etag because ... (ask Juliusz) *)
  Ocsistream.consume stream >>= fun () ->
  return (Some Int64.zero, "", (Ocsistream.empty_stream None), finalize)

let filter ri res =
  (* Ici il faut regarder dans l'arbre de configuration
     s'il faut compresser ou pas *)
  return
    {res with
   (* res_headers = ("Content-Encoding",...)::res.res_headers; *)
     res_filter= Some stream_filter
   }


(*****************************************************************************)
(** A function that will be called for each virtual host,
   generating two functions: 
    - one that will be called to filter the output
    - one to parse the configuration file. *)
let virtual_host_creator hostpattern = (filter, parse_config)
   (* hostpattern has type Extensions.virtual_hosts
      and represents the name of the virtual host *)
   

(*****************************************************************************)
(** Registration of the extension *)
let _ = R.register_output_filter (* takes a quadruple *)
    (virtual_host_creator,
     start_init,
     end_init,
     exn_handler)

