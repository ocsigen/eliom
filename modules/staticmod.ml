(* Ocsigen
 * http://www.ocsigen.org
 * Module staticmod.ml
 * Copyright (C) 2005 Vincent Balat
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
(* Ocsigen module to load static pages                                       *)
(*****************************************************************************)
(*****************************************************************************)


open Lwt
open Ocsimisc
open Extensions


(*****************************************************************************)
(** table of page trees *)

let page_tree_table = ref []

let find k = Module_static_cgi.find k !page_tree_table

let add k a = Module_static_cgi.add k a page_tree_table


let find_static_page staticdirref path =
  let find_file = function
    | None -> raise Ocsigen_404
    | Some filename ->
        (* See also module Files in eliom.ml *)
        Messages.debug ("--Staticmod: Testing \""^filename^"\".");
        let stat= Unix.LargeFile.stat filename in
        let (filename, stat) = 
          if (stat.Unix.LargeFile.st_kind = Unix.S_DIR)
          then 
            (if (filename.[(String.length filename) - 1]) = '/'
            then
              let fn2 = filename^"index.html" in
              Messages.debug ("--Staticmod: Testing \""^fn2^"\".");
              (fn2,(Unix.LargeFile.stat fn2))
            else
              (if (path= []) || (path = [""])
              then 
              let fn2 = filename^"/index.html" in
              Messages.debug ("--Staticmod: Testing \""^fn2^"\".");
              (fn2,(Unix.LargeFile.stat fn2))
              else (Messages.debug ("--Staticmod: "^filename^" is a directory");
                    raise Ocsigen_Is_a_directory)))
          else (filename, stat)
        in
        Messages.debug ("--Staticmod: Looking for \""^filename^"\".");

        if (stat.Unix.LargeFile.st_kind 
              = Unix.S_REG)
        then begin
          Unix.access filename [Unix.R_OK];
          (filename, stat)
        end
        else raise Ocsigen_404 (* ??? *)
  in
  find_file (Module_static_cgi.find_page None !staticdirref path)

let gen pages_tree charset ri = 
  catch
    (* Is it a static page? *)
    (fun () ->
      if ri.ri_get_params_string = None
          (* static pages do not have parameters *)
      then begin
        Messages.debug ("--Staticmod: Is it a static file?");
        let (filename, stat) =
          find_static_page pages_tree ri.ri_path
        in
        return
          (Ext_found
             {res_cookies=[];
              res_send_page=Predefined_senders.send_file ~content:filename;
              res_headers=[];
              res_code=None;
              res_lastmodified=Some stat.Unix.LargeFile.st_mtime;
              res_etag=
              Some (Predefined_senders.File_content.get_etag filename);
              res_charset=charset})
      end
      else return Ext_not_found)
    (function
        Unix.Unix_error (Unix.EACCES,_,_)
      | Ocsigen_Is_a_directory
      | Ocsigen_malformed_url as e -> fail e
(*      | Ocsigen_404 -> return Ext_not_found *)
      | e -> return Ext_not_found)
          

(*****************************************************************************)
(** Parsing of config file *)
open Simplexmlparser

let parse_config page_tree path = function
  | Element ("static", atts, []) -> 
        let dir = match atts with
        | [] -> 
            raise (Error_in_config_file
                     "dir attribute expected for <staticdir>")
        | [("dir", s)] -> Module_static_cgi.Dir s
        | [("regexp", s);("dest",t)] -> 
	    Module_static_cgi.Regexp ((Netstring_pcre.regexp s), t)
        | _ -> raise (Error_in_config_file "Wrong attribute for <staticdir>")
        in
        Module_static_cgi.set_dir page_tree dir path
  | Element (t, _, _) -> 
      raise (Bad_config_tag_for_extension t)
  | _ -> raise (Error_in_config_file "(staticmod extension) Bad data")


(*****************************************************************************)
(** Function to be called at the beginning of the initialisation phase *)
let start_init () =
  ()

(** Function to be called at the end of the initialisation phase *)
let end_init () =
  match Ocsiconfig.get_default_static_dir () with
  | None -> ()
  | Some path -> 
      let page_tree = Module_static_cgi.new_pages_tree () in
      Module_static_cgi.set_dir page_tree (Module_static_cgi.Dir path) [];
      add_virthost ([([Wildcard], None)], 
                    fun ri -> 
                      gen page_tree (Ocsiconfig.get_default_charset ()) ri >>=
                      (fun r -> return (r,[])))
  (* for default static dir *)




(*****************************************************************************)
(** extension registration *)
let _ = R.register_extension
    ((fun hostpattern -> 
      let page_tree = 
        try 
          find hostpattern
        with Not_found -> 
          let n = Module_static_cgi.new_pages_tree () in
          add hostpattern n;
          n
      in
      (gen page_tree, 
       parse_config page_tree)),
     start_init,
     end_init,
     raise)

