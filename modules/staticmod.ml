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
(* The table of static pages for each virtual server                         *)

type pages_tree = 
    static_dir ref (* static pages *)

let new_pages_tree () =
  (ref (Static_dir (None, [])))


(*****************************************************************************)
(* static pages *)
let set_static_dir staticdirref s path =
  let rec assoc_and_remove a = function
      [] -> raise Not_found
    | (b,v)::l when a = b -> (v,l)
    | e::l -> let v,ll = assoc_and_remove a l
          in v,(e::ll)
  in
  let rec add_path = function
      [] -> Static_dir (Some s,[])
    | a::l -> Static_dir (None, [(a,add_path l)])
  in
  let rec aux (Static_dir (s1,l1)) = function
      [] -> Static_dir (Some s,l1)
    | a::l -> 
        try
          let sd1,l2 = assoc_and_remove a l1 in
          let sd = aux sd1 l in
          Static_dir (s1,(a,sd)::l2)
        with Not_found -> Static_dir (s1,(a,(add_path l))::l1)
  in
  staticdirref := aux !staticdirref path


let find_static_page staticdirref path =
  let rec aux dir (Static_dir (dir_option, subdir_list)) = function
      [] -> (match dir_option with
        None -> dir
      | s -> s)
    | [""] -> (match dir, dir_option with
        None, None -> None
      | Some dir, None -> Some (dir^"/")
      | _, Some s -> Some (s^"/"))
    | ""::l
    | ".."::l -> raise Ocsigen_malformed_url
          (* For security reasons, .. is not allowed in paths *)
    | a::l -> 
        try 
          let e = (List.assoc a subdir_list) in
          match dir with
            None -> aux None e l
          | Some dir -> aux (Some (dir^"/"^a)) e l
        with 
          Not_found -> 
            (match dir, dir_option with
              None, None -> None
            | (Some d), None -> 
                Some (d^"/"^(Ocsimisc.string_of_url_path (a::l)))
            | _, Some s -> 
                Some (s^"/"^(Ocsimisc.string_of_url_path (a::l))))
  in 
  let find_file = function
      None -> raise Ocsigen_404
    | Some filename ->
        (* See also module Files in eliom.ml *)
        Messages.debug ("- Testing \""^filename^"\".");
        let stat= Unix.LargeFile.stat filename in
        let (filename, stat) = 
          if (stat.Unix.LargeFile.st_kind = Unix.S_DIR)
          then 
            (if (filename.[(String.length filename) - 1]) = '/'
            then
              let fn2 = filename^"index.html" in
              Messages.debug ("- Testing \""^fn2^"\".");
              (fn2,(Unix.LargeFile.stat fn2))
            else
              (if (path= []) || (path = [""])
              then 
              let fn2 = filename^"/index.html" in
              Messages.debug ("- Testing \""^fn2^"\".");
              (fn2,(Unix.LargeFile.stat fn2))
              else (Messages.debug ("- "^filename^" is a directory");
                    raise Ocsigen_Is_a_directory)))
          else (filename, stat)
        in
        Messages.debug ("- Looking for \""^filename^"\".");

        if (stat.Unix.LargeFile.st_kind 
              = Unix.S_REG)
        then begin
          Unix.access filename [Unix.R_OK];
          (filename, stat)
        end
        else raise Ocsigen_404 (* ??? *)
  in
  find_file (aux None !staticdirref path)

let gen pages_tree charset ri = 
  catch
    (* Is it a static page? *)
    (fun () ->
      if ri.ri_params = "" (* static pages do not have parameters *)
      then begin
        Messages.debug ("--- Is it a static file?");
        let (filename, stat) =
          find_static_page pages_tree ri.ri_path
        in
        return
          (Ext_found
             {res_cookies=[];
              res_send_page=Predefined_senders.send_file ~content:filename;
              res_create_sender=Predefined_senders.create_file_sender;
              res_code=None;
              res_lastmodified=Some stat.Unix.LargeFile.st_mtime;
              res_etag=
              Some (Predefined_senders.File_content.get_etag filename);
              res_charset=charset})
      end
      else return Ext_not_found)
    (function
        (Unix.Unix_error (Unix.EACCES,_,_))
      | Ocsigen_Is_a_directory
      | Ocsigen_malformed_url as e -> fail e
(*      | Ocsigen_404 -> return Ext_not_found *)
      | e -> return Ext_not_found)
          

(*****************************************************************************)
(** Parsing of config file *)
open Simplexmlparser.ExprOrPatt
let parse_config page_tree path = function
    EPanytag
      ("static", atts, PLEmpty) -> 
        let dir = match atts with
        | PLEmpty -> 
            raise (Error_in_config_file
                     "dir attribute expected for <staticdir>")
        | PLCons ((EPanyattr (EPVstr("dir"), EPVstr(s))), PLEmpty) -> s
        | _ -> raise (Error_in_config_file "Wrong attribute for <staticdir>")
        in
        set_static_dir page_tree dir path
  | EPanytag (t, _, _) -> 
      raise (Bad_config_tag_for_extension t)
  | _ -> raise (Error_in_config_file "(staticmod extension)")


(*****************************************************************************)
(** Function to be called at the beginning of the initialisation phase *)
let start_init () =
  ()

(** Function to be called at the end of the initialisation phase *)
let end_init () =
  match Ocsiconfig.get_default_static_dir () with
    None -> ()
  | Some path -> 
      let page_tree = new_pages_tree () in
      set_static_dir page_tree path [];
      add_virthost ([([Ocsimisc.Wildcard],None)], 
                    fun ri -> 
                      gen page_tree (Ocsiconfig.get_default_charset ()) ri >>=
                      (fun r -> return (r,[])))
  (* for default static dir *)


(*****************************************************************************)
(** extension registration *)
let _ = register_extension
    ((fun hostpattern -> 
      let page_tree = new_pages_tree () in
      (gen page_tree, 
       parse_config page_tree)),
     start_init,
     end_init,
     raise)

