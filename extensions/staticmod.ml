(* Ocsigen
 * http://www.ocsigen.org
 * Module staticmod.ml
 * Copyright (C) 2005 Vincent Balat
 * Laboratoire PPS - CNRS Université Paris Diderot
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
type assockind = 
  | Dir of string * bool
  | Regexp of Netstring_pcre.regexp * string * bool



(*****************************************************************************)
(* Finding files *)

let user_dir_regexp = Netstring_pcre.regexp "(.*)\\$u\\(([^\\)]*)\\)(.*)"

type res = 
  | RFile of string
  | RDir of string

let find_static_page dir path =
  let find_file (filename, readable) =
    (* See also module Files in eliom.ml *)
    try
      Messages.debug (fun () -> "--Staticmod: Testing \""^filename^"\".");
      let stat = Unix.LargeFile.stat filename in
      let (filename, stat) = 
        if (stat.Unix.LargeFile.st_kind = Unix.S_DIR)
        then 
          (if (filename.[(String.length filename) - 1]) = '/'
          then
            let fn2 = filename^"index.html" in
            Messages.debug (fun () -> "--Staticmod: Testing \""^fn2^"\".");
	    try
	      (fn2, (Unix.LargeFile.stat fn2))
	    with
	    | Unix.Unix_error (Unix.ENOENT,_,_) -> 
	        if readable
	        then (filename, stat)
	        else raise Ocsigen_403
          else
            (if (path= []) || (path = [""])
            then 
              let fn2 = filename^"/index.html" in
              Messages.debug (fun () -> "--Staticmod: Testing \""^fn2^"\".");
              try
	        (fn2, (Unix.LargeFile.stat fn2))
	      with
	      | Unix.Unix_error (Unix.ENOENT, _, _) -> 
		  if readable
		  then (filename^"/", stat)
		  else raise Ocsigen_403
            else (Messages.debug
                    (fun () -> "--Staticmod: "^filename^" is a directory");
                  raise Ocsigen_Is_a_directory)))
        else (filename, stat)
      in
      Messages.debug (fun () -> "--Staticmod: Looking for \""^filename^"\".");
      if (stat.Unix.LargeFile.st_kind = Unix.S_REG)
      then begin 
        Unix.access filename [Unix.R_OK];
        RFile filename
      end
      else (
        if (stat.Unix.LargeFile.st_kind = Unix.S_DIR)
        then
	  RDir filename
        else raise Ocsigen_404)
    with Unix.Unix_error (Unix.ENOENT,_,_) -> raise Ocsigen_404
  in

  let path = Ocsimisc.string_of_url_path path in

  match dir with
  | Dir (d, readable) -> find_file ((d^path), readable)
  | Regexp (regexp, dest, readable) ->
      (match Netstring_pcre.string_match regexp path 0 with
      | None -> raise Ocsigen_404
      | Some _ -> (* Matching regexp found! *)
          let s = Netstring_pcre.global_replace regexp dest path in
          (* hack to get user dirs *)
          match Netstring_pcre.string_match user_dir_regexp s 0 with
          | None -> find_file (s, readable)
          | Some result -> 
	      let user = Netstring_pcre.matched_group result 2 s in
              let userdir = (Unix.getpwnam user).Unix.pw_dir in
              find_file
                ((Netstring_pcre.matched_group result 1 s)^
                 userdir^
                 (Netstring_pcre.matched_group result 3 s),
                 readable)
      )





let gen dir charset ri = 
  catch
    (* Is it a static page? *)
    (fun () ->
      if ri.ri_get_params_string = None
          (* static pages do not have parameters *)
      then begin
        Messages.debug2 "--Staticmod: Is it a static file?";
        match find_static_page dir ri.ri_sub_path with
        | RDir dirname ->
            Predefined_senders.Directory_content.result_of_content 
              (dirname, ri.ri_sub_path) >>= fun r ->
	    return (Ext_found r)
        | RFile filename ->
            Predefined_senders.File_content.result_of_content filename 
            >>= fun r ->	    
            return
              (Ext_found
                 {r with
		  Http_frame.res_charset= Some charset;
                })
              
      end
      else return (Ext_not_found Ocsigen_404))

    (function
      | Unix.Unix_error (Unix.EACCES,_,_)
      | Ocsigen_Is_a_directory
      | Ocsigen_malformed_url as e -> fail e
(*    | Ocsigen_403 as e ->  fail e *)
(*    | Ocsigen_404 -> return Ext_not_found *)
      | e -> return (Ext_not_found e))
          

(*****************************************************************************)
(** Parsing of config file *)
open Simplexmlparser

let (default_static_dir : (string * bool) option ref) = ref None

let set_default_static_dir s p = default_static_dir := Some (s, p)

let get_default_static_dir () = !default_static_dir

let rec parse_global_config = function
  | [] -> ()
  | (Element ("static", [("dir", di)], []))::ll -> 
      set_default_static_dir (remove_end_slash di) false
  | (Element ("static", [("dir", di);("readable","readable")], []))::ll -> 
      set_default_static_dir (remove_end_slash di) true
  | _ -> raise (Error_in_config_file 
                  ("Unexpected content inside static config"))

let _ = parse_global_config (Extensions.get_config ())



let parse_config path charset = function
  | Element ("static", atts, []) -> 
        let dir = match atts with
        | [] -> 
            raise (Error_in_config_file
                     "dir or regexp attributes expected for <static>")
        | [("dir", s)] -> Dir (remove_end_slash s, false)
        | [("dir", s);("readable","readable")] -> Dir (remove_end_slash s, true)
        | [("regexp", s);("dest",t)] -> 
	    Regexp ((Netstring_pcre.regexp ("/"^s)), t, false)
        | [("regexp", s);("dest",t);("readable","readable")] -> 
	    Regexp ((Netstring_pcre.regexp ("/"^s)), t, true)
        | _ -> raise (Error_in_config_file "Wrong attribute for <static>")
        in
        Page_gen (gen dir)
  | Element (t, _, _) -> 
      raise (Bad_config_tag_for_extension t)
  | _ -> raise (Error_in_config_file "(staticmod extension) Bad data")


(*****************************************************************************)
(** Function to be called at the beginning of the initialisation phase *)
let start_init () =
  ()

(** Function to be called at the end of the initialisation phase *)
let end_init () =
  match get_default_static_dir () with
  | None -> ()
  | Some (path, r) -> 
      add_site ([([Wildcard], None)],
                [],
                None,
                [Extensions.Page_gen
                  (fun charset ri -> 
                    gen 
                      (Dir (remove_end_slash path, r)) 
                      (match Ocsiconfig.get_default_charset () with 
                      | None -> "utf-8"
                      | Some charset -> charset)
                      ri
                  )])
  (* for default static dir *)



(*****************************************************************************)
(** extension registration *)
let _ = register_extension
    ((fun hostpattern -> parse_config),
     start_init,
     end_init,
     raise)

