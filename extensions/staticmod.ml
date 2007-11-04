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
(* directory listing - by Gabriel Kerneis *)

let rec space=function
  | 0 -> ""
  | i -> " "^space (i-1)

let date fl = 
  let t = Unix.gmtime fl in
  Printf.sprintf 
    "%02d-%02d-%04d %02d:%02d:%02d" 
    t.Unix.tm_mday 
    (t.Unix.tm_mon + 1)
    (1900 + t.Unix.tm_year)
    t.Unix.tm_hour
    t.Unix.tm_min
    t.Unix.tm_sec 


let image_found fich =
  if fich="README" || fich="README.Debian"
  then "/ocsigenstuff/readme.png"
  else
    let reg=Netstring_pcre.regexp "([^//.]*)(.*)"
    in match Netstring_pcre.global_replace reg "$2" fich with
      | ".jpeg" | ".jpg" | ".gif" | ".tif"
      | ".png" -> "/ocsigenstuff/image.png"
      | ".ps" -> "/ocsigenstuff/postscript.png"
      | ".pdf" -> "/ocsigenstuff/pdf.png"
      | ".html" | ".htm"
      | ".php" -> "/ocsigenstuff/html.png"
      | ".mp3"
      | ".wma" -> "/ocsigenstuff/sound.png"
      | ".c" -> "/ocsigenstuff/source_c.png"
      | ".java" -> "/ocsigenstuff/source_java.png"
      | ".pl" -> "/ocsigenstuff/source_pl.png"
      | ".py" -> "/ocsigenstuff/source_py.png"
      | ".iso" | ".mds" | ".mdf" | ".cue" | ".nrg"
      | ".cdd" -> "/ocsigenstuff/cdimage.png"
      | ".deb" -> "/ocsigenstuff/deb.png"
      | ".dvi" -> "/ocsigenstuff/dvi.png"
      | ".rpm" -> "/ocsigenstuff/rpm.png"
      | ".tar" | ".rar" -> "/ocsigenstuff/tar.png"
      | ".gz" | ".tar.gz" | ".tgz" | ".zip"
      | ".jar"  -> "/ocsigenstuff/tgz.png"
      | ".tex" -> "/ocsigenstuff/tex.png"
      | ".avi" | ".mov" -> "/ocsigenstuff/video.png"
      | ".txt" -> "/ocsigenstuff/txt.png"
      | _ -> "/ocsigenstuff/unknown.png"



let directory filename =
  let dir = Unix.opendir filename in
  let rec aux d =
    try
      let f = Unix.readdir dir in
      let stat = Unix.LargeFile.stat (filename^f) in
      if (stat.Unix.LargeFile.st_kind = Unix.S_DIR && f <> "." && f <> "..")
      then 
	(
	  `Dir, f, (
	  "<tr>\n"^
	  "<td class=\"img\"><img src=\"/ocsigenstuff/folder_open.png\" alt=\"\" /></td>\n"^
	  "<td><a href=\""^f^"\">"^f^"</a></td>\n"^
	  "<td>"^(Int64.to_string stat.Unix.LargeFile.st_size)^"</td>\n"^
	  "<td>"^(date stat.Unix.LargeFile.st_mtime)^"</td>\n"^
	  "</tr>\n")
      )::aux d
      else
	if (stat.Unix.LargeFile.st_kind 
              = Unix.S_REG)
	then
	  (
	    if f.[(String.length f) - 1] = '~'
	    then aux d
	    else 
	  (
	    `Reg, f,
	    "<tr>\n"^
	    "<td class=\"img\"><img src=\""^image_found f^"\" alt=\"\" /></td>\n"^
	    "<td><a href=\""^f^"\">"^f^"</a></td>\n"^
	    "<td>"^(Int64.to_string stat.Unix.LargeFile.st_size)^"</td>\n"^
	    "<td>"^(date stat.Unix.LargeFile.st_mtime)^"</td>\n"^
	    "</tr>\n"
	  )::aux d
	  )
	else aux d
    with
	End_of_file -> Unix.closedir d;[]

  in 
  let trie li =
    List.sort (fun (a1, b1, _) (a2, b2, _) -> match a1, a2 with
		 | `Dir, `Dir -> 
		     if b1<b2
		     then 0
		     else 1
		 | `Dir, _ -> 0
		 | _, `Dir -> 1
		 | _, _->
		     if b1<b2
		     then 0
		     else 1) li

  in let rec aux2 = function 
    | [] -> ""
    | (_, _, i)::l -> i^(aux2 l)
  in aux2 (trie (aux dir))

let index_of filename stat path=
  let rec back=function
    | [] -> assert false
    | [a] -> "/"
    | [a;""] -> "/"
    | i::j -> "/"^i^(back j)
  in let parent=
    if (path= []) || (path = [""])
    then "/"
    else back path
  in let before =
    let st = (Ocsimisc.string_of_url_path path) in
    "<html>\n"^
    "<head><meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" />"^
    "<link rel=\"stylesheet\" type=\"text/css\" href=\"/ocsigenstuff/style.css\" media=\"screen\" />"^
    "<title>Listing Directory: "^st^"</title>\n</head>\n"^
    "<body><h1>"^st^"</h1>\n"^
    "<table summary=\"Contenu du dossier "^st^"\">\n"^
    "<tr id=\"headers\"><th></th><th>Name</th><th>Size</th>"^
    "<th>Last modified</th></tr>"^
    "<tr>\n"^
    "<td class=\"img\"><img src=\"/ocsigenstuff/back.png\" alt=\"\" /></td>\n"^
    "<td><a href=\""^parent^"\">Parent Directory</a></td>\n"^
    "<td>"^(Int64.to_string stat.Unix.LargeFile.st_size)^"</td>\n"^
    "<td>"^(date stat.Unix.LargeFile.st_mtime)^"</td>\n"^
    "</tr>\n"

  and after=
      "</table>"^
      "<p id=\"footer\">Ocsigen Webserver</p>"^
      "</body></html>"
  in before^(directory filename)^after




(*****************************************************************************)
(* Finding files *)

let user_dir_regexp = Netstring_pcre.regexp "(.*)\\$u\\(([^\\)]*)\\)(.*)"

let find_static_page dir path =
  let find_file (filename, readable) =
    (* See also module Files in eliom.ml *)
    try
      Messages.debug ("--Staticmod: Testing \""^filename^"\".");
      let stat = Unix.LargeFile.stat filename in
      let (filename, stat) = 
        if (stat.Unix.LargeFile.st_kind = Unix.S_DIR)
        then 
          (if (filename.[(String.length filename) - 1]) = '/'
          then
            let fn2 = filename^"index.html" in
            Messages.debug ("--Staticmod: Testing \""^fn2^"\".");
	    try
	      (fn2, (Unix.LargeFile.stat fn2))
	    with
	    | Unix.Unix_error (Unix.ENOENT,_,_) -> 
	        if readable=true
	        then (filename, stat)
	        else raise Ocsigen_404
          else
            (if (path= []) || (path = [""])
            then 
              let fn2 = filename^"/index.html" in
              Messages.debug ("--Staticmod: Testing \""^fn2^"\".");
              try
	        (fn2,(Unix.LargeFile.stat fn2))
	      with
	      | Unix.Unix_error (Unix.ENOENT,_,_) -> 
		  if readable=true
		  then (filename^"/", stat)
		  else raise Ocsigen_404
            else (Messages.debug ("--Staticmod: "^filename^" is a directory");
                  raise Ocsigen_Is_a_directory)))
        else (filename, stat)
      in
      Messages.debug ("--Staticmod: Looking for \""^filename^"\".");
      if (stat.Unix.LargeFile.st_kind 
            = Unix.S_REG)
      then begin 
        Unix.access filename [Unix.R_OK];
        (filename, stat, false)
      end
      else (
        if (stat.Unix.LargeFile.st_kind = Unix.S_DIR)
        then 
	  ((index_of filename stat path), stat, true)
        else raise Ocsigen_404)
    with Unix.Unix_error (Unix.ENOENT,_,_) -> raise Ocsigen_404
  in

  let path = Ocsimisc.string_of_url_path path in

  match dir with
  | Dir (d, readable) -> find_file ((d^path), 
                                    readable)
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





let stream_of_string st =
  Ocsistream.make
    (fun () -> Ocsistream.cont st (fun () -> Ocsistream.empty None))

let gen dir charset ri = 
  catch
    (* Is it a static page? *)
    (fun () ->
      if ri.ri_get_params_string = None
          (* static pages do not have parameters *)
      then begin
        Messages.debug ("--Staticmod: Is it a static file?");
        let (filename, stat, index) = find_static_page dir ri.ri_sub_path in
	let content = stream_of_string filename in
	if index
	then(
	  return
	    (Ext_found
               {res_cookies= [];
		res_send_page= 
		   Predefined_senders.send_stream 
		     ~contenttype:"text/html"
                     ~content;
		res_headers=Http_headers.empty;
		res_code= None; (* 200 by default *)
		res_lastmodified= None;
		res_etag= None;
		res_charset= None;
                res_filter=None}))      
	else 
	  return
            (Ext_found
               {res_cookies=[];
		res_send_page=Predefined_senders.send_file ~content:filename;
		res_headers=Http_headers.empty;
		res_code=None;
		res_lastmodified=Some stat.Unix.LargeFile.st_mtime;
		res_etag=
		Some (Predefined_senders.File_content.get_etag filename);
		res_charset=Some charset;
                res_filter=None})
            
      end
      else return (Ext_not_found Ocsigen_404))

    (function
      | Unix.Unix_error (Unix.EACCES,_,_)
      | Ocsigen_Is_a_directory
      | Ocsigen_malformed_url  
      | Ocsigen_403 as e->  fail e
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



let parse_config path = function
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
    ((fun hostpattern path charset -> parse_config path),
     start_init,
     end_init,
     raise)

