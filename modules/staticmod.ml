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



exception Ocsigen_403

(*****************************************************************************)
(* The table of static pages for each virtual server                            *)
type assockind = 
  | Dir of string * string
  | Regexp of Netstring_pcre.regexp * string * string

(* cgi or static pages *)
type page_dir = 
    Page_dir of (string * string) option *
        (Netstring_pcre.regexp * string * string) list * (string * page_dir) list

(* cgi or static pages *)
type pages_tree = 
    page_dir ref

let new_pages_tree () =
  (ref (Page_dir (None, [], [])))


(*****************************************************************************)

(** table of page trees *)

let page_tree_table = ref []

let find k = List.assoc k !page_tree_table

let add k a = page_tree_table:= (k,a)::!page_tree_table

(*****************************************************************************)

let set_dir dirref assoc path =
  let rec assoc_and_remove a = function
    | [] ->raise Not_found
    | (b,v)::l when a = b -> (v,l)
    | e::l -> let v,ll = assoc_and_remove a l
          in v,(e::ll)
  in
  let rec add_path = function
    | [] -> 
        (match assoc with
        | Dir (s, p) -> Page_dir (Some (s, p), [], [])
        | Regexp (r, s, p) -> Page_dir (None, [(r, s, p)], []))
    | a::l -> Page_dir (None, [], [(a, add_path l)])
  in
  let rec aux (Page_dir (s1, rl, l1)) = function
    | [] ->
        (match assoc with
        | Dir (s, p) -> Page_dir (Some (s, p), rl, l1)
        | Regexp (r, s, p) -> Page_dir (s1, rl@[(r, s, p)], l1)) (* at the end! *)
    | a::l -> 
        try
          let sd1,l2 = assoc_and_remove a l1 in
          let sd = aux sd1 l in
          Page_dir (s1, rl, (a, sd)::l2)
        with Not_found -> Page_dir (s1, rl, (a,(add_path l))::l1)
  in
  dirref := aux !dirref path


let rec replace_first_match stringpath = function
  | [] -> None
  | (regexp, dest, readable)::l ->
      (match Netstring_pcre.string_match regexp stringpath 0 with
      | None -> replace_first_match stringpath l
      | Some _ -> Some (Netstring_pcre.global_replace regexp dest stringpath, readable)
      )

let user_dir_regexp = Netstring_pcre.regexp "(.*)\\$u\\(([^\\)]*)\\)(.*)"


 
let rec find_page dir (Page_dir (dir_option, regexps, subdir_list)) path = 
  (* First we try the regexps *)
  match 
    (match regexps with
       | [] -> None
       | _ -> 
           let stringpath = Ocsimisc.string_of_url_path path in
           replace_first_match stringpath regexps)
  with
    | Some (s, p) -> (* Matching regexp found! *)
        Some 
          ((* hack to get user dirs *)
            match Netstring_pcre.string_match user_dir_regexp s 0 with
              | None -> (s, p)
              | Some result -> 
		  let user = Netstring_pcre.matched_group result 2 s in
		  try
                    let userdir = (Unix.getpwnam user).Unix.pw_dir in
                    ((Netstring_pcre.matched_group result 1 s)^
                      userdir^
                      (Netstring_pcre.matched_group result 3 s), p)
		  with _ -> raise Not_found
          )
    | None ->
        (* Then we continue *)
        match path with
          | [] -> (match dir_option with
		     | None -> dir
		     | _ -> dir_option)
          | [""] -> (match dir, dir_option with
		       | None, None -> None
		       | Some (dir, p), None -> Some (dir^"/", p)
		       | _, Some (s, p) -> Some (s^"/", p))
        | ""::l
        | ".."::l -> raise Ocsigen_malformed_url
            (* For security reasons, .. is not allowed in paths *)
            (* Actually it has already been removed by server.ml *)
        | a::l -> 
            try 
              let e = List.assoc a subdir_list in
              match dir, dir_option with
		| None, None -> find_page None e l
		| Some (dir, p), None -> find_page (Some (dir^"/"^a, p)) e l
		| _, Some (s, p)-> find_page (Some (s^"/"^a, p)) e l
            with 
		Not_found -> 
                  (match dir, dir_option with
                     | None, None -> None
                     | Some (d, p), None -> 
			 Some (d^"/"^(Ocsimisc.string_of_url_path (a::l)), p)
                     | _, Some (s, p) -> 
			 Some (s^"/"^(Ocsimisc.string_of_url_path (a::l)), p))



(*****************************************************************************)


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


let image_found fich=
  let reg=Netstring_pcre.regexp "([^//.]*)(.*)"
  in match Netstring_pcre.global_replace reg "$2" fich with
    | ".png" -> "/icons/image.png"
    | _ -> "/icons/unknown.png"



let directory filename=
  let dir=Unix.opendir filename in
  let rec aux d=
    try
      let f=Unix.readdir dir in
      let stat= Unix.LargeFile.stat (filename^f) in
      if (stat.Unix.LargeFile.st_kind = Unix.S_DIR && f<>"." && f<>"..")
      then 
	(
	  "DIR",f,(
	  "<tr>\n"^
	  "<td class='img'><img src='/icons/folder_open.png' alt='' /></td>\n"^
	  "<td><a href='"^f^"'>"^f^"</a></td>\n"^
	  "<td>"^(Int64.to_string stat.Unix.LargeFile.st_size)^"</td>\n"^
	  "<td>"^(date stat.Unix.LargeFile.st_mtime)^"</td>\n"^
	  "</tr>\n")
      )::aux d
      else
	if (stat.Unix.LargeFile.st_kind 
              = Unix.S_REG)
	then
	  (
	    "",f,
	    "<tr>\n"^
	    "<td class='img'><img src='"^image_found f^"' alt='' /></td>\n"^
	    "<td><a href='"^f^"'>"^f^"</a></td>\n"^
	    "<td>"^(Int64.to_string stat.Unix.LargeFile.st_size)^"</td>\n"^
	    "<td>"^(date stat.Unix.LargeFile.st_mtime)^"</td>\n"^
	    "</tr>\n"
	  )::aux d
	else aux d
    with
	End_of_file -> Unix.closedir d;[]

  in let trie li=
    List.sort (fun (a1, b1, _) (a2, b2, _) -> match a1,a2 with
		 | "DIR", "DIR" -> 
		     if b1<b2
		     then 0
		     else 1
		 | "DIR", _ -> 0
		 | _, "DIR" -> 1
		 | _, _->
		     if b1<b2
		     then 0
		     else 1) li

  in let rec aux2=function 
    |[]->""
    |(_, _, i)::l->i^(aux2 l)
  in aux2 (trie (aux dir))

let index_of filename stat path=
  let rec back=function
    | [] -> failwith "CAS IMPOSSIBLE"
    | [a] -> "/"
    | [a;""] -> "/"
    | i::j -> "/"^i^(back j)
  in let parent=
    if (path= []) || (path = [""])
    then "/"
    else back path
  in let before=
    let st=(Ocsimisc.string_of_url_path path) in
    "<HTML>\n"^
    "<HEAD><meta http-equiv='Content-Type' content='text/html; charset=utf-8' />"^
    "<link rel='stylesheet' type='text/css' href='/icons/style.css' media='screen' />"^
    "<TITLE>Listing Directory: "^st^"</TITLE>\n"^
    "<body><h1>"^st^"</h1>\n"^
    "<table summary='Contenu du dossier "^st^"'>\n"^
    "<tr id='headers'><th></th><th>Name</th><th>Size</th>"^
    "<th>Last modified</th></tr>"^
    "<tr>\n"^
    "<td class='img'><img src='/icons/back.png' alt='' /></td>\n"^
    "<td><a href='"^parent^"'>Parent Directory</a></td>\n"^
    "<td>"^(Int64.to_string stat.Unix.LargeFile.st_size)^"</td>\n"^
    "<td>"^(date stat.Unix.LargeFile.st_mtime)^"</td>\n"^
    "</tr>\n"

  and after=
      "</table>"^
      "<p id='footer'>Webserver powered Ocsigen</p>"^
      "</body></html>"
  in before^(directory filename)^after




let find_static_page staticdirref path =
  let find_file = function
    | None -> raise Ocsigen_404
    | Some (filename, readable) ->
        (* See also module Files in eliom.ml *)
        Messages.debug ("--Staticmod: Testing \""^filename^"\".");
        let stat= Unix.LargeFile.stat filename in
        let (filename, stat) = 
          if (stat.Unix.LargeFile.st_kind = Unix.S_DIR)
          then 
            (if (filename.[(String.length filename) - 1]) = '/'
            then
              let fn2 = filename^"index.html" in
              Messages.debug ("--Staticmod2: Testing \""^fn2^"\".");
	      try
		(fn2,(Unix.LargeFile.stat fn2))
	      with
		  _ -> 
		    if readable="readable"
		    then (filename, stat)
		    else raise Ocsigen_403
            else
              (if (path= []) || (path = [""])
              then 
              let fn2 = filename^"/index.html" in
              Messages.debug ("--Staticmod3: Testing \""^fn2^"\".");
              try
		(fn2,(Unix.LargeFile.stat fn2))
	      with
		  _ -> 
		    if readable="readable"
		    then (filename^"/", stat)
		    else raise Ocsigen_403
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
	  if (stat.Unix.LargeFile.st_kind 
              = Unix.S_DIR)
	  then 
	    ((index_of filename stat path), stat, true)
	  else raise Ocsigen_404 (* ??? *))
  in
  find_file (find_page None !staticdirref path)



let stream_of_string st=
  let rec aux bo=
    if bo
    then return (Ocsistream.new_stream st (fun () -> aux false))
    else return (Ocsistream.empty_stream None)
  in aux true

let gen pages_tree charset ri = 
  catch
    (* Is it a static page? *)
    (fun () ->
      if ri.ri_get_params_string = None
          (* static pages do not have parameters *)
      then begin
        Messages.debug ("--Staticmod: Is it a static file?");
        let (filename, stat, index) =
          find_static_page pages_tree ri.ri_path
        in
	stream_of_string filename >>= fun content ->
	if index
	then(
	  return
	    (Ext_found
               {res_cookies= [];
		res_send_page= 
		   Predefined_senders.send_stream_page 
		     ~contenttype:"text/html" ~content:(fun () -> content);
		res_headers=[];
		res_code= None; (* 200 by default *)
		res_lastmodified= None;
		res_etag= None;
		res_charset= None}))      
	else 
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
(*      | Ocsigen_403
        | Ocsigen_404 -> return Ext_not_found *)
      | e -> return Ext_not_found)
          

(*****************************************************************************)
(** Parsing of config file *)
open Simplexmlparser


let remove_end_slash s=
  try
    if s.[(String.length s) - 1]='/'
    then String.sub s 0 ((String.length s) - 1)
    else s
  with _ -> s

let rec parse_global_config = function
  | [] -> ()
  | (Element ("static", [("dir", di);("readable",p)], []))::ll -> 
      Ocsiconfig.set_default_static_dir (remove_end_slash di) p
  | _ -> raise (Error_in_config_file 
                  ("Unexpected content inside static config"))

let _ = parse_global_config (Extensions.get_config ())



let parse_config page_tree path = function
  | Element ("static", atts, []) -> 
        let dir = match atts with
        | [] -> 
            raise (Error_in_config_file
                     "dir attribute expected for <staticdir>")
        | [("dir", s);("readable",p)] -> Dir (remove_end_slash s, p)
        | [("regexp", s);("dest",t);("readable",p)] -> 
	    Regexp ((Netstring_pcre.regexp s), t, p)
        | _ -> raise (Error_in_config_file "Wrong attribute for <staticdir>")
        in
        set_dir page_tree dir path
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
  | Some (path, p) -> 
      let page_tree = new_pages_tree () in
      set_dir page_tree (Dir (path, p)) [];
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
          let n = new_pages_tree () in
          add hostpattern n;
          n
      in
      (gen page_tree, 
       parse_config page_tree)),
     start_init,
     end_init,
     raise)

