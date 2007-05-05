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
type assockind = 
  | Dir of string
  | Regexp of Netstring_pcre.regexp * string

(* static pages *)
type static_dir = 
    Static_dir of string option *
        (Netstring_pcre.regexp * string) list * (string * static_dir) list

type pages_tree = 
    static_dir ref (* static pages *)

let new_pages_tree () =
  (ref (Static_dir (None, [], [])))


(*****************************************************************************)
(* static pages *)
let set_static_dir staticdirref assoc path =
  let rec assoc_and_remove a = function
    | [] -> raise Not_found
    | (b,v)::l when a = b -> (v,l)
    | e::l -> let v,ll = assoc_and_remove a l
          in v,(e::ll)
  in
  let rec add_path = function
    | [] -> 
        (match assoc with
        | Dir s -> Static_dir (Some s, [], [])
        | Regexp (r, s) -> Static_dir (None, [(r, s)], []))
    | a::l -> Static_dir (None, [], [(a, add_path l)])
  in
  let rec aux (Static_dir (s1, rl, l1)) = function
    | [] ->
        (match assoc with
        | Dir s -> Static_dir (Some s, rl, l1)
        | Regexp (r, s) -> Static_dir (s1, rl@[(r, s)], l1)) (* at the end! *)
    | a::l -> 
        try
          let sd1,l2 = assoc_and_remove a l1 in
          let sd = aux sd1 l in
          Static_dir (s1, rl, (a, sd)::l2)
        with Not_found -> Static_dir (s1, rl, (a,(add_path l))::l1)
  in
  staticdirref := aux !staticdirref path


let rec replace_first_match stringpath = function
  | [] -> None
  | (regexp, dest)::l -> 
      (match Netstring_pcre.string_match regexp stringpath 0 with
      | None -> replace_first_match stringpath l
      | Some _ -> Some (Netstring_pcre.global_replace regexp dest stringpath)
      )

let user_dir_regexp = Netstring_pcre.regexp "(.*)\\$u\\(([^\\)]*)\\)(.*)"

let find_static_page staticdirref path =
  let rec aux dir (Static_dir (dir_option, regexps, subdir_list)) path = 
    (* First we try the regexps *)
    match 
      (match regexps with
      | [] -> None
      | _ -> 
          let stringpath = Ocsimisc.string_of_url_path path in
          replace_first_match stringpath regexps)
    with
    | Some s -> (* Matching regexp found! *)
        Some 
          ((* hack to get user dirs *)
           match Netstring_pcre.string_match user_dir_regexp s 0 with
          | None -> s
          | Some result -> 
              let user = Netstring_pcre.matched_group result 2 s in
              try
                let userdir = (Unix.getpwnam user).Unix.pw_dir in
                (Netstring_pcre.matched_group result 1 s)^
                userdir^
                (Netstring_pcre.matched_group result 3 s)
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
          | Some dir, None -> Some (dir^"/")
          | _, Some s -> Some (s^"/"))
        | ""::l
        | ".."::l -> raise Ocsigen_malformed_url
              (* For security reasons, .. is not allowed in paths *)
              (* Actually it has already been removed by server.ml *)
        | a::l -> 
            try 
              let e = List.assoc a subdir_list in
              match dir with
              | None -> aux None e l
              | Some dir -> aux (Some (dir^"/"^a)) e l
            with 
              Not_found -> 
                (match dir, dir_option with
                | None, None -> None
                | (Some d), None -> 
                    Some (d^"/"^(Ocsimisc.string_of_url_path (a::l)))
                | _, Some s -> 
                    Some (s^"/"^(Ocsimisc.string_of_url_path (a::l))))
  in 
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
  find_file (aux None !staticdirref path)

let gen pages_tree charset ri = 
  catch
    (* Is it a static page? *)
    (fun () ->
      if ri.ri_params = "" (* static pages do not have parameters *)
      then begin
        Messages.debug ("--Staticmod: Is it a static file?");
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
open Simplexmlparser

let parse_config page_tree path = function
  | Element ("static", atts, []) -> 
        let dir = match atts with
        | [] -> 
            raise (Error_in_config_file
                     "dir attribute expected for <staticdir>")
        | [("dir", s)] -> Dir s
        | [("regexp", s);("dest",t)] -> Regexp ((Netstring_pcre.regexp s), t)
        | _ -> raise (Error_in_config_file "Wrong attribute for <staticdir>")
        in
        set_static_dir page_tree dir path
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
      let page_tree = new_pages_tree () in
      set_static_dir page_tree (Dir path) [];
      add_virthost ([([Ocsimisc.Wildcard], None)], 
                    fun ri -> 
                      gen page_tree (Ocsiconfig.get_default_charset ()) ri >>=
                      (fun r -> return (r,[])))
  (* for default static dir *)


(*****************************************************************************)
(** table of page trees *)

let page_tree_table = ref []

let find k = List.assoc k !page_tree_table

let add k a = page_tree_table := (k,a)::!page_tree_table

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

