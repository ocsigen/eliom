(* Ocsigen
 * http://www.ocsigen.org
 * Common functions for some extensions (staticmod, cgimod)
 * Copyright (C) 2005 Vincent Balat, Denis Berthod, Nataliya Guts
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

open Lwt
open Ocsimisc
open Extensions


(*****************************************************************************)
(* The table of cgi pages for each virtual server                            *)
type assockind = 
  | Dir of string
  | Regexp of Netstring_pcre.regexp * string

(* cgi or static pages *)
type page_dir = 
    Page_dir of string option *
        (Netstring_pcre.regexp * string) list * (string * page_dir) list

(* cgi or static pages *)
type pages_tree = 
    page_dir ref

let new_pages_tree () =
  (ref (Page_dir (None, [], [])))


(*****************************************************************************)

let find k table = List.assoc k table

let add k a table = table := (k,a)::!table



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
        | Dir s -> Page_dir (Some s, [], [])
        | Regexp (r, s) -> Page_dir (None, [(r, s)], []))
    | a::l -> Page_dir (None, [], [(a, add_path l)])
  in
  let rec aux (Page_dir (s1, rl, l1)) = function
    | [] ->
        (match assoc with
        | Dir s -> Page_dir (Some s, rl, l1)
        | Regexp (r, s) -> Page_dir (s1, rl@[(r, s)], l1)) (* at the end! *)
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
  | (regexp, dest)::l -> 
      (match Netstring_pcre.string_match regexp stringpath 0 with
      | None -> replace_first_match stringpath l
      | Some _ -> Some (Netstring_pcre.global_replace regexp dest stringpath)
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
		| None -> find_page None e l
		| Some dir -> find_page (Some (dir^"/"^a)) e l
            with 
		Not_found -> 
                  (match dir, dir_option with
                     | None, None -> None
                     | (Some d), None -> 
			 Some (d^"/"^(Ocsimisc.string_of_url_path (a::l)))
                     | _, Some s -> 
			 Some (s^"/"^(Ocsimisc.string_of_url_path (a::l))))


