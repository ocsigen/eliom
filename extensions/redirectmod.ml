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

type page_dir = 
    Page_dir of assockind list * (string * page_dir) list

type pages_tree = 
    page_dir ref

let new_pages_tree () =
  (ref (Page_dir ([], [])))


(*****************************************************************************)
(** table of page trees *)

let page_tree_table = ref []

let find k = List.assoc k !page_tree_table

let add k a = page_tree_table:= (k,a)::!page_tree_table


(*****************************************************************************)
let set_dir dirref assoc path =
  let rec assoc_and_remove a = function
    | [] -> raise Not_found
    | (b,v)::l when a = b -> (v,l)
    | e::l -> 
        let v,ll = assoc_and_remove a l in
        v,(e::ll)
  in
  let rec add_path = function
    | [] -> Page_dir ([assoc], [])
    | a::l -> Page_dir ([], [(a, add_path l)])
  in
  let rec aux (Page_dir (dl, l1)) = function
    | [] -> Page_dir (dl@[assoc], l1) (* at the end! *)
    | a::l -> 
        try
          let sd1,l2 = assoc_and_remove a l1 in
          let sd = aux sd1 l in
          Page_dir (dl, (a, sd)::l2)
        with Not_found -> Page_dir (dl, (a, (add_path l))::l1)
  in
  dirref := aux !dirref path







(*****************************************************************************)
let rec parse_global_config = function
  | [] -> ()
  | _ -> raise (Error_in_config_file 
                  ("Unexpected content inside redirectmod config"))

let _ = parse_global_config (Extensions.get_config ())



(*****************************************************************************)
(** Configuration for each site.
    These tags are inside <site ...>...</site> in the config file.
        
   For example:
   <site dir="">
     <redirect regexp="" dest="" />
   </extension>

 *)

let parse_config page_tree path = function
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
        set_dir page_tree dir path
  | Element (t, _, _) -> 
      raise (Bad_config_tag_for_extension t)
  | _ -> raise (Error_in_config_file "(redirectmod extension) Bad data")



(*****************************************************************************)
(* Finding redirections *)

let find_redirection dirref path =

  let rec find_in_dir dirtotry path handler =
    match dirtotry with
    | [] -> handler ()
    | (Regexp (regexp, dest, temp))::l ->
        (match Netstring_pcre.string_match regexp path 0 with
        | None -> find_in_dir l path handler
        | Some _ -> (* Matching regexp found! *)
            (Netstring_pcre.global_replace regexp dest path, temp)
        )
  in


  let rec find_page 
      dirtotry pathtotry (Page_dir (dir_list, subdir_list)) path handler = 
    match path with
    | [] -> 
        find_in_dir dir_list ""
          (fun () -> 
            find_in_dir dirtotry 
              (Ocsimisc.string_of_url_path pathtotry) handler)
    | [""] -> 
        find_in_dir dir_list "/"
          (fun () -> 
            find_in_dir dirtotry 
              (Ocsimisc.string_of_url_path
                 (pathtotry@[""])) handler)
    | ""::l
    | ".."::l -> raise Ocsigen_malformed_url
          (* For security reasons, .. is not allowed in paths *)
          (* Actually it has already been removed by server.ml *)
    | a::l -> 
        try 
          let e = List.assoc a subdir_list in
          match dir_list with
          | [] ->
              find_page dirtotry (pathtotry@[a]) e l handler
          | _ ->
	      find_page dir_list [""; a] e l
                (fun () -> 
                  find_in_dir dirtotry 
                    (Ocsimisc.string_of_url_path (pathtotry@[a])) handler)
        with 
	| Not_found -> 
            let p2 = Ocsimisc.string_of_url_path path in
            match dir_list with
            | [] ->
                find_in_dir dirtotry 
                  (Ocsimisc.string_of_url_path (pathtotry@[p2]))
                  handler
            | _ ->
                find_in_dir dir_list ("/"^p2)
                  (fun () -> 
                    find_in_dir dirtotry 
                      (Ocsimisc.string_of_url_path (pathtotry@[p2])) handler)

  in
  find_page [] [] !dirref path (fun () -> raise Ocsigen_404)




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
let gen pages_tree charset ri =
  catch
    (* Is it a redirection? *)
    (fun () ->
      Messages.debug ("--Redirectmod: Is it a redirection?");
      let path = if ri.ri_path = [] then ""::ri.ri_path else ri.ri_path in
      let (redir, temp) = find_redirection pages_tree path in
      Messages.debug ("--Redirectmod: YES! "^
                      (if temp then "Temporary " else "Permanent ")^
                      "redirection to: "^redir);      
      return
        (Ext_found
           {res_cookies=[];
	    res_send_page=
            (fun ?filter ?cookies waiter ~clientproto ?code ?etag ~keep_alive
                ?last_modified ?location ~head ?headers ?charset s ->
                  Predefined_senders.send_empty
                    ~content:()
                    ?filter
                    ?cookies
                    waiter 
                    ~clientproto
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
    (function e -> return (Ext_not_found e))







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

