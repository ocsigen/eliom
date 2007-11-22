(* Ocsigen
 * Copyright (C) 2005 Vincent Balat
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


open XHTML.M
open Eliompredefmod.Xhtml
open Eliomservices


type ('a, 'b) one_page =
    (unit, unit, 
     'a,
     [ `WithoutSuffix ],
     unit, unit, 
     'b) service

let menu_class = "eliomtools_menu"
let last_class = "eliomtools_last"
let current_class = "eliomtools_current"
let disabled_class = "eliomtools_disabled"
let first_class = "eliomtools_first"
let level_class = "eliomtools_level"

let menu ?(classe=[]) first l ~service:current ~sp =
  let rec aux = function
    | [] -> []
    | [(url, text)] -> 
        let classe = [last_class] in
        if url = (* problem with preapplied services with == *) current 
        then [li ~a:[a_class (current_class::classe)] text]
        else [li ~a:[a_class classe] [a url sp text ()]]
    | (url, text)::l -> 
        (if url = (* problem with preapplied services with == *) current 
        then  (li ~a:[a_class [current_class]] text)
        else (li [a url sp text ()]))::(aux l)
  in match first::l with
  | [] -> assert false
  | [(url, text)] ->
      ul ~a:[a_class (menu_class::classe)] 
        (let liclasse = [first_class; last_class] in
        if url = (* problem with preapplied services with == *) current 
        then (li ~a:[a_class (current_class::liclasse)] text) 
        else (li ~a:[a_class liclasse] [a url sp text ()])) []
  | (url, text)::l -> 
      ul ~a:[a_class (menu_class::classe)]
        (let liclasse = [first_class] in
        if url = (* problem with preapplied services with == *) current 
        then (li ~a:[a_class (current_class::liclasse)] text)
        else (li ~a:[a_class liclasse] [a url sp text ()])) (aux l)


type ('a, 'b, 'c) hierarchical_site_item =
  | Disabled
  | Site_tree of ('a, 'b, 'c) hierarchical_site
and ('a, 'b, 'c) main_page =
  | Main_page of ('a, 'b) one_page
  | Default_page of ('a, 'b) one_page
  | Not_clickable
and ('a, 'b, 'c) hierarchical_site =
      (('a, 'b, 'c) main_page * 
         ('c XHTML.M.elt list * ('a, 'b, 'c) hierarchical_site_item) list)

let find_in_hierarchy service (main, pages) =
  let rec aux i = function
    | [] -> raise Not_found
    | (_, Site_tree (Main_page s, hsl))::_ when s = service -> 
        (try
          i::aux 0 hsl
        with Not_found -> [i])
    | (_, Disabled)::l -> aux (i+1) l
    | (_, Site_tree (_, hsl))::l ->
        (try
          i::aux 0 hsl
        with Not_found -> aux (i+1) l)
  in 
  try
    aux 0 pages
  with Not_found -> []
  

let hierarchical_menu_depth_first
    ?(classe=[]) 
    ?(whole_tree=false)
    ((page, pages) as the_menu)
    ~service
    ~sp =
  
  let rec depth_first_fun pages level pos : [ `Ul ] XHTML.M.elt list =
    let rec one_item first last i s = 
      let (classe, pos2, deplier) =
        match pos with
        | [] -> ([], [], false)
        | a::l when a = i -> ([current_class], l, true)
        | _::l -> ([], [], false)
      in
      let classe =
        if last then
          last_class::classe
        else classe
      in
      let classe =
        if first then
          first_class::classe
        else classe
      in
      let attclass = 
        if classe = [] then
          []
        else [a_class classe]
      in
      match s with
      | (text, Site_tree (Default_page page, []))
      | (text, Site_tree (Main_page page, [])) -> 
          li ~a:attclass [a page sp text ()]
      | (text, Site_tree (Not_clickable, [])) -> 
          li ~a:attclass text
      | (text, Disabled) -> 
          li ~a:[a_class (disabled_class::classe)] text
      | (text, Site_tree (Default_page page, hsl))
      | (text, Site_tree (Main_page page, hsl)) -> 
          li ~a:attclass
            ((a page sp text ())::
             if deplier || whole_tree then
               (depth_first_fun hsl (level+1) pos2
                  : [ `Ul ] XHTML.M.elt list
                  :> [< Xhtmltypes.li_content > `Ul ] XHTML.M.elt list)
             else [])
      | (text, Site_tree (Not_clickable, hsl)) -> 
          li ~a:attclass
            ((text : Xhtmltypes.a_content XHTML.M.elt list
                :> Xhtmltypes.li_content XHTML.M.elt list)@
             if deplier || whole_tree then
               (depth_first_fun hsl (level+1) pos2
                  : [ `Ul ] XHTML.M.elt list
                  :> [< Xhtmltypes.li_content > `Ul ] XHTML.M.elt list)
             else [])
            
    and one_menu first i = function
      | [] -> []
      | [a] -> [one_item first true i a]
      | a::l -> (one_item first false i a)::(one_menu false (i+1) l)
    in
    let classe = (level_class^string_of_int level)::classe in
    match one_menu true 0 pages with
    | [] -> []
    | li::lis -> [ul ~a:[a_class (menu_class::classe)] li lis]
  in

  (depth_first_fun pages 0 (find_in_hierarchy service the_menu)
     : [ `Ul ] XHTML.M.elt list :> [> `Ul ] XHTML.M.elt list)


let hierarchical_menu_breadth_first
    ?(classe=[]) 
    ((page, pages) as the_menu)
    ~service
    ~sp =

  let rec breadth_first_fun pages level pos
      : [ `Ul ] XHTML.M.elt list =
    let rec one_item first last i s = 
      let (classe, pos2, deplier) =
        match pos with
        | [] -> ([], [], false)
        | a::l when a = i -> ([current_class], l, true)
        | _::l -> ([], l, false)
      in
      let classe =
        if last then
          last_class::classe
        else classe
      in
      let classe =
        if first then
          first_class::classe
        else classe
      in
      let attclass = 
        if classe = [] then
          []
        else [a_class classe]
      in
      match s with
      | (text, Site_tree (Default_page page, _))
      | (text, Site_tree (Main_page page, _)) -> 
          li ~a:attclass [a page sp text ()]
      | (text, Site_tree (Not_clickable, _)) -> 
          li ~a:attclass text
      | (text, Disabled) -> 
          li ~a:[a_class (disabled_class::classe)] text
    and one_menu first i = function
      | [] -> []
      | [a] -> [one_item first true i a]
      | a::l -> (one_item first false i a)::(one_menu false (i+1) l)
    and submenu i pos pages =
      match snd (List.nth pages i) with
      | Disabled
      | Site_tree (_, []) -> []
      | Site_tree (_, hsl) -> breadth_first_fun hsl (level+1) pos
    in
    let classe =
      (level_class^string_of_int level)::classe
    in
    let l =
      match pos with
      | [] -> []
      | a::l -> submenu a l pages
    in
    match one_menu true 0 pages with
    | [] -> l
    | li::lis -> (ul ~a:[a_class (menu_class::classe)] li lis)::l
          
  in
  (breadth_first_fun pages 0 (find_in_hierarchy service the_menu)
    : [ `Ul ] XHTML.M.elt list :> [> `Ul ] XHTML.M.elt list)


let structure_links (default, pages) ~service ~sp =
  let make_rev s endlist =
    (* I am a subsection of s *)
    match s with
    | None -> endlist
    | Some s ->
        (link ~a:[a_rev [`Subsection]; 
                 a_href (make_uri s sp ()); 
               ] ())::endlist
  in
  let make_rel s =
    (* s is a subsection of mine *)
    link ~a:[a_rel [`Subsection]; 
             a_href (make_uri s sp ()); 
           ] ()
  in
  let make_rels beg a = 
    match snd a with
    | Site_tree (Main_page page, _) -> (make_rel page)::beg
    | _ -> beg
  in
  let rec create_rev parent = function
  | [] -> raise Not_found
  | (_, (Site_tree (Main_page s, [])))::l when s = service -> 
      make_rev parent []
  | (_, Disabled)::l
  | (_, Site_tree (_, []))::l -> create_rev parent l
  | (_, Site_tree (Main_page page, hsl))::_ when service = page ->
      make_rev parent (List.fold_left make_rels [] hsl)
  | (_, Site_tree (Main_page page, hsl))::l ->
      (try create_rev (Some page) hsl
      with Not_found -> create_rev parent l)
  | (_, Site_tree (_, hsl))::l ->
      (try create_rev None hsl
      with Not_found -> create_rev parent l)
  in 
  try
    match default with
    | Main_page def -> 
        if def = service then
          List.fold_left make_rels [] pages
        else create_rev (Some def) pages
    | _ ->
        create_rev None pages
  with Not_found -> []
