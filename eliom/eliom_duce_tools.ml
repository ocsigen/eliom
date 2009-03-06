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


(*open XHTML.M *)
(* open Eliom_predefmod.Xhtml *)
open Eliom_duce.Xhtml
open Eliom_services
open Xhtmltypes_duce
open Eliom_tools_common

let attrib_list (s: string list): string = String.concat " " s

let menu ?(classe=[]) first l ?service:current ~sp =
  let rec aux = function
    | [] -> []
    | [(url, text)] ->
        let classe = [last_class] in
        if Some url = (* problem with preapplied services with == *) current
        then
          [{{ <li class={: attrib_list (current_class::classe) :}>{: text :} }}]
            (* [li ~a:[a_class (current_class::classe)] text] *)
        else
          [{{ <li class={: attrib_list classe :}>[{: a url sp {: text :} () :}] }}]
            (* [li ~a:[a_class classe] [a url sp text ()] *)
    | (url, text)::l ->
        (if Some url = (* problem with preapplied services with == *) current
         then
           {{ <li class={: current_class :}>{: text :} }}
             (* (li ~a:[a_class [current_class]] text) *)
         else
           {{ <li>[{: a url sp {: text :} () :}] }})::(aux l)
          (* (li [a url sp text ()]))::(aux l) *)
  in match first::l with
    | [] -> assert false
    | [(url, text)] ->
        {{ <ul class={: attrib_list (menu_class::classe) :}>
             [{:
               let liclasse = [first_class; last_class] in
                 if Some url = current then
                   {{ <li class={: attrib_list (current_class::liclasse) :}>{: text :} }}
                 else
                   {{ <li class={: attrib_list liclasse :}>[{: a url sp {: text :} () :}] }} :}] }}
    | (url, text)::l ->
        {{ <ul class={: attrib_list (menu_class::classe) :}>
             [{:
               let liclasse = [first_class] in
                 if Some url = current then
                   {{ <li class={: attrib_list (current_class::liclasse) :}>{: text :} }}
                 else
                   {{ <li class={: attrib_list liclasse :}>[{: a url sp {: text :} () :}] }} :}
                !{: aux l :}] }}


let find_in_hierarchy service (main, pages) =
  let rec aux service i = function
    | [] -> raise Not_found
    | (_, Site_tree (Main_page s, hsl))::_ when s = service ->
        (try
          i::aux service 0 hsl
        with Not_found -> [i])
    | (_, Disabled)::l -> aux service (i+1) l
    | (_, Site_tree (_, hsl))::l ->
        (try
          i::aux service 0 hsl
        with Not_found -> aux service (i+1) l)
  in
  try
    match service with
      | None -> []
      | Some service -> aux service 0 pages
  with Not_found -> []


let hierarchical_menu_depth_first
    ?(classe=[])
    ?(whole_tree=false)
    ((page, pages) as the_menu)
    ?service
    ~sp =

  let rec depth_first_fun pages level pos =
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
      (* let attclass =
        if classe = [] then
          []
        else [a_class classe]
      in *)
      match s with
      | (text, Site_tree (Default_page page, []))
      | (text, Site_tree (Main_page page, [])) ->
                                        {{ <li class={: attrib_list classe :}>[{: a page sp {: text :} () :}] }}
      | (text, Site_tree (Not_clickable, [])) ->
                                        {{ <li class={: attrib_list classe :}>{: text :} }}
      | (text, Disabled) ->
                                        {{ <li class={: attrib_list (disabled_class::classe) :}>{: text :} }}
      | (text, Site_tree (Default_page page, hsl))
      | (text, Site_tree (Main_page page, hsl)) ->
                                {{ <li class={: attrib_list classe :}>[
                                        {: a page sp {: text :} () :}
                                        !{: if deplier || whole_tree then
                                                (depth_first_fun hsl (level+1) pos2)
                                                else []
                                        :}
                                ] }}
      | (text, Site_tree (Not_clickable, hsl)) ->
                                        {{ <li class={: attrib_list classe :}>[
                                                !{: text :}
                                                !{: if deplier || whole_tree then
                                                        (depth_first_fun hsl (level+1) pos2)
                                                        else
                                                        [] :}
                                        ] }}
          (* li ~a:attclass
            ((text : Xhtmltypes.a_content XHTML.M.elt list
                :> Xhtmltypes.li_content XHTML.M.elt list)@
             if deplier || whole_tree then
               (depth_first_fun hsl (level+1) pos2
                  : [ `Ul ] XHTML.M.elt list
                  :> [< Xhtmltypes.li_content > `Ul ] XHTML.M.elt list)
             else []) *)

    and one_menu first i = function
      | [] -> []
      | [a] -> [one_item first true i a]
      | a::l -> (one_item first false i a)::(one_menu false (i+1) l)
    in
    let classe = (level_class^string_of_int level)::classe in
    match one_menu true 0 pages with
    | [] -> []
    | li::lis -> [{{ <ul class={: attrib_list (menu_class::classe) :}>[{: li :} !{: lis :}] }}]
  in

  (depth_first_fun pages 0 (find_in_hierarchy service the_menu))


let hierarchical_menu_breadth_first
    ?(classe=[])
    (((page, pages): ([< Eliom_services.get_service_kind],
                [< Eliom_services.registrable ],
                Eliom_duce.Blocks.a_content_elt_list) hierarchical_site) as the_menu)
    ?service
    ~sp =

  let rec breadth_first_fun pages level pos: Xhtmltypes_duce.ul list =
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
      (* let attclass =
        if classe = [] then
          []
        else [a_class classe]
      in *)
      match s with
      | (text, Site_tree (Default_page page, _))
      | (text, Site_tree (Main_page page, _)) ->
                                        {{ <li class={: attrib_list classe :}>[{: a page sp {: text :} () :}] }}
      | (text, Site_tree (Not_clickable, _)) ->
                                        {{ <li class={: attrib_list classe :}>{: text :} }}
      | (text, Disabled) ->
                                        {{ <li class={: attrib_list (disabled_class::classe) :}>{: text :} }}
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
    | li::lis -> {{ <ul class={: attrib_list (menu_class::classe) :}>[ {: li :} !{: lis :} ] }}::l

  in
  (breadth_first_fun pages 0 (find_in_hierarchy service the_menu)
    (* : [ `Ul ] XHTML.M.elt list :> [> `Ul ] XHTML.M.elt lis *))


let structure_links (default, pages) ?service ~sp =
  let make_rev s endlist =
    (* I am a subsection of s *)
    match s with
    | None -> endlist
    | Some s ->
                                {{ <link rev="Subsection" href={: make_uri s sp () :}>[] }}::endlist
  in
  let make_rel s =
    (* s is a subsection of mine *)
                {{ <link rel="Subsection" href={: make_uri s sp () :}>[] }}
  in
  let make_rels beg a =
    match snd a with
    | Site_tree (Main_page page, _) -> (make_rel page)::beg
    | _ -> beg
  in
  let rec create_rev parent = function
  | [] -> raise Not_found
  | (_, (Site_tree (Main_page s, [])))::l when Some s = service ->
      make_rev parent []
  | (_, Disabled)::l
  | (_, Site_tree (_, []))::l -> create_rev parent l
  | (_, Site_tree (Main_page page, hsl))::_ when service = Some page ->
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
        if Some def = service then
          List.fold_left make_rels [] pages
        else create_rev (Some def) pages
    | _ ->
        create_rev None pages
  with Not_found -> []
