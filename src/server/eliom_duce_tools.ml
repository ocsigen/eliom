(* Ocsigen
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


open Eliom_duce.Xhtml
open Eliom_services
open XHTML_types_duce
open Eliom_tools_common


let same_service_opt s sopt =
  let same_url url = make_uri ~absolute_path:true ~service:s () = url in
  match sopt with
    | None -> (* MAYBE : use this or get_original_full_path_string *)
        same_url ("/" ^ Eliom_request_info.get_current_full_path_string ())
    | Some s' -> same_url (make_uri ~absolute_path:true ~service:s' ())


let attrib_list (s: string list): string = String.concat " " s
let ul_attribs classes id level =
  {{ { class={: attrib_list classes :} } ++
     {{ match id, level with
          | Some id, 0 -> {{ { id={{Ocamlduce.Utf8.make id}} } }}
          | _ -> {{ {} }}
     }} }}

let menu ?(classe=[]) ?id first l ?service:current () =
  let rec aux = function
    | [] -> []
    | [(url, text)] ->
        let classe = [last_class] in
        if same_service_opt url current
        then
          [{{ <li class={: attrib_list (current_class::classe) :}>{: text :} }}]
            (* [li ~a:[a_class (current_class::classe)] text] *)
        else
          [{{ <li class={: attrib_list classe :}>[{: a url {: text :} () :}] }}]
            (* [li ~a:[a_class classe] [a url text ()] *)
    | (url, text)::l ->
        (if same_service_opt url current
         then
           {{ <li class={: current_class :}>{: text :} }}
             (* (li ~a:[a_class [current_class]] text) *)
         else
           {{ <li>[{: a url {: text :} () :}] }})::(aux l)
          (* (li [a url text ()]))::(aux l) *)
  in match first::l with
    | [] -> assert false
    | [(url, text)] ->
        {{ <ul ({{ ul_attribs (menu_class::classe) id 0 }})>
             [{:
               let liclasse = [first_class; last_class] in
                 if same_service_opt url current then
                   {{ <li class={: attrib_list (current_class::liclasse) :}>{: text :} }}
                 else
                   {{ <li class={: attrib_list liclasse :}>[{: a url {: text :} () :}] }} :}] }}
    | (url, text)::l ->
        {{ <ul ({{ ul_attribs (menu_class::classe) id 0 }})>
             [{:
               let liclasse = [first_class] in
                 if same_service_opt url current then
                   {{ <li class={: attrib_list (current_class::liclasse) :}>{: text :} }}
                 else
                   {{ <li class={: attrib_list liclasse :}>[{: a url {: text :} () :}] }} :}
                !{: aux l :}] }}

let string_prefix s1 s2 =
  String.length s1 <= String.length s2 &&
    s1 = String.sub s2 0 (String.length s1)

let service_prefix s sopt =
  let service_url = make_string_uri ~absolute_path:true ~service:s () in
  match sopt with
    | None -> string_prefix service_url
      ((* MAYBE : use get_original_full_path_string? *)
        ("/" ^ Eliom_request_info.get_current_full_path_string ()))
    | Some s' ->
      let node_url = make_string_uri ~absolute_path:true ~service:s' () in
      string_prefix service_url node_url

let find_longest_prefix_in_hierarchy service (main, pages) =
  let rec aux prefix (max_len, _ as max) i = function
    | [] -> max
    | (_, Site_tree (Main_page s, hsl)) :: pages when service_prefix s service ->
      let len =
	String.length (make_string_uri ~absolute_path:true ~service:s ()) in
      let max = if len >= max_len then (len, List.rev (i::prefix)) else max in
      let max = aux (i::prefix) max 0 hsl in
      aux prefix max (i+1) pages
    | (_, Disabled)::pages -> aux prefix max (i+1) pages
    | (_, Site_tree (_, hsl))::pages ->
      let max = aux (i::prefix) max 0 hsl in
      aux prefix max (i+1) pages
  in
  let length, path = aux [] (0,[]) 0 pages in
  path

let find_in_hierarchy service (main, pages) =
  let rec aux service i = function
    | [] -> raise Not_found
    | (_, Site_tree (Main_page s, hsl))::_ when same_service_opt s service ->
        (try
          i::aux service 0 hsl
        with Not_found -> [i])
    | (_, Disabled)::l -> aux service (i+1) l
    | (_, Site_tree (_, hsl))::l ->
        (try
          i::aux service 0 hsl
        with Not_found -> aux service (i+1) l)
  in
  try aux service 0 pages
  with Not_found ->
    find_longest_prefix_in_hierarchy service (main, pages)


let hierarchical_menu_depth_first
    ?(classe=[])
    ?id
    ?(whole_tree=false)
    ((page, pages) as the_menu)
    ?service
    () =

  let rec depth_first_fun pages level pos : {{ [XHTML_types_duce.ul*] }} =
    let rec one_item first last i s : XHTML_types_duce.li =
      let (classe, pos2, deplier) =
        match pos with
        | [] -> ([], [], false)
        | [a] when a = i -> ([current_class], [], true)
        | a::l when a = i -> ([current_path_class], l, true)
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
                                        {{ <li class={: attrib_list classe :}>[{: a page {: text :} () :}] }}
      | (text, Site_tree (Not_clickable, [])) ->
                                        {{ <li class={: attrib_list classe :}>{: text :} }}
      | (text, Disabled) ->
                                        {{ <li class={: attrib_list (disabled_class::classe) :}>{: text :} }}
      | (text, Site_tree (Default_page page, hsl))
      | (text, Site_tree (Main_page page, hsl)) ->
                                {{ <li class={: attrib_list classe :}>[
                                        {: a page {: text :} () :}
                                        !{: if deplier || whole_tree then
                                            (depth_first_fun hsl (level+1) pos2)
                                          else {{ [] }}
                                        :}
                                ] }}
      | (text, Site_tree (Not_clickable, hsl)) ->
                                        {{ <li class={: attrib_list classe :}>[
                                                !{: text :}
                                                !{: if deplier || whole_tree then
                                                        (depth_first_fun hsl (level+1) pos2)
                                                        else
                                                        {{ [] }} :}
                                        ] }}
          (* li ~a:attclass
            ((text : XHTML_types.a_content XHTML.M.elt list
                :> XHTML_types.li_content XHTML.M.elt list)@
             if deplier || whole_tree then
               (depth_first_fun hsl (level+1) pos2
                  : [ `Ul ] XHTML.M.elt list
                  :> [< XHTML_types.li_content > `Ul ] XHTML.M.elt list)
             else []) *)

    and one_menu first i l : {{ [XHTML_types_duce.li*] }} = match l with
      | [] -> {{ [] }}
      | [a] -> let aa = one_item first true i a in {{ [ aa ] }}
      | a::l -> 
          let aa = one_item first false i a in
          let al = one_menu false (i+1) l in
          {{ [ {: aa :} !{: al :} ] }}
    in
    let classe = (level_class^string_of_int level)::classe in
    match one_menu true 0 pages with
    | {{ [] }} -> {{ [] }}
    | {{ l }} ->
        {{ [ <ul ({{ ul_attribs (menu_class::classe) id level }})>l ] }}
  in

  (depth_first_fun pages 0 (find_in_hierarchy service the_menu))


let hierarchical_menu_breadth_first
    ?(classe=[])
    ?id
    (((page, pages): ([< Eliom_services.get_service_kind],
                [< Eliom_services.registrable ],
                'elts Eliom_duce_types.a_content_elt_list) hierarchical_site) as the_menu)
    ?service
    () =

  let rec breadth_first_fun pages level pos : {{ [XHTML_types_duce.ul*] }} =
    let rec one_item first last i s =
      let (classe, pos2, deplier) =
        match pos with
        | [] -> ([], [], false)
        | [a] when a = i -> ([current_class], [], true)
        | a::l when a = i -> ([current_path_class], l, true)
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
                                        {{ <li class={: attrib_list classe :}>[{: a page {: text :} () :}] }}
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
      | Site_tree (_, []) -> {{ [] }}
      | Site_tree (_, hsl) -> breadth_first_fun hsl (level+1) pos
    in
    let classe =
      (level_class^string_of_int level)::classe
    in
    let l =
      match pos with
      | [] -> {{ [] }}
      | a::l -> submenu a l pages
    in
    match one_menu true 0 pages with
    | [] -> l
    | li::lis ->
        {{ [ <ul ({{ ul_attribs (menu_class::classe) id level}})>[ {: li :} !{: lis :} ] !l ] }}

  in
  (breadth_first_fun pages 0 (find_in_hierarchy service the_menu))


let structure_links (default, pages) ?service () =
  let make_rev s endlist =
    (* I am a subsection of s *)
    match s with
    | None -> {{ [ !{: endlist :} ] }}
    | Some s ->
        {{ [ <link rev="Subsection" href={: make_uri ~service:s () :}>[] 
               !{: endlist :} ] }}
  in
  let make_rel s =
    (* s is a subsection of mine *)
                {{ <link rel="Subsection" href={: make_uri ~service:s () :}>[] }}
  in
  let make_rels beg a =
    match snd a with
    | Site_tree (Main_page page, _) -> (make_rel page)::beg
    | _ -> beg
  in
  let rec create_rev parent = function
  | [] -> raise Not_found
  | (_, (Site_tree (Main_page s, [])))::l when same_service_opt s service ->
      make_rev parent []
  | (_, Disabled)::l
  | (_, Site_tree (_, []))::l -> create_rev parent l
  | (_, Site_tree (Main_page page, hsl))::l ->
      if same_service_opt page service
      then make_rev parent (List.fold_left make_rels [] hsl)
      else
        (try create_rev (Some page) hsl
         with Not_found -> create_rev parent l)
  | (_, Site_tree (_, hsl))::l ->
      (try create_rev None hsl
      with Not_found -> create_rev parent l)
  in
  try
    match default with
    | Main_page def ->
        if same_service_opt def service then
          {{ [ !{: List.fold_left make_rels [] pages :} ] }}
        else create_rev (Some def) pages
    | _ ->
        create_rev None pages
  with Not_found -> {{ [] }}
