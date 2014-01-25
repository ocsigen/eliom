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

open Eliom_lib
open Eliom_content
open Eliom_service

include Eliom_tools_common

let string_prefix s1 s2 =
  String.length s1 <= String.length s2 &&
    s1 = String.sub s2 0 (String.length s1)

module type HTML5_TOOLS = sig

(** {2 Simple menu } *)

  (** The function [menu elts ()], where [elts] is a list of pair
      [(service, content)], creates a list of link towards the
      [service]s. See the Eliom manual for an {% <<a_manual
      chapter="misc" fragment="basic_menu"|example of menu>>%}.

      The optional parameter [service] is used to find which item(s)
      to highlight (by adding the class [eliomtools_current] to the
      corresponding [<li>] node). The default is to highlight the item
      corresponding to the current url.

      The optional parameters [id] and [classe] allow to specify the
      corresponding attributes in the generated [<ul>] node. The
      default class for the [<ul>] node is [eliomtools_menu]. *)
  val menu :
    ?classe:Html5_types.nmtoken list ->
    ?id:string ->
    (([< get_service_kind ] as 'a,
      [< registrable ] as 'b,
      [< Eliom_registration.non_ocaml_service ] as 'c) one_page *
        Html5_types.flow5_without_interactive Html5.elt list)
      list ->
    ?service:('a, 'b, 'c) one_page ->
    unit ->
    [> `Ul ] Html5.elt

(** {2 Hierchical sites } *)

  (** The function [hierarchical_menu_depth_first site ()] constructs
      a hierarchical menu by exploring the hierarchical [site]
      description using a depth-first algorithm: the first menu item
      will be displayed, followed by the whole sub-menu for this item,
      then the second menu item with its sub-menu, and so on.

      By default, only the sub-menus for to the url corresponding to
      the optional argument [service] are displayed, others sub-menu
      are collapsed. If you want all the sub-menus to be displayed,
      specify [~whole_tree:true]. If the optional parameter [service]
      is not given, the current page is used.

      See {!menu} for a description of the optional parameters [id]
      and [classe]. *)
  val hierarchical_menu_depth_first :
    ?classe:Html5_types.nmtoken list ->
    ?id:string ->
    ?whole_tree:bool ->
    ([< Eliom_service.get_service_kind ] as 'a,
     [< Eliom_service.registrable ] as 'b,
     Html5_types.a_content Html5.elt list)
      hierarchical_site ->
    ?service:('a, 'b, 'c) one_page ->
    unit ->
    [> `Ul ] Html5.elt list



  (** The function [hierarchical_menu_breadth_first site ()]
      constructs a hierarchical menu by exploring the hierarchical
      [site] description using a breadth_first algorithm: the whole
      menu for one level will be displayed, followed by the sub-menu
      leading to the current service, and so one.

      By default the current service correspond to the current
      url. The optional parameter [service] allow to override the
      current service.

      See {!menu} for a description of the optional parameters [id]
      and [classe].
  *)
  val hierarchical_menu_breadth_first :
    ?classe:Html5_types.nmtoken list ->
    ?id:string ->
    ([< Eliom_service.get_service_kind ] as 'a,
     [< Eliom_service.registrable ] as 'b,
     Html5_types.a_content Html5.elt list)
      hierarchical_site ->
    ?service:('a, 'b, [< Eliom_registration.non_ocaml_service]) one_page ->
    unit ->
    [> `Ul ] Html5.elt list

  (** The function [structure_links site ()] returns the tags [<link
      rel="subsection" ...>] and [<link rev="subsection" ...>] for the
      given hierarchical [site].

      By default the current service correspond to the current
      url. The optional parameter [service] allow to override the
      current service. *)
  val structure_links :
    ([< Eliom_service.get_service_kind ] as 'a,
     [< Eliom_service.registrable ] as 'b,
     Html5_types.a_content Html5.elt list)
    hierarchical_site ->
    ?service:('a, 'b, [< Eliom_registration.non_ocaml_service ]) one_page ->
    unit ->
    [> `Link ] Html5.elt list

  (** An auxiliary function for creating an HTML head elements. Resources (JS,
      CSS) are taken from the static directory. *)
  val head :
    title:string ->
    ?css:string list list ->
    ?js:string list list ->
    ?other:Html5_types.head_content_fun Html5.elt list ->
    unit ->
    Html5_types.head Html5.elt

  val html :
    title:string ->
    ?a:Html5_types.html_attrib Html5.attrib list ->
    ?css:string list list ->
    ?js:string list list ->
    ?other_head:Html5_types.head_content_fun Html5.elt list ->
    Html5_types.body Html5.elt ->
    Html5_types.html Html5.elt
end


let css_files = Eliom_reference.Volatile.eref ~scope:Eliom_common.request_scope []
let js_files = Eliom_reference.Volatile.eref ~scope:Eliom_common.request_scope []
let with_css_file file =
  Eliom_reference.Volatile.modify css_files (fun files -> file :: files)
let with_js_file file =
  Eliom_reference.Volatile.modify js_files (fun files -> file :: files)


module Make(DorF : module type of Eliom_content.Html5.F) : HTML5_TOOLS = struct
  open Html5_types
  open Html5.F

  let a_ul classes id level =
    let classes = [a_class classes] in
    match id, level with
      | Some id, 0 -> a_id id :: classes
      | _ -> classes

  let same_service_opt s sopt =
    let same_url url =
      make_string_uri ~absolute_path:true ~service:s () = url in
    match sopt with
      | None ->
        same_url ("/"^(Eliom_request_info.get_current_sub_path_string ()))
      | Some s' -> same_url (make_string_uri ~absolute_path:true ~service:s' ())


  let menu ?(classe=[]) ?id l ?service:current () =
    let rec aux = function
      | [] -> []
      | [(url, text)] ->
        let classe = [last_class] in
        let _ = li [a url text ()] in
        if same_service_opt url current
        then [li ~a:[a_class (current_class::classe)] text]
        else [li ~a:[a_class classe] [a url text ()]]
      | (url, text)::l ->
        (if same_service_opt url current
         then  (li ~a:[a_class [current_class]] text)
         else (li [a url text ()]))::(aux l)
    in match l with
      | [] ->  DorF.ul ~a:(a_ul (menu_class::classe) id 0) []
      | [(url, text)] ->
        DorF.ul ~a:(a_ul (menu_class::classe) id 0)
          [let liclasse = [first_class; last_class] in
           if same_service_opt url current
           then (li ~a:[a_class (current_class::liclasse)] text)
           else (li ~a:[a_class liclasse] [a url text ()])]
      | (url, text)::l ->
        DorF.ul ~a:(a_ul (menu_class::classe) id 0)
          (let liclasse = [first_class] in
           (if same_service_opt url current
           then (li ~a:[a_class (current_class::liclasse)] text)
           else (li ~a:[a_class liclasse] [a url text ()])) :: (aux l))

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

    let rec depth_first_fun pages level pos =
      let rec one_item first last i s =
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
        let attclass =
          if classe = [] then
            []
          else [a_class classe]
        in
        match s with
          | (text, Site_tree (Default_page page, []))
          | (text, Site_tree (Main_page page, [])) ->
            li ~a:attclass [a page text ()]
          | (text, Site_tree (Not_clickable, [])) ->
            li ~a:attclass text
          | (text, Disabled) ->
            li ~a:[a_class (disabled_class::classe)] text
          | (text, Site_tree (Default_page page, hsl))
          | (text, Site_tree (Main_page page, hsl)) ->
            li ~a:attclass
              ((a page text ())::
                  if deplier || whole_tree then
                    (depth_first_fun hsl (level+1) pos2
                       : [ `Ul ] elt list
                     :> [< li_content > `Ul ] elt list)
                  else [])
          | (text, Site_tree (Not_clickable, hsl)) ->
            li ~a:attclass
              ((text : a_content elt list
                :> li_content elt list)@
                  if deplier || whole_tree then
                    (depth_first_fun hsl (level+1) pos2
                       : [ `Ul ] elt list
                     :> [< li_content > `Ul ] elt list)
                  else [])

      and one_menu first i = function
        | [] -> []
        | [a] -> [one_item first true i a]
        | a::l -> (one_item first false i a)::(one_menu false (i+1) l)
      in
      let classe = (level_class^string_of_int level)::classe in
      let ul = if level = 0 then DorF.ul else ul in
      [ul ~a:(a_ul (menu_class::classe) id level) (one_menu true 0 pages)]
    in

    (depth_first_fun pages 0 (find_in_hierarchy service the_menu)
       : [ `Ul ] elt list :> [> `Ul ] elt list)


  let hierarchical_menu_breadth_first
      ?(classe=[])
      ?id
      ((page, pages) as the_menu)
      ?service
      () =

    let rec breadth_first_fun pages level pos
        : [ `Ul ] elt list =
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
        let attclass =
          if classe = [] then
            []
          else [a_class classe]
        in
        match s with
          | (text, Site_tree (Default_page page, _))
          | (text, Site_tree (Main_page page, _)) ->
            li ~a:attclass [a page text ()]
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
      let ul = if level = 0 then DorF.ul else ul in
      ul ~a:(a_ul (menu_class::classe) id level) (one_menu true 0 pages)::l

    in
    (breadth_first_fun pages 0 (find_in_hierarchy service the_menu)
       : [ `Ul ] elt list :> [> `Ul ] elt list)


  let structure_links (default, pages) ?service () =
    let make_rev s endlist =
    (* I am a subsection of s *)
      match s with
        | None -> endlist
        | Some s ->
          (link ~rel: [ `Next ] (* ?? *)
             ~href: (make_uri ~service: s ()) ()) :: endlist
    in
    let make_rel s =
    (* s is a subsection of mine *)
      link ~rel: [`Next ]
        ~href: (make_uri ~service: s ())
        ()
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
      | (_, Site_tree (Main_page page, hsl))::_ when same_service_opt page service ->
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
          if same_service_opt def service then
            List.fold_left make_rels [] pages
          else create_rev (Some def) pages
        | _ ->
          create_rev None pages
    with Not_found -> []

  let head ~title:ttl ?(css=[]) ?(js=[]) ?(other=[]) () =
    let open DorF in
    let mk_css_link path =
      let uri = make_uri (Eliom_service.static_dir ()) path in
      css_link ~uri () in
    let mk_js_script path =
      let uri = make_uri  (Eliom_service.static_dir ()) path in
      js_script ~uri () in
    DorF.head
      (title (pcdata ttl))
      List.(map mk_css_link css @ map mk_js_script js @ other)

  let html ~title ?a ?(css=[]) ?(js=[]) ?other_head body =
    let css =
      List.rev (Eliom_reference.Volatile.get css_files) @ css
    in
    let js =
      List.rev (Eliom_reference.Volatile.get js_files) @ js
    in
    DorF.html ?a
      (head ~title ~css ~js ?other:other_head ())
      body
end


module F = Make(Html5.F)
module D = Make(Html5.D)

let wrap_handler information none some =
  fun get post ->
    match_lwt information () with
      | None -> none get post
      | Some value -> some value get post
