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

(** Predefined boxes for Eliom *)

open Eliom_service
open Eliom_parameter
open Eliom_state
open Eliom_tools_common

(** {2 Menus } *)

val menu :
  ?classe:string list ->
  ?id:string ->
  (([< get_service_kind ] as 'a, [< registrable ] as 'b, [< Eliom_output.non_caml_service ] as 'c) one_page *
     'elts Eliom_duce_types.a_content_elt_list)
  ->
  (('a, 'b, 'c) one_page *
     'elts Eliom_duce_types.a_content_elt_list)
    list ->
  ?service:('a, 'b, 'c) one_page ->
  unit ->
  Xhtml_types_duce.ul
(** Creates a menu

   Example:

  [menu ~classe:["mainmenu"]
    [
     (home, <:xmllist< Home >>);
     (infos, <:xmllist< More infos >>)
   ] current sp]

   The [service] argument is used to find which item(s) to highlight. If
   service is [None], the current url is used.

*)

(** {2 Hierchical sites } *)

(**
    [hierarchical_menu_depth_first menu] constructs a function taking
    as parameters a service and [~sp] (server parameters)
    and displaying a hierarchical menu for this service.

    The menu is constructed by exploring the tree using
    a depth-first algorithm. It means that the first menu item will be
    displayed, followed by the whole sub-menu for this item, then the second
    menu item with its sub-menu, and so on.
    By default, only the sub-menus for to the url corresponding to
    the argument [service] are displayed. If you want all the sub-menus to be
    displayed, specify [?whole_tree=true]. If [service] is [None], the current
    page is used.
 *)
val hierarchical_menu_depth_first :
  ?classe:string list ->
  ?id:string ->
  ?whole_tree:bool ->
  ([< Eliom_service.get_service_kind ] as 'a,
   [< Eliom_service.registrable ] as 'b,
   'elts Eliom_duce_types.a_content_elt_list)
      hierarchical_site ->
  ?service:('a, 'b, [< Eliom_output.non_caml_service ]) one_page ->
  unit ->
    {{ [Xhtml_types_duce.ul*] }}


(**
    [hierarchical_menu_breadth_first menu] constructs a function taking
    as parameters a service and [~sp] (server parameters)
    and displaying a hierarchical menu for this service.

    The menu is constructed by exploring the tree using
    a breadth_first algorithm. It means that the whole menu for one
    level will be displayed, followed by all sub-menus.

    Only the sub-menus for to the url corresponding to the argument [service]
    are displayed. If [service] is [None], the current url is used.
 *)
val hierarchical_menu_breadth_first :
  ?classe:string list ->
  ?id:string ->
  ([< Eliom_service.get_service_kind ] as 'a,
   [< Eliom_service.registrable ] as 'b,
   'elts Eliom_duce_types.a_content_elt_list)
      hierarchical_site ->
  ?service:('a, 'b, [< Eliom_output.non_caml_service ]) one_page ->
  unit ->
    {{ [Xhtml_types_duce.ul*] }}


(** Returns the tags [<link rel="subsection" ...>] and
   [<link rev="subsection" ...>] for the given hierarchical site.
 *)
val structure_links :
  ([< Eliom_service.get_service_kind ] as 'a,
     [< Eliom_service.registrable ] as 'b,
     'elts Eliom_duce_types.a_content_elt_list)
    hierarchical_site ->
  ?service:('a, 'b, [< Eliom_output.non_caml_service ]) one_page ->
  unit ->
  {{ [Xhtml_types_duce.link*] }}

