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


(** Predefined boxes for Eliom *)

open Eliom_services
open Eliom_parameters
open Eliom_sessions

(** {2 Menus } *)

type ('a, 'b) one_page =
    (unit, unit,
     'a,
     [ `WithoutSuffix ],
     unit, unit,
     'b) service

val menu :
  ?classe:XHTML.M.nmtoken list ->
  (([< get_service_kind ] as 'a, [< registrable ] as 'b) one_page *
     Xhtmltypes.a_content XHTML.M.elt list)
  ->
  (('a, 'b) one_page *
     Xhtmltypes.a_content XHTML.M.elt list)
    list ->
  ?service:('a, 'b) one_page ->
  sp:Eliom_sessions.server_params ->
  [> `Ul ] XHTML.M.elt
(** Creates a menu

   Example:

  [menu ~classe:["mainmenu"]
    [
     (home, <:xmllist< Home >>);
     (infos, <:xmllist< More infos >>)
   ] current sp]

*)

(** {2 Hierchical sites } *)


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
(** The type of hierarchical sites.
    A hierarchical site is a pair (main page, subpages).

    The difference between
    [Main_page], [Default_page] and [Not_clickable] is a bit subtle:

    - [Main_page] is when you want to create a main page for your
    subsite. All the subpages are subsections of that page.

    - [Default_page] is like [Main_page] but is not taken into account
    for computing which is the current page in the menu.
    Use it for example when there is no main page, but you want
    one of the subpages to be the default page for your subsite.
    The service you use as default page
    must appear another time in the subtree!

    - [Not_clickable] is when you do not want the menu entry to be a link
    but you want subpages.

    Each subpage is defined by the text to be displayed in menus
    and a [hierarchical_site_item].
    If the latter is [Disabled], the menu entry is disabled.

 *)


(**
    [hierarchical_menu_depth_first menu] constructs a function taking
    as parameters a service and [~sp] (server parameters)
    and displaying a hierarchical menu for this service.

    The menu is constructed by exploring the tree using
    a depth-first algorithm. It means that the first menu item will be
    displayed, followed by the whole sub-menu for this item, then the second
    menu item with its sub-menu, and so on.
    By default, only the sub-menus corresponding to the current page
    are displayed. If you want all the sub-menus to be displayed, specify
    [?whole_tree=true].
 *)
val hierarchical_menu_depth_first :
  ?classe:XHTML.M.nmtoken list ->
  ?whole_tree:bool ->
  ([< Eliom_services.get_service_kind ] as 'a,
   [< Eliom_services.registrable ] as 'b,
   Xhtmltypes.a_content)
      hierarchical_site ->
  ?service:('a, 'b) one_page ->
  sp:Eliom_sessions.server_params ->
    [> `Ul ] XHTML.M.elt list



(**
    [hierarchical_menu_breadth_first menu] constructs a function taking
    as parameters a service and [~sp] (server parameters)
    and displaying a hierarchical menu for this service.

    The menu is constructed by exploring the tree using
    a breadth_first algorithm. It means that the whole menu for one
    level will be displayed, followed by all sub-menus.
    Only the sub-menu corresponding to the current page
    is displayed.
 *)
val hierarchical_menu_breadth_first :
  ?classe:XHTML.M.nmtoken list ->
  ([< Eliom_services.get_service_kind ] as 'a,
   [< Eliom_services.registrable ] as 'b,
   Xhtmltypes.a_content)
      hierarchical_site ->
  ?service:('a, 'b) one_page ->
  sp:Eliom_sessions.server_params ->
    [> `Ul ] XHTML.M.elt list


(** Returns the tags [<link rel="subsection" ...>] and
   [<link rev="subsection" ...>] for the given hierarchical site.
 *)
val structure_links :
    ([< Eliom_services.get_service_kind ] as 'a,
     [< Eliom_services.registrable ] as 'b,
     Xhtmltypes.a_content)
    hierarchical_site ->
  ?service:('a, 'b) one_page ->
  sp:Eliom_sessions.server_params ->
  [> `Link ] XHTML.M.elt list
