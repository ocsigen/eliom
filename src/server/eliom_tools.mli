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


(** Helpers for (hierarchical) menu generation in HTML5 and XHTML. See
    the Eliom manual for more information about {% <<a_manual
    chapter="misc" fragment="basic_menu"| menu>>%} or {% <<a_manual
    chapter="misc" fragment="hier_menu"| hierarchical site>>%}. *)

open Eliom_lib
open Eliom_content
open Eliom_service
open Eliom_parameter
open Eliom_state
open Eliom_tools_common

(** Restriction of {!type:Eliom_service.service} to services without
    parameters that do not returns a marshalled OCaml value. *)
type ('a, 'b, 'c) one_page =
    (unit, unit,
     'a,
     [ `WithoutSuffix ],
     unit, unit,
     'b, 'c) service
constraint 'c = [< Eliom_registration.non_caml_service ]

(** Restriction of {!type:Eliom_service.service} to registrable GET
    services without parameters that do not returns a marshalled OCaml
    value. *)
type get_page =
    (Eliom_service.get_service_kind,
     Eliom_service.registrable,
     Eliom_registration.non_caml_service) one_page

(** Hierarchical sites description. This is is a pair [(main page,
    subpages list)]. Each subpage is defined by the text to be
    displayed in menus and a {!hierarchical_site_item}. *)
type ('a, 'b, 'c) hierarchical_site =
    (('a, 'b) main_page *
        ('c * ('a, 'b, 'c) hierarchical_site_item) list)
constraint 'b = [< Eliom_service.registrable ]

(* Be kind with ocamldoc when source code is preprocessed with camlp4,
   do not remove this comment ! *)

(** Main page description for a section of a hierarchical site. *)
and ('a, 'b) main_page =
  | Main_page of ('a, 'b, Eliom_registration.non_caml_service) one_page
    (** Main page for your subsite: all the subpages are subsections
	of that page. *)
  | Default_page of ('a, 'b, Eliom_registration.non_caml_service) one_page
    (** Like [Main_page] but is not taken into account for computing
	which is the current page in the menu. Use it for example when
	there is no main page, but you want one of the subpages to be
	the default page for your subsite.  The service you use as
	default page must appear another time in the subtree! *)
  | Not_clickable
    (** When you do not want the menu entry to be a link
        but you want subpages. *)
constraint 'b = [< Eliom_service.registrable ]

(* Be kind with ocamldoc when source code is preprocessed with camlp4:
   do not remove this comment ! *)

(** Menu entry description in a hierarchical site. *)
and ('a, 'b, 'c) hierarchical_site_item =
  | Disabled (** The menu entry is disabled. *)
  | Site_tree of ('a, 'b, 'c) hierarchical_site (** The menu entry as a label and subsections. *)
constraint 'b = [< Eliom_service.registrable ]

(* Be kind with ocamldoc when source code is preprocessed with camlp4:
   do not remove this comment ! *)

(** Menu and hierarchical site for HTML5 *)
module type MENU = sig

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
      [< Eliom_registration.non_caml_service ] as 'c) one_page *
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
    ?service:('a, 'b, [< Eliom_registration.non_caml_service]) one_page ->
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
    ?service:('a, 'b, [< Eliom_registration.non_caml_service ]) one_page ->
    unit ->
    [> `Link ] Html5.elt list

  (** An auxiliary function for creating an HTML head elements. Resources (JS,
      CSS) are taken from the static directory. *)
  val head :
    title:string ->
    ?css:string list list ->
    ?js:string list list ->
    unit ->
    Html5_types.head Html5.elt
end

module Menu : sig
  (** Menus with functional node semantics *)
  module F : MENU
    
  (** Menus with DOM semantics *)
  module D : MENU
end
