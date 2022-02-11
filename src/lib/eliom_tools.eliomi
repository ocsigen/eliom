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

[%%shared.start]

(** Helpers for (hierarchical) menu generation in HTML5. See
    the Eliom manual for more information about {% <<a_manual
    chapter="misc" fragment="basic_menu"| menu>>%} or {% <<a_manual
    chapter="misc" fragment="hier_menu"| hierarchical site>>%}. *)

open Eliom_content

type srv =
  | Srv :
      ( unit
      , unit
      , Eliom_service.get
      , _
      , _
      , _
      , _
      , [`WithoutSuffix]
      , unit
      , unit
      , Eliom_service.non_ocaml )
      Eliom_service.t
      -> srv

type 'a hierarchical_site = main_page * ('a * 'a hierarchical_site_item) list
(** Hierarchical sites description. This is a pair [(main page,
    subpages list)]. Each subpage is defined by the text to be
    displayed in menus and a {!hierarchical_site_item}. *)

(** Menu entry description in a hierarchical site. *)
and 'a hierarchical_site_item = Disabled | Site_tree of 'a hierarchical_site

(** Main page description for a section of a hierarchical site. *)
and main_page =
  | Main_page of srv
      (** Main page for your subsite: all the subpages are subsections of
      that page. *)
  | Default_page of srv
      (** Like [Main_page] but is not taken into account for computing
      which is the current page in the menu. Use it for example when
      there is no main page, but you want one of the subpages to be
      the default page for your subsite.  The service you use as
      default page must appear another time in the subtree! *)
  | Not_clickable
      (** When you do not want the menu entry to be a link but you want
      subpages. *)

(** {2 Tools for generating certain HTML elements} *)

(* Be kind with ocamldoc when source code is preprocessed with camlp4:
   do not remove this comment ! *)

module type HTML5_TOOLS = sig
  (** {2 Simple menu } *)

  val menu
    :  ?classe:Html_types.nmtoken list
    -> ?id:string
    -> (( unit
        , unit
        , Eliom_service.get
        , _
        , _
        , _
        , _
        , [`WithoutSuffix]
        , unit
        , unit
        , Eliom_service.non_ocaml )
        Eliom_service.t
       * [< Html_types.flow5_without_interactive] Html.elt list)
       list
    -> ?service:
         ( unit
         , unit
         , Eliom_service.get
         , _
         , _
         , _
         , _
         , [`WithoutSuffix]
         , unit
         , unit
         , Eliom_service.non_ocaml )
         Eliom_service.t
    -> unit
    -> [> `Ul] Html.elt
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

  (** {2 Hierchical sites } *)

  val hierarchical_menu_depth_first
    :  ?classe:Html_types.nmtoken list
    -> ?id:string
    -> ?whole_tree:bool
    -> [< Html_types.a_content] Html.elt list hierarchical_site
    -> ?service:
         ( unit
         , unit
         , Eliom_service.get
         , _
         , _
         , _
         , _
         , [`WithoutSuffix]
         , unit
         , unit
         , Eliom_service.non_ocaml )
         Eliom_service.t
    -> unit
    -> [> `Ul] Html.elt list
  (** The function [hierarchical_menu_depth_first site ()] constructs
      a hieranrchical menu by exploring the hierarchical [site]
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

  val hierarchical_menu_breadth_first
    :  ?classe:Html_types.nmtoken list
    -> ?id:string
    -> [< Html_types.a_content] Html.elt list hierarchical_site
    -> ?service:
         ( unit
         , unit
         , Eliom_service.get
         , _
         , _
         , _
         , _
         , [`WithoutSuffix]
         , unit
         , unit
         , Eliom_service.non_ocaml )
         Eliom_service.t
    -> unit
    -> [> `Ul] Html.elt list
  (** The function [hierarchical_menu_breadth_first site ()]
      constructs a hierarchical menu by exploring the hierarchical
      [site] description using a breadth_first algorithm: the whole
      menu for one level will be displayed, followed by the sub-menu
      leading to the current service, and so one.

      By default the current service correspond to the current
      url. The optional parameter [service] allow to override the
      current service.

      See {!menu} for a description of the optional parameters [id]
      and [classe].  *)

  val structure_links
    :  [< Html_types.a_content] Html.elt list hierarchical_site
    -> ?service:
         ( unit
         , unit
         , Eliom_service.get
         , _
         , _
         , _
         , _
         , [`WithoutSuffix]
         , unit
         , unit
         , Eliom_service.non_ocaml )
         Eliom_service.t
    -> unit
    -> [> `Link] Html.elt list
  (** The function [structure_links site ()] returns the tags [<link
      rel="subsection" ...>] and [<link rev="subsection" ...>] for the
      given hierarchical [site].

      By default the current service correspond to the current
      url. The optional parameter [service] allow to override the
      current service. *)

  val head
    :  title:string
    -> ?css:string list list
    -> ?js:string list list
    -> ?other:[< Html_types.head_content_fun] Html.elt list
    -> unit
    -> [`Head] Html.elt
  (** An auxiliary function for creating an HTML head
      elements. Resources (JS, CSS) are taken from the static
      directory. *)

  val html
    :  title:string
    -> ?a:[< Html_types.html_attrib] Html.attrib list
    -> ?css:string list list
    -> ?js:string list list
    -> ?other_head:[< Html_types.head_content_fun] Html.elt list
    -> [`Body] Html.elt
    -> [`Html] Html.elt
end

module F : HTML5_TOOLS
(** Menus with functional node semantics *)

module D : HTML5_TOOLS
(** Menus with DOM semantics *)

val with_js_file : string list -> unit
(** Record an (external) JavaScript file to be included in
    {!Eliom_tools.F.html}. *)

val with_css_file : string list -> unit
(** Record an CSS file to be included in {!Eliom_tools.F.html}. *)

(** {2 Other tools} *)

val wrap_handler
  :  (unit -> 'a option Lwt.t)
  -> ('get -> 'post -> 'res Lwt.t)
  -> ('a -> 'get -> 'post -> 'res Lwt.t)
  -> 'get
  -> 'post
  -> 'res Lwt.t
(** This function allows one to wrap a service handler easily depending
    on whether certain information is available or not.

    The first arguments provides that information ([Some value]) of
    not ([None]), the second argument is called just with two
    arguments when the information is not available (the two arguments
    are suggesting GET and POST parameters of a request). The third
    argument is called with that information if available and the
    parameters.

    {% <<code language="ocaml"|
    let user_eref = Eliom_reference.eref ~scope None
    let anonymous_handler _ _ =
      Lwt.return (html (head (title "not allowed")) (body []))
    let authenticated_handler f =
      Eliom_tools.wrap_handler
        (fun () -> Eliom_reference.get user_eref)
        anonymous_handler f
    let guarded_service =
      My_app.register_service ~path ~get_param
        (authenticated_handler
           (fun user get () ->
              Lwt.return (html (head (title ("hello "^user))) (body []))))
    >> %}
  *)

[%%server.start]

(**/**)

val menu_class : string
val last_class : string
val current_class : string
val current_path_class : string
val disabled_class : string
val first_class : string
val level_class : string
