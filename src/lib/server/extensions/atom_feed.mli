(* Ocsigen
 * Copyright (C) 2010 Archibald Pontier
 *
 * This source file is part of Ocsigen < http://ocsigen.org/ >
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

(** Everything we need to make an atom feed. *)

open Eliom_lib

(*
 * types {{{
 *)
type uri = Xml.uri
type lang = string
type base = uri
type ncname = string
type dateConstruct = string
type emailAddress = string
type mediaType = string
type length = int
type href = Xml.uri
type hrefLang = string
type rel = string
type ltitle = string
type scheme  = string
type label = string
type term = string

(**  Common optional attributes *)
type metaAttr = [ `Base of base | `Lang of lang ]

(** Children tags allowed for the author and contributor tags *)
type personConstruct = [ `Email of emailAddress | `Uri of uri ]

type author
type contributor
type generator
type id
type icon
type category
type link
type logo
type published
type updated
type source
type entry
type feed
type content
type textConstruct

(** Children tags allowed for the link tag *)
type linkOAttr = [ metaAttr
    | `Type of string
    | `Hrefl of hrefLang
    | `Length of length
    | `Medtype of mediaType
    | `Rel of rel
    | `Title of ltitle ]

(** Children tags allowed for the source tag *)
type sourceOAttr = [ metaAttr
    | `Authors of author list
    | `Cats of category list
    | `Contribs of contributor list
    | `Gen of generator
    | `Icon of icon
    | `Links of link list
    | `Logo of logo
    | `Rights of textConstruct
    | `Sub of textConstruct ]

(** Children tags allowed for the entry tag *)
type entryOAttr = [ metaAttr
    | `Authors of author list
    | `Cats of category list
    | `Content of content
    | `Contribs of contributor list
    | `Links of link list
    | `Pub of published
    | `Rights of textConstruct
    | `Source of source
    | `Sum of textConstruct ]

(** Children tags allowed for the feed tag *)
type feedOAttr = [ metaAttr
    | `Authors of author list
    | `Cats of category list
    | `Contribs of contributor list
    | `Gen of generator
    | `Icon of icon
    | `Links of link list
    | `Logo of logo
    | `Rights of textConstruct
    | `Sub of textConstruct ]
(*
 * }}}
 *)

(*
 * Constructors {{{
 *)

val xml_of_feed : feed -> Xml.elt

(*
 * attr converters {{{
val a_base : base -> Xml.attrib
val a_lang : lang -> Xml.attrib
val a_scheme : scheme -> Xml.attrib
val a_label : label -> Xml.attrib
val a_href : href -> Xml.attrib
val a_rel : rel -> Xml.attrib
val a_hreflang : hrefLang -> Xml.attrib
val a_medtype : mediaType -> Xml.attrib
val a_title : ltitle -> Xml.attrib
val a_length : length -> Xml.attrib
val a_term : term -> Xml.attrib
val a_type : string -> Xml.attrib
 * }}}
 *)

(** An inline text or html content *)
val inlineC : ?meta:[> metaAttr ] list
   -> ?html:bool
   -> string list
   -> [> `Content of content ]

(** An xhtml content, embedded in a div *)
val xhtmlC : ?meta:[> metaAttr ] list
   -> ([ `PCDATA | Xhtml_types.flow ] Xhtml.M.elt list)
   -> [> `Content of content ]

(** An html5 content, embedded in a div *)
val html5C : ?meta:[> metaAttr ] list
   -> ([ `PCDATA | Html5_types.flow5 ] Eliom_content.Html5.elt list)
   -> [> `Content of content ]

(** Inline content from another kind *)
val inlineOtherC : ?meta:[> metaAttr ] list
   -> string * Xml.elt list
   -> [> `Content of content ]

(** Every other content *)
val outOfLineC : ?meta:[> metaAttr ] list
   -> string * uri
   -> [> `Content of content ]

(** Plain text construct *)
val plain : ?meta:[> metaAttr ] list
   -> ?html:bool
   -> string
   -> textConstruct

(** XHTML text construct *)
val xhtml : ?meta:[> metaAttr ] list
   -> [ `PCDATA | Xhtml_types.flow ] Xhtml.M.elt list
   -> textConstruct

(** HTML5 text construct *)
val html5 : ?meta:[> metaAttr ] list
   -> [ `PCDATA | Html5_types.flow5 ] Eliom_content.Html5.elt list
   -> textConstruct

(** Rights tag *)
val rights : textConstruct
   -> [> `Rights of textConstruct ]

(** Subtitle tag *)
val subtitle : textConstruct
   -> [> `Sub of textConstruct ]

(** Summary tag *)
val summary : textConstruct
   -> [> `Sum of textConstruct ]

(** Feed tag *)
val feed : updated:CalendarLib.Calendar.t
   -> id:uri
   -> title:textConstruct
   -> ?fields:[> feedOAttr ] list
   -> entry list
   -> feed

(** Entry tag *)
val entry :
  updated:CalendarLib.Calendar.t ->
  id:uri ->
  title:textConstruct ->
  [> entryOAttr ] list -> entry

(** Source tag *)
val source :
  updated:CalendarLib.Calendar.t ->
  id:uri ->
  title:textConstruct ->
  [> sourceOAttr ] list -> [> `Source of source ]

(** Link tag *)
val link :
  ?elt:[> linkOAttr ] list ->
  href -> link


(** We need a list of links, this is only a converter from link list to `Links
 *)
val links : link list -> [> `Links of link list ]

(** email tag *)
val email : string -> [> `Email of string ]

(** uri tag, basically, simply a converter *)
val uri : uri -> [> `Uri of uri ]

(** author tag *)
val author :
  ?elt:[> personConstruct ] list -> string -> author

(** We need a list of authors, this is only a converter from author list to
 `Authors *)
val authors : author list -> [> `Authors of author list ]

(** contributor tag *)
val contributor :
  ?elt:[> personConstruct ] list -> string -> contributor

(** We need a list of contributors, this is only a converter from contributor
 list to `Contributors *)
val contributors : contributor list -> [> `Contribs of contributor list ]

(** icon tag, basically, simply a converter  *)
val icon : uri -> [> `Icon of icon ]

(** logo tag, basically, simply a converter *)
val logo : uri -> [> `Logo of logo ]

(** category tag *)
val category :
  ?meta:[> metaAttr ] list ->
  ?scheme:scheme -> ?label:label ->
  term -> Xml.elt list -> category

(** We need a list of categories, this is only a converter from category list
 to `Categories *)
val categories : category list -> [> `Cats of category list]

(* published tag *)
val published : CalendarLib.Calendar.t -> [> `Pub of published ]

(*
 * }}}
 *)

(** Technically not used elsewhere than in eliom_feed.ml, since the links tags
 related to each hub are added when registering the feed. *)
val insert_hub_links : uri list -> feed -> feed
