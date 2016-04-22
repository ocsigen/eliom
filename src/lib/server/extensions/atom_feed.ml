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

open Eliom_lib

(*
 * types {{{
 *)
type uri = Tyxml_xml.uri
type lang = string
type base = uri
type ncname = string
type dateConstruct = string
type emailAddress = string
type mediaType = string
type length = int
type href = Tyxml_xml.uri
type hrefLang = string
type rel = string
type ltitle = string
type scheme  = string
type label = string
type term = string
type metaAttr = [ `Base of base | `Lang of lang ]
type personConstruct = [ `Uri of uri | `Email of emailAddress ]
type author = Tyxml_xml.elt
type contributor = Tyxml_xml.elt
type generator = Tyxml_xml.elt
type id = Tyxml_xml.elt
type icon = Tyxml_xml.elt
type category = Tyxml_xml.elt
type link = Tyxml_xml.elt
type logo = Tyxml_xml.elt
type published = Tyxml_xml.elt
type updated = Tyxml_xml.elt
type source = Tyxml_xml.elt
type entry = Tyxml_xml.elt
type feed = Tyxml_xml.elt
type content = Tyxml_xml.elt
type textConstruct = Tyxml_xml.attrib list * Tyxml_xml.elt list
type linkOAttr = [ metaAttr
   | `Type of string
   | `Rel of rel
   | `Medtype of mediaType
   | `Hrefl of hrefLang
   | `Title of ltitle
   | `Length of length ]
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

let date d = CalendarLib.Printer.Calendar.sprint "%iT%TZ" d
let xml_of_feed f = f

(*
 * attr converters {{{
 *)
let a_base = Tyxml_xml.uri_attrib "base"
let a_lang = Tyxml_xml.string_attrib "lang"
let a_scheme = Tyxml_xml.string_attrib "scheme"
let a_label = Tyxml_xml.string_attrib "label"
let a_href = Tyxml_xml.uri_attrib "href"
let a_rel = Tyxml_xml.string_attrib "rel"
let a_hreflang = Tyxml_xml.string_attrib "hreflang"
let a_medtype = Tyxml_xml.string_attrib "mediatype"
let a_title = Tyxml_xml.string_attrib "title"
let a_length = Tyxml_xml.int_attrib "length"
let a_term = Tyxml_xml.string_attrib "term"
let a_type = Tyxml_xml.string_attrib "type"
(*
 * }}}
 *)

let rec metaAttr_extract l = match l with
   | []              -> []
   | `Base a :: r    -> a_base a :: metaAttr_extract r
   | `Lang a :: r    -> a_lang a :: metaAttr_extract r | _ :: r          ->
   metaAttr_extract r

let rec c_pcdata l = match l with | [] -> [] | a::r -> Tyxml_xml.pcdata a :: c_pcdata
r

let print_html5 l =
  let buffer = Buffer.create 500 in
  let output = Buffer.add_string buffer in
  let encode x = fst (Xml_print.Utf8.normalize_html x) in
  Eliom_content.Html.Printer.print_list ~encode ~output l;
  Buffer.contents buffer

let inlineC ?(meta = []) ?(html = false) c = `Content (Tyxml_xml.node ~a:(a_type (if
            html then "html" else "text") :: metaAttr_extract meta) "content"
      (c_pcdata c))

let html5C ?meta c =
  inlineC ?meta ~html:true [print_html5 [Eliom_content.Html.F.div c]]

let inlineOtherC ?(meta = []) (a,b) = `Content (Tyxml_xml.node ~a:(a_medtype a ::
         metaAttr_extract meta) "content" b)

let outOfLineC ?(meta = []) (a,b) = `Content (Tyxml_xml.node ~a:(a_medtype a ::
         Tyxml_xml.uri_attrib "src" b :: metaAttr_extract meta) "content" [])

(*
 * Extraction functions {{{
 *)
let rec personConstruct_extract l = match l with
   | []              -> []
   |`Email a :: r   -> Tyxml_xml.node ~a:[] "email" [(Tyxml_xml.pcdata a)] ::
      personConstruct_extract r
   | `Uri a :: r     -> Tyxml_xml.node ~a:[] "uri" [(Tyxml_xml.pcdata (Tyxml_xml.string_of_uri a))] ::
      personConstruct_extract r
   | _ :: r          -> personConstruct_extract r

let rec linkOAttr_extract l = match l with
   | []              -> []
   | `Type a :: r    -> Tyxml_xml.string_attrib "type" a :: linkOAttr_extract r
   | `Rel a :: r     -> a_rel a :: linkOAttr_extract r
   | `Medtype a :: r -> a_medtype a :: linkOAttr_extract r
   | `Hrefl a :: r   -> a_hreflang a :: linkOAttr_extract r
   | `Title a :: r   -> a_title a :: linkOAttr_extract r
   | `Length a :: r  -> a_length a :: linkOAttr_extract r
   | _ :: r          -> linkOAttr_extract r

let rec sourceOAttr_extract l = match l with
   | []                 -> []
   | `Authors a :: r
   | `Cats a :: r
   | `Contribs a :: r
   | `Links a :: r      -> a @ sourceOAttr_extract r
   | `Gen a :: r
   | `Icon a :: r
   | `Logo a :: r       -> a :: sourceOAttr_extract r
   | `Rights (a,b) :: r -> Tyxml_xml.node ~a "rights" b :: sourceOAttr_extract r
   | `Sub (a,b) :: r    -> Tyxml_xml.node ~a "subtitle" b :: sourceOAttr_extract r
   | _ :: r             -> sourceOAttr_extract r

let rec entryOAttr_extract l = match l with
   | []                 -> []
   | `Authors a :: r
   | `Cats a :: r
   | `Contribs a :: r
   | `Links a :: r      -> a @ entryOAttr_extract r
   | `Content a :: r
   | `Pub a :: r
   | `Source a :: r     -> a :: entryOAttr_extract r
   | `Rights (a,b) :: r -> Tyxml_xml.node ~a "rights" b :: entryOAttr_extract r
   | `Sum (a,b) :: r    -> Tyxml_xml.node ~a "summary" b :: entryOAttr_extract r
   | _ :: r             -> entryOAttr_extract r

let rec feedOAttr_extract l = match l with
   | []                 -> []
   | `Authors a :: r
   | `Cats a :: r
   | `Contribs a :: r
   | `Links a :: r      -> a @ feedOAttr_extract r
   | `Gen a :: r
   | `Icon a :: r
   | `Logo a :: r       -> a :: feedOAttr_extract r
   | `Rights (a,b) :: r -> Tyxml_xml.node ~a "rights" b :: feedOAttr_extract r
   | `Sub (a,b) :: r    -> Tyxml_xml.node ~a "subtitle" b :: feedOAttr_extract r
   | _ :: r          -> feedOAttr_extract r
 (*
 * }}}
 *)

(*
 * Textconstructs [Rights, Subtitle, Summary, Title] {{{
 *)
let plain ?(meta = []) ?(html = false) content = (Tyxml_xml.string_attrib "type"
    (if html then "html" else "text"):: metaAttr_extract meta, [Tyxml_xml.pcdata
    content])

let html5 ?meta content =
  plain ?meta ~html:true (print_html5 content)

let rights t = `Rights t

let subtitle t = `Sub t

let summary t = `Sum t
 (*
 * }}}
 *)

let feed ~updated ~id ~title:(a,b) ?(fields = []) entries =
   Tyxml_xml.node ~a:(Tyxml_xml.string_attrib "xmlns" "http://www.w3.org/2005/Atom" ::
         metaAttr_extract fields)
         "feed"
         (Tyxml_xml.node ~a:[] "updated" [ Tyxml_xml.pcdata (date updated) ] ::
            Tyxml_xml.node ~a:[] "id" [ Tyxml_xml.pcdata (Tyxml_xml.string_of_uri id) ] :: Tyxml_xml.node ~a "title" b ::
            feedOAttr_extract fields @ entries)

let entry ~updated ~id ~title:(a,b) elt =
   Tyxml_xml.node ~a:(metaAttr_extract elt)
         "entry"
         (Tyxml_xml.node ~a:[] "updated" [ Tyxml_xml.pcdata (date updated) ] ::
            Tyxml_xml.node ~a:[] "id" [ Tyxml_xml.pcdata (Tyxml_xml.string_of_uri id) ] ::
            Tyxml_xml.node ~a "title" b ::
            entryOAttr_extract elt)

let source ~updated ~id ~title:(a,b) elt = `Source (
   Tyxml_xml.node ~a:(metaAttr_extract elt)
         "source"
         (Tyxml_xml.node ~a:[] "updated" [ Tyxml_xml.pcdata (date updated) ] ::
            Tyxml_xml.node ~a:[] "id" [ Tyxml_xml.pcdata (Tyxml_xml.string_of_uri id) ] ::
	       Tyxml_xml.node ~a "title" b :: sourceOAttr_extract elt)
	 )

let link ?(elt = []) href = Tyxml_xml.leaf ~a:(a_href href :: (linkOAttr_extract elt)
      @ (metaAttr_extract elt)) "link"

let links l = `Links l

let email s = `Email s

let uri s = `Uri s

let author ?(elt = []) name = Tyxml_xml.node ~a:[] "author" (Tyxml_xml.node ~a:[] "name"
      [Tyxml_xml.pcdata name] :: personConstruct_extract elt)

let authors l = `Authors l

let contributor ?(elt = []) name = Tyxml_xml.node ~a:[] "contributor" (Tyxml_xml.node ~a:[]
      "name" [Tyxml_xml.pcdata name] :: personConstruct_extract elt)

let contributors l = `Contribs l

let icon address = `Icon (Tyxml_xml.node ~a:[] "icon" [ Tyxml_xml.pcdata (Tyxml_xml.string_of_uri address) ])

let logo address = `Logo (Tyxml_xml.node ~a:[] "icon" [ Tyxml_xml.pcdata (Tyxml_xml.string_of_uri address) ])

let category ?(meta = []) ?(scheme = "") ?(label = "") term content =
   Tyxml_xml.node ~a:(a_scheme scheme :: a_label label ::
               a_term term :: metaAttr_extract meta)
         "category"
         content

let categories l = `Cats l

let published d = `Pub (Tyxml_xml.node ~a:[] "published" [ Tyxml_xml.pcdata (date d) ])

(*
 * }}}
 *)

let insert_hub_links hubs feed = match Tyxml_xml.content feed with
   | Tyxml_xml.Node (b, a, c)  -> Tyxml_xml.node ~a b (List.map
         (fun uri -> link ~elt:[`Rel ("hub")] uri) hubs @ c) | _ -> assert false
