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

open Eliom_pervasives
open Uri

(*
 * types {{{
 *)
type uri = Uri.uri
type lang = string
type base = uri
type ncname = string
type dateConstruct = string
type emailAddress = string
type mediaType = string
type length = int
type href = string
type hrefLang = string
type rel = string
type ltitle = string
type scheme  = string
type label = string
type term = string
type metaAttr = [ `Base of base | `Lang of lang ]
type personConstruct = [ `Uri of uri | `Email of emailAddress ]
type author = XML.elt
type contributor = XML.elt
type generator = XML.elt
type id = XML.elt
type icon = XML.elt
type category = XML.elt
type link = XML.elt
type logo = XML.elt
type published = XML.elt
type updated = XML.elt
type source = XML.elt
type entry = XML.elt
type feed = XML.elt
type content = XML.elt
type textConstruct = XML.attrib list * XML.elt list
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
let a_base = XML.string_attrib "base"
let a_lang = XML.string_attrib "lang"
let a_scheme = XML.string_attrib "scheme"
let a_label = XML.string_attrib "label"
let a_href s = XML.string_attrib "href" (string_of_uri s)
let a_rel = XML.string_attrib "rel"
let a_hreflang = XML.string_attrib "hreflang"
let a_medtype = XML.string_attrib "mediatype"
let a_title = XML.string_attrib "title"
let a_length = XML.int_attrib "length"
let a_term = XML.string_attrib "term"
let a_type = XML.string_attrib "type"
(*
 * }}}
 *)

let rec metaAttr_extract l = match l with
   | []              -> []
   | `Base a :: r    -> a_base a :: metaAttr_extract r
   | `Lang a :: r    -> a_lang a :: metaAttr_extract r | _ :: r          ->
   metaAttr_extract r

let rec c_pcdata l = match l with | [] -> [] | a::r -> XML.pcdata a :: c_pcdata
r

let xhtmlDiv b = XML.node ~a:[(XML.string_attrib "xmlns"
      "http://www.w3.org/1999/xhtml")] "div" (XHTML.M.toeltl b)

let inlineC ?(meta = []) ?(html = false) c = `Content (XML.node ~a:(a_type (if
            html then "html" else "text") :: metaAttr_extract meta) "content"
      (c_pcdata c))

let xhtmlC ?(meta = []) c = `Content (XML.node ~a:(a_type "xhtml" ::
         metaAttr_extract meta) "content" [xhtmlDiv c])

let inlineOtherC ?(meta = []) (a,b) = `Content (XML.node ~a:(a_medtype a ::
         metaAttr_extract meta) "content" b)

let outOfLineC ?(meta = []) (a,b) = `Content (XML.node ~a:(a_medtype a ::
         XML.string_attrib "src" b :: metaAttr_extract meta) "content" [])

(*
 * Extraction functions {{{
 *) 
let rec personConstruct_extract l = match l with 
   | []              -> [] 
   |`Email a :: r   -> XML.node ~a:[] "email" [(XML.pcdata a)] :: 
      personConstruct_extract r 
   | `Uri a :: r     -> XML.node ~a:[] "uri" [(XML.pcdata a)] :: 
      personConstruct_extract r 
   | _ :: r          -> personConstruct_extract r

let rec linkOAttr_extract l = match l with 
   | []              -> [] 
   | `Type a :: r    -> XML.string_attrib "type" a :: linkOAttr_extract r 
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
   | `Rights (a,b) :: r -> XML.node ~a "rights" b :: sourceOAttr_extract r 
   | `Sub (a,b) :: r    -> XML.node ~a "subtitle" b :: sourceOAttr_extract r 
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
   | `Rights (a,b) :: r -> XML.node ~a "rights" b :: entryOAttr_extract r 
   | `Sum (a,b) :: r    -> XML.node ~a "summary" b :: entryOAttr_extract r 
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
   | `Rights (a,b) :: r -> XML.node ~a "rights" b :: feedOAttr_extract r 
   | `Sub (a,b) :: r    -> XML.node ~a "subtitle" b :: feedOAttr_extract r 
   | _ :: r          -> feedOAttr_extract r 
 (*
 * }}}
 *)

(*
 * Textconstructs [Rights, Subtitle, Summary, Title] {{{
 *) 
let plain ?(meta = []) ?(html = false) content = (XML.string_attrib "type"
    (if html then "html" else "text"):: metaAttr_extract meta, [XML.pcdata
    content])

let xhtml ?(meta = []) content = (XML.string_attrib "type" "xhtml" ::
      metaAttr_extract meta, [xhtmlDiv content])

let rights t = `Rights t

let subtitle t = `Sub t

let summary t = `Sum t 
 (*
 * }}}
 *)

let feed ~updated ~id ~title:(a,b) ?(fields = []) entries = 
   XML.node ~a:(XML.string_attrib "xmlns" "http://www.w3.org/2005/Atom" :: 
         metaAttr_extract fields) 
         "feed" 
         (XML.node ~a:[] "updated" [ XML.pcdata (date updated) ] ::
            XML.node ~a:[] "id" [ XML.pcdata id ] :: XML.node ~a "title" b ::
            feedOAttr_extract fields @ entries)

let entry ~updated ~id ~title:(a,b) elt = 
   XML.node ~a:(metaAttr_extract elt)
         "entry"  
         (XML.node ~a:[] "updated" [ XML.pcdata (date updated) ] :: XML.node
            ~a:[] "id" [ XML.pcdata id ] :: XML.node ~a "title" b ::
            entryOAttr_extract elt)

let source ~updated ~id ~title:(a,b) elt = `Source (
   XML.node ~a:(metaAttr_extract elt) 
         "source" 
         (XML.node ~a:[] "updated" [ XML.pcdata (date updated) ] :: 
            XML.node ~a:[] "id" [ XML.pcdata id ] :: XML.node
            ~a "title" b :: sourceOAttr_extract elt)
   )

let link ?(elt = []) href = XML.leaf ~a:(a_href href :: (linkOAttr_extract elt)
      @ (metaAttr_extract elt)) "link"

let links l = `Links l

let email s = `Email s

let uri s = `Uri s

let author ?(elt = []) name = XML.node ~a:[] "author" (XML.node ~a:[] "name"
      [XML.pcdata name] :: personConstruct_extract elt)

let authors l = `Authors l

let contributor ?(elt = []) name = XML.node ~a:[] "contributor" (XML.node ~a:[]
      "name" [XML.pcdata name] :: personConstruct_extract elt)

let contributors l = `Contribs l

let icon address = `Icon (XML.node ~a:[] "icon" [ XML.pcdata address ])

let logo address = `Logo (XML.node ~a:[] "logo" [ XML.pcdata address ])

let category ?(meta = []) ?(scheme = "") ?(label = "") term content = 
   XML.node ~a:(a_scheme scheme :: a_label label :: 
               a_term term :: metaAttr_extract meta)
         "category" 
         content

let categories l = `Cats l

let published d = `Pub (XML.node ~a:[] "published" [ XML.pcdata (date d) ])

(*
 * }}}
 *)

let insert_hub_links hubs feed = match XML.content feed with 
   | XML.Node (b, a, c)  -> XML.node ~a b (List.map 
         (fun uri -> link ~elt:[`Rel ("hub")] uri) hubs @ c) | _ -> assert false
