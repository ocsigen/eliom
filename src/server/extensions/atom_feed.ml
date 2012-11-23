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
type uri = Xhtml.M.uri
type lang = string
type base = uri
type ncname = string
type dateConstruct = string
type emailAddress = string
type mediaType = string
type length = int
type href = Xhtml.M.uri
type hrefLang = string
type rel = string
type ltitle = string
type scheme  = string
type label = string
type term = string
type metaAttr = [ `Base of base | `Lang of lang ]
type personConstruct = [ `Uri of uri | `Email of emailAddress ]
type author = Xml.elt
type contributor = Xml.elt
type generator = Xml.elt
type id = Xml.elt
type icon = Xml.elt
type category = Xml.elt
type link = Xml.elt
type logo = Xml.elt
type published = Xml.elt
type updated = Xml.elt
type source = Xml.elt
type entry = Xml.elt
type feed = Xml.elt
type content = Xml.elt
type textConstruct = Xml.attrib list * Xml.elt list
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
let a_base = Xml.uri_attrib "base"
let a_lang = Xml.string_attrib "lang"
let a_scheme = Xml.string_attrib "scheme"
let a_label = Xml.string_attrib "label"
let a_href = Xml.uri_attrib "href"
let a_rel = Xml.string_attrib "rel"
let a_hreflang = Xml.string_attrib "hreflang"
let a_medtype = Xml.string_attrib "mediatype"
let a_title = Xml.string_attrib "title"
let a_length = Xml.int_attrib "length"
let a_term = Xml.string_attrib "term"
let a_type = Xml.string_attrib "type"
(*
 * }}}
 *)

let rec metaAttr_extract l = match l with
   | []              -> []
   | `Base a :: r    -> a_base a :: metaAttr_extract r
   | `Lang a :: r    -> a_lang a :: metaAttr_extract r | _ :: r          ->
   metaAttr_extract r

let rec c_pcdata l = match l with | [] -> [] | a::r -> Xml.pcdata a :: c_pcdata
r

let xhtmlDiv b = Xml.node ~a:[(Xml.string_attrib "xmlns"
      "http://www.w3.org/1999/xhtml")] "div" (Xhtml.M.toeltl b)

let html5Div content =
  let content = Eliom_content.Html5.F.toeltl content in
  let module Eliom_xml = Xml_iter.Make(Eliom_content_core.Xml) in
  let content =
    let f ename attibs =
      let map_attibs f =
        List.map
          (fun x ->
            let aname = Eliom_content_core.Xml.aname x in
            let acontent = Eliom_content_core.Xml.acontent x in
            f aname acontent
          )
          attibs
      in
      let reconstruct_attibs aname = function
        | Eliom_content_core.Xml.AFloat x -> Xml.float_attrib aname x
        | Eliom_content_core.Xml.AInt x -> Xml.int_attrib aname x
        | Eliom_content_core.Xml.AStr x -> Xml.string_attrib aname x
        | Eliom_content_core.Xml.AStrL (sep, x) -> match sep with
            | Eliom_content_core.Xml.Space -> Xml.space_sep_attrib aname x
            | Eliom_content_core.Xml.Comma -> Xml.comma_sep_attrib aname x
      in
      let reconstruct_url aname = function
        | Eliom_content_core.Xml.AStr url ->
            let url =
              try
                let neturl =
                  let base_syntax =
                    {Neturl.ip_url_syntax
                     with Neturl.url_enable_relative = true;
                    }
                  in
                  Neturl.parse_url ~base_syntax url
                in
                let base =
                  let base =
                    Eliom_request_info.get_original_full_path_string
                      ()
                  in
                  let base =
                    Eliom_lib.Url.remove_end_slash base
                  in
                  Neturl.parse_url (base ^ "/")
                in
                Neturl.string_of_url
                  (Neturl.ensure_absolute_url ~base neturl)
              with Neturl.Malformed_URL -> url
            in
            Xml.string_attrib aname url
        | acontent -> reconstruct_attibs aname acontent
      in
      match ename with
        | "a" ->
            map_attibs
              (fun aname acontent -> match aname with
                | "href" -> reconstruct_url aname acontent
                | _ -> reconstruct_attibs aname acontent
              )
        | "img" ->
            map_attibs
              (fun aname acontent -> match aname with
                | "src" -> reconstruct_url aname acontent
                | _ -> reconstruct_attibs aname acontent
              )
        | _ -> map_attibs reconstruct_attibs
    in
    let leaf ename attibs =
      let attibs = f ename attibs in
      Xml.leaf ~a:attibs ename
    in
    let node ename attibs elts =
      let attibs = f ename attibs in
      Xml.node ~a:attibs ename elts
    in
    List.map
      (Eliom_xml.fold
         Xml.empty
         Xml.comment
         Xml.encodedpcdata
         Xml.pcdata
         Xml.entity
         leaf
         node
      )
      content
  in
  Xml.node "div" content

let inlineC ?(meta = []) ?(html = false) c = `Content (Xml.node ~a:(a_type (if
            html then "html" else "text") :: metaAttr_extract meta) "content"
      (c_pcdata c))

let xhtmlC ?(meta = []) c = `Content (Xml.node ~a:(a_type "xhtml" ::
         metaAttr_extract meta) "content" [xhtmlDiv c])

let html5C ?(meta = []) c =
  `Content
    (Xml.node
       ~a:(a_type "html" :: metaAttr_extract meta)
       "content"
       [html5Div c]
    )

let inlineOtherC ?(meta = []) (a,b) = `Content (Xml.node ~a:(a_medtype a ::
         metaAttr_extract meta) "content" b)

let outOfLineC ?(meta = []) (a,b) = `Content (Xml.node ~a:(a_medtype a ::
         Xml.uri_attrib "src" b :: metaAttr_extract meta) "content" [])

(*
 * Extraction functions {{{
 *)
let rec personConstruct_extract l = match l with
   | []              -> []
   |`Email a :: r   -> Xml.node ~a:[] "email" [(Xml.pcdata a)] ::
      personConstruct_extract r
   | `Uri a :: r     -> Xml.node ~a:[] "uri" [(Xml.pcdata (Xml.string_of_uri a))] ::
      personConstruct_extract r
   | _ :: r          -> personConstruct_extract r

let rec linkOAttr_extract l = match l with
   | []              -> []
   | `Type a :: r    -> Xml.string_attrib "type" a :: linkOAttr_extract r
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
   | `Rights (a,b) :: r -> Xml.node ~a "rights" b :: sourceOAttr_extract r
   | `Sub (a,b) :: r    -> Xml.node ~a "subtitle" b :: sourceOAttr_extract r
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
   | `Rights (a,b) :: r -> Xml.node ~a "rights" b :: entryOAttr_extract r
   | `Sum (a,b) :: r    -> Xml.node ~a "summary" b :: entryOAttr_extract r
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
   | `Rights (a,b) :: r -> Xml.node ~a "rights" b :: feedOAttr_extract r
   | `Sub (a,b) :: r    -> Xml.node ~a "subtitle" b :: feedOAttr_extract r
   | _ :: r          -> feedOAttr_extract r
 (*
 * }}}
 *)

(*
 * Textconstructs [Rights, Subtitle, Summary, Title] {{{
 *)
let plain ?(meta = []) ?(html = false) content = (Xml.string_attrib "type"
    (if html then "html" else "text"):: metaAttr_extract meta, [Xml.pcdata
    content])

let xhtml ?(meta = []) content = (Xml.string_attrib "type" "xhtml" ::
      metaAttr_extract meta, [xhtmlDiv content])

let html5 ?(meta = []) content =
  (Xml.string_attrib "type" "html" :: metaAttr_extract meta,
   [html5Div content]
  )

let rights t = `Rights t

let subtitle t = `Sub t

let summary t = `Sum t
 (*
 * }}}
 *)

let feed ~updated ~id ~title:(a,b) ?(fields = []) entries =
   Xml.node ~a:(Xml.string_attrib "xmlns" "http://www.w3.org/2005/Atom" ::
         metaAttr_extract fields)
         "feed"
         (Xml.node ~a:[] "updated" [ Xml.pcdata (date updated) ] ::
            Xml.node ~a:[] "id" [ Xml.pcdata (Xml.string_of_uri id) ] :: Xml.node ~a "title" b ::
            feedOAttr_extract fields @ entries)

let entry ~updated ~id ~title:(a,b) elt =
   Xml.node ~a:(metaAttr_extract elt)
         "entry"
         (Xml.node ~a:[] "updated" [ Xml.pcdata (date updated) ] ::
            Xml.node ~a:[] "id" [ Xml.pcdata (Xml.string_of_uri id) ] ::
            Xml.node ~a "title" b ::
            entryOAttr_extract elt)

let source ~updated ~id ~title:(a,b) elt = `Source (
   Xml.node ~a:(metaAttr_extract elt)
         "source"
         (Xml.node ~a:[] "updated" [ Xml.pcdata (date updated) ] ::
            Xml.node ~a:[] "id" [ Xml.pcdata (Xml.string_of_uri id) ] ::
	       Xml.node ~a "title" b :: sourceOAttr_extract elt)
	 )

let link ?(elt = []) href = Xml.leaf ~a:(a_href href :: (linkOAttr_extract elt)
      @ (metaAttr_extract elt)) "link"

let links l = `Links l

let email s = `Email s

let uri s = `Uri s

let author ?(elt = []) name = Xml.node ~a:[] "author" (Xml.node ~a:[] "name"
      [Xml.pcdata name] :: personConstruct_extract elt)

let authors l = `Authors l

let contributor ?(elt = []) name = Xml.node ~a:[] "contributor" (Xml.node ~a:[]
      "name" [Xml.pcdata name] :: personConstruct_extract elt)

let contributors l = `Contribs l

let icon address = `Icon (Xml.node ~a:[] "icon" [ Xml.pcdata (Xml.string_of_uri address) ])

let logo address = `Logo (Xml.node ~a:[] "icon" [ Xml.pcdata (Xml.string_of_uri address) ])

let category ?(meta = []) ?(scheme = "") ?(label = "") term content =
   Xml.node ~a:(a_scheme scheme :: a_label label ::
               a_term term :: metaAttr_extract meta)
         "category"
         content

let categories l = `Cats l

let published d = `Pub (Xml.node ~a:[] "published" [ Xml.pcdata (date d) ])

(*
 * }}}
 *)

let insert_hub_links hubs feed = match Xml.content feed with
   | Xml.Node (b, a, c)  -> Xml.node ~a b (List.map
         (fun uri -> link ~elt:[`Rel ("hub")] uri) hubs @ c) | _ -> assert false
