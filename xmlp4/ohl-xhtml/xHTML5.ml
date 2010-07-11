(* BEGIN INTERFACE *)
(*
   Copyright (C) 2004 by Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
   Copyright (C) 2007 by Vincent Balat, Gabriel Kerneis, CNRS, Université Paris Diderot
   Copyright (C) 2010 by Cecile Herbelin, CNRS, Université Paris Diderot

   xHTML5.ml is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   xHTML5.ml is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  *)

(* I had based on the file xHTML.ml to create xHTML5.ml
*)

(* to do :
   attributes srcdoc, script-documentation, input_value
   module MathML and Svg
   element noscript may be transparent
*)

(* IDEAS:

     The [a_] prefix would have to be maintained and the
     only advantage are a potentially better mapping of the XHTML modularization
     to O'Caml modules. *)
(** Typesafe constructors for HTML5 documents.
    @see <http://www.w3.org/TR/html5/> W3C Recommendation *)

module type T =
  sig

(** The elements, attributes, attribute types and data types are given names
    that match the names in the W3C recommendation as closely as allowed by
    a strict typing discipline and the lexical conventions of O'Caml:
    {ul
      {- {e elements} are implemented as O'Caml constructors with the same name as
         in the W3C recommendation.  The domain and codomain are specified as ['a elt],
         where ['a] is a concrete phantom type build out of polymorphic variants.}
      {- {e attributes} are implemented as O'Caml constructors with [a_] prefixed to the
         name.  The name is the same as in the W3C recommendation }
      {- {e attribute types} are implemented as O'Caml types that all have the same names
         as in the W3C recommendation, but are all lowercase.}
      {- {e data types} are also implemented as O'Caml types that all have the same names
         as in the W3C recommendation and are again all lowercase.}}

    Finite sets of alternatives are mapped to polymorphic variants.

    The phantom type is always the {e most general} required by any (supported)
    version of the standard.  Type discipline is enforced by exporting or not-exporting
    the corresponding constructor.  *)

(**  {1 Attribute Types} *)

    type cdata = string
(** Character data *)

    type id = string
(** A document-unique identifier *)

    type idref = string
(** A reference to a document-unique identifier *)

    type idrefs = idref list
(** A space-separated list of references to document-unique identifiers *)

    type name = string
(** A name with the same character constraints as ID above *)

    type nmtoken = string
(** A name composed of only name tokens as defined in XML 1.0
    @see <http://www.w3.org/TR/2000/REC-xml-20001006> XML 1.0 *)

    type nmtokens = nmtoken list
(** One or more white space separated NMTOKEN values *)

    type pcdata = string
(** Processed character data *)

(** {2 Data Types} *)

    type character = char
(** A single character from ISO 10646. *)

    type charset = string
(** A character encoding, as per RFC2045 (MIME).
    @see <http://www.ietf.org/rfc/rfc2045.txt> RFC2045 *)

    type charsets = charset list
(** A space-separated list of character encodings, as per RFC2045 (MIME).
    @see <http://www.ietf.org/rfc/rfc2045.txt> RFC2045 *)

    type contenttype = string
(** A media type, as per RFC2045 (MIME).
    @see <http://www.ietf.org/rfc/rfc2045.txt> RFC2045 *)

    type contenttypes = contenttype list
(** A comma-separated list of media types, as per RFC2045 (MIME).
    @see <http://www.ietf.org/rfc/rfc2045.txt> RFC2045 *)

    type coords = string list
(** Comma- separated list of coordinates to use in defining areas. *)

    type datetime = string
(** Date and time information. *)

    type fpi = string
(** A character string representing an SGML Formal Public Identifier. *)

    type frametarget = string
(** Frame name used as destination for results of certain actions. *)

    type languagecode = string
(** A language code, as per RFC3066.
    @see <http://www.ietf.org/rfc/rfc3066.txt> RFC3066 *)

    type length = [ `Pixels of int | `Percent of int ]
(** The value may be either in pixels or a percentage of the available
    horizontal or vertical space. Thus, the value [`Percent 50] means half of
    the available space. *)

    type linktypes =
        [`Alternate | `Archives |`Author | `Bookmark | `External
        | `First | `Help |`Icon | `Index |`Last |`License | `Next 
        |`Nofollow |`Noreferrer |`Pingback |`Prefetch | `Prev
        | `Search | `Stylesheet | `Sidebar |`Tag | `Up] list

(** Authors may use the following recognized link types, listed here with
    their conventional interpretations. A LinkTypes value refers to a
    space-separated list of link types. White space characters are not
    permitted within link types.  These link types are case-insensitive, i.e.,
    ["Alternate"] has the same meaning as ["alternate"].

    User agents, search engines, etc. may interpret these link types in a
    variety of ways. For example, user agents may provide access to linked
    documents through a navigation bar.

    {ul
      {- [`Alternate]:
         Gives alternate representations of the current document.}
      {- [`Archives]:
         Provides a link to a collection of records, documents, or other materials of historical interest.}
      {- [`Author]:
         Gives a link to the current document's author.}
      {- [`Bookmark]:
         Gives the permalink for the nearest ancestor section.}
      {- [`External]:
         Indicates that the referenced document is not part of the same site as the current document.}
      {- [`First]:
         Indicates that the current document is a part of a series, and that the first document in the series is the referenced document.}
      {- [`Help]:
         Provides a link to context-sensitive help.}
      {- [`Icon]:
         Imports an icon to represent the current document.}
      {- [`Index]:
         Gives a link to the document that provides a table of contents or index listing the current document.}
      {- [`Last]:
         Indicates that the current document is a part of a series, and that the last document in the series is the referenced document.}
      {- [`Licence]:
         Indicates that the main content of the current document is covered by the copyright license described by the referenced document.}
      {- [`Next]:
         Indicates that the current document is a part of a series, and that the next document in the series is the referenced document.}
      {- [`Nofollow]:
         Indicates that the current document's original author or publisher does not endorse the referenced document.}
      {- [`Noreferrer]:
         Requires that the user agent not send an HTTP Referer (sic) header if the user follows the hyperlink.}
      {- [`Pingback]:
         Gives the address of the pingback server that handles pingbacks to the current document.}
      {- [`Prefetch]:
         Specifies that the target resource should be preemptively cached.}
      {- [`Prev]:
         Indicates that the current document is a part of a series, and that the previous document in the series is the referenced document.}
      {- [`Search]:
         Gives a link to a resource that can be used to search through the current document and its related pages.}
      {- [`Stylesheet]:
         Imports a stylesheet.}
      {- [`Sidebar]:
         Specifies that the referenced document, if retrieved, is intended to be shown in the browser's sidebar (if it has one).}
      {- [`Tag]:
         Gives a tag (identified by the given address) that applies to the current document.}
      {- [`Up]:
         Provides a link to a document giving the context for the current document.}
         } *)

    type mediadesc =
        [ `All | `Aural | `Braille | `Embossed | `Handheld | `Print
        | `Projection | `Screen | `Speech | `TTY | `TV ] list 
(** The MediaDesc attribute is a comma-separated list of media descriptors.
    The following is a list of recognized media descriptors:
    {ul
      {- [`Screen]:
         For non-paged computer screens.}
      {- [`TTY]:
         For media using a fixed-pitch character grid (like teletypes, terminals, or devices with limited display capabilities).}
      {- [`TV]:
         For TV-type devices (low resolution, limited scrollability).}
      {- [`Projection]:
         For projectors.}
      {- [`Handheld]:
         For handheld devices (small screen, limited bandwidth).}
      {- [`Print]:
         For paged and for documents viewed on screen in print preview mode.}
      {- [`Braille]:
         For braille tactile feedback devices.}
      {- [`Aural]:
         For speech synthesizers.}}
      {- [`All]:
         For all devices.}}

    {ol
      {- The value is a comma-separated list of entries. For example,
         [media="screen, 3d-glasses, print and resolution > 90dpi"]
         is mapped to: ["screen"], ["3d-glasses"],
         ["print and resolution > 90dpi"].}
      {- Each entry is truncated just before the first character that
         isn't a US ASCII letter [\[a-zA-Z\]] (ISO 10646 hex 41-5a,
         61-7a), digit [\[0-9\]] (hex 30-39), or hyphen-minus (hex 2d).
         In the example, this gives: ["screen"], ["3d-glasses"], ["print"].}
      {- A case-insensitive match is then made with the set of media
         types defined above. User agents may ignore entries that
         don't match.  In the example we are left with ["screen"] and
         ["print"].}}

    Note. Style sheets may include media-dependent variations within them
    (e.g., the [CSS \@media] construct). In such cases it may be appropriate
    to use ["media=all"]. *)

    type multilength = [ length | `Relative of int ]
(** The value may be a Length or a relative length. A relative length
    has the form ["i*"], where ["i"] is an integer. When allotting space
    among elements competing for that space, user agents allot pixel
    and percentage lengths first, then divide up remaining available
    space among relative lengths. Each relative length receives a
    portion of the available space that is proportional to the integer
    preceding the ["*"]. The value ["*"] is equivalent to ["1*"]. Thus, if
    60 pixels of space are available after the user agent allots pixel
    and percentage space, and the competing relative lengths are ["1*"],
    ["2*"], and ["3*"], the ["1*"] will be allotted 10 pixels, the ["2*"] will be
    allotted 20 pixels, and the ["3*"] will be allotted 30 pixels. *)

    type multilengths = multilength list (* comma-separated *)
(** A comma separated list of items of type MultiLength. *)

    type number = int
    type numbers = number list (* space-separated *)

    type float_number = float

    type pixels = int

(** The value is an integer that represents the number of pixels of
    the canvas (screen, paper). Thus, the value ["50"] means fifty
    pixels. For normative information about the definition of a pixel,
    please consult CSS2.
    @see <http://www.w3.org/TR/1998/REC-CSS2-19980512> CSS2 *)

    type script = string
(** Script data can be the content of the ["script"] element and the
    value of intrinsic event attributes. User agents must not evaluate
    script data as HTML markup but instead must pass it on as data to a
    script engine.

    The case-sensitivity of script data depends on the scripting
    language.

    Please note that script data that is element content may not
    contain character references, but script data that is the value of
    an attribute may contain them. *)

    type text = string
(** Arbitrary textual data, likely meant to be human-readable. *)

    type uri (* I abstract this for Ocsigen -- VB *)
    val uri_of_string : string -> uri
    val string_of_uri : uri -> string
(** A Uniform Resource Identifier, as per RFC2396.
    @see <http://www.ietf.org/rfc/rfc2396.txt> RFC2396 *)

    type uris = uri
(** A space-separated list of Uniform Resource Identifiers, as per RFC2396.
    @see <http://www.ietf.org/rfc/rfc2396.txt> RFC2396 *)


(** {1 Common Attributes} *)

    type +'a attrib
    type +'a attribs
    val to_xmlattribs : 'a attrib list -> XML.attrib list (* VB *)

    (** ['a] is known as a {i phantom type}.  The implementation is
       actually monomorphic (the different element types are distinguished
       by a homogeneous variable, such as their textual representation)
       and the type variable [`a] is just used by the type checker.

       NB: It might be possible to use polymorphic variants directly, without
       phantom types, but the implementation is likely to be more involved. *)

(** {2 Core} *)
    type i18n = [ `XML_lang ]

    type core = [ `Accesskey | `Class | `Contenteditable | `Contextmenu | `Dir | `Draggable | `Hidden | `Id | i18n | `Spellcheck | `Style_Attr | `Tabindex| `Title | `User_data]

   val a_autocomplete : [< `On | `Off ] -> [>`Autocomplete] attrib
   val a_async : [< `Async ] -> [>`Async] attrib
   val a_autofocus : [< `Autofocus ] -> [>`Autofocus] attrib 
   val a_autoplay : [< `Autoplay ] -> [>`Autoplay] attrib
   val a_challenge : text -> [>`Challenge] attrib
   val a_contenteditable : [< `True | `False ] -> [>`Contexteditable] attrib
   val a_contextmenu : idref -> [>`Contextmenu] attrib
   val a_controls : [< `Controls ] -> [>`Controls] attrib
   val a_dir : [< `Rtl | `Ltr ] -> [>`Dir] attrib
   val a_draggable : [< `True | `False ] -> [>`Draggable] attrib
   val a_form : idref -> [>`Form] attrib
   val a_formaction : uri -> [>`Formaction] attrib
   val a_formenctype : contenttype -> [>`Formenctype] attrib
   val a_formmethod : [< `Get | `Post | `Put| `Delete] -> [>`Formmethod] attrib
   val a_formnovalidate : [< `Formnovalidate ] -> [>`Formnovalidate] attrib
   val a_formtarget : text -> [>`Formtarget] attrib
   val a_hidden : [< `Hidden ] -> [>`Hidden] attrib
   val a_high : float_number -> [>`High] attrib
   val a_icon : uri -> [>`Icon] attrib
   val a_ismap : [< `Ismap ] -> [>`Ismap] attrib
   val a_keytype : text -> [>`Keytype] attrib
   val a_list : idref -> [>`List] attrib
   val a_loop : [< `Loop ] -> [>`Loop] attrib
   val a_low : float_number -> [>`High] attrib
   val a_max : float_number -> [>`Max] attrib
   val a_input_max : number  -> [>`Max] attrib
   val a_min : float_number -> [>`Min] attrib
   val a_input_min : number  -> [>`Min] attrib
   val a_novalidate : [< `Novalidate ] -> [>`Novalidate] attrib
   val a_open : [< `Open ] -> [>`Open] attrib
   val a_optimum : float_number -> [>`Optimum] attrib
   val a_pattern : text -> [>`Pattern] attrib
   val a_placeholder : text -> [>`Placeholder] attrib
   val a_poster : uri -> [>`Poster] attrib
   val a_preload : [< `None | `Metadata | `Audio ] -> [>`Preload] attrib
   val a_pubdate : [< `Pubdate ] -> [>`Pubdate] attrib
   val a_radiogroup : text -> [>`Radiogroup] attrib
   val a_required : [< `Required ] -> [>`Required] attrib
   val a_reserved : [< `Reserved ] -> [>`Reserved] attrib
   val a_sandbox : ([< `AllowSameOrigin | `AllowForms | `AllowScript] list) -> [>`Sandbox] attrib
   val a_spellcheck : [< `True | `False ] -> [>`Spellcheck] attrib
   val a_scoped : [< `Scoped ] -> [>`Scoped] attrib
   val a_seamless : [< `Seamless ] -> [>`Seamless] attrib
   val a_sizes : numbers -> [>`Sizes] attrib
   val a_span : number -> [>`Span] attrib
   (*val a_srcdoc*)
   val a_srclang : nmtoken -> [>`XML_lang] attrib
   val a_start : number -> [>`Start] attrib
   val a_step : float_number -> [>`Step] attrib
   val a_wrap : [< `Soft | `Hard ] ->
      [>`Wrap] attrib

    val a_class : nmtokens -> [>`Class] attrib
(** This attribute assigns a class name or set of class names to an
    element. Any number of elements may be assigned the same class
    name or names.  *)

   val a_user_data : nmtoken -> text -> [> `User_data] attrib
(** May be used to specify custom attribs.
    The example given by the W3C is as follows :
    <ol>
    <li data-length="2m11s">Beyond The Sea</li>
    </ol>
    It should be used for preprocessing ends only. *)

    val a_id : text -> [>`Id] attrib
(** This attribute assigns a name to an element. This name must be
    unique in a document. The text should be without any space. *)

    val a_title : text -> [>`Title] attrib
(** This attribute offers advisory information about the element for
    which it is set. *)

(** Values of the title attribute may be rendered by user agents in a
    variety of ways. For instance, visual browsers frequently display
    the title as a {i tool tip} (a short message that appears when the
    pointing device pauses over an object). Audio user agents may
    speak the title information in a similar context.  *)

(** The title attribute has an additional role when used with the [link]
    element to designate an external style sheet. Please consult the
    section on links and style sheets for details.  *)

(** {2 I18N} *)

    val a_xml_lang : nmtoken -> [>`XML_lang] attrib

(** {2 Events} *)

(** Javascript events *)

    type events = [ `OnAbort|`OnBlur | `OnCanPlay |`OnCanPlayThrough
    |`OnChange | `OnClick|`OnContextMenu | `OnDblClick |`OnDrag |`OnDragEnd
    |`OnDragEnter|`OnDragLeave| `OnDragOver|`OnDragStart| `OnDrop
    |`OnDurationChange|`OnEmptied| `OnEnded|`OnError| `OnFocus
    |`OnFormChange|`OnFormInput|`OnInput| `OnInvalid |`OnMouseDown
    | `OnMouseUp|`OnMouseOver| `OnMouseMove | `OnMouseOut|`OnMouseWheel
    |`OnPause |`OnPlay|`OnPlaying|`OnProgress |`OnRateChange
    |`OnReadyStateChange |`OnScroll|`OnSeeked |`OnSeeking |`OnSelect
    |`OnShow |`OnStalled |`OnSubmit|`OnSuspend |`OnTimeUpdate
    |`OnVolumeChange|`OnWaiting|`OnKeyPress|`OnKeyDown |`OnKeyUp |`OnLoad
    |`OnLoadedData | `OnLoadedMetaData|`OnLoadStart]

    val a_onabort : XML.event -> [>`OnAbort] attrib
    val a_onafterprint : XML.event -> [>`OnAfterPrint] attrib
    val a_onbeforeprint : XML.event -> [>`OnBeforePrint] attrib
    val a_onbeforeunload : XML.event -> [>`OnBeforeUnload] attrib
    val a_onblur : XML.event -> [>`OnBlur] attrib
    val a_oncanplay : XML.event -> [>`OnCanPlay] attrib
    val a_oncanplaythrough : XML.event -> [>`OnCanPlayThrough] attrib
    val a_onchange : XML.event -> [>`OnChange] attrib
    val a_onclick : XML.event -> [>`OnClick] attrib
    val a_oncontextmenu : XML.event -> [>`OnContextMenu] attrib
    val a_ondblclick : XML.event -> [>`OnDblClick] attrib
    val a_ondrag : XML.event -> [>`OnDrag] attrib
    val a_ondragend : XML.event -> [>`OnDragEnd] attrib
    val a_ondragenter : XML.event -> [>`OnDragEnter] attrib
    val a_ondragleave : XML.event -> [>`OnDragLeave] attrib
    val a_ondragover : XML.event -> [>`OnDragOver] attrib
    val a_ondragstart: XML.event -> [>`OnDragStart] attrib
    val a_ondrop : XML.event -> [>`OnDrop] attrib
    val a_ondurationchange : XML.event -> [>`OnDurationChange] attrib
    val a_onemptied : XML.event -> [>`OnEmptied] attrib
    val a_onended : XML.event -> [>`OnEnded] attrib
    val a_onerror : XML.event -> [>`OnError] attrib
    val a_onfocus : XML.event -> [>`OnFocus] attrib
    val a_onformchange : XML.event -> [>`OnFormChange] attrib
    val a_onforminput : XML.event -> [>`OnFormInput] attrib
    val a_onhashchange : XML.event -> [>`OnHashChange] attrib
    val a_oninput : XML.event -> [>`OnInput] attrib
    val a_oninvalid : XML.event -> [>`OnInvalid] attrib
    val a_onmousedown : XML.event -> [>`OnMouseDown] attrib
    val a_onmouseup : XML.event -> [>`OnMouseUp] attrib
    val a_onmouseover : XML.event -> [>`OnMouseOver] attrib
    val a_onmousemove : XML.event -> [>`OnMouseMove] attrib
    val a_onmouseout : XML.event -> [>`OnMouseOut] attrib
    val a_onmousewheel : XML.event -> [>`OnMouseWheel] attrib
    val a_onoffline: XML.event -> [>`OnOffLine] attrib
    val a_ononline : XML.event -> [>`OnOnLine] attrib
    val a_onpause : XML.event -> [>`OnPause] attrib
    val a_onplay : XML.event -> [>`OnPlay] attrib
    val a_onplaying : XML.event -> [>`OnPlaying] attrib
    val a_onpagehide : XML.event -> [>`OnPageHide] attrib
    val a_onpageshow : XML.event -> [>`OnPageShow] attrib
    val a_onpopstate : XML.event -> [>`OnPopState] attrib
    val a_onprogress : XML.event -> [>`OnProgress] attrib
    val a_onratechange : XML.event -> [>`OnRateChange] attrib
    val a_onreadystatechange : XML.event -> [>`OnReadyStateChange] attrib
    val a_onredo : XML.event -> [>`OnRedo] attrib
    val a_onresize : XML.event -> [>`OnResize] attrib
    val a_onscroll : XML.event -> [>`OnScroll] attrib
    val a_onseeked : XML.event -> [>`OnSeeked] attrib
    val a_onseeking : XML.event -> [>`OnSeeking] attrib
    val a_onselect : XML.event -> [>`OnSelect] attrib
    val a_onshow : XML.event -> [>`OnShow] attrib
    val a_onstalled : XML.event -> [>`OnStalled] attrib
    val a_onstorage : XML.event -> [>`OnStorage] attrib
    val a_onsubmit : XML.event -> [>`OnSubmit] attrib
    val a_onsuspend : XML.event -> [>`OnSuspend] attrib
    val a_ontimeupdate : XML.event -> [>`OnTimeUpdate] attrib
    val a_onundo : XML.event -> [>`OnUndo] attrib
    val a_onunload : XML.event -> [>`OnUnload] attrib
    val a_onvolumechange : XML.event -> [>`OnVolumeChange] attrib
    val a_onwaiting : XML.event -> [>`OnWaiting] attrib

    val a_onkeypress : XML.event -> [>`OnKeyPress] attrib
    val a_onkeydown : XML.event -> [>`OnKeyDown] attrib
    val a_onkeyup : XML.event -> [>`OnKeyUp] attrib
    val a_onload : XML.event -> [>`OnLoad] attrib
    val a_onloadeddata : XML.event -> [>`OnLoadedData] attrib
    val a_onloadedmetadata : XML.event -> [>`OnLoadedMetaData] attrib
    val a_onloadstart : XML.event -> [>`OnLoadStart] attrib
    val a_onmessage : XML.event -> [>`OnMessage] attrib


    type common = [ core | i18n | events ]

    val a_version : cdata -> [>`Version] attrib
    val a_xmlns : [< `W3_org_1999_xhtml ] -> [>`XMLns] attrib
    val a_manifest : uri -> [>`Manifest] attrib

    val a_cite : uri -> [>`Cite] attrib
    val a_xml_space : [< `Preserve ] -> [>`XML_space] attrib

    val a_accesskey : character -> [>`Accesskey] attrib
(** This attribute assigns an access key to an element. An access key
    is a single character from the document character
    set. NB: authors should consider the input method of the
    expected reader when specifying an accesskey. *)

    val a_charset : charset -> [>`Charset] attrib
(** This attribute specifies the character encoding of the resource
    designated by the link. Please consult the section on character
    encodings for more details. *)

    val a_accept_charset : charsets -> [>`Accept_charset] attrib
    val a_accept : contenttypes -> [>`Accept] attrib

    val a_href : uri -> [>`Href] attrib
(** This attribute specifies the location of a Web resource, thus
    defining a link between the current element (the source anchor)
    and the destination anchor defined by this attribute. *)

    val a_hreflang : languagecode -> [>`Hreflang] attrib
(** This attribute specifies the base language of the resource
    designated by href and may only be used when href is specified. *)

    val a_rel : linktypes -> [>`Rel] attrib
(** This attribute describes the relationship from the current document
    to the anchor specified by the href attribute. The value of this attribute
    is a space-separated list of link types. *)

(** This attribute is used to describe a reverse link from the anchor specified
    by the href attribute to the current document. The value of this attribute
    is a space-separated list of link types. *)

    val a_tabindex : number -> [>`Tabindex] attrib
(** This attribute specifies the position of the current element in
    the tabbing order for the current document. This value must be a
    number between 0 and 32767. User agents should ignore leading
    zeros. *)

    val a_mime_type : contenttype -> [>`Mime_type] attrib
(** This attribute gives an advisory hint as to the content type of
    the content available at the link target address. It allows user
    agents to opt to use a fallback mechanism rather than fetch the
    content if they are advised that they will get content in a
    content type they do not support.Authors who use this attribute
    take responsibility to manage the risk that it may become
    inconsistent with the content available at the link target
    address. *)



(* VB *)
    val a_datetime : cdata -> [>`Datetime] attrib
(* VB *)

    val a_action : uri -> [>`Action] attrib
(** This attribute specifies a form processing agent. User agent
    behavior for a value other than an HTTP URI is undefined. *)

    val a_checked : [< `Checked ] -> [>`Checked] attrib
(** When the [type] attribute has the value ["radio"] or ["checkbox"],
    this boolean attribute specifies that the button is on. User
    agents must ignore this attribute for other control types. *)

    val a_cols : number -> [>`Cols] attrib
(** This attribute specifies the visible width in average character
    widths. Users should be able to enter longer lines than this, so
    user agents should provide some means to scroll through the
    contents of the control when the contents extend beyond the
    visible area. User agents may wrap visible text lines to keep long
    lines visible without the need for scrolling. *)

    val a_enctype : contenttype -> [>`Enctype] attrib
    val a_for : idref -> [>`For] attrib
    val a_for_list : idrefs -> [>`For_List] attrib
    val a_maxlength : number -> [>`Maxlength] attrib
    val a_method : [< `Get | `Post | `Put| `Delete] -> [>`Method] attrib
    val a_multiple : [< `Multiple ] -> [>`Multiple] attrib

    val a_name : text -> [>`Name] attrib
(** This attribute assigns the control name. *)

    val a_rows : number -> [>`Rows] attrib
(** This attribute specifies the number of visible text lines. Users
    should be able to enter more lines than this, so user agents
    should provide some means to scroll through the contents of the
    control when the contents extend beyond the visible area. *)

    val a_selected : [< `Selected ] -> [>`Selected] attrib
(** When set, this boolean attribute specifies that this option is pre-selected. *)

    val a_size : number -> [>`Size] attrib
    val a_src : uri -> [>`Src] attrib
    val a_input_type : [< `Url | `Tel | `Text | `Time | `Search | `Password
      | `Checkbox | `Range | `Radio | `Submit | `Reset | `Number | `Hidden
      | `Month | `Week | `File | `Email | `Image | `Datetime_local| `Datetime
      | `Date | `Color | `Button ] -> [>`Input_Type] attrib 

    val a_text_value : text -> [>`Text_Value] attrib
(** This attribute specifies the initial value of the control. If this
    attribute is not set, the initial value is set to the contents of
    the [option] element. *)
    val a_int_value : number ->[>`Int_Value] attrib

(*VVV NO *)
    val a_value : cdata -> [>`Value] attrib

    val a_float_value : float_number -> [>`Float_Value] attrib

    val a_disabled : [< `Disabled ] -> [>`Disabled] attrib
    val a_readonly : [< `Readonly ] -> [>`Readonly] attrib
    val a_button_type : [< `Button | `Submit | `Reset ] ->
      [>`Button_Type] attrib
    val a_command_type : [< `Command | `Checkbox | `Radio ] ->
      [>`Command_Type] attrib
    val a_menu_type : [< `Context | `Toolbar ] ->
      [>`Menu_Type] attrib
    val a_label : text -> [> `Label ] attrib


    val a_align : [< `Left | `Right | `Justify | `Char ] ->
      [>`Align] attrib
    val a_axis : cdata -> [>`Axis] attrib
    val a_colspan : number -> [>`Colspan] attrib
    val a_headers : idrefs -> [>`Headers] attrib
    val a_rowspan : number -> [>`Rowspan] attrib
    val a_scope : [< `Row | `Col | `Rowgroup | `Colgroup ] -> [>`Scope] attrib
    val a_summary : text -> [>`Summary] attrib

    val a_border : pixels -> [>`Border] attrib
    val a_cellpadding : length -> [>`Cellpadding] attrib
    val a_cellspacing : length -> [>`Cellspacing] attrib
    val a_datapagesize : cdata -> [>`Datapagesize] attrib
    val a_rules : [< `None | `Groups | `Rows | `Cols | `All ] -> [>`Rules] attrib
    val a_char : character -> [>`Char] attrib
    val a_charoff : length -> [>`Charoff] attrib

    val a_alt : text -> [>`Alt] attrib
    val a_height : number -> [>`Height] attrib
    val a_width : number -> [>`Width] attrib

    type shape = [ `Rect | `Circle | `Poly | `Default ]
    val a_shape : shape -> [>`Shape] attrib
    val a_coords : numbers -> [>`Coords] attrib
    val a_usemap : idref -> [>`Usemap] attrib

    val a_data : uri -> [> `Data ] attrib
    val a_codetype : contenttype -> [>`Codetype] attrib

    val a_fs_rows : multilengths -> [>`FS_Rows] attrib
    val a_fs_cols : multilengths -> [>`FS_Cols] attrib
    val a_frameborder : [< `Zero | `One ] -> [>`Frameborder] attrib
    val a_marginheight : pixels -> [>`Marginheight] attrib
    val a_marginwidth : pixels -> [>`Marginwidth] attrib
    val a_scrolling : [< `Yes | `No | `Auto ] -> [>`Scrolling] attrib


    val a_target : frametarget -> [>`Target] attrib

    val a_content : text -> [>`Content] attrib
    val a_http_equiv : text -> [>`Http_equiv] attrib

(* VB *)
    val a_defer : [< `Defer ] -> [>`Defer] attrib
(* VB *)

    val a_media : mediadesc -> [>`Media] attrib

    val a_style : string -> [>`Style_Attr] attrib



(** *)
(* VB *)
    type edit = [ `Ins |`Ins_flow | `Del | `Del_flow]
    type scripttag = [ `Script | `Noscript ]
    type misc = [ edit | scripttag ]

(** {1 Combined Element Sets:} *)

    type heading = [`H1 | `H2 | `H3 | `H4 | `H5 | `H6 | `Hgroup]
    type sectioning = [ `Section | `Nav | `Aside | `Article]
    type resetable = [ `Textarea | `Select | `Output | `Keygen | `Input ]
    type submitable = [`Textarea | `Select | `Object | `Keygen | `Input| `Button ]
    type embedded = [`Video | `Video_src|(* `Math |`Svg |*) `Object | `Img | `Iframe | `Embed | `Canvas | `Audio | `Audio_src]

    type labelable = [ resetable | `Progress | `Meter | `Button]
    type formatblock = [ heading | sectioning | `Pre | `P | `Header | `Footer | `Div | `Blockquote | `Address ]
    type sectionningroot = [`Td | `Figure | `Fieldset | `Details | `Body | `Blockquote]
    type listed = [resetable | submitable | `Fieldset]
    type formassociated = [ listed | `Progress | `Meter | `Label]
    type metadata_without_title = [ `Style | `Script | `Noscript | `Meta | `Link | `Command | `Base]
    type metadata = [ metadata_without_title | `Title ]


        (******************)
        (* Video 
           Type
           Object
           Menu
           Input
           Img (if usemap attribute)
           Audio          *)
        (* in Interactive *)
        (* with conditions*)
        (******************)
    type interactive = [ `Video |`Video_src| `Textarea | `Select | `Object | `Menu | `Label | `Keygen | `Input | `Img | `Iframe | `Embed | `Details | `Button | `Audio | `Audio_src | `A ]
    type interactive_flow = [ interactive | `Video_flow | `Video_src_flow| `Object_flow | `Audio_flow | `Audio_src_flow | `A_flow ]

    type phrasing_without_interactive =  [embedded | labelable | submitable | `Wbr | `Var | `Time | `Sup | `Sub | `Strong | `Span | `Small | `Script | `Samp | `Ruby| `Q | `Noscript | `Mark | `Map | `Kbd | `Ins | `I |`Em | `Dfn | `Del | `Datalist | `Command | `Code | `Cite | `Br | `Bdo | `B | `Abbr |`PCDATA ]
    type phrasing_without_dfn = [embedded | labelable | submitable | `Wbr | `Var | `Time | `Sup | `Sub | `Strong | `Span | `Small | `Script | `Samp | `Ruby | `Q | `Noscript | `Mark | `Map |`Label| `Kbd | `Ins | `I |`Em | `Del | `Datalist | `Command | `Code | `Cite | `Br | `Bdo | `B  | `Abbr | `A | `A_flow |`PCDATA ]
    type phrasing_without_lab_form_and_label = [embedded | `Wbr | `Var | `Time | `Sup | `Sub | `Strong | `Span | `Small | `Script | `Samp | `Ruby | `Q | `Noscript | `Mark | `Map | `Kbd | `Ins | `I |`Em | `Dfn | `Del | `Datalist | `Command | `Code | `Cite | `Br | `Bdo | `B | `Abbr | `A |`PCDATA ]
    type phrasing_without_progress = [embedded | resetable | submitable | `Wbr | `Var | `Time | `Sup | `Sub | `Strong | `Span | `Small | `Script | `Samp | `Ruby | `Q | `Noscript | `Meter | `Mark | `Map |`Label| `Kbd | `Ins | `I |`Em | `Dfn | `Del | `Datalist | `Command | `Code | `Cite |`Button | `Br | `Bdo | `B | `Abbr | `A |`PCDATA ]
    type phrasing_without_time = [embedded | labelable | submitable | `Wbr | `Var | `Sup | `Sub | `Strong | `Span | `Small | `Script | `Samp | `Ruby | `Q | `Noscript | `Mark | `Map |`Label| `Kbd | `Ins | `I |`Em | `Dfn | `Del | `Datalist | `Command | `Code | `Cite | `Br | `Bdo | `B | `Abbr | `A |`PCDATA ]
    type phrasing_without_media = [ labelable | submitable |(* `Math |`Svg |*) `Object | `Img | `Iframe | `Embed | `Canvas | `Wbr | `Var | `Time | `Sup | `Sub | `Strong | `Span | `Small | `Script | `Samp | `Ruby | `Q | `Noscript | `Mark | `Map |`Label| `Kbd | `Ins | `I |`Em | `Dfn | `Del | `Datalist | `Command | `Code | `Cite | `Br | `Bdo | `B | `Abbr | `A |`PCDATA ]
    type phrasing_without_meter = [embedded | submitable | resetable | `Progress | `Button| `Wbr | `Var | `Time | `Sup | `Sub | `Strong | `Span | `Small | `Script | `Samp | `Ruby | `Q | `Noscript | `Mark | `Map |`Label| `Kbd | `Ins | `I |`Em | `Dfn | `Del | `Datalist | `Command | `Code | `Cite | `Br | `Bdo | `B | `Abbr | `A |`PCDATA ]
    type phrasing_without_noscript = [embedded | labelable | submitable | `Wbr | `Var | `Time | `Sup | `Sub | `Strong | `Span | `Small | `Script | `Samp | `Ruby | `Q | `Mark | `Map |`Label| `Kbd | `Ins | `I |`Em | `Dfn | `Del | `Datalist | `Command | `Code | `Cite | `Br | `Bdo | `B | `Abbr | `A |`PCDATA ]
        (******************)
        (* Map  Ins
           Del  A         *)
        (*  in Phrasing   *)
        (* with conditions*)
        (******************)
    type phrasing = [embedded | labelable | submitable | `Wbr | `Var | `Time | `Sup | `Sub | `Strong | `Span | `Small | `Script | `Samp | `Ruby | `Q | `Noscript | `Mark | `Map |`Label| `Kbd | `Ins | `I |`Em | `Dfn | `Del | `Datalist | `Command | `Code | `Cite | `Br | `Bdo | `B | `Abbr | `A |`PCDATA ]

    type flow5_without_interactive = [ phrasing_without_interactive | formassociated | formatblock | `Ul | `Table |`Style | `Ol | `Menu | `Map |`Map_flow| `Hr | `Form |`Figure | `Dl|`Ins |`Ins_flow | `Del| `Del_flow| `Canvas_flow]
    type flow5_without_table = [ phrasing | interactive_flow | formassociated | formatblock | `Ul |`Style | `Ol | `Menu | `Map |`Map_flow| `Hr | `Form |`Figure | `Dl|`Ins |`Ins_flow | `Del| `Del_flow| `Canvas_flow]
    type flow5_without_header_footer = [ heading | sectioning | `Pre | `P | `Div | `Blockquote | `Address | phrasing | interactive_flow | formassociated | `Ul | `Table |`Style | `Ol | `Menu | `Map |`Map_flow| `Hr | `Form |`Figure | `Dl|`Ins |`Ins_flow | `Del| `Del_flow| `Canvas_flow]
    type flow5_without_form = [ phrasing | interactive_flow | formassociated | formatblock | `Ul | `Table |`Style | `Ol | `Menu | `Map |`Map_flow| `Hr |`Figure | `Dl|`Ins |`Ins_flow | `Del| `Del_flow| `Canvas_flow]
    type flow5_without_sectioning_heading_header_footer_address = [ phrasing | interactive_flow | formassociated | `Pre | `P | `Div | `Blockquote| `Ul | `Table |`Style | `Ol | `Menu | `Map |`Map_flow| `Hr | `Form |`Figure | `Dl|`Ins |`Ins_flow | `Del| `Del_flow| `Canvas_flow]
    type flow5_without_media = [phrasing_without_media |  `Textarea | `Select |`Object_flow | `Object | `Menu | `Label | `Keygen | `Input | `Img | `Iframe | `Embed | `Details | `Button | `A | `A_flow | formassociated | formatblock | `Ul | `Table |`Style | `Ol | `Menu | `Map |`Map_flow| `Hr | `Form |`Figure | `Dl|`Ins |`Ins_flow | `Del| `Del_flow]

        (******************)
        (* Style  in Flow *)
        (* with conditions*)
        (******************)
    type flow5 = [ phrasing | interactive_flow | formassociated | formatblock | `Ul | `Table |`Style | `Ol | `Menu | `Map |`Map_flow| `Hr | `Form |`Figure| `Ins |`Ins_flow | `Dl| `Del| `Del_flow| `Canvas_flow]

(** {1 Elements} *)

    (* For Ocsigen I need to specify the variance --Vincent *)
    type +'a elt

(** {2 Element Constructor Types} *)

    type ('a, 'b) nullary = ?a:('a attrib list) -> unit -> 'b elt
    type ('a, 'b, 'c) unary = ?a:('a attrib list) -> 'b elt -> 'c elt
    type ('a, 'b, 'c, 'd) binary = ?a:('a attrib list) -> 'b elt -> 'c elt -> 'd elt
    type ('b, 'c, 'd, 'e) tri = 'b elt -> 'c elt -> 'd elt -> 'e elt
    type ('a, 'b, 'c) star = ?a:('a attrib list) -> 'b elt list -> 'c elt
(** Star '*' denotes any number of children, uncluding zero. *)

    type ('a, 'b, 'c) plus = ?a:('a attrib list) -> 'b elt -> 'b elt list -> 'c elt
(** Plus '+' requires at least one child.  *)

    type rt
    type ruby_content = phrasing elt list * rt
    type rp 

(** {2 Structure} *)

    type html = [`Html] elt

    val html : ?a:([< common | `Manifest] attrib list) ->
      [< `Head ] elt -> [< `Body ] elt -> html

  (********************************)
  (*            In Head           *)
  (********************************)
  (*  If the document is an       *)
  (*  iframe srcdoc document or if*)
  (*title information is available*)
  (*from a higher-level protocol: *)
  (*   Zero or more elements of   *)
  (*   metadata content.          *)
  (*Otherwise:                    *)
  (*   One or more elements of    *)
  (*   metadata content, of which *)
  (*exactly one is a title element*)
  (********************************)
    val head : ?a:([< common ] attrib list) ->
       [< `Title ] elt -> [< metadata_without_title ] elt list -> [>`Head] elt
    val title : ([< common], [< `PCDATA],[>`Title]) unary
    val body : ([< common | `OnAfterPrint | `OnBeforePrint | `OneBeforeUnload
                | `OnHashChange
                | `OnMessage | `OnOffLine | `OnOnLine | `OnPageHide
                | `OnPageShow | `OnPopState | `OnRedo | `OnResize | `OnStorage
                | `OnUndo | `OnUnload ], [< flow5 ], [>`Body]) star

(** {2 Section} *)

    val footer : ([< common ], [< flow5_without_header_footer ], [>`Footer]) star
    val header : ([< common ], [< flow5_without_header_footer ], [>`Header]) star
    val section : ([< common ], [< flow5 ], [>`Section]) star
    val nav : ([< common ], [< flow5 ], [>`Nav]) star

    val h1 : ([< common ], [< phrasing ], [>`H1]) star
    val h2 : ([< common ], [< phrasing ], [>`H2]) star
    val h3 : ([< common ], [< phrasing ], [>`H3]) star
    val h4 : ([< common ], [< phrasing ], [>`H4]) star
    val h5 : ([< common ], [< phrasing ], [>`H5]) star
    val h6 : ([< common ], [< phrasing ], [>`H6]) star
    val hgroup : ([< common ], [< `H1 | `H2 | `H3 | `H4 | `H5 | `H6  ], [>`Hgroup]) plus

    val address : ([< common ], [< flow5_without_sectioning_heading_header_footer_address ], [>`Address]) star

    val article : ([< common ], [< flow5 ], [>`Article]) star

    val aside : ([< common ], [< flow5 ], [>`Aside]) star

(** {2 Grouping content} *)

    val p : ([< common ], [<phrasing ], [>`P]) star
    val pre : ([< common ],[< phrasing ], [>`Pre]) star
    val blockquote : ([< common | `Cite ],[< flow5 ], [>`Blockquote]) star
    val div : ([< common ], [< flow5 ], [>`Div]) star

  (********************************)
  (*            In Dl             *)
  (********************************)
  (*   Zero or more groups each   *)
  (*   consisting of              *)
  (*      one or more dt element  *)
  (*      followed by             *)
  (*      one or more dd  elements*)
  (********************************)
    val dl : ([< common ], [< `Dt | `Dd ], [>`Dl]) star
    val ol : ([< common | `Reserved |`Start ], [< `Li ], [>`Ol]) star
    val ul : ([< common ], [< `Li ], [>`Ul]) star
    val dd : ([< common ], [< flow5 ], [>`Dd]) star
    val dt : ([< common ], [< phrasing], [>`Dt]) star
  (********************************)
  (*            In Li             *)
  (********************************)
  (*  Only if the element is a    *)
  (*  child of an ol element:     *)
  (*          value attribute     *)
  (********************************)
    val li : ([< common | `Int_Value], [<flow5 ], [>`Li]) star

    val figcaption : ([< common ], [< flow5], [>`Figcaption]) star

  (********************************)
  (*          In Figure           *)
  (********************************)
  (*Either: One figcaption element*)
  (*     followed by flow content.*)
  (*Or: Flow content followed by  *)
  (*     one figcaption element.  *)
  (*Or: Flow content.             *)
  (********************************)
    val figure : ?figcaption:([< `Figcaption ] elt) ->
      ([< common ], [< flow5 ], [>`Figure]) star

    val hr : ([< common ], [>`Hr]) nullary


(** {2 Ruby} *)
  (**********************************)
  (*            In Ruby             *)
  (**********************************)
  (* One or more groups of:         *)
  (*phrasing content followed either*)
  (*    by a single rt element,     *)
  (*    or an rp element            *)
  (*       an rt element, and       *)
  (*       another rp element.      *)
  (**********************************)
    val rt : ?rp:(rp * rp) -> ?a:([< common ] attrib list) ->
      [< phrasing ] elt list -> rt

    val rp : ?a:([< common ] attrib list) -> [< phrasing ] elt list -> rp

    val ruby :  ?a:([< common ] attrib list) -> ruby_content -> ruby_content list -> [> `Ruby ] elt


(** {2 Semantic} *)

    val b : ([< common ], [< phrasing ], [>`B]) star
    val i : ([< common ], [< phrasing ], [>`I]) star
    val small : ([< common ], [< phrasing ], [>`Small]) star
    val sub : ([< common ], [< phrasing ], [>`Sub]) star
    val sup : ([< common ], [< phrasing ], [>`Sup]) star
    val mark : ([< common ],[< phrasing ],[> `Mark ]) star


    val wbr : ([< common ],[> `Wbr ]) nullary

    val bdo : dir:[< `Ltr | `Rtl ] -> ([< common ],[<  phrasing ],[> `Bdo ]) star

    val abbr : ([< common ], [<phrasing ], [>`Abbr]) star
    val br :  ([< common ], [>`Br]) nullary
    val cite : ([< common ], [< phrasing ], [>`Cite]) star
    val code : ([< common ], [< phrasing ], [>`Code]) star
    val dfn : ([< common ], [< phrasing_without_dfn ], [>`Dfn]) star
    val em : ([< common ], [< phrasing ], [>`Em]) star
    val kbd : ([< common ], [< phrasing ], [>`Kbd]) star
    val q : ([< common | `Cite ], [< phrasing ], [>`Q]) star
    val samp : ([< common ], [< phrasing ], [>`Samp]) star
    val span : ([< common ], [< phrasing ], [>`Span]) star
    val strong : ([< common ], [< phrasing ], [>`Strong]) star
    val time : ([< common |`Datetime |`Pubdate], [< phrasing_without_time ], [>`Time]) star
    val var : ([< common ], [< phrasing ], [>`Var]) star


(** {2 Hypertext} *)
  (********************************)
  (*             In A             *)
  (********************************)
  (*    The target, rel, media,   *)
  (* hreflang, and type attributes*)
  (*  must be omitted if the href *)
  (*   attribute is not present.  *)
  (********************************)
  (*Only phasing instead of flow ?*)
  (********************************)
  (* a's children are transparents*)
  (********************************)

    val a : ([< common | `Href | `Hreflang | `Media | `Rel | `Target | `Mime_type ], [< phrasing_without_interactive], [>`A]) star
    val a_flow : ([< common | `Href | `Hreflang | `Media | `Rel | `Target | `Mime_type ], [< flow5_without_interactive], [>`A_flow]) star

(** {2 Edit} *)
    (**********************************)
    (* del's children are transparents*)
    (**********************************)
    val del : ([< common | `Cite | `Datetime ],[< phrasing ],[>`Del]) star
    val del_flow : ([< common | `Cite | `Datetime ],[< flow5 ],[>`Del_flow]) star
    (**********************************)
    (* ins's children are transparents*)
    (**********************************)
    val ins : ([< common | `Cite | `Datetime ],[< phrasing ],[>`Ins]) star
    val ins_flow : ([< common | `Cite | `Datetime ],[< flow5 ],[>`Ins_flow]) star

(** {2 Embedded} *)

    val img : src:uri -> alt:text ->
      ([< common | `Height | `Ismap | `Width | `Usemap ], [>`Img]) nullary

    val iframe : ([< common | `Src (*| `Srcdoc*) | `Name | `Sandbox | `Seamless | `Width | `Height ], [< `PCDATA], [>`Iframe]) star

    (************************************)
    (*object's children are transparents*)
    (************************************)
    val object_ : ?param:([< `Params of ([< `Param ] elt list )]) ->
      ([< common | `Data | `Form | `Mime_type | `Height | `Width | `Name | `Usemap ],[< phrasing], [> `Object ]) star
    val object_flow : ?param:([< `Params of ([< `Param ] elt list )]) ->
      ([< common | `Data | `Form | `Mime_type | `Height | `Width | `Name | `Usemap ],[< flow5], [> `Object_flow ]) star

    val param : ([< common | `Name | `Text_Value ],[> `Param ]) nullary

  (**********************************)
  (*            In Embed            *)
  (**********************************)
  (*  Any namespace-less attribute  *)
  (* other than name, align, hspace,*)
  (* and vspace  may be specified on*)
  (* the embed element, so long as  *)
  (* its name is XML-compatible and *)
  (* contains no characters in the  *)
  (* range U+0041 to U+005A         *)
  (*(LATIN CAPITAL LETTER A to LATIN*)
  (*CAPITAL LETTER Z).              *)
  (*These attributes are then passed*)
  (*  as parameters to the plugin.  *)
  (**********************************)
    val embed : ([< common | `Src | `Height | `Mime_type | `Width], [>`Embed]) nullary

  (**************************************)
  (*         In Audio and Video         *)
  (**************************************)
  (* If the element has a src attribute:*)
  (*   transparent, but with no media   *)
  (*   element descendants.             *)
  (* If the element does not have a src *)
  (* attribute:                         *)
  (*   one or more source elements, then*)
  (*   transparent, but with no media   *)
  (*   element descendants.             *)
  (**************************************)

    (************************************)
    (* audio's children are transparents*)
    (************************************)
    val audio : ?srcs:([<`Srcs of ([< `Source ] elt list)]) -> ([< common |`Preload |`Autoplay |`Loop |`Controls],[< phrasing_without_media], [>`Audio]) star
    val audio_flow : ?srcs:([<`Srcs of ([< `Source ] elt list)]) -> ([< common |`Preload |`Autoplay |`Loop |`Controls],[< flow5_without_media], [>`Audio_flow]) star
    val audio_src : src:uri -> ([< common |`Preload |`Autoplay |`Loop |`Controls],[< phrasing_without_media ], [>`Audio_src]) star
    val audio_src_flow : src:uri -> ([< common |`Preload |`Autoplay |`Loop |`Controls],[< flow5_without_media ], [>`Audio_src_flow]) star

    (************************************)
    (* video's children are transparents*)
    (************************************)
    val video : ?srcs:([<`Srcs of ([< `Source ] elt list)]) -> ([< common |`Poster |`Preload |`Autoplay |`Loop |`Controls |`Width |`Height],[< phrasing_without_media ], [>`Video]) star
    val video_flow : ?srcs:([<`Srcs of ([< `Source ] elt list)]) -> ([< common |`Poster |`Preload |`Autoplay |`Loop |`Controls |`Width |`Height],[< flow5_without_media ], [>`Video_flow]) star
    val video_src : src:uri -> ([< common |`Poster |`Preload |`Autoplay |`Loop |`Controls |`Width |`Height],[< phrasing_without_media ], [>`Video_src]) star
    val video_src_flow : src:uri -> ([< common |`Poster |`Preload |`Autoplay |`Loop |`Controls |`Width |`Height],[< flow5_without_media ], [>`Video_src_flow]) star
    (************************************)
    (*canvas's children are transparents*)
    (************************************)
    val canvas : ([< common |`Width |`Height],[< phrasing ], [>`Canvas]) star
    val canvas_flow : ([< common |`Width |`Height],[< flow5 ], [>`Canvas_flow]) star
    val source : ([< common |`Src |`Mime_type |`Media ], [>`Source]) nullary


  (********************************)
  (*           In Area            *)
  (********************************)
  (* The alt, target, rel, media, *)
  (* hreflang, and type attributes*)
  (*  must be omitted if the href *)
  (*   attribute is not present.  *)
  (********************************)
    val area : alt:text -> ([< common | `Coords | `Shape| `Target
                            | `Rel | `Media| `Hreflang | `Mime_type],[>`Area]) nullary
    (**********************************)
    (* map's children are transparents*)
    (**********************************)
    val map : ([<common | `Name ],[< phrasing | `Area ],[>`Map]) plus
    val map_flow : ([<common | `Name ],[< flow5 | `Area ],[>`Map_flow]) plus


(** {2 Tables Data} *)

    val caption : ([< common ], [< flow5_without_table], [>`Caption]) star

  (********************************)
  (*      In Table and Tablex     *)
  (********************************)
  (*    In this order:            *)
  (* optionally a caption element,*)
  (* followed by either           *)
  (*zero or more colgroup elements*)
  (* followed optionally by a     *)
  (*thead element,                *)
  (* followed optionally by a     *)
  (*tfoot element,                *)
  (* followed by either           *)
  (*zero or more tbody elements   *)
  (*or one or more tr elements,   *)
  (* followed optionally by       *)
  (*a tfoot element               *)
  (********************************)
  (*   BUT ONLY ONE FOOT ELEMENT  *)
  (*         CHILD IN TOTAL       *)
  (********************************)
    val table : ?caption:([< `Caption ] elt) ->
      ?columns:([< `Colgroups of ([< `Colgroup ] elt list )]) ->
      ?thead:([< `Thead ] elt) -> ?tfoot:([< `Tfoot ] elt) ->
      ([< common | `Summary ], [< `Tr ], [>`Table]) plus
    val tablex : ?caption:([< `Caption ] elt) ->
      ?columns:([< `Colgroups of ([< `Colgroup ] elt list) ]) ->
      ?thead:([< `Thead ] elt) -> ?tfoot:([< `Tfoot ] elt) ->
      ([< common | `Summary ], [< `Tbody ], [>`Table]) star


  (********************************)
  (*          In Colgroup         *)
  (********************************)
  (*   If span attribute is:      *)
  (*       -present: Empty.       *)
  (*       -absent: Zero or more  *)
  (*                col elements. *)
  (********************************)
    val colgroup : ([< common | `Span ],[< `Col ], [>`Colgroup]) star
    val col : ([< common | `Span], [>`Col]) nullary
    val thead : ([< common],[< `Tr ], [>`Thead]) star
    val tbody : ([< common],[< `Tr ], [>`Tbody]) star
    val tfoot : ([< common],[< `Tr ], [>`Tfoot]) star
    val td : ([< common | `Colspan | `Headers | `Rowspan ],
              [< flow5 ], [>`Td]) star
    val th : ([< common | `Colspan | `Headers | `Rowspan | `Scope],
              [< phrasing], [>`Th]) star

  (****************************************)
  (*                 In Tr                *)
  (****************************************)
  (*If the parent node is a thead element:*)
  (*      Zero or more th elements        *)
  (* Otherwise:                           *)
  (*    Zero or more td or th elements    *)
  (****************************************)
    val tr : ([< common ],[< `Td | `Th ], [>`Tr]) star

(** {2 Forms} *)

   val form : ([< common |`Accept_charset | `Action | `Enctype | `Method
               | `Name | `Target | `Autocomplete | `Novalidate ],
               [< flow5_without_form ], [>`Form]) plus
   val fieldset : ?legend:([ `Legend ] elt) ->
     ([< common | `Disabled | `Form | `Name], [< flow5 ], [>`Fieldset]) star
   val legend : ([< common ],[< phrasing], [>`Legend]) star
   val label : ([< common | `For | `Form ],[< phrasing_without_lab_form_and_label], [>`Label]) star
   val input : ([< common | `Accept | `Alt | `Autocomplete
                | `Autofocus | `Checked | `Disabled | `Form
                | `Formation | `Formenctype | `Formmethod
                | `Formnovalidate | `Formtarget | `Height | `List
                | `Input_Max | `Maxlength | `Input_Min | `Multiple | `Name
                | `Pattern | `Placeholder | `ReadOnly | `Required|`Size
                | `Src | `Step | `Input_Type | `Value | `Width ], [>`Input]) nullary


  (********************************)
  (*          In Button           *)
  (********************************)
  (* The formaction, formenctype, *)
  (*  formmethod, formnovalidate, *)
  (*  and formtarget must not be  *)
  (*  specified if the element's  *)
  (* type  attribute is not in the*)
  (*     Submit Button  state.    *)
  (********************************)
    val button : ([< common | `Autofocus | `Disabled | `Form
                  | `Formaction | `Formenctype | `Formmethod
                  | `Formnovalidate | `Formtarget | `Name
                  | `Text_Value | `Button_Type ],
                  [< phrasing_without_interactive ], [>`Button]) star

    val select : ([< common |`Autofocus | `Multiple | `Name | `Size | `Form | `Disabled ], [ `Optgroup | `Option ],[>`Select]) star
    val datalist : ?child:([< `Options of ([< `Option ] elt list) | `Phras of ([< phrasing ] elt list) ]) -> ([< common ], [>`Datalist]) nullary
    val optgroup : label:text ->
      ([< common | `Disabled ],
       [< `Option ], [>`Optgroup]) star
    val option : ([< common | `Selected | `Text_Value | `Disabled | `Label ],
                  [<`PCDATA], [>`Option]) unary
    val textarea : ([< common | `Autofocus | `Disabled | `Form | `Maxlength
       | `Name | `Placeholder| `Readonly| `Required | `Wrap | `Rows | `Cols],
       [<`PCDATA], [>`Textarea]) unary
    val keygen : ([< common | `Autofcus | `Challenge | `Disabled | `Form | `Keytype | `Name ], [>`Keygen]) nullary
    val progress : ([< common | `Float_Value |`Max| `Form ],[< phrasing_without_progress], [>`Progress]) star
    val meter : ([< common |`Float_Value |`Min |`Max |`Low |`High |`Optimum |`Form],[< phrasing_without_meter ],[>`Meter]) star

    val output_elt : ([< common |`Form |`For_List |`Name],[< phrasing ],[>`Output]) star



(** {2 Data} *)

    val pcdata : string -> [>`PCDATA] elt
    val entity : string -> [>`PCDATA] elt
    val space : unit -> [>`PCDATA] elt
    val cdata : string -> [>`PCDATA] elt (* GK *)
    val cdata_script : string -> [>`PCDATA] elt (* GK *)
    val cdata_style : string -> [>`PCDATA] elt (* GK *)
(**/**)
    val unsafe_data : string -> 'a elt
(**/**)


(** {2 Interactive} *)

    val details : ?a:([< common | `Open ] attrib list) ->
      [< `Summary ] elt -> [< flow5] elt list -> [>`Details] elt
    val summary : ([< common ],[< phrasing ], [>`Summary]) star

    val command : label:text -> ([< common |`Icon |`Disabled |`Checked|`Radiogroup |`Command_Type], [>`Command]) nullary
    val menu : ?child:([<`Lis of ([< `Li ] elt list) | `Flows of ([< flow5 ]elt list) ]) -> ([< common |`Label |`Menu_Type  ],[>`Menu]) nullary

(** {2 Scripting} *)

    val script : contenttype:contenttype -> ([< common | `Async | `Charset | `Src | `Defer |`Mime_type ], [< `PCDATA (*VVV ??? `Script |`Noscript (*a implem. |`Script-Documentation*)*) ], [>`Script]) unary

  (****************************************************)
  (*                   In Noscript                    *)
  (****************************************************)
  (*When scripting is DISABLED, IN a HEAD element:    *)
  (*   in any order, zero or more link elements,      *)
  (*   zero or more style elements, and zero or more  *)
  (*   meta elements.                                 *)
  (*When scripting is DISABLED, NOT IN a HEAD element:*)
  (*   transparent, but there must be no noscript     *)
  (*   element descendants.                           *)
  (*When scripting is ENABLED, IN a HEAD element:     *)
  (*   only text, except that invoking the HTML       *)
  (*   fragment parsing algorithm with the noscript   *)
  (*   element as the context element and the text    *)
  (*   contents as the input must result in a list of *)
  (*   nodes that consists only of link, style, and   *)
  (*   meta elements that would be conforming if they *)
  (*   were children of the noscript element, and no  *)
  (*   parse errors.                                  *)
  (*When scripting is ENABLED, NOT IN a HEAD element: *)
  (*   only text, except that the text must be such   *)
  (*   that running the following algorithm results in*)
  (*   a conforming document with no noscript elements*)
  (*   and no script elements, and such that no step  *)
  (*   in the algorithm causes an HTML parser to flag *)
  (*   a parse error                                  *)
  (****************************************************)
    val noscript : ([< common ],[< phrasing_without_noscript ],[>`Noscript]) plus

    val meta : ([< common | `Http_equiv | `Name | `Content | `Charset ], [>`Meta]) nullary

(** {2 Style Sheets} *)

  (*********************************)
  (*            In Style           *)
  (*********************************)
  (* the content model depends on  *)
  (*the value of the type attribute*)
  (*********************************)
  (*          BUT WHAT ???         *)
  (*********************************)
    val style : contenttype:text -> ([< common | `Media | `Mime_type | `Scoped ], [< `PCDATA ], [>`Style]) star

(** {2 Link} *)

    val link : href:uri -> rel:linktypes ->
      ([< common | `Href | `Hreflang | `Media
       | `Rel | `Sizes | `Mime_type ], [>`Link]) nullary

(** {2 Base} *)

    val base : ([< common | `Target| `Href],[>`Base]) nullary


(** [?encode] maps strings to HTML and {e must} encode the unsafe characters
    ['<'], ['>'], ['"'], ['&'] and the control characters 0-8, 11-12, 14-31, 127
    to HTML entities.  [XML.encode_unsafe] is the default for [?encode] in [output]
    and [pretty_print] below.  Other implementations are provided by the module
    [Netencoding] in the
    {{:http://www.ocaml-programming.de/programming/ocamlnet.html}OcamlNet} library, e.g.:
    [let encode = Netencoding.Html.encode ~in_enc:`Enc_iso88591 ~out_enc:`Enc_usascii ()],
    Where national characters are replaced by HTML entities.
    The user is of course free to write her own implementation.
    @see <http://www.ocaml-programming.de/programming/ocamlnet.html> OcamlNet *)

(** [~encoding] is the official name of the external character set encoding that
    is used by [outs : string -> unit]. *)

    type doctypes = 
        [ `HTML_v03_02 | `HTML_v04_01 | `XHTML_01_00 | `XHTML_01_01
        | `XHTML_05_00 | `Doctype of string ]

    val doctype : [< doctypes ] -> string

(** {1 Tools} *)

    val version : string
    val standard : uri
(*
    val validator : uri
    val validator_icon : unit -> [>`A] elt
(** A hyperlink to the W3C validator, including the logo.
    @see <http://validator.w3.org> Validator *)
*)

    val tot : XML.elt -> 'a elt
    val totl : XML.elt list -> 'a elt list
    val toelt : 'a elt -> XML.elt
    val toeltl : 'a elt list -> XML.elt list

  end

(** An alias for XHTML5:
    @see <http://www.w3.org/TR/html5/> HTML5 *)
module type T_05_00 = T

(* END INTERFACE *)

(* BEGIN INTERFACE

module M : T
module M_05_00 : T_05_00

   END INTERFACE *)

module Version =
  struct

    (* Directly from http://www.w3.org/TR/xhtml-modularization/abstract_modules.html *)
    type i18n = [ `XML_lang ]
    type core = [ `Accesskey | `Class | `Contenteditable | `Contextmenu | `Dir | `Draggable | `Hidden | `Id  | i18n | `Spellcheck | `Style_Attr | `Tabindex| `Title | `User_data ]

    type events = [ `OnAbort|`OnBlur | `OnCanPlay |`OnCanPlayThrough
    |`OnChange | `OnClick|`OnContextMenu | `OnDblClick |`OnDrag |`OnDragEnd
    |`OnDragEnter|`OnDragLeave| `OnDragOver|`OnDragStart| `OnDrop
    |`OnDurationChange|`OnEmptied| `OnEnded|`OnError| `OnFocus
    |`OnFormChange|`OnFormInput|`OnInput| `OnInvalid |`OnMouseDown
    |`OnMouseUp|`OnMouseOver| `OnMouseMove | `OnMouseOut|`OnMouseWheel
    |`OnPause |`OnPlay|`OnPlaying|`OnProgress |`OnRateChange
    |`OnReadyStateChange |`OnScroll|`OnSeeked |`OnSeeking |`OnSelect
    |`OnShow |`OnStalled |`OnSubmit|`OnSuspend |`OnTimeUpdate
    |`OnVolumeChange|`OnWaiting|`OnKeyPress|`OnKeyDown |`OnKeyUp |`OnLoad
    |`OnLoadedData | `OnLoadedMetaData|`OnLoadStart]

    type common = [ core | i18n | events ]

    type 'a attrib = XML.attrib
    type 'a attribs = XML.attribs

    let to_xmlattribs x = x (* VB *)

    let float_attrib = XML.float_attrib
    let int_attrib = XML.int_attrib
    let string_attrib = XML.string_attrib
    let space_sep_attrib = XML.space_sep_attrib
    let comma_sep_attrib = XML.comma_sep_attrib
    let event_attrib = XML.event_attrib

    type cdata = string
    type id = string
    type idref = string
    type idrefs = idref list (* space-separated *)
    type name = string
    type nmtoken = string
    type nmtokens = nmtoken list (* space-separated *)
    type pcdata = string

    type character = char
    type charset = string
    type charsets = charset list (* space-separated *)

    type contenttype = string
    type contenttypes = contenttype list (* comma-separated *)
    type coords = string list (* Comma separated list of coordinates to use in defining areas. *)
    type datetime = string
    type fpi = string
    type frametarget = string
    type languagecode = string
    type length = [ `Pixels of int | `Percent of int ]
    type linktypes =
        [`Alternate | `Archives |`Author | `Bookmark | `External
        | `First | `Help |`Icon | `Index |`Last |`License | `Next 
        |`Nofollow |`Noreferrer |`Pingback |`Prefetch | `Prev
        | `Search | `Stylesheet | `Sidebar |`Tag | `Up] list

    type mediadesc = 
        [ `All | `Aural | `Braille | `Embossed | `Handheld | `Print
        | `Projection | `Screen| `Speech | `TTY | `TV ] list 

    type multilength = [ length | `Relative of int ]
    type multilengths = multilength list (* comma-separated *)
    type number = int
    type numbers = number list (* space-separated *)
    type float_number = float
    type pixels = int
    type script = string
    type text = string
    type uri = string
    type uris = uri (* space-separated *)
    let uri_of_string s = s
    let string_of_uri s = s

    let length_attrib name = function
      | `Pixels p -> int_attrib name p
      | `Percent p -> string_attrib name (string_of_int p ^ "%")

    let multilength_attrib name = function
      | #length as l -> length_attrib name l
      | `Relative 1 -> string_attrib name "*"
      | `Relative i -> string_attrib name (string_of_int i ^ "*")

    let multilength_to_string = function
      | `Pixels p -> string_of_int p
      | `Percent p -> string_of_int p ^ "%"
      | `Relative 1 -> "*"
      | `Relative i -> string_of_int i ^ "*"

    let multilengths_attrib name multilengths =
      string_attrib name
        (String.concat ", " (List.map multilength_to_string multilengths))

    let linktype_to_string = function
      | `Alternate -> "alternate"
      | `Archives -> "archives"
      | `Author -> "author"
      | `Bookmark -> "bookmark"
      | `External -> "external"
      | `First -> "first"
      | `Help -> "help"
      | `Icon -> "icon"
      | `Index -> "index"
      | `Last -> "last"
      | `License -> "license"
      | `Next -> "next"
      | `Nofollow -> "nofollow"
      | `Noreferrer -> "noreferrer"
      | `Pingback -> "pingback"
      | `Prefetch -> "prefetch"
      | `Prev -> "prev"
      | `Search -> "search"
      | `Stylesheet -> "stylesheet"
      | `Sidebar -> "sidebar"
      | `Tag -> "tag"
      | `Up -> "up"
      | `Other t -> t

    let linktypes_attrib name linktypes =
      string_attrib name
        (String.concat " " (List.map linktype_to_string linktypes))

    let mediadesc_to_string = function
      | `All -> "all"
      | `Aural -> "aural"
      | `Braille -> "braille"
      | `Embossed -> "embossed"
      | `Handheld -> "handheld"
      | `Print -> "print"
      | `Projection -> "projection"
      | `Screen -> "screen"
      | `Speech -> "speech"
      | `TTY -> "tty"
      | `TV -> "tv" 

    let mediadesc_attrib name mediadescs =
      string_attrib name
        (String.concat ", " (List.map mediadesc_to_string mediadescs))

    (* Core: *)

    let a_class = space_sep_attrib "class"
    let a_id = string_attrib "id"
    let a_user_data name = string_attrib ("data-"^name)
    let a_title = string_attrib "title"

    (* I18N: *)

    let a_xml_lang = string_attrib "xml:lang"

    (* Style: *)

    let a_style = string_attrib "style"


    (* Events: *)

    let a_onabort = event_attrib "onabort"
    let a_onafterprint = event_attrib "onafterprint"
    let a_onbeforeprint = event_attrib "onbeforeprint"
    let a_onbeforeunload = event_attrib "onbeforeunload"
    let a_onblur = event_attrib "onblur"
    let a_oncanplay = event_attrib "oncanplay"
    let a_oncanplaythrough = event_attrib "oncanplaythrough"
    let a_onchange = event_attrib "onchange"
    let a_onclick = event_attrib "onclick"
    let a_oncontextmenu = event_attrib "oncontextmenu"
    let a_ondblclick = event_attrib "ondblclick"
    let a_ondrag = event_attrib "ondrag"
    let a_ondragend = event_attrib "ondragend"
    let a_ondragenter = event_attrib "ondragenter"
    let a_ondragleave = event_attrib "ondragleave"
    let a_ondragover = event_attrib "ondragover"
    let a_ondragstart = event_attrib "ondragstart"
    let a_ondrop = event_attrib "ondrop"
    let a_ondurationchange = event_attrib "ondurationchange"
    let a_onemptied = event_attrib "onemptied"
    let a_onended = event_attrib "onended"
    let a_onerror = event_attrib "onerror"
    let a_onfocus = event_attrib "onfocus"
    let a_onformchange = event_attrib "onformchange"
    let a_onforminput = event_attrib "onforminput"
    let a_onhashchange = event_attrib "onhashchange"
    let a_oninput = event_attrib "oninput"
    let a_oninvalid = event_attrib "oninvalid"
    let a_onmousedown = event_attrib "onmousedown"
    let a_onmouseup = event_attrib "onmouseup"
    let a_onmouseover = event_attrib "onmouseover"
    let a_onmousemove = event_attrib "onmousemove"
    let a_onmouseout = event_attrib "onmouseout"
    let a_onmousewheel = event_attrib "onmousewheel"
    let a_onoffline = event_attrib "onoffline"
    let a_ononline = event_attrib "ononline"
    let a_onpause = event_attrib "onpause"
    let a_onplay = event_attrib "onplay"
    let a_onplaying = event_attrib "onplaying" 
    let a_onpagehide = event_attrib "onpagehide"
    let a_onpageshow = event_attrib "onpageshow"
    let a_onpopstate = event_attrib "onpopstate"
    let a_onprogress = event_attrib "onprogress"
    let a_onratechange = event_attrib "onratechange"
    let a_onreadystatechange = event_attrib "onreadystatechange"
    let a_onredo = event_attrib "onredo"
    let a_onresize = event_attrib "onresize"
    let a_onscroll = event_attrib "onscroll"
    let a_onseeked = event_attrib "onseeked"
    let a_onseeking = event_attrib "onseeking"
    let a_onselect = event_attrib "onselect"
    let a_onshow = event_attrib "onshow"
    let a_onstalled = event_attrib "onstalled"
    let a_onstorage = event_attrib "onstorage"
    let a_onsubmit = event_attrib "onsubmit"
    let a_onsuspend = event_attrib "onsuspend"
    let a_ontimeupdate = event_attrib "ontimeupdate"
    let a_onundo = event_attrib "onundo"
    let a_onunload = event_attrib "onunload"
    let a_onvolumechange = event_attrib "onvolumechange"
    let a_onwaiting = event_attrib "onwaiting"
    let a_onkeypress = event_attrib "onkeypress"
    let a_onkeydown = event_attrib "onkeydown"
    let a_onkeyup = event_attrib "onkeyup"
    let a_onload = event_attrib "onload"
    let a_onloadeddata = event_attrib "onloadeddata"
    let a_onloadedmetadata = event_attrib ""
    let a_onloadstart = event_attrib "onloadstart"
    let a_onmessage = event_attrib "onmessage"

    (* Other Attributes *)

    let a_version = string_attrib "version"
    let a_xmlns = function
      | `W3_org_1999_xhtml -> string_attrib "xmlns" "http://www.w3.org/1999/xhtml"
    let a_manifest = string_attrib "manifest"

    let a_cite = string_attrib "cite"
    let a_xml_space = function
      | `Preserve -> string_attrib "xml:space" "preserve"

    let a_accesskey c = string_attrib "accesskey" (String.make 1 c)
    let a_charset = string_attrib "charset"
    let a_accept_charset = space_sep_attrib "accept-charset"
    let a_accept = space_sep_attrib "accept"
    let a_href = string_attrib "href"
    let a_hreflang = string_attrib "hreflang"
    let a_rel = linktypes_attrib "rel"
    let a_tabindex = int_attrib "tabindex"

    let a_mime_type =  string_attrib "type"

    let a_alt = string_attrib "alt"
    let a_height p = int_attrib "height" p
    let a_src = string_attrib "src"
    let a_width p = int_attrib "width" p

    let a_for = string_attrib "for"
    let a_for_list = space_sep_attrib "for"
    let a_selected = function
      | `Selected -> string_attrib "selected" "selected"
    let a_text_value = string_attrib "value"
    let a_int_value = int_attrib "value"
    let a_value = string_attrib "value"
    let a_float_value = float_attrib "value"
    let a_action = string_attrib "action"
    let a_method m =
      string_attrib "method" (match m with
                                | `Get ->  "GET"
                                | `Post -> "POST"
                                | `Put -> "PUT"
                                | `Delete -> "DELETE")
    let a_enctype = string_attrib "enctype"

    let a_checked `Checked = string_attrib "checked" "checked"
    let a_disabled `Disabled = string_attrib "disabled" "disabled"
    let a_readonly `Readonly = string_attrib "readonly" "readonly"
    let a_maxlength = int_attrib "maxlength"
    let a_name = string_attrib "name"

    let a_autocomplete ac = string_attrib "autocomplete"
      (match ac with
         | `On -> "on"
         | `Off -> "off")
    let a_async `Async = string_attrib "async" "async"
    let a_autofocus `Autofocus = string_attrib "autofocus" "autofocus"
    let a_autoplay `Autoplay = string_attrib "autoplay" "autoplay"
    let a_challenge = string_attrib "challenge"
    let a_contenteditable ce = string_attrib "contexteditable"
      (match ce with
         | `True -> "true"
         | `False -> "false")
    let a_contextmenu = string_attrib "contextmenu"
    let a_controls `Controls = string_attrib "controls" "controls"
    let a_dir d = string_attrib "dir"
      (match d with
         | `Ltr -> "ltr"
         | `Rtl -> "rtl")
    let a_draggable d = string_attrib "draggable"
      (match d with
         | `True -> "true"
         | `False -> "false")
    let a_form = string_attrib "form"
    let a_formaction = string_attrib "formaction"
    let a_formenctype = string_attrib "formenctype"
    let a_formmethod m =
      string_attrib "method" (match m with
                                | `Get ->  "GET"
                                | `Post -> "POST"
                                | `Put -> "PUT"
                                | `Delete -> "DELETE")
    let a_formnovalidate `Formnovalidate =
      string_attrib "formnovalidate" "formnovalidate"
    let a_formtarget = string_attrib "formtarget"
    let a_hidden `Hidden = string_attrib "hidden" "hidden"
    let a_high = float_attrib "high"
    let a_icon = string_attrib "icon"
    let a_ismap `Ismap = string_attrib "ismap" "ismap"
    let a_keytype = string_attrib "keytype"
    let a_list = string_attrib "list"
    let a_loop `Loop = string_attrib "loop" "loop"
   let a_low = float_attrib "low"
   let a_max = float_attrib "max"
   let a_input_max = int_attrib "max"
   let a_min = float_attrib "min"
   let a_input_min = int_attrib "min"
   let a_novalidate `Novalidate = string_attrib "novalidate" "novalidate"
   let a_open `Open = string_attrib "open" "open"
   let a_optimum = float_attrib "optimum"
   let a_pattern = string_attrib "pattern"
   let a_placeholder = string_attrib "placeholder"
   let a_poster = string_attrib "poster"
   let a_preload pl = string_attrib "preload"
     (match pl with
        | `None -> "none"
        | `Metadata -> "metadata"
        | `Audio -> "audio")

   let a_pubdate `Pubdate = string_attrib "pubdate" "pubdate"
   let a_radiogroup = string_attrib "radiogroup"
   let a_required `Required = string_attrib "required" "required"
   let a_reserved `Reserved = string_attrib "reserved" "reserved"
   let rec a_sandbox sb = 
     let rec aux sb =
       (match sb with
          | `AllowSameOrigin::a -> "allow-same-origin"::aux a
          | `AllowForms::a -> "allow-forms"::aux a 
          | `AllowScript::a -> "allow-script"::aux a
          | [] -> [])
     in space_sep_attrib "sandbox" (aux sb)

   let a_spellcheck sc = string_attrib "spellckeck"
     (match sc with
        | `True -> "true"
        | `False -> "false")
   let a_scoped `Scoped = string_attrib "scoped" "scoped"
   let a_seamless `Seamless = string_attrib "seamless" "seamless"
   let a_sizes sizes =
     string_attrib "sizes" (String.concat " "
                             (List.map string_of_int sizes))
   let a_span = int_attrib "span"
   (*let a_srcdoc*)
   let a_srclang = string_attrib "xml:lang"
   let a_start = int_attrib "start"
   let a_step = float_attrib "step"
   let a_wrap w = string_attrib "wrap" (match w with
        | `Soft -> "soft"
        | `Hard -> "hard")

    let a_size = int_attrib "size"
    let a_input_type it =
      string_attrib "type"
        (match it with
        | `Url -> "url"
        | `Tel -> "tel"
        | `Text -> "text"
        | `Time -> "time"
        | `Search -> "search"
        | `Password -> "password"
        | `Checkbox -> "checkbox"
        | `Range -> "range"
        | `Radio -> "radio"
        | `Submit -> "submit"
        | `Reset -> "reset"
        | `Number -> "number"
        | `Month -> "month"
        | `Week -> "week"
        | `File -> "file"
        | `Email -> "email"
        | `Image -> "image"
        | `Date -> "date"
        | `Datetime -> "datetime"
        | `Datetime_local -> "datetime-locale"
        | `Color -> "color"
        | `Button -> "button"
        | `Hidden -> "hidden") 
    let a_menu_type mt = string_attrib "type" (match mt with
        | `Context -> "context"
        | `Toolbar -> "toolbar")
    let a_command_type ct = string_attrib "type" (match ct with
        | `Command -> "command"
        | `Checkbox -> "checkbox"
        | `Radio -> "radio")
    let a_button_type bt =
      string_attrib "type"
        (match bt with
        | `Button -> "button"
        | `Submit -> "submit"
        | `Reset -> "reset")
    let a_multiple = function
      | `Multiple -> string_attrib "multiple" "multiple"
    let a_cols = int_attrib "cols"
    let a_rows = int_attrib "rows"

    let a_summary = string_attrib "summary"

    let a_align a =
      string_attrib "align"
        (match a with
        | `Left -> "left"
        | `Right -> "right"
        | `Justify -> "justify"
        | `Char -> "char")
    let a_axis = string_attrib "axis"
    let a_colspan = int_attrib "colspan"
    let a_headers = space_sep_attrib "headers"
    let a_rowspan = int_attrib "rowspan"
    let a_scope s =
      string_attrib "scope"
        (match s with
        | `Row -> "row"
        | `Col -> "col"
        | `Rowgroup -> "rowgroup"
        | `Colgroup -> "colgroup")


    let a_border = int_attrib "border"
    let a_cellpadding = length_attrib "cellpadding"
    let a_cellspacing = length_attrib "cellspacing"
    let a_datapagesize = string_attrib "datapagesize"
    let a_rules r =
      string_attrib "rules"
        (match r with
        | `None -> "none"
        | `Groups -> "groups"
        | `Rows -> "rows"
        | `Cols -> "cols"
        | `All -> "all")
    let a_char c = string_attrib "char" (String.make 1 c)
    let a_charoff = length_attrib "charoff"

    let a_data = string_attrib "data"
    let a_codetype = string_attrib "codetype"

    let a_fs_rows mls = multilengths_attrib "rows" mls
    let a_fs_cols mls = multilengths_attrib "cols" mls
    let a_frameborder b =
      int_attrib "frameborder" (match b with `Zero -> 0 | `One -> 1)
    let a_marginheight = int_attrib "marginheight"
    let a_marginwidth = int_attrib "marginwidth"
    let a_scrolling s =
      string_attrib "scrolling"
        (match s with
        | `Yes -> "yes"
        | `No -> "no"
        | `Auto -> "auto")

    let a_target = string_attrib "target"

    let a_content = string_attrib "content"
    let a_http_equiv = string_attrib "http-equiv"

    let a_media = mediadesc_attrib "media"

    type 'a elt = XML.elt

    type html = [`Html] elt

    (* NB: These are more general than the ones in xHTML.mli *)

    type ('a, 'b) nullary = ?a:('a attrib list) -> unit -> 'b elt
    type ('a, 'b, 'c) unary = ?a:('a attrib list) -> 'b elt -> 'c elt
    type ('a, 'b, 'c, 'd) binary = ?a:('a attrib list) -> 'b elt -> 'c elt -> 'd elt
    type ('b, 'c, 'd, 'e) tri =  'b elt -> 'c elt -> 'd elt -> 'e elt
    type ('a, 'b, 'c) star = ?a:('a attrib list) -> 'b elt list -> 'c elt
    type ('a, 'b, 'c) plus = ?a:('a attrib list) -> 'b elt -> 'b elt list -> 'c elt

    let terminal tag ?a () = XML.leaf ?a tag
    (* let nullary tag ?a () = XML.node ?a tag [] *)
    let unary tag ?a elt = XML.node ?a tag [elt]
    let binary tag ?a elt1 elt2 = XML.node ?a tag [elt1; elt2]
    let tri tag elt1 elt2 elt3 = XML.node tag [elt1; elt2; elt3]
    let star tag ?a elts = XML.node ?a tag elts
    let plus tag ?a elt elts = XML.node ?a tag (elt :: elts)

    let list_of_option = function
      | Some x -> [x]
      | None -> []
    let list_of_list_option = function
      | Some x -> x
      | None -> []

    let srcs_option = function
      | Some (`Srcs s) -> s
      | None -> []
    let phrasing_option = function
      | Some (`Phras p) -> p
      | None -> []
    let ruby_option = function
      | Some (`Rt_elt r) -> r
      | Some (`Group g) -> g
      | None -> []
    let body_option = function
      | Some (`Body b) -> b
      | Some (`Trs t) -> t
      | None -> []
    let colg_option = function
      | Some (`Colgroups c) -> c
      | None -> []
    let opts_option = function
      | Some (`Options o) -> o
      | Some (`Optgroups o) -> o
      | None -> []
    let li_option = function
      | Some (`Lis l) -> l
      | Some (`Flows f) -> f
      | None -> []
    let opt_option = function
      | Some (`Options o) -> o
      | Some (`Phras p) -> p
      | None -> []
    let param_option = function
      | Some (`Params p) -> p
      | None -> []
    let cols_option = function
      | Some (`Cols c) -> c
      | Some (`Colgroups c) -> c
      | None -> []

    let body = star "body"
    let head = plus "head"
    let title = unary "title"
    let html = binary "html"

    let footer = star "footer"
    let header = star "header"
    let section = star "section"
    let nav = star "nav"

    let pcdata = XML.pcdata
    let entity = XML.entity

    let space () = entity "nbsp"

    let cdata = XML.cdata

    let cdata_script = XML.cdata_script

    let cdata_style = XML.cdata_style

    let unsafe_data s = XML.encodedpcdata s

    let unsafe_data s = XML.encodedpcdata s

    let h1 = star "h1"
    let h2 = star "h2"
    let h3 = star "h3"
    let h4 = star "h4"
    let h5 = star "h5"
    let h6 = star "h6"
    let hgroup = plus "hgroup"

    let address = star "address"
    let blockquote = star "blockquote"
    let div = star "div"
    let p = star "p"
    let pre = star "pre"

    let abbr = star "abbr"
    let br = terminal "br"
    let cite = star "cite"
    let code = star "code"
    let dfn = star "dfn"
    let em = star "em"
    let kbd = star "kbd"
    let q = star "q"
    let samp = star "samp"
    let span = star "span"
    let strong = star "strong"
    let time = star "time"
    let var = star "var"

    let a = star "a"
    let a_flow = star "a"

    let dl = star "dl"
    let ol = star "ol"
    let ul = star "ul"
    let dd = star "dd"
    let dt = star "dt"
    let li = star "li"

    let hr = terminal "hr"
    let b = star "b"
    let i = star "i"
    let small = star "small"
    let sub = star "sub"
    let sup = star "sup"
    let mark = star "mark"
    let rp ?(a = []) elts = (a, elts)
    let rt ?rp ?a elts=
      (match rp with
         | Some ((a1, e1), (a2, e2)) ->
              `Rpt (XML.node ~a:a1 "rp" e1,
                   XML.node ?a "rt" elts,
                   XML.node ~a:a2 "rp" e2)
         | None -> `Rt (XML.node ?a "rt" elts))

    let ruby ?a elt elts = 
      let rec aux = function
        | [] -> []
        | (pel, `Rt e)::l -> pel@(e::aux l)
        | (pel, `Rpt (e1, e2, e3))::l -> pel@(e1::e2::e3::aux l)
      in
      XML.node ?a "ruby" (aux (elt :: elts))

    let wbr = terminal "wbr"

(* VB *)
    type shape = [ `Rect | `Circle | `Poly | `Default ]

    let bdo ~dir ?(a = []) elts =
      XML.node ~a:(a_dir dir :: a) "bdo" elts

    let a_datetime = string_attrib "datetime"

    let a_shape d =
      string_attrib "shape"
        (match d with
          `Rect -> "rect"
        | `Circle -> "circle"
        | `Poly -> "poly"
        | `Default -> "default")
    let a_coords coords = 
      string_attrib "coords" (String.concat ","
                                (List.map string_of_int coords))

    let a_usemap = string_attrib "usemap"
    let a_defer `Defer = string_attrib "defer" "defer"
    let a_label = string_attrib "label"

    let area ~alt ?(a = []) () =
      XML.leaf ~a:(a_alt alt :: a) "area"
    let map = plus "map"
    let map_flow = plus "map"
    let del = star "del"
    let del_flow = star "del"
    let ins = star "ins"
    let ins_flow = star "ins"
    let script ~contenttype ?(a = []) elt =
      XML.node ~a:(a_mime_type contenttype :: a) "script" [elt]
    let noscript = plus "noscript"

    let article = star "article"
    let aside = star "aside"
 
   let audio ?srcs ?a elts =
      XML.node ?a "audio"
        (srcs_option srcs @ elts)
    let audio_flow ?srcs ?a elts =
      XML.node ?a "audio"
        (srcs_option srcs @ elts)
    let audio_src ~src ?(a = []) elts =
      XML.node ~a:(a_src src :: a) "audio" elts
    let audio_src_flow ~src ?(a = []) elts =
      XML.node ~a:(a_src src :: a) "audio" elts

    let video ?srcs ?a elts =
      XML.node ?a "video"
        (srcs_option srcs @ elts)
    let video_flow ?srcs ?a elts =
      XML.node ?a "video"
        (srcs_option srcs @ elts) 
    let video_src ~src ?(a = []) elts =
      XML.node ~a:(a_src src :: a) "video" elts
    let video_src_flow ~src ?(a = []) elts =
      XML.node ~a:(a_src src :: a) "video" elts

    let canvas = star "canvas"
    let canvas_flow = star "canvas"
    let command ~label ?(a = []) () =
      XML.leaf ~a:(a_label label :: a) "command"
    let menu ?child ?a () =
      XML.node ?a "menu" (li_option child)
    let embed = terminal "embed"
    let source = terminal "source"
    let meter = star "meter"
    let output_elt = star "output"

    let form = plus "form"
    let input = terminal "input"
    let keygen = terminal "keygen"
    let label = star "label"
    let option = unary "option"
    let select = star "select"
    let textarea = unary "textarea"
    let button = star "button"
    let datalist ?child ?a ()=
      XML.node ?a "datalist" (opt_option child)
    let progress = star "proress"
    let legend = star "legend"
    let details = plus "details"
    let summary = star "summary"
    let fieldset ?legend ?a elts =
      XML.node ?a "fieldset" (list_of_option legend @ elts)
    let optgroup ~label ?(a = []) elts =
      XML.node ~a:(a_label label :: a) "optgroup" elts

    let figcaption = star "figcaption"

    let figure ?figcaption ?a elts =
      XML.node ?a "figure" (list_of_option figcaption @ elts)

    let caption = star "caption"

    let table ?caption ?columns ?thead ?tfoot ?a elt elts =
      XML.node ?a "table"
        (list_of_option caption @ colg_option columns @
           list_of_option thead @ list_of_option tfoot @ elt ::elts)

    let tablex ?caption ?columns ?thead ?tfoot ?a elts =
      XML.node ?a "table"
        (list_of_option caption @ colg_option columns @
         list_of_option thead @ list_of_option tfoot @ elts)

    let td = star "td"
    let th = star "th"

    let tr = star "tr"

    let colgroup = star "colgroup"
    let col = terminal "col"
    let thead = star "thead"
    let tbody = star "tbody"
    let tfoot = star "tfoot"

    let iframe = star "iframe"
    let object_ ?param ?a elts =
      XML.node ?a "object" (param_option param @ elts)
    let object_flow ?param ?a elts =
      XML.node ?a "object" (param_option param @ elts)
    let param = terminal "param"

    let img ~src ~alt ?(a = []) () =
      XML.leaf ~a:(a_src src :: a_alt alt :: a) "img"

    let meta = terminal "meta"

   let style ~contenttype ?(a = []) elts =
      XML.node ~a:(a_mime_type contenttype :: a) "style" elts

    let link ~href ~rel ?(a = []) () =
      XML.leaf ~a:(a_href href :: a_rel rel :: a) "link"

    let base = terminal "base"

(* VB *)
    type edit = [ `Ins_flow | `Ins | `Del | `Del_flow]
    type scripttag = [ `Script | `Noscript ]
    type misc = [ edit | scripttag ]

    type heading = [`H1 | `H2 | `H3 | `H4 | `H5 | `H6 | `Hgroup]
    type sectioning = [ `Section | `Nav | `Aside | `Article]
    type resetable = [ `Textarea | `Select | `Output | `Keygen | `Input ]
    type submitable = [`Textarea | `Select | `Object | `Keygen | `Input| `Button ]
    type embedded = [`Video | `Video_src|(* `Math |`Svg |*) `Object | `Img | `Iframe | `Embed | `Canvas | `Audio| `Audio_src]

    type labelable = [ resetable | `Progress | `Meter | `Button]
    type formatblock = [ heading | sectioning | `Pre | `P | `Header | `Footer | `Div | `Blockquote | `Address ]
    type sectionningroot = [`Td | `Figure | `Fieldset | `Details | `Body | `Blockquote]
    type listed = [resetable | submitable | `Fieldset]
    type formassociated = [ listed | `Progress | `Meter | `Label]
    type metadata_without_title = [ `Style | `Script | `Noscript | `Meta | `Link | `Command | `Base]
    type metadata = [ metadata_without_title | `Title ]
        (******************)
        (* Video 
           Type
           Object
           Menu
           Input
           Img (if usemap attribute)
           Audio          *)
        (* in Interactive *)
        (* with conditions*)
        (******************)
    type interactive = [ `Video |`Video_src| `Textarea | `Select | `Object | `Menu | `Label | `Keygen | `Input | `Img | `Iframe | `Embed | `Details | `Button | `Audio | `Audio_src | `A ]
    type interactive_flow = [ interactive | `Video_flow | `Video_src_flow| `Object_flow | `Audio_flow | `Audio_src_flow | `A_flow ]

    type phrasing_without_dfn = [embedded | labelable | submitable | `Wbr | `Var | `Time | `Sup | `Sub | `Strong | `Span | `Small | `Script | `Samp | `Ruby | `Q | `Noscript | `Mark | `Map |`Label| `Kbd | `Ins | `I |`Em | `Del | `Datalist | `Command | `Code | `Cite | `Br | `Bdo | `B | `Abbr | `A | `A_flow |`PCDATA ]
    type phrasing_without_interactive =  [embedded | labelable | submitable | `Wbr | `Var | `Time | `Sup | `Sub | `Strong | `Span | `Small | `Script | `Samp | `Ruby | `Q | `Noscript | `Mark | `Map | `Kbd | `Ins | `I |`Em | `Dfn | `Ins | `Del | `Datalist | `Command | `Code | `Cite | `Br | `Bdo | `B | `Abbr |`PCDATA ]
    type phrasing_without_lab_form_and_label = [embedded | `Wbr | `Var | `Time | `Sup | `Sub | `Strong | `Span | `Small | `Script | `Samp | `Ruby | `Q | `Noscript | `Mark | `Map | `Kbd | `Ins | `I |`Em | `Dfn | `Ins | `Del | `Datalist | `Command | `Code | `Cite | `Br | `Bdo | `B | `Abbr | `A |`PCDATA ]
    type phrasing_without_progress = [embedded | resetable | submitable | `Wbr | `Var | `Time | `Sup | `Sub | `Strong | `Span | `Small | `Script | `Samp | `Ruby | `Q | `Noscript | `Meter | `Mark | `Map |`Label| `Kbd | `Ins | `I |`Em | `Dfn | `Del | `Datalist | `Command | `Code | `Cite |`Button | `Br | `Bdo | `B | `Abbr | `A |`PCDATA ]
    type phrasing_without_time = [embedded | labelable | submitable | `Wbr | `Var | `Sup | `Sub | `Strong | `Span | `Small | `Script | `Samp | `Ruby | `Q | `Noscript | `Mark | `Map |`Label| `Kbd | `I |`Em | `Dfn | `Ins | `Del | `Datalist | `Command | `Code | `Cite | `Br | `Bdo | `B | `Abbr | `A |`PCDATA ]
    type phrasing_without_media = [ labelable | submitable |(*a ajouter* `Math |`Svg |*) `Object | `Img | `Iframe | `Embed | `Canvas | `Wbr | `Var | `Time | `Sup | `Sub | `Strong | `Span | `Small | `Script | `Samp | `Ruby | `Q | `Noscript | `Mark | `Map |`Label| `Kbd | `I | `Ins |`Em | `Dfn | `Del | `Datalist | `Command | `Code | `Cite | `Br | `Bdo | `B | `Abbr | `A |`PCDATA ]
    type phrasing_without_meter = [embedded | submitable | resetable | `Progress | `Button| `Wbr | `Var | `Time | `Sup | `Sub | `Strong | `Span | `Small | `Script | `Samp | `Ruby | `Q | `Noscript | `Mark | `Map |`Label| `Kbd | `Ins | `I |`Em | `Dfn | `Del | `Datalist | `Command | `Code | `Cite | `Br | `Bdo | `B | `Abbr | `A |`PCDATA ]
    type phrasing_without_noscript = [embedded | labelable | submitable | `Wbr | `Var | `Time | `Sup | `Sub | `Strong | `Span | `Small | `Script | `Samp | `Ruby | `Q | `Mark | `Map |`Label| `Kbd | `I |`Em | `Dfn | `Ins | `Del | `Datalist | `Command | `Code | `Cite | `Br | `Bdo | `B | `Abbr | `A |`PCDATA ]
        (******************)
        (* Map  Ins
           Del  A         *)
        (*  in Phrasing   *)
        (* with conditions*)
        (******************)
    type phrasing = [embedded | labelable | submitable | `Wbr | `Var | `Time | `Sup | `Sub | `Strong | `Span | `Small | `Script | `Samp | `Ruby | `Q | `Noscript | `Mark | `Map |`Label| `Kbd  | `I |`Em | `Dfn | `Ins | `Del | `Datalist | `Command | `Code | `Cite | `Br | `Bdo | `B | `Abbr | `A | `PCDATA ]

    type flow5_without_interactive = [ phrasing_without_interactive | formassociated | formatblock | `Ul | `Table |`Style | `Ol | `Menu | `Map |`Map_flow| `Hr | `Form |`Figure | `Dl| `Ins |`Ins_flow | `Del| `Del_flow| `Canvas_flow]
    type flow5_without_table = [ phrasing | interactive_flow | formassociated | formatblock | `Ul |`Style | `Ol | `Menu | `Map |`Map_flow| `Hr | `Form |`Figure | `Dl| `Ins |`Ins_flow | `Del| `Del_flow| `Canvas_flow]
    type flow5_without_header_footer = [ heading | sectioning | `Pre | `P | `Div | `Blockquote | `Address | phrasing | interactive_flow | formassociated | `Ul | `Table |`Style | `Ol | `Menu | `Map |`Map_flow| `Hr | `Form |`Figure | `Ins |`Ins_flow | `Dl| `Del| `Del_flow| `Canvas_flow]
    type flow5_without_form = [ phrasing | interactive_flow | formassociated | formatblock | `Ul | `Table |`Style | `Ol | `Menu | `Map |`Map_flow| `Hr |`Figure | `Ins |`Ins_flow | `Dl| `Ins |`Ins_flow | `Del| `Del_flow| `Canvas_flow]
    type flow5_without_sectioning_heading_header_footer_address = [ phrasing | interactive_flow | formassociated | `Pre | `P | `Div | `Blockquote| `Ul | `Table |`Style | `Ol | `Menu | `Map |`Map_flow| `Hr | `Form |`Figure | `Dl| `Ins |`Ins_flow | `Del| `Del_flow| `Canvas_flow]

    type flow5_without_media = [phrasing_without_media |  `Textarea | `Select |`Object_flow | `Object | `Menu | `Label | `Keygen | `Input | `Img | `Iframe | `Embed | `Details | `Button | `A | `A_flow | formassociated | formatblock | `Ul | `Table |`Style | `Ol | `Menu | `Map |`Map_flow| `Hr | `Form |`Figure | `Dl| `Ins |`Ins_flow | `Del | `Del_flow]

        (******************)
        (* Style  in Flow *)
        (* with conditions*)
        (******************)
    type flow5 = [ phrasing | interactive_flow | formassociated | formatblock | `Ul | `Table |`Style | `Ol | `Menu | `Map |`Map_flow| `Ins |`Ins_flow | `Hr | `Form |`Figure | `Dl| `Del| `Del_flow| `Canvas_flow]


    type rt = 
      [ `Rt of [ `Rt ] elt
      | `Rpt of ( [ `Rp ] elt * [ `Rt ] elt * [ `Rp ] elt )]
    type ruby_content = phrasing elt list * rt
    type rp = (common attrib list * phrasing  elt list)


    (* I/O *)

    let compose_doctype dt args =
      "<!DOCTYPE " ^ dt ^ " PUBLIC " ^
      String.concat " " (List.map (fun a -> "\"" ^ a ^ "\"") args) ^ ">\n"

    type doctypes = 
        [ `HTML_v03_02 | `HTML_v04_01 | `XHTML_01_00 | `XHTML_01_01
        | `XHTML_05_00 | `Doctype of string ]

    let doctype = function
      | `HTML_v03_02 ->
          compose_doctype "html" ["-//W3C//DTD HTML 3.2 Final//EN"]
      | `HTML_v04_01 ->
          compose_doctype "html" ["-//W3C//DTD HTML 4.01//EN";
                                  "http://www.w3.org/TR/html4/strict.dtd"]
      | `XHTML_01_00 ->
          compose_doctype "html" ["-//W3C//DTD XHTML 1.0 Strict//EN";
                                  "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd"]
      | `XHTML_01_01 ->
          compose_doctype "html" ["-//W3C//DTD XHTML 1.1//EN";
                                  "http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd"]
(*VVV Vérifier !!! *)
      | `XHTML_05_00 -> "<!DOCTYPE html>"
      | `Doctype s -> s

    let no_break =
      ["title";
       "h1"; "h2"; "h3"; "h4"; "h5"; "h6";
       "address"; "blockquote"; "div"; "p";
       "li"; "dd"; "dt"; "td"; "th"]

    let preformatted =
      ["pre"]

    (* Tools *)

    let version = function
      | `XHTML_01_00 -> "XHTML 1.0"
      | `XHTML_01_01 -> "XHTML 1.1"
      | `XHTML_05_00 -> "XHTML 5.0"

    let standard = function
      | `XHTML_01_00 -> "http://www.w3.org/TR/xhtml1/"
      | `XHTML_01_01 -> "http://www.w3.org/TR/xhtml11/"
      | `XHTML_05_00 -> "http://www.w3.org/TR/xhtml5"

(*
    let validator =
      "http://validator.w3.org/check/referer"

    let compose_validator_icon icon alt =
      a_flow ~a:[a_href validator]
        [img ~src:icon ~alt ~a:[a_height ((*`Pixels*) 31); a_width ((*`Pixels*) 88)] ()]

    let validator_icon = function
      | `XHTML_01_00 -> compose_validator_icon
            "http://www.w3.org/Icons/valid-xhtml10" "Valid XHTML 1.0!"
      | `XHTML_01_01 -> compose_validator_icon
            "http://www.w3.org/Icons/valid-xhtml11" "Valid XHTML 1.1!"
      | `XHTML_05_00 -> ???
*)

    (******************************************************************)
    (* In the following, my own stuffs for Ocsigen -- Vincent: *)

    let tot x = x
    let totl x = x
    let toelt x = x
    let toeltl x = x

  end

module M_05_00 : T_05_00 =
  struct
    module M = Version
    include M
    let xhtml_version = `XHTML_05_00
    let version = M.version xhtml_version
    let standard = M.standard xhtml_version
  end

module M = M_05_00



