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
(* TODO :
   - MathML and SVG
   - forbid construction like that noscript (a [a []])
   by playing on interactive_without*
*)
(* IDEAS:
     The [a_] prefix would have to be maintained and the
     only advantage are a potentially better mapping of the XHTML modularization
     to O'Caml modules. *)
(** Typesafe constructors for HTML5 documents.
    @see <http://www.w3.org/TR/html5/> W3C Recommendation *)
open Xhtml5types
  
module type T =
  sig
    (** Uri — Alias for the module Uri *)
    type uri = Uri.uri
    type uris = Uri.uris
    val string_of_uri: uri -> string
    val uri_of_string: string -> uri

    (** {1 Common Attributes} *)
    type +'a attrib
    
    type +'a attribs
    
    val to_xmlattribs : ('a attrib) list -> XML.attrib list
      
    (* VB *)
    (** ['a] is known as a {i phantom type}.  The implementation is
       actually monomorphic (the different element types are distinguished
       by a homogeneous variable, such as their textual representation)
       and the type variable [`a] is just used by the type checker.

       NB: It might be possible to use polymorphic variants directly, without
       phantom types, but the implementation is likely to be more involved. *)
    val a_autocomplete : [< | `On | `Off] -> [> | `Autocomplete] attrib
      
    val a_async : [< | `Async] -> [> | `Async] attrib
      
    val a_autofocus : [< | `Autofocus] -> [> | `Autofocus] attrib
      
    val a_autoplay : [< | `Autoplay] -> [> | `Autoplay] attrib
      
    val a_challenge : text -> [> | `Challenge] attrib
      
    val a_contenteditable :
      [< | `True | `False] -> [> | `Contexteditable] attrib
      
    val a_contextmenu : idref -> [> | `Contextmenu] attrib
      
    val a_controls : [< | `Controls] -> [> | `Controls] attrib
      
    val a_dir : [< | `Rtl | `Ltr] -> [> | `Dir] attrib
      
    val a_draggable : [< | `True | `False] -> [> | `Draggable] attrib
      
    val a_form : idref -> [> | `Form] attrib
      
    val a_formaction : uri -> [> | `Formaction] attrib
      
    val a_formenctype : contenttype -> [> | `Formenctype] attrib
      
    val a_formmethod :
      [< | `Get | `Post | `Put | `Delete] -> [> | `Formmethod] attrib
      
    val a_formnovalidate :
      [< | `Formnovalidate] -> [> | `Formnovalidate] attrib
      
    val a_formtarget : text -> [> | `Formtarget] attrib
      
    val a_hidden : [< | `Hidden] -> [> | `Hidden] attrib
      
    val a_high : float_number -> [> | `High] attrib
      
    val a_icon : uri -> [> | `Icon] attrib
      
    val a_ismap : [< | `Ismap] -> [> | `Ismap] attrib
      
    val a_keytype : text -> [> | `Keytype] attrib
      
    val a_list : idref -> [> | `List] attrib
      
    val a_loop : [< | `Loop] -> [> | `Loop] attrib
      
    val a_low : float_number -> [> | `High] attrib
      
    val a_max : float_number -> [> | `Max] attrib
      
    val a_input_max : number -> [> | `Max] attrib
      
    val a_min : float_number -> [> | `Min] attrib
      
    val a_input_min : number -> [> | `Min] attrib
      
    val a_novalidate : [< | `Novalidate] -> [> | `Novalidate] attrib
      
    val a_open : [< | `Open] -> [> | `Open] attrib
      
    val a_optimum : float_number -> [> | `Optimum] attrib
      
    val a_pattern : text -> [> | `Pattern] attrib
      
    val a_placeholder : text -> [> | `Placeholder] attrib
      
    val a_poster : uri -> [> | `Poster] attrib
      
    val a_preload : [< | `None | `Metadata | `Audio] -> [> | `Preload] attrib
      
    val a_pubdate : [< | `Pubdate] -> [> | `Pubdate] attrib
      
    val a_radiogroup : text -> [> | `Radiogroup] attrib
      
    val a_required : [< | `Required] -> [> | `Required] attrib
      
    val a_reversed : [< | `Reversed] -> [> | `Reversed] attrib
      
    val a_sandbox :
      [< | `AllowSameOrigin | `AllowForms | `AllowScript] list ->
        [> | `Sandbox] attrib
      
    val a_spellcheck : [< | `True | `False] -> [> | `Spellcheck] attrib
      
    val a_scoped : [< | `Scoped] -> [> | `Scoped] attrib
      
    val a_seamless : [< | `Seamless] -> [> | `Seamless] attrib
      
    val a_sizes : numbers -> [> | `Sizes] attrib
      
    val a_span : number -> [> | `Span] attrib
      
    (*val a_srcdoc*)
    val a_srclang : nmtoken -> [> | `XML_lang] attrib
      
    val a_start : number -> [> | `Start] attrib
      
    val a_step : float_number -> [> | `Step] attrib
      
    val a_wrap : [< | `Soft | `Hard] -> [> | `Wrap] attrib
      
    val a_class : nmtokens -> [> | `Class] attrib
      
    (** This attribute assigns a class name or set of class names to an
    element. Any number of elements may be assigned the same class
    name or names.  *)
    val a_user_data : nmtoken -> text -> [> | `User_data] attrib
      
    (** May be used to specify custom attribs.
    The example given by the W3C is as follows :
    {v <ol>
    <li data-length="2m11s">Beyond The Sea</li>
    </ol> v}
    It should be used for preprocessing ends only. *)
    val a_id : text -> [> | `Id] attrib
      
    (** This attribute assigns a name to an element. This name must be
    unique in a document. The text should be without any space. *)
    val a_title : text -> [> | `Title] attrib
      
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
    val a_xml_lang : nmtoken -> [> | `XML_lang] attrib
      
    (** {2 Events} *)
    (** Javascript events *)
    val a_onabort : XML.event -> [> | `OnAbort] attrib
      
    val a_onafterprint : XML.event -> [> | `OnAfterPrint] attrib
      
    val a_onbeforeprint : XML.event -> [> | `OnBeforePrint] attrib
      
    val a_onbeforeunload : XML.event -> [> | `OnBeforeUnload] attrib
      
    val a_onblur : XML.event -> [> | `OnBlur] attrib
      
    val a_oncanplay : XML.event -> [> | `OnCanPlay] attrib
      
    val a_oncanplaythrough : XML.event -> [> | `OnCanPlayThrough] attrib
      
    val a_onchange : XML.event -> [> | `OnChange] attrib
      
    val a_onclick : XML.event -> [> | `OnClick] attrib
      
    val a_oncontextmenu : XML.event -> [> | `OnContextMenu] attrib
      
    val a_ondblclick : XML.event -> [> | `OnDblClick] attrib
      
    val a_ondrag : XML.event -> [> | `OnDrag] attrib
      
    val a_ondragend : XML.event -> [> | `OnDragEnd] attrib
      
    val a_ondragenter : XML.event -> [> | `OnDragEnter] attrib
      
    val a_ondragleave : XML.event -> [> | `OnDragLeave] attrib
      
    val a_ondragover : XML.event -> [> | `OnDragOver] attrib
      
    val a_ondragstart : XML.event -> [> | `OnDragStart] attrib
      
    val a_ondrop : XML.event -> [> | `OnDrop] attrib
      
    val a_ondurationchange : XML.event -> [> | `OnDurationChange] attrib
      
    val a_onemptied : XML.event -> [> | `OnEmptied] attrib
      
    val a_onended : XML.event -> [> | `OnEnded] attrib
      
    val a_onerror : XML.event -> [> | `OnError] attrib
      
    val a_onfocus : XML.event -> [> | `OnFocus] attrib
      
    val a_onformchange : XML.event -> [> | `OnFormChange] attrib
      
    val a_onforminput : XML.event -> [> | `OnFormInput] attrib
      
    val a_onhashchange : XML.event -> [> | `OnHashChange] attrib
      
    val a_oninput : XML.event -> [> | `OnInput] attrib
      
    val a_oninvalid : XML.event -> [> | `OnInvalid] attrib
      
    val a_onmousedown : XML.event -> [> | `OnMouseDown] attrib
      
    val a_onmouseup : XML.event -> [> | `OnMouseUp] attrib
      
    val a_onmouseover : XML.event -> [> | `OnMouseOver] attrib
      
    val a_onmousemove : XML.event -> [> | `OnMouseMove] attrib
      
    val a_onmouseout : XML.event -> [> | `OnMouseOut] attrib
      
    val a_onmousewheel : XML.event -> [> | `OnMouseWheel] attrib
      
    val a_onoffline : XML.event -> [> | `OnOffLine] attrib
      
    val a_ononline : XML.event -> [> | `OnOnLine] attrib
      
    val a_onpause : XML.event -> [> | `OnPause] attrib
      
    val a_onplay : XML.event -> [> | `OnPlay] attrib
      
    val a_onplaying : XML.event -> [> | `OnPlaying] attrib
      
    val a_onpagehide : XML.event -> [> | `OnPageHide] attrib
      
    val a_onpageshow : XML.event -> [> | `OnPageShow] attrib
      
    val a_onpopstate : XML.event -> [> | `OnPopState] attrib
      
    val a_onprogress : XML.event -> [> | `OnProgress] attrib
      
    val a_onratechange : XML.event -> [> | `OnRateChange] attrib
      
    val a_onreadystatechange : XML.event -> [> | `OnReadyStateChange] attrib
      
    val a_onredo : XML.event -> [> | `OnRedo] attrib
      
    val a_onresize : XML.event -> [> | `OnResize] attrib
      
    val a_onscroll : XML.event -> [> | `OnScroll] attrib
      
    val a_onseeked : XML.event -> [> | `OnSeeked] attrib
      
    val a_onseeking : XML.event -> [> | `OnSeeking] attrib
      
    val a_onselect : XML.event -> [> | `OnSelect] attrib
      
    val a_onshow : XML.event -> [> | `OnShow] attrib
      
    val a_onstalled : XML.event -> [> | `OnStalled] attrib
      
    val a_onstorage : XML.event -> [> | `OnStorage] attrib
      
    val a_onsubmit : XML.event -> [> | `OnSubmit] attrib
      
    val a_onsuspend : XML.event -> [> | `OnSuspend] attrib
      
    val a_ontimeupdate : XML.event -> [> | `OnTimeUpdate] attrib
      
    val a_onundo : XML.event -> [> | `OnUndo] attrib
      
    val a_onunload : XML.event -> [> | `OnUnload] attrib
      
    val a_onvolumechange : XML.event -> [> | `OnVolumeChange] attrib
      
    val a_onwaiting : XML.event -> [> | `OnWaiting] attrib
      
    val a_onkeypress : XML.event -> [> | `OnKeyPress] attrib
      
    val a_onkeydown : XML.event -> [> | `OnKeyDown] attrib
      
    val a_onkeyup : XML.event -> [> | `OnKeyUp] attrib
      
    val a_onload : XML.event -> [> | `OnLoad] attrib
      
    val a_onloadeddata : XML.event -> [> | `OnLoadedData] attrib
      
    val a_onloadedmetadata : XML.event -> [> | `OnLoadedMetaData] attrib
      
    val a_onloadstart : XML.event -> [> | `OnLoadStart] attrib
      
    val a_onmessage : XML.event -> [> | `OnMessage] attrib
      
    val a_version : cdata -> [> | `Version] attrib
      
    val a_xmlns : [< | `W3_org_1999_xhtml] -> [> | `XMLns] attrib
      
    val a_manifest : uri -> [> | `Manifest] attrib
      
    val a_cite : uri -> [> | `Cite] attrib
      
    val a_xml_space : [< | `Preserve] -> [> | `XML_space] attrib
      
    val a_accesskey : character -> [> | `Accesskey] attrib
      
    (** This attribute assigns an access key to an element. An access key
    is a single character from the document character
    set. NB: authors should consider the input method of the
    expected reader when specifying an accesskey. *)
    val a_charset : charset -> [> | `Charset] attrib
      
    (** This attribute specifies the character encoding of the resource
    designated by the link. Please consult the section on character
    encodings for more details. *)
    val a_accept_charset : charsets -> [> | `Accept_charset] attrib
      
    val a_accept : contenttypes -> [> | `Accept] attrib
      
    val a_href : uri -> [> | `Href] attrib
      
    (** This attribute specifies the location of a Web resource, thus
    defining a link between the current element (the source anchor)
    and the destination anchor defined by this attribute. *)
    val a_hreflang : languagecode -> [> | `Hreflang] attrib
      
    (** This attribute specifies the base language of the resource
    designated by href and may only be used when href is specified. *)
    val a_rel : linktypes -> [> | `Rel] attrib
      
    (** This attribute describes the relationship from the current document
    to the anchor specified by the href attribute. The value of this attribute
    is a space-separated list of link types. *)
    (** This attribute is used to describe a reverse link from the anchor specified
    by the href attribute to the current document. The value of this attribute
    is a space-separated list of link types. *)
    val a_tabindex : number -> [> | `Tabindex] attrib
      
    (** This attribute specifies the position of the current element in
    the tabbing order for the current document. This value must be a
    number between 0 and 32767. User agents should ignore leading
    zeros. *)
    val a_mime_type : contenttype -> [> | `Mime_type] attrib
      
    (** This attribute gives an advisory hint as to the content type of
    the content available at the link target address. It allows user
    agents to opt to use a fallback mechanism rather than fetch the
    content if they are advised that they will get content in a
    content type they do not support.Authors who use this attribute
    take responsibility to manage the risk that it may become
    inconsistent with the content available at the link target
    address. *)
    val a_datetime : cdata -> [> | `Datetime] attrib
      
    val a_action : uri -> [> | `Action] attrib
      
    (** This attribute specifies a form processing agent. User agent
    behavior for a value other than an HTTP URI is undefined. *)
    val a_checked : [< | `Checked] -> [> | `Checked] attrib
      
    (** When the [type] attribute has the value ["radio"] or ["checkbox"],
    this boolean attribute specifies that the button is on. User
    agents must ignore this attribute for other control types. *)
    val a_cols : number -> [> | `Cols] attrib
      
    (** This attribute specifies the visible width in average character
    widths. Users should be able to enter longer lines than this, so
    user agents should provide some means to scroll through the
    contents of the control when the contents extend beyond the
    visible area. User agents may wrap visible text lines to keep long
    lines visible without the need for scrolling. *)
    val a_enctype : contenttype -> [> | `Enctype] attrib
      
    val a_for : idref -> [> | `For] attrib
      
    val a_for_list : idrefs -> [> | `For_List] attrib
      
    val a_maxlength : number -> [> | `Maxlength] attrib
      
    val a_method :
      [< | `Get | `Post | `Put | `Delete] -> [> | `Method] attrib
      
    val a_multiple : [< | `Multiple] -> [> | `Multiple] attrib
      
    val a_name : text -> [> | `Name] attrib
      
    (** This attribute assigns the control name. *)
    val a_rows : number -> [> | `Rows] attrib
      
    (** This attribute specifies the number of visible text lines. Users
    should be able to enter more lines than this, so user agents
    should provide some means to scroll through the contents of the
    control when the contents extend beyond the visible area. *)
    val a_selected : [< | `Selected] -> [> | `Selected] attrib
      
    (** When set, this boolean attribute specifies that this option is pre-selected. *)
    val a_size : number -> [> | `Size] attrib
      
    val a_src : uri -> [> | `Src] attrib
      
    val a_input_type :
      [<
        | `Url
        | `Tel
        | `Text
        | `Time
        | `Search
        | `Password
        | `Checkbox
        | `Range
        | `Radio
        | `Submit
        | `Reset
        | `Number
        | `Hidden
        | `Month
        | `Week
        | `File
        | `Email
        | `Image
        | `Datetime_local
        | `Datetime
        | `Date
        | `Color
        | `Button] -> [> | `Input_Type] attrib
      
    val a_text_value : text -> [> | `Text_Value] attrib
      
    (** This attribute specifies the initial value of the control. If this
    attribute is not set, the initial value is set to the contents of
    the [option] element. *)
    val a_int_value : number -> [> | `Int_Value] attrib
      
    (*VVV NO *)
    val a_value : cdata -> [> | `Value] attrib
      
    val a_float_value : float_number -> [> | `Float_Value] attrib
      
    val a_disabled : [< | `Disabled] -> [> | `Disabled] attrib
      
    val a_readonly : [< | `Readonly] -> [> | `Readonly] attrib
    val a_button_type :
      [< | `Button | `Submit | `Reset] -> [> | `Button_Type] attrib
      
    val a_command_type :
      [< | `Command | `Checkbox | `Radio] -> [> | `Command_Type] attrib
      
    val a_menu_type : [< | `Context | `Toolbar] -> [> | `Menu_Type] attrib
      
    val a_label : text -> [> | `Label] attrib
      
    val a_align :
      [< | `Left | `Right | `Justify | `Char] -> [> | `Align] attrib
      
    val a_axis : cdata -> [> | `Axis] attrib
      
    val a_colspan : number -> [> | `Colspan] attrib
      
    val a_headers : idrefs -> [> | `Headers] attrib
      
    val a_rowspan : number -> [> | `Rowspan] attrib
      
    val a_scope :
      [< | `Row | `Col | `Rowgroup | `Colgroup] -> [> | `Scope] attrib
      
    val a_summary : text -> [> | `Summary] attrib
      
    val a_border : pixels -> [> | `Border] attrib
      
    val a_cellpadding : length -> [> | `Cellpadding] attrib
      
    val a_cellspacing : length -> [> | `Cellspacing] attrib
      
    val a_datapagesize : cdata -> [> | `Datapagesize] attrib
      
    val a_rules :
      [< | `None | `Groups | `Rows | `Cols | `All] -> [> | `Rules] attrib
      
    val a_char : character -> [> | `Char] attrib
      
    val a_charoff : length -> [> | `Charoff] attrib
      
    val a_alt : text -> [> | `Alt] attrib
      
    val a_height : number -> [> | `Height] attrib
      
    val a_width : number -> [> | `Width] attrib
      
    type shape = [ | `Rect | `Circle | `Poly | `Default ]
    
    val a_shape : shape -> [> | `Shape] attrib
      
    val a_coords : numbers -> [> | `Coords] attrib
      
    val a_usemap : idref -> [> | `Usemap] attrib
      
    val a_data : uri -> [> | `Data] attrib
      
    val a_codetype : contenttype -> [> | `Codetype] attrib
      
    val a_fs_rows : multilengths -> [> | `FS_Rows] attrib
      
    val a_fs_cols : multilengths -> [> | `FS_Cols] attrib
      
    val a_frameborder : [< | `Zero | `One] -> [> | `Frameborder] attrib
      
    val a_marginheight : pixels -> [> | `Marginheight] attrib
      
    val a_marginwidth : pixels -> [> | `Marginwidth] attrib
      
    val a_scrolling : [< | `Yes | `No | `Auto] -> [> | `Scrolling] attrib
      
    val a_target : frametarget -> [> | `Target] attrib
      
    val a_content : text -> [> | `Content] attrib
      
    val a_http_equiv : text -> [> | `Http_equiv] attrib
      
    val a_defer : [< | `Defer] -> [> | `Defer] attrib
      
    val a_media : mediadesc -> [> | `Media] attrib
      
    val a_style : string -> [> | `Style_Attr] attrib
      
    (** {1 Phantom types and XML elements} *)
    (* For Ocsigen I need to specify the variance --Vincent *)
    type +'a elt
    
    type ('a, 'b) nullary = ?a: (('a attrib) list) -> unit -> 'b elt
    
    type ('a, 'b, 'c) unary = ?a: (('a attrib) list) -> 'b elt -> 'c elt
    
    type ('a, 'b, 'c, 'd) binary =
      ?a: (('a attrib) list) -> 'b elt -> 'c elt -> 'd elt
    
    type ('b, 'c, 'd, 'e) tri = 'b elt -> 'c elt -> 'd elt -> 'e elt
    
    type ('a, 'b, 'c) star =
      ?a: (('a attrib) list) -> ('b elt) list -> 'c elt
    
    (** Star '*' denotes any number of children, uncluding zero. *)
    type ('a, 'b, 'c) plus =
      ?a: (('a attrib) list) -> 'b elt -> ('b elt) list -> 'c elt
    
    (** Root element *)
    type html = [ | `Html ] elt
    
    type rt =
      [
        | `Rt of [ | `Rt ] elt
        | `Rpt of (([ | `Rp ] elt) * ([ | `Rt ] elt) * ([ | `Rp ] elt))
      ]
    
    type ruby_content = (((phrasing elt) list) * rt)
    
    type rp = (((common attrib) list) * ((phrasing elt) list))
    
    (** {1 Combined Element Sets:} *)
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
    val html :
      ?a: ((html_attrib attrib) list) ->
        [< | `Head] elt -> [< | `Body] elt -> [> | `Html] elt
      
    val head :
      ?a: ((head_attrib attrib) list) ->
        [< | `Title] elt -> (head_content_fun elt) list -> [> | head] elt
      
    val base : ([< | base_attrib], [> | base]) nullary
      
    val title : (title_attrib, [< | title_content_fun], [> | title]) unary
      
    val body : ([< | body_attrib], [< | body_content_fun], [> | body]) star
      
    (** {2 Section} *)
    val footer :
      ([< | common], [< | flow5_without_header_footer], [> | `Footer]) star
      
    val header :
      ([< | common], [< | flow5_without_header_footer], [> | `Header]) star
      
    val section :
      ([< | section_attrib], [< | section_content_fun], [> | section]) star
      
    val nav : ([< | nav_attrib], [< | nav_content_fun], [> | nav]) star
      
    val h1 : ([< | h1_attrib], [< | h1_content_fun], [> | h1]) star
      
    val h2 : ([< | h2_attrib], [< | h2_content_fun], [> | h2]) star
      
    val h3 : ([< | h3_attrib], [< | h3_content_fun], [> | h3]) star
      
    val h4 : ([< | h4_attrib], [< | h4_content_fun], [> | h4]) star
      
    val h5 : ([< | h5_attrib], [< | h5_content_fun], [> | h5]) star
      
    val h6 : ([< | h6_attrib], [< | h6_content_fun], [> | h6]) star
      
    val hgroup :
      ([< | hgroup_attrib], [< | hgroup_content_fun], [> | hgroup]) plus
      
    val address :
      ([< | address_attrib], [< | address_content_fun], [> | address]) star
      
    val article :
      ([< | article_attrib], [< | article_content_fun], [> | article]) star
      
    val aside :
      ([< | aside_attrib], [< | aside_content_fun], [> | aside]) star
      
    (** {2 Grouping content} *)
    val p : ([< | p_attrib], [< | p_content_fun], [> | p]) star
      
    val pre : ([< | pre_attrib], [< | pre_content_fun], [> | pre]) star
      
    val blockquote :
      ([< | blockquote_attrib], [< | blockquote_content_fun], [> | blockquote
        ]) star
      
    val div : ([< | div_attrib], [< | div_content_fun], [> | div]) star
      
    (********************************)
    (*            In Dl             *)
    (********************************)
    (*   Zero or more groups each   *)
    (*   consisting of              *)
    (*      one or more dt element  *)
    (*      followed by             *)
    (*      one or more dd  elements*)
    (********************************)
    val dl :
      ?a: (([< | common] attrib) list) ->
        ((([< | `Dt] elt) * (([< | `Dt] elt) list)) *
         (([< | `Dd] elt) * (([< | `Dd] elt) list))) list -> [> | `Dl] elt
      
    val ol : ([< | ol_attrib], [< | ol_content_fun], [> | ol]) star
      
    val ul : ([< | ul_attrib], [< | ul_content_fun], [> | ul]) star
      
    val dd : ([< | dd_attrib], [< | dd_content_fun], [> | dd]) star
      
    val dt : ([< | dt_attrib], [< | dt_content_fun], [> | dt]) star
      
    (********************************)
    (*            In Li             *)
    (********************************)
    (*  Only if the element is a    *)
    (*  child of an ol element:     *)
    (*          value attribute     *)
    (********************************)
    (** A list element.
        The 'a type is used to know whether the element has
        a int_value attribute or not. *)
    val li : ([< | li_attrib], [< | li_content_fun], [> | li]) star
      
    val figcaption :
      ([< | figcaption_attrib], [< | figcaption_content_fun], [> | figcaption
        ]) star
      
    (********************************)
    (*          In Figure           *)
    (********************************)
    (*Either: One figcaption element*)
    (*     followed by flow content.*)
    (*Or: Flow content followed by  *)
    (*     one figcaption element.  *)
    (*Or: Flow content.             *)
    (********************************)
    val figure :
      ?figcaption: ([< | `Figcaption] elt) ->
        ([< | common], [< | flow5], [> | `Figure]) star
      
    val hr : ([< | hr_attrib], [> | hr]) nullary
      
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
    val rt :
      ?rp: (rp * rp) ->
        ?a: (([< | common] attrib) list) -> ([< | phrasing] elt) list -> rt
      
    val rp :
      ?a: (([< | common] attrib) list) -> ([< | phrasing] elt) list -> rp
      
    val ruby :
      ?a: (([< | common] attrib) list) ->
        ruby_content -> ruby_content list -> [> | `Ruby] elt
      
    (** {2 Semantic} *)
    val b : ([< | b_attrib], [< | b_content_fun], [> | b]) star
      
    val i : ([< | i_attrib], [< | i_content_fun], [> | i]) star
      
    val small :
      ([< | small_attrib], [< | small_content_fun], [> | small]) star
      
    val sub : ([< | sub_attrib], [< | sub_content_fun], [> | sub]) star
      
    val sup : ([< | sup_attrib], [< | sup_content_fun], [> | sup]) star
      
    val mark : ([< | mark_attrib], [< | mark_content_fun], [> | mark]) star
      
    val wbr : ([< | wbr_attrib], [> | wbr]) nullary
      
    val bdo :
      dir: [< | `Ltr | `Rtl] ->
        ([< | common], [< | phrasing], [> | `Bdo]) star
      
    val abbr : ([< | abbr_attrib], [< | abbr_content_fun], [> | abbr]) star
      
    val br : ([< | br_attrib], [> | br]) nullary
      
    val cite : ([< | cite_attrib], [< | cite_content_fun], [> | cite]) star
      
    val code : ([< | code_attrib], [< | code_content_fun], [> | code]) star
      
    val dfn : ([< | dfn_attrib], [< | dfn_content_fun], [> | dfn]) star
      
    val em : ([< | em_attrib], [< | em_content_fun], [> | em]) star
      
    val kbd : ([< | kbd_attrib], [< | kbd_content_fun], [> | kbd]) star
      
    val q : ([< | q_attrib], [< | q_content_fun], [> | q]) star
      
    val samp : ([< | samp_attrib], [< | samp_content_fun], [> | samp]) star
      
    val span : ([< | span_attrib], [< | span_content_fun], [> | span]) star
      
    val strong :
      ([< | strong_attrib], [< | strong_content_fun], [> | strong]) star
      
    val time : ([< | time_attrib], [< | time_content_fun], [> | time]) star
      
    val var : ([< | var_attrib], [< | var_content_fun], [> | var]) star
      
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
    val a : ([< | a_attrib], 'a, [> | `A of 'a]) star
      
    (** {2 Edit} *)
    (**********************************)
    (* del's children are transparents*)
    (**********************************)
    val del : ([< | del_attrib], 'a, [> | `Del of 'a]) star
      
    (**********************************)
    (* ins's children are transparents*)
    (**********************************)
    val ins : ([< | ins_attrib], 'a, [> | `Ins of 'a]) star
      
    (** {2 Embedded} *)

    val img :
      src: uri ->
        alt: text ->
      ([< | common | `Height | `Ismap | `Width], [> `Img ])
        nullary
      
    val iframe : (*| `Srcdoc*)
      ([< | common | `Src | `Name | `Sandbox | `Seamless | `Width | `Height],
        [< | `PCDATA], [> | `Iframe]) star
      
    val object_ :
      ?params: (([< | `Param] elt) list) ->
      ([<
       | common
       | `Data
       | `Form
       | `Mime_type
       | `Height
       | `Width
       | `Name
       | `Usemap
       ], 'a, [> | `Object of 'a ]) star
      
    val param : ([< | param_attrib], [> | param]) nullary
      
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
    val embed :
      ([< | common | `Src | `Height | `Mime_type | `Width], [> | `Embed])
        nullary
      
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
    val audio :
      ?srcs: (uri * (([< | `Source] elt) list)) ->
      ([< | common | `Preload | `Autoplay | `Loop | `Controls], 'a,
       [> | `Audio of 'a ]) star
      
    val video :
      ?srcs: (uri * (([< | `Source] elt) list)) ->
          ([<
             | common
             | `Poster
             | `Preload
             | `Autoplay
             | `Loop
             | `Controls
             | `Width
             | `Height
            ], 'a, [> | `Video of 'a]) star
      
    val canvas : ([< | canvas_attrib], 'a, [> | `Canvas of 'a]) star
      
    val source : ([< | source_attrib], [> | source]) nullary
      
    (********************************)
    (*           In Area            *)
    (********************************)
    (* The alt, target, rel, media, *)
    (* hreflang, and type attributes*)
    (*  must be omitted if the href *)
    (*   attribute is not present.  *)
    (********************************)
    val area :
      alt: text ->
        ([<
           | common
           | `Alt
           | `Coords
           | `Shape
           | `Target
           | `Rel
           | `Media
           | `Hreflang
           | `Mime_type
          ], [> | `Area]) nullary
      
    (* XXX: SC : the current system doesn't allow
         to put <area> tag inside a map (a priori) *)
    val map : ([< | map_attrib], 'a, [> | `A of 'a]) plus
      
    (** {2 Tables Data} *)
    val caption :
      ([< | caption_attrib], [< | caption_content_fun], [> | caption]) star
      
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
    val table :
      ?caption: ([< | `Caption] elt) ->
        ?columns: (([< | `Colgroup] elt) list) ->
          ?thead: ([< | `Thead] elt) ->
            ?tfoot: ([< | `Tfoot] elt) ->
              ([< | common | `Summary], [< | `Tr], [> | `Table]) plus
      
    val tablex :
      ?caption: ([< | `Caption] elt) ->
        ?columns: (([< | `Colgroup] elt) list) ->
          ?thead: ([< | `Thead] elt) ->
            ?tfoot: ([< | `Tfoot] elt) ->
              ([< | common | `Summary], [< | `Tbody], [> | `Table]) star
      
    (********************************)
    (*          In Colgroup         *)
    (********************************)
    (*   If span attribute is:      *)
    (*       -present: Empty.       *)
    (*       -absent: Zero or more  *)
    (*                col elements. *)
    (********************************)
    val colgroup :
      ([< | colgroup_attrib], [< | colgroup_content_fun], [> | colgroup])
        star
      
    val col : ([< | col_attrib], [> | col]) nullary
      
    val thead :
      ([< | thead_attrib], [< | thead_content_fun], [> | thead]) star
      
    val tbody :
      ([< | tbody_attrib], [< | tbody_content_fun], [> | tbody]) star
      
    val tfoot :
      ([< | tfoot_attrib], [< | tfoot_content_fun], [> | tfoot]) star
      
    val td : ([< | td_attrib], [< | td_content_fun], [> | td]) star
      
    val th : ([< | th_attrib], [< | th_content_fun], [> | th]) star
      
    (****************************************)
    (*                 In Tr                *)
    (****************************************)
    (*If the parent node is a thead element:*)
    (*      Zero or more th elements        *)
    (* Otherwise:                           *)
    (*    Zero or more td or th elements    *)
    (****************************************)
    val tr : ([< | tr_attrib], [< | tr_content_fun], [> | tr]) star
      
    (** {2 Forms} *)
    val form : ([< | form_attrib], [< | form_content_fun], [> | form]) plus
      
    val fieldset :
      ?legend: ([ | `Legend ] elt) ->
        ([< | common | `Disabled | `Form | `Name], [< | flow5],
          [> | `Fieldset]) star
      
    val legend :
      ([< | legend_attrib], [< | legend_content_fun], [> | legend]) star
      
    (** Label authorizes only one  control inside them
        that should be labelled with a [for] attribute
        (although it is not necessary). Such constraints are not currently
        enforced by the type-system *)
    val label :
      ([< | label_attrib], [< | label_content_fun], [> | label]) star
      
    (** If the [type] attribute is not "hidden", must be considered
        as interactive. Distinction not made for now. *)
    val input : ([< | input_attrib], [> | input]) nullary
      
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
    val button :
      ([< | button_attrib], [< | button_content_fun], [> | button]) star
      
    val select :
      ([< | select_attrib], [< | select_content_fun], [> | select]) star
      
    val datalist :
      ?children:
        [<
          | `Options of ([< | `Option] elt) list
          | `Phras of ([< | phrasing] elt) list
        ] -> ([< | common], [> | `Datalist]) nullary
      
    val optgroup :
      label: text ->
        ([< | common | `Disabled | `Label], [< | `Option], [> | `Optgroup])
          star
      
    val option :
      ([< | option_attrib], [< | option_content_fun], [> | selectoption])
        unary
      
    val textarea :
      ([< | textarea_attrib], [< | textarea_content_fun], [> | textarea])
        unary
      
    val keygen : ([< | keygen_attrib], [> | keygen]) nullary
      
    val progress :
      ([< | progress_attrib], [< | progress_content_fun], [> | progress])
        star
      
    val meter :
      ([< | meter_attrib], [< | meter_content_fun], [> | meter]) star
      
    val output_elt :
      ([< | output_elt_attrib], [< | output_elt_content_fun], [> | output_elt
        ]) star
      
    (** {2 Data} *)
    val pcdata : string -> [> | `PCDATA] elt
      
    val entity : string -> [> | `PCDATA] elt
      
    val space : unit -> [> | `PCDATA] elt
      
    val cdata : string -> [> | `PCDATA] elt
      
    (* GK *)
    val cdata_script : string -> [> | `PCDATA] elt
      
    (* GK *)
    val cdata_style : string -> [> | `PCDATA] elt
      
    (* GK *)
    (**/**)
    val unsafe_data : string -> 'a elt
      
    (**/**)
    (** {2 Interactive} *)
    val details :
      [< | `Summary] elt ->
        ([< | common | `Open], [< | flow5] elt, [> | `Details]) star
      
    val summary :
      ([< | summary_attrib], [< | summary_content_fun], [> | summary]) star
      
    val command :
      label: text ->
        ([<
           | common
           | `Icon
           | `Disabled
           | `Checked
           | `Radiogroup
           | `Command_Type
          ], [> | `Command]) nullary
      
    val menu :
      ?child:
        [<
          | `Lis of ([< | `Li of [< | common]] elt) list
          | `Flows of ([< | flow5] elt) list
        ] -> ([< | common | `Label | `Menu_Type], [> | `Menu]) nullary
      
    (** {2 Scripting} *)
    val script :
      ([< | script_attrib], [< | script_content_fun], [> | script]) unary
      
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
    (* PLUS ?? *)
    val noscript :
      ([< | noscript_attrib], [< | noscript_content_fun], [> | noscript])
        plus
      
    val meta : ([< | meta_attrib], [> | meta]) nullary
      
    (** {2 Style Sheets} *)
    (*********************************)
    (*            In Style           *)
    (*********************************)
    (* the content model depends on  *)
    (*the value of the type attribute*)
    (*********************************)
    (*          BUT WHAT ???         *)
    (* SC: contenttype defaults to   *)
    (*  text/css                     *)
    (*********************************)
    val style :
      ([< | style_attrib], [< | style_content_fun], [> | style]) star
      
    (** {2 Link} *)
    val link :
      rel: linktypes ->
        href: uri ->
          ([<
             | common
             | `Hreflang
             | `Media
             | `Rel
             | `Href
             | `Sizes
             | `Mime_type
            ], [> | `Link]) nullary
      
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
      [
        | `HTML_v03_02
        | `HTML_v04_01
        | `XHTML_01_00
        | `XHTML_01_01
        | `XHTML_05_00
        | `Doctype of string
      ]
    
    val doctype : [< | doctypes] -> string
      
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
      
    val totl : XML.elt list -> ('a elt) list
      
    val toelt : 'a elt -> XML.elt
      
    val toeltl : ('a elt) list -> XML.elt list
      
  end
  
(** An alias for XHTML5:
    @see <http://www.w3.org/TR/html5/> HTML5 *)
module type T_05_00 = T
  
(* END INTERFACE *)
(* BEGIN INTERFACE

module M : T
module M_05_00 : T_05_00

   END INTERFACE *)
module M_05_00 : T_05_00 =
  struct
    include Uri
    type 'a attrib = XML.attrib
    
    type 'a attribs = XML.attribs
    
    let to_xmlattribs x = x
      
    (* VB *)
    let float_attrib = XML.float_attrib
      
    let int_attrib = XML.int_attrib
      
    let string_attrib = XML.string_attrib
      
    let uri_attrib a s = XML.string_attrib a (string_of_uri s)
      
    let space_sep_attrib = XML.space_sep_attrib
      
    let comma_sep_attrib = XML.comma_sep_attrib
      
    let event_attrib = XML.event_attrib
      
    (* space-separated *)
    let length_attrib name =
      function
      | `Pixels p -> int_attrib name p
      | `Percent p -> string_attrib name ((string_of_int p) ^ "%")
      
    let multilength_attrib name =
      function
      | (#length as l) -> length_attrib name l
      | `Relative 1 -> string_attrib name "*"
      | `Relative i -> string_attrib name ((string_of_int i) ^ "*")
      
    let multilength_to_string =
      function
      | `Pixels p -> string_of_int p
      | `Percent p -> (string_of_int p) ^ "%"
      | `Relative 1 -> "*"
      | `Relative i -> (string_of_int i) ^ "*"
      
    let multilengths_attrib name multilengths =
      string_attrib name
        (String.concat ", " (List.map multilength_to_string multilengths))
      
    let linktype_to_string =
      function
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
      
    let mediadesc_to_string =
      function
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
    let a_class = space_sep_attrib XML.class_name
      (* class is different on client side.
         We put the value in xML.ml 
         because this file has a different implementation client side.
      *)
      
    let a_id = string_attrib "id"
      
    let a_user_data name = string_attrib ("data-" ^ name)
      
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
      
    let a_xmlns =
      function
      | `W3_org_1999_xhtml ->
          string_attrib "xmlns" "http://www.w3.org/1999/xhtml"
      
    let a_manifest = uri_attrib "manifest"
      
    let a_cite = uri_attrib "cite"
      
    let a_xml_space =
      function | `Preserve -> string_attrib "xml:space" "preserve"
      
    let a_accesskey c = string_attrib "accesskey" (String.make 1 c)
      
    let a_charset = string_attrib "charset"
      
    let a_accept_charset = space_sep_attrib "accept-charset"
      
    let a_accept = space_sep_attrib "accept"
      
    let a_href = uri_attrib "href"
      
    let a_hreflang = string_attrib "hreflang"
      
    let a_rel = linktypes_attrib "rel"
      
    let a_tabindex = int_attrib "tabindex"
      
    let a_mime_type = string_attrib "type"
      
    let a_alt = string_attrib "alt"
      
    let a_height p = int_attrib "height" p
      
    let a_src = uri_attrib "src"
      
    let a_width p = int_attrib "width" p
      
    let a_for = string_attrib "for"
      
    let a_for_list = space_sep_attrib "for"
      
    let a_selected =
      function | `Selected -> string_attrib "selected" "selected"
      
    let a_text_value = string_attrib "value"
      
    let a_int_value = int_attrib "value"
      
    let a_value = string_attrib "value"
      
    let a_float_value = float_attrib "value"
      
    let a_action = uri_attrib "action"
      
    let a_method m =
      string_attrib "method"
        (match m with
         | `Get -> "GET"
         | `Post -> "POST"
         | `Put -> "PUT"
         | `Delete -> "DELETE")
      
    let a_enctype = string_attrib "enctype"
      
    let a_checked = function | `Checked -> string_attrib "checked" "checked"
      
    let a_disabled =
      function | `Disabled -> string_attrib "disabled" "disabled"
      
    let a_readonly =
      function | `Readonly -> string_attrib "readonly" "readonly"
      
    let a_maxlength = int_attrib "maxlength"
      
    let a_name = string_attrib "name"
      
    let a_autocomplete ac =
      string_attrib "autocomplete"
        (match ac with | `On -> "on" | `Off -> "off")
      
    let a_async = function | `Async -> string_attrib "async" "async"
      
    let a_autofocus =
      function | `Autofocus -> string_attrib "autofocus" "autofocus"
      
    let a_autoplay =
      function | `Autoplay -> string_attrib "autoplay" "autoplay"
      
    let a_challenge = string_attrib "challenge"
      
    let a_contenteditable ce =
      string_attrib "contexteditable"
        (match ce with | `True -> "true" | `False -> "false")
      
    let a_contextmenu = string_attrib "contextmenu"
      
    let a_controls =
      function | `Controls -> string_attrib "controls" "controls"
      
    let a_dir d =
      string_attrib "dir" (match d with | `Ltr -> "ltr" | `Rtl -> "rtl")
      
    let a_draggable d =
      string_attrib "draggable"
        (match d with | `True -> "true" | `False -> "false")
      
    let a_form = string_attrib "form"
      
    let a_formaction = uri_attrib "formaction"
      
    let a_formenctype = string_attrib "formenctype"
      
    let a_formmethod m =
      string_attrib "method"
        (match m with
         | `Get -> "GET"
         | `Post -> "POST"
         | `Put -> "PUT"
         | `Delete -> "DELETE")
      
    let a_formnovalidate =
      function
      | `Formnovalidate -> string_attrib "formnovalidate" "formnovalidate"
      
    let a_formtarget = string_attrib "formtarget"
      
    let a_hidden = function | `Hidden -> string_attrib "hidden" "hidden"
      
    let a_high = float_attrib "high"
      
    let a_icon = uri_attrib "icon"
      
    let a_ismap = function | `Ismap -> string_attrib "ismap" "ismap"
      
    let a_keytype = string_attrib "keytype"
      
    let a_list = string_attrib "list"
      
    let a_loop = function | `Loop -> string_attrib "loop" "loop"
      
    let a_low = float_attrib "low"
      
    let a_max = float_attrib "max"
      
    let a_input_max = int_attrib "max"
      
    let a_min = float_attrib "min"
      
    let a_input_min = int_attrib "min"
      
    let a_novalidate =
      function | `Novalidate -> string_attrib "novalidate" "novalidate"
      
    let a_open = function | `Open -> string_attrib "open" "open"
      
    let a_optimum = float_attrib "optimum"
      
    let a_pattern = string_attrib "pattern"
      
    let a_placeholder = string_attrib "placeholder"
      
    let a_poster = uri_attrib "poster"
      
    let a_preload pl =
      string_attrib "preload"
        (match pl with
         | `None -> "none"
         | `Metadata -> "metadata"
         | `Audio -> "audio")
      
    let a_pubdate = function | `Pubdate -> string_attrib "pubdate" "pubdate"
      
    let a_radiogroup = string_attrib "radiogroup"
      
    let a_required =
      function | `Required -> string_attrib "required" "required"
      
    let a_reversed =
      function | `Reversed -> string_attrib "reserved" "reserved"
      
    let rec a_sandbox sb =
      let rec aux sb =
        match sb with
        | `AllowSameOrigin :: a -> "allow-same-origin" :: (aux a)
        | `AllowForms :: a -> "allow-forms" :: (aux a)
        | `AllowScript :: a -> "allow-script" :: (aux a)
        | [] -> []
      in space_sep_attrib "sandbox" (aux sb)
      
    let a_spellcheck sc =
      string_attrib "spellckeck"
        (match sc with | `True -> "true" | `False -> "false")
      
    let a_scoped = function | `Scoped -> string_attrib "scoped" "scoped"
      
    let a_seamless =
      function | `Seamless -> string_attrib "seamless" "seamless"
      
    let a_sizes sizes =
      string_attrib "sizes"
        (String.concat " " (List.map string_of_int sizes))
      
    let a_span = int_attrib "span"
      
    (*let a_srcdoc*)
    let a_srclang = string_attrib "xml:lang"
      
    let a_start = int_attrib "start"
      
    let a_step = float_attrib "step"
      
    let a_wrap w =
      string_attrib "wrap" (match w with | `Soft -> "soft" | `Hard -> "hard")
      
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
      
    let a_menu_type mt =
      string_attrib "type"
        (match mt with | `Context -> "context" | `Toolbar -> "toolbar")
      
    let a_command_type ct =
      string_attrib "type"
        (match ct with
         | `Command -> "command"
         | `Checkbox -> "checkbox"
         | `Radio -> "radio")
      
    let a_button_type bt =
      string_attrib "type"
        (match bt with
         | `Button -> "button"
         | `Submit -> "submit"
         | `Reset -> "reset")
      
    let a_multiple =
      function | `Multiple -> string_attrib "multiple" "multiple"
      
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
      
    let a_data = uri_attrib "data"
      
    let a_codetype = string_attrib "codetype"
      
    let a_fs_rows mls = multilengths_attrib "rows" mls
      
    let a_fs_cols mls = multilengths_attrib "cols" mls
      
    let a_frameborder b =
      int_attrib "frameborder" (match b with | `Zero -> 0 | `One -> 1)
      
    let a_marginheight = int_attrib "marginheight"
      
    let a_marginwidth = int_attrib "marginwidth"
      
    let a_scrolling s =
      string_attrib "scrolling"
        (match s with | `Yes -> "yes" | `No -> "no" | `Auto -> "auto")
      
    let a_target = string_attrib "target"
      
    let a_content = string_attrib "content"
      
    let a_http_equiv = string_attrib "http-equiv"
      
    let a_media = mediadesc_attrib "media"
      
    type 'a elt = XML.elt
    
    type html = [ | `Html ] elt
    
    (* NB: These are more general than the ones in xHTML.mli *)
    type ('a, 'b) nullary = ?a: (('a attrib) list) -> unit -> 'b elt
    
    type ('a, 'b, 'c) unary = ?a: (('a attrib) list) -> 'b elt -> 'c elt
    
    type ('a, 'b, 'c, 'd) binary =
      ?a: (('a attrib) list) -> 'b elt -> 'c elt -> 'd elt
    
    type ('b, 'c, 'd, 'e) tri = 'b elt -> 'c elt -> 'd elt -> 'e elt
    
    type ('a, 'b, 'c) star =
      ?a: (('a attrib) list) -> ('b elt) list -> 'c elt
    
    type ('a, 'b, 'c) plus =
      ?a: (('a attrib) list) -> 'b elt -> ('b elt) list -> 'c elt
    
    let terminal tag ?a () = XML.leaf ?a tag
      
    (* let nullary tag ?a () = XML.node ?a tag [] *)
    let unary tag ?a elt = XML.node ?a tag [ elt ]
      
    let binary tag ?a elt1 elt2 = XML.node ?a tag [ elt1; elt2 ]
      
    let tri tag elt1 elt2 elt3 = XML.node tag [ elt1; elt2; elt3 ]
      
    let star tag ?a elts = XML.node ?a tag elts
      
    let plus tag ?a elt elts = XML.node ?a tag (elt :: elts)
      
    let list_of_option = function | Some x -> [ x ] | None -> []
      
    let list_of_list_option = function | Some x -> x | None -> []
      
    let srcs_option = function | Some (`Srcs s) -> s | None -> []
      
    let phrasing_option = function | Some (`Phras p) -> p | None -> []
      
    let ruby_option =
      function | Some (`Rt_elt r) -> r | Some (`Group g) -> g | None -> []
      
    let body_option =
      function | Some (`Body b) -> b | Some (`Trs t) -> t | None -> []
      
    let colg_option = function | Some (`Colgroups c) -> c | None -> []
      
    let opts_option =
      function
      | Some (`Options o) -> o
      | Some (`Optgroups o) -> o
      | None -> []
      
    let li_option =
      function | Some (`Lis l) -> l | Some (`Flows f) -> f | None -> []
      
    let opt_option =
      function | Some (`Options o) -> o | Some (`Phras p) -> p | None -> []
      
    let param_option = function | Some (`Params p) -> p | None -> []
      
    let cols_option =
      function | Some (`Cols c) -> c | Some (`Colgroups c) -> c | None -> []
      
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
      
    let dl ?a list =
      XML.node ?a "dl"
        (List.concat
           (List.map
              (fun ((elt, elts), (elt', elts')) ->
                 (elt :: elts) @ (elt' :: elts'))
              list))
      
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
      
    let rt ?rp ?a elts =
      match rp with
      | Some ((a1, e1), (a2, e2)) ->
          `Rpt (XML.node ~a: a1 "rp" e1, XML.node ?a "rt" elts,
            XML.node ~a: a2 "rp" e2)
      | None -> `Rt (XML.node ?a "rt" elts)
      
    let ruby ?a elt elts =
      let rec aux =
        function
        | [] -> []
        | (pel, `Rt e) :: l -> pel @ (e :: (aux l))
        | (pel, `Rpt (e1, e2, e3)) :: l -> pel @ (e1 :: e2 :: e3 :: (aux l))
      in XML.node ?a "ruby" (aux (elt :: elts))
      
    let wbr = terminal "wbr"
      
    (* VB *)
    type shape = [ | `Rect | `Circle | `Poly | `Default ]
    
    let bdo ~dir ?(a = []) elts = XML.node ~a: ((a_dir dir) :: a) "bdo" elts
      
    let a_datetime = string_attrib "datetime"
      
    let a_shape d =
      string_attrib "shape"
        (match d with
         | `Rect -> "rect"
         | `Circle -> "circle"
         | `Poly -> "poly"
         | `Default -> "default")
      
    let a_coords coords =
      string_attrib "coords"
        (String.concat "," (List.map string_of_int coords))
      
    let a_usemap = string_attrib "usemap"
      
    let a_defer = function | `Defer -> string_attrib "defer" "defer"
      
    let a_label = string_attrib "label"
      
    let area ~alt ?(a = []) () = XML.leaf ~a: ((a_alt alt) :: a) "area"
      
    let map = plus "map"
      
    let del = star "del"
      
    let ins = star "ins"
      
    let script ?(a = []) elt = XML.node ~a "script" [ elt ]
      
    let noscript = plus "noscript"
      
    let article = star "article"
      
    let aside = star "aside"
      
    let video_audio name ?srcs ?(a = []) elts =
      let (a, children) =
        match srcs with
        | None -> (a, elts)
        | Some (uri, srcs) -> (((a_src uri) :: a), (srcs @ elts))
      in XML.node ~a name children
      
    let audio = video_audio "audio"
      
    let video = video_audio "video"
      
    let canvas = star "canvas"
      
    let command ~label ?(a = []) () =
      XML.leaf ~a: ((a_label label) :: a) "command"
      
    let menu ?child ?a () = XML.node ?a "menu" (li_option child)
      
    let embed = terminal "embed"
      
    let source = terminal "source"
      
    let meter = star "meter"
      
    let output_elt = star "output"
      
    let form = plus "form"
      
    type input_attr =
      [
        | common
        | `Accept
        | `Alt
        | `Autocomplete
        | `Autofocus
        | `Checked
        | `Disabled
        | `Form
        | `Formation
        | `Formenctype
        | `Formmethod
        | `Formnovalidate
        | `Formtarget
        | `Height
        | `List
        | `Input_Max
        | `Maxlength
        | `Input_Min
        | `Multiple
        | `Name
        | `Pattern
        | `Placeholder
        | `ReadOnly
        | `Required
        | `Size
        | `Src
        | `Step
        | `Input_Type
        | `Value
        | `Width
      ]
    
    let input = terminal "input"
      
    let keygen = terminal "keygen"
      
    let label = star "label"
      
    let option = unary "option"
      
    let select = star "select"
      
    let textarea = unary "textarea"
      
    let button = star "button"
      
    let datalist ?children ?a () =
      let children =
        match children with
        | Some (`Options x) -> x
        | Some (`Phras x) -> x
        | None -> []
      in XML.node ?a "datalist" children
      
    let progress = star "proress"
      
    let legend = star "legend"
      
    let details summary ?a children =
      XML.node "details" ?a (summary :: children)
      
    let summary = star "summary"
      
    let fieldset ?legend ?a elts =
      XML.node ?a "fieldset" ((list_of_option legend) @ elts)
      
    let optgroup ~label ?(a = []) elts =
      XML.node ~a: ((a_label label) :: a) "optgroup" elts
      
    let figcaption = star "figcaption"
      
    let figure ?figcaption ?a elts =
      XML.node ?a "figure" ((list_of_option figcaption) @ elts)
      
    let caption = star "caption"
      
    let table ?caption ?(columns = []) ?thead ?tfoot ?a elt elts =
      XML.node ?a "table"
        ((list_of_option caption) @
           (columns @
              ((list_of_option thead) @
                 ((list_of_option tfoot) @ (elt :: elts)))))
      
    let tablex ?caption ?(columns = []) ?thead ?tfoot ?a elts =
      XML.node ?a "table"
        ((list_of_option caption) @
           (columns @
              ((list_of_option thead) @ ((list_of_option tfoot) @ elts))))
      
    let td = star "td"
      
    let th = star "th"
      
    let tr = star "tr"
      
    let colgroup = star "colgroup"
      
    let col = terminal "col"
      
    let thead = star "thead"
      
    let tbody = star "tbody"
      
    let tfoot = star "tfoot"
      
    let iframe = star "iframe"
      
    let object_ ?(params = []) ?(a = []) elts =
      XML.node ~a "object" (params @ elts)
      
    let param = terminal "param"
      
    let img ~src ~alt ?(a = []) () =
      let a = (a_src src) :: (a_alt alt) :: a in
      XML.leaf ~a "img"
      
    let meta = terminal "meta"
      
    let style ?(a = []) elts = XML.node ~a "style" elts
      
    let link ~rel ~href ?(a = []) () =
      XML.leaf ~a: ((a_rel rel) :: (a_href href) :: a) "link"
      
    let base = terminal "base"
      
    (* VB *)
    
    type rt =
      [
        | `Rt of [ | `Rt ] elt
        | `Rpt of (([ | `Rp ] elt) * ([ | `Rt ] elt) * ([ | `Rp ] elt))
      ]
    
    type ruby_content = (((phrasing elt) list) * rt)
    
    type rp = (((common attrib) list) * ((phrasing elt) list))
    
    (* I/O *)
    let compose_doctype dt args =
      "<!DOCTYPE " ^
        (dt ^
           (" PUBLIC " ^
              ((String.concat " "
                  (List.map (fun a -> "\"" ^ (a ^ "\"")) args))
                 ^ ">\n")))
      
    type doctypes =
      [
        | `HTML_v03_02
        | `HTML_v04_01
        | `XHTML_01_00
        | `XHTML_01_01
        | `XHTML_05_00
        | `Doctype of string
      ]
    
    let doctype =
      function
      | `HTML_v03_02 ->
          compose_doctype "html" [ "-//W3C//DTD HTML 3.2 Final//EN" ]
      | `HTML_v04_01 ->
          compose_doctype "html"
            [ "-//W3C//DTD HTML 4.01//EN";
              "http://www.w3.org/TR/html4/strict.dtd" ]
      | `XHTML_01_00 ->
          compose_doctype "html"
            [ "-//W3C//DTD XHTML 1.0 Strict//EN";
              "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd" ]
      | `XHTML_01_01 ->
          compose_doctype "html"
            [ "-//W3C//DTD XHTML 1.1//EN";
              "http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd" ]
      | `XHTML_05_00 -> "<!DOCTYPE html>"
      | `Doctype s -> s
      
    let no_break =
      [ "title"; "h1"; "h2"; "h3"; "h4"; "h5"; "h6"; "address"; "blockquote";
        "div"; "p"; "li"; "dd"; "dt"; "td"; "th" ]
      
    let preformatted = [ "pre" ]
      
    (* Tools *)
    let version =
      function
      | `XHTML_01_00 -> "XHTML 1.0"
      | `XHTML_01_01 -> "XHTML 1.1"
      | `XHTML_05_00 -> "XHTML 5.0"
      
    let standard =
      function
      | `XHTML_01_00 -> uri_of_string "http://www.w3.org/TR/xhtml1/"
      | `XHTML_01_01 -> uri_of_string "http://www.w3.org/TR/xhtml11/"
      | `XHTML_05_00 -> uri_of_string "http://www.w3.org/TR/xhtml5"
      
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
      
    let xhtml_version = `XHTML_05_00
      
    let version = version xhtml_version
      
    let standard = standard xhtml_version
      
  end
  
module M = M_05_00
  

