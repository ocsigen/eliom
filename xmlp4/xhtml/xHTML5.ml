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
open Xhtml5types;
module type T =
  sig
    (** {1 Common Attributes} *)
    type attrib +'a;
    type attribs +'a;
    value to_xmlattribs : list (attrib 'a) -> list XML.attrib;
    (* VB *)
    (** ['a] is known as a {i phantom type}.  The implementation is
       actually monomorphic (the different element types are distinguished
       by a homogeneous variable, such as their textual representation)
       and the type variable [`a] is just used by the type checker.

       NB: It might be possible to use polymorphic variants directly, without
       phantom types, but the implementation is likely to be more involved. *)
    value a_autocomplete : [ < `On | `Off] -> attrib [ > `Autocomplete];
    value a_async : [ < `Async] -> attrib [ > `Async];
    value a_autofocus : [ < `Autofocus] -> attrib [ > `Autofocus];
    value a_autoplay : [ < `Autoplay] -> attrib [ > `Autoplay];
    value a_challenge : text -> attrib [ > `Challenge];
    value a_contenteditable :
      [ < `True | `False] -> attrib [ > `Contexteditable];
    value a_contextmenu : idref -> attrib [ > `Contextmenu];
    value a_controls : [ < `Controls] -> attrib [ > `Controls];
    value a_dir : [ < `Rtl | `Ltr] -> attrib [ > `Dir];
    value a_draggable : [ < `True | `False] -> attrib [ > `Draggable];
    value a_form : idref -> attrib [ > `Form];
    value a_formaction : uri -> attrib [ > `Formaction];
    value a_formenctype : contenttype -> attrib [ > `Formenctype];
    value a_formmethod :
      [ < `Get | `Post | `Put | `Delete] -> attrib [ > `Formmethod];
    value a_formnovalidate :
      [ < `Formnovalidate] -> attrib [ > `Formnovalidate];
    value a_formtarget : text -> attrib [ > `Formtarget];
    value a_hidden : [ < `Hidden] -> attrib [ > `Hidden];
    value a_high : float_number -> attrib [ > `High];
    value a_icon : uri -> attrib [ > `Icon];
    value a_ismap : [ < `Ismap] -> attrib [ > `Ismap];
    value a_keytype : text -> attrib [ > `Keytype];
    value a_list : idref -> attrib [ > `List];
    value a_loop : [ < `Loop] -> attrib [ > `Loop];
    value a_low : float_number -> attrib [ > `High];
    value a_max : float_number -> attrib [ > `Max];
    value a_input_max : number -> attrib [ > `Max];
    value a_min : float_number -> attrib [ > `Min];
    value a_input_min : number -> attrib [ > `Min];
    value a_novalidate : [ < `Novalidate] -> attrib [ > `Novalidate];
    value a_open : [ < `Open] -> attrib [ > `Open];
    value a_optimum : float_number -> attrib [ > `Optimum];
    value a_pattern : text -> attrib [ > `Pattern];
    value a_placeholder : text -> attrib [ > `Placeholder];
    value a_poster : uri -> attrib [ > `Poster];
    value a_preload :
      [ < `None | `Metadata | `Audio] -> attrib [ > `Preload];
    value a_pubdate : [ < `Pubdate] -> attrib [ > `Pubdate];
    value a_radiogroup : text -> attrib [ > `Radiogroup];
    value a_required : [ < `Required] -> attrib [ > `Required];
    value a_reserved : [ < `Reserved] -> attrib [ > `Reserved];
    value a_sandbox :
      list [ < `AllowSameOrigin | `AllowForms | `AllowScript] ->
        attrib [ > `Sandbox];
    value a_spellcheck : [ < `True | `False] -> attrib [ > `Spellcheck];
    value a_scoped : [ < `Scoped] -> attrib [ > `Scoped];
    value a_seamless : [ < `Seamless] -> attrib [ > `Seamless];
    value a_sizes : numbers -> attrib [ > `Sizes];
    value a_span : number -> attrib [ > `Span];
    (*val a_srcdoc*)
    value a_srclang : nmtoken -> attrib [ > `XML_lang];
    value a_start : number -> attrib [ > `Start];
    value a_step : float_number -> attrib [ > `Step];
    value a_wrap : [ < `Soft | `Hard] -> attrib [ > `Wrap];
    value a_class : nmtokens -> attrib [ > `Class];
    (** This attribute assigns a class name or set of class names to an
    element. Any number of elements may be assigned the same class
    name or names.  *)
    value a_user_data : nmtoken -> text -> attrib [ > `User_data];
    (** May be used to specify custom attribs.
    The example given by the W3C is as follows :
    <ol>
    <li data-length="2m11s">Beyond The Sea</li>
    </ol>
    It should be used for preprocessing ends only. *)
    value a_id : text -> attrib [ > `Id];
    (** This attribute assigns a name to an element. This name must be
    unique in a document. The text should be without any space. *)
    value a_title : text -> attrib [ > `Title];
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
    value a_xml_lang : nmtoken -> attrib [ > `XML_lang];
    (** {2 Events} *)
    (** Javascript events *)
    value a_onabort : XML.event -> attrib [ > `OnAbort];
    value a_onafterprint : XML.event -> attrib [ > `OnAfterPrint];
    value a_onbeforeprint : XML.event -> attrib [ > `OnBeforePrint];
    value a_onbeforeunload : XML.event -> attrib [ > `OnBeforeUnload];
    value a_onblur : XML.event -> attrib [ > `OnBlur];
    value a_oncanplay : XML.event -> attrib [ > `OnCanPlay];
    value a_oncanplaythrough : XML.event -> attrib [ > `OnCanPlayThrough];
    value a_onchange : XML.event -> attrib [ > `OnChange];
    value a_onclick : XML.event -> attrib [ > `OnClick];
    value a_oncontextmenu : XML.event -> attrib [ > `OnContextMenu];
    value a_ondblclick : XML.event -> attrib [ > `OnDblClick];
    value a_ondrag : XML.event -> attrib [ > `OnDrag];
    value a_ondragend : XML.event -> attrib [ > `OnDragEnd];
    value a_ondragenter : XML.event -> attrib [ > `OnDragEnter];
    value a_ondragleave : XML.event -> attrib [ > `OnDragLeave];
    value a_ondragover : XML.event -> attrib [ > `OnDragOver];
    value a_ondragstart : XML.event -> attrib [ > `OnDragStart];
    value a_ondrop : XML.event -> attrib [ > `OnDrop];
    value a_ondurationchange : XML.event -> attrib [ > `OnDurationChange];
    value a_onemptied : XML.event -> attrib [ > `OnEmptied];
    value a_onended : XML.event -> attrib [ > `OnEnded];
    value a_onerror : XML.event -> attrib [ > `OnError];
    value a_onfocus : XML.event -> attrib [ > `OnFocus];
    value a_onformchange : XML.event -> attrib [ > `OnFormChange];
    value a_onforminput : XML.event -> attrib [ > `OnFormInput];
    value a_onhashchange : XML.event -> attrib [ > `OnHashChange];
    value a_oninput : XML.event -> attrib [ > `OnInput];
    value a_oninvalid : XML.event -> attrib [ > `OnInvalid];
    value a_onmousedown : XML.event -> attrib [ > `OnMouseDown];
    value a_onmouseup : XML.event -> attrib [ > `OnMouseUp];
    value a_onmouseover : XML.event -> attrib [ > `OnMouseOver];
    value a_onmousemove : XML.event -> attrib [ > `OnMouseMove];
    value a_onmouseout : XML.event -> attrib [ > `OnMouseOut];
    value a_onmousewheel : XML.event -> attrib [ > `OnMouseWheel];
    value a_onoffline : XML.event -> attrib [ > `OnOffLine];
    value a_ononline : XML.event -> attrib [ > `OnOnLine];
    value a_onpause : XML.event -> attrib [ > `OnPause];
    value a_onplay : XML.event -> attrib [ > `OnPlay];
    value a_onplaying : XML.event -> attrib [ > `OnPlaying];
    value a_onpagehide : XML.event -> attrib [ > `OnPageHide];
    value a_onpageshow : XML.event -> attrib [ > `OnPageShow];
    value a_onpopstate : XML.event -> attrib [ > `OnPopState];
    value a_onprogress : XML.event -> attrib [ > `OnProgress];
    value a_onratechange : XML.event -> attrib [ > `OnRateChange];
    value a_onreadystatechange :
      XML.event -> attrib [ > `OnReadyStateChange];
    value a_onredo : XML.event -> attrib [ > `OnRedo];
    value a_onresize : XML.event -> attrib [ > `OnResize];
    value a_onscroll : XML.event -> attrib [ > `OnScroll];
    value a_onseeked : XML.event -> attrib [ > `OnSeeked];
    value a_onseeking : XML.event -> attrib [ > `OnSeeking];
    value a_onselect : XML.event -> attrib [ > `OnSelect];
    value a_onshow : XML.event -> attrib [ > `OnShow];
    value a_onstalled : XML.event -> attrib [ > `OnStalled];
    value a_onstorage : XML.event -> attrib [ > `OnStorage];
    value a_onsubmit : XML.event -> attrib [ > `OnSubmit];
    value a_onsuspend : XML.event -> attrib [ > `OnSuspend];
    value a_ontimeupdate : XML.event -> attrib [ > `OnTimeUpdate];
    value a_onundo : XML.event -> attrib [ > `OnUndo];
    value a_onunload : XML.event -> attrib [ > `OnUnload];
    value a_onvolumechange : XML.event -> attrib [ > `OnVolumeChange];
    value a_onwaiting : XML.event -> attrib [ > `OnWaiting];
    value a_onkeypress : XML.event -> attrib [ > `OnKeyPress];
    value a_onkeydown : XML.event -> attrib [ > `OnKeyDown];
    value a_onkeyup : XML.event -> attrib [ > `OnKeyUp];
    value a_onload : XML.event -> attrib [ > `OnLoad];
    value a_onloadeddata : XML.event -> attrib [ > `OnLoadedData];
    value a_onloadedmetadata : XML.event -> attrib [ > `OnLoadedMetaData];
    value a_onloadstart : XML.event -> attrib [ > `OnLoadStart];
    value a_onmessage : XML.event -> attrib [ > `OnMessage];
    value a_version : cdata -> attrib [ > `Version];
    value a_xmlns : [ < `W3_org_1999_xhtml] -> attrib [ > `XMLns];
    value a_manifest : uri -> attrib [ > `Manifest];
    value a_cite : uri -> attrib [ > `Cite];
    value a_xml_space : [ < `Preserve] -> attrib [ > `XML_space];
    value a_accesskey : character -> attrib [ > `Accesskey];
    (** This attribute assigns an access key to an element. An access key
    is a single character from the document character
    set. NB: authors should consider the input method of the
    expected reader when specifying an accesskey. *)
    value a_charset : charset -> attrib [ > `Charset];
    (** This attribute specifies the character encoding of the resource
    designated by the link. Please consult the section on character
    encodings for more details. *)
    value a_accept_charset : charsets -> attrib [ > `Accept_charset];
    value a_accept : contenttypes -> attrib [ > `Accept];
    value a_href : uri -> attrib [ > `Href];
    (** This attribute specifies the location of a Web resource, thus
    defining a link between the current element (the source anchor)
    and the destination anchor defined by this attribute. *)
    value a_hreflang : languagecode -> attrib [ > `Hreflang];
    (** This attribute specifies the base language of the resource
    designated by href and may only be used when href is specified. *)
    value a_rel : linktypes -> attrib [ > `Rel];
    (** This attribute describes the relationship from the current document
    to the anchor specified by the href attribute. The value of this attribute
    is a space-separated list of link types. *)
    (** This attribute is used to describe a reverse link from the anchor specified
    by the href attribute to the current document. The value of this attribute
    is a space-separated list of link types. *)
    value a_tabindex : number -> attrib [ > `Tabindex];
    (** This attribute specifies the position of the current element in
    the tabbing order for the current document. This value must be a
    number between 0 and 32767. User agents should ignore leading
    zeros. *)
    value a_mime_type : contenttype -> attrib [ > `Mime_type];
    (** This attribute gives an advisory hint as to the content type of
    the content available at the link target address. It allows user
    agents to opt to use a fallback mechanism rather than fetch the
    content if they are advised that they will get content in a
    content type they do not support.Authors who use this attribute
    take responsibility to manage the risk that it may become
    inconsistent with the content available at the link target
    address. *)
    value a_datetime : cdata -> attrib [ > `Datetime];
    value a_action : uri -> attrib [ > `Action];
    (** This attribute specifies a form processing agent. User agent
    behavior for a value other than an HTTP URI is undefined. *)
    value a_checked : [ < `Checked] -> attrib [ > `Checked];
    (** When the [type] attribute has the value ["radio"] or ["checkbox"],
    this boolean attribute specifies that the button is on. User
    agents must ignore this attribute for other control types. *)
    value a_cols : number -> attrib [ > `Cols];
    (** This attribute specifies the visible width in average character
    widths. Users should be able to enter longer lines than this, so
    user agents should provide some means to scroll through the
    contents of the control when the contents extend beyond the
    visible area. User agents may wrap visible text lines to keep long
    lines visible without the need for scrolling. *)
    value a_enctype : contenttype -> attrib [ > `Enctype];
    value a_for : idref -> attrib [ > `For];
    value a_for_list : idrefs -> attrib [ > `For_List];
    value a_maxlength : number -> attrib [ > `Maxlength];
    value a_method :
      [ < `Get | `Post | `Put | `Delete] -> attrib [ > `Method];
    value a_multiple : [ < `Multiple] -> attrib [ > `Multiple];
    value a_name : text -> attrib [ > `Name];
    (** This attribute assigns the control name. *)
    value a_rows : number -> attrib [ > `Rows];
    (** This attribute specifies the number of visible text lines. Users
    should be able to enter more lines than this, so user agents
    should provide some means to scroll through the contents of the
    control when the contents extend beyond the visible area. *)
    value a_selected : [ < `Selected] -> attrib [ > `Selected];
    (** When set, this boolean attribute specifies that this option is pre-selected. *)
    value a_size : number -> attrib [ > `Size];
    value a_src : uri -> attrib [ > `Src];
    value a_input_type :
      [ < `Url | `Tel | `Text | `Time | `Search | `Password | `Checkbox
        | `Range | `Radio | `Submit | `Reset | `Number | `Hidden | `Month
        | `Week | `File | `Email | `Image | `Datetime_local | `Datetime
        | `Date | `Color | `Button] -> attrib [ > `Input_Type];
    value a_text_value : text -> attrib [ > `Text_Value];
    (** This attribute specifies the initial value of the control. If this
    attribute is not set, the initial value is set to the contents of
    the [option] element. *)
    value a_int_value : number -> attrib [ > `Int_Value];
    (*VVV NO *)
    value a_value : cdata -> attrib [ > `Value];
    value a_float_value : float_number -> attrib [ > `Float_Value];
    value a_disabled : [ < `Disabled] -> attrib [ > `Disabled];
    value a_readonly : [ < `Readonly] -> attrib [ > `Readonly];
    value a_button_type :
      [ < `Button | `Submit | `Reset] -> attrib [ > `Button_Type];
    value a_command_type :
      [ < `Command | `Checkbox | `Radio] -> attrib [ > `Command_Type];
    value a_menu_type : [ < `Context | `Toolbar] -> attrib [ > `Menu_Type];
    value a_label : text -> attrib [ > `Label];
    value a_align :
      [ < `Left | `Right | `Justify | `Char] -> attrib [ > `Align];
    value a_axis : cdata -> attrib [ > `Axis];
    value a_colspan : number -> attrib [ > `Colspan];
    value a_headers : idrefs -> attrib [ > `Headers];
    value a_rowspan : number -> attrib [ > `Rowspan];
    value a_scope :
      [ < `Row | `Col | `Rowgroup | `Colgroup] -> attrib [ > `Scope];
    value a_summary : text -> attrib [ > `Summary];
    value a_border : pixels -> attrib [ > `Border];
    value a_cellpadding : length -> attrib [ > `Cellpadding];
    value a_cellspacing : length -> attrib [ > `Cellspacing];
    value a_datapagesize : cdata -> attrib [ > `Datapagesize];
    value a_rules :
      [ < `None | `Groups | `Rows | `Cols | `All] -> attrib [ > `Rules];
    value a_char : character -> attrib [ > `Char];
    value a_charoff : length -> attrib [ > `Charoff];
    value a_alt : text -> attrib [ > `Alt];
    value a_height : number -> attrib [ > `Height];
    value a_width : number -> attrib [ > `Width];
    type shape = [ = `Rect | `Circle | `Poly | `Default ];
    value a_shape : shape -> attrib [ > `Shape];
    value a_coords : numbers -> attrib [ > `Coords];
    value a_usemap : idref -> attrib [ > `Usemap];
    value a_data : uri -> attrib [ > `Data];
    value a_codetype : contenttype -> attrib [ > `Codetype];
    value a_fs_rows : multilengths -> attrib [ > `FS_Rows];
    value a_fs_cols : multilengths -> attrib [ > `FS_Cols];
    value a_frameborder : [ < `Zero | `One] -> attrib [ > `Frameborder];
    value a_marginheight : pixels -> attrib [ > `Marginheight];
    value a_marginwidth : pixels -> attrib [ > `Marginwidth];
    value a_scrolling : [ < `Yes | `No | `Auto] -> attrib [ > `Scrolling];
    value a_target : frametarget -> attrib [ > `Target];
    value a_content : text -> attrib [ > `Content];
    value a_http_equiv : text -> attrib [ > `Http_equiv];
    value a_defer : [ < `Defer] -> attrib [ > `Defer];
    value a_media : mediadesc -> attrib [ > `Media];
    value a_style : string -> attrib [ > `Style_Attr];


  (** {1 Phantom types and XML elements} *)
  (* For Ocsigen I need to specify the variance --Vincent *)
  type elt +'a;
  type nullary 'a 'b = ?a:(list (attrib 'a) ) -> unit -> elt 'b;
  type unary 'a 'b 'c = ?a:(list (attrib 'a)) -> elt 'b -> elt 'c ;
  type binary 'a 'b 'c 'd = ?a:(list (attrib 'a)) -> elt 'b -> elt 'c -> elt 'd;
  type tri 'b 'c 'd 'e = elt 'b -> elt 'c -> elt 'd -> elt 'e;
  type star 'a 'b 'c = ?a:(list (attrib 'a)) -> list (elt 'b) -> elt 'c;
      (** Star '*' denotes any number of children, uncluding zero. *)
  
  type plus 'a 'b 'c = ?a:(list (attrib 'a)) -> elt 'b -> list (elt 'b) -> elt 'c;
  
  (** Root element *)
  type html = elt [=`Html] ;
  type rt = 
    [= `Rt of elt [= `Rt ]
    | `Rpt of (elt [= `Rp ] * elt [= `Rt ] * elt [= `Rp ])];
  type ruby_content = (list (elt phrasing) * rt);
  type rp = (list (attrib common) * list (elt phrasing));
  

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
    value html : ?a: (list (attrib html_attrib)) -> elt [ < `Head ] -> elt [ < `Body ] -> elt [ > `Html ];
    value head : ?a: (list (attrib head_attrib)) -> elt [ < `Title ] -> 
                     list (elt head_content_fun) -> elt [> head];
    value base : nullary [< base_attrib] [> base];
    value title : unary title_attrib [ < title_content_fun ] [> title];
    value body : star [< body_attrib ] [ < body_content_fun ] [> body];
    (** {2 Section} *)
    value footer :
      star [ < common] [ < flow5_without_header_footer] [ > `Footer];
    value header :
      star [ < common] [ < flow5_without_header_footer] [ > `Header];
    value section : star [< section_attrib ] [ < section_content_fun ] [> section];
    value nav : star [< nav_attrib ] [ < nav_content_fun ] [> nav];
    value h1 : star [< h1_attrib ] [ < h1_content_fun ] [> h1];
    value h2 : star [< h2_attrib ] [ < h2_content_fun ] [> h2];
    value h3 : star [< h3_attrib ] [ < h3_content_fun ] [> h3];
    value h4 : star [< h4_attrib ] [ < h4_content_fun ] [> h4];
    value h5 : star [< h5_attrib ] [ < h5_content_fun ] [> h5];
    value h6 : star [< h6_attrib ] [ < h6_content_fun ] [> h6];
    value hgroup : plus [< hgroup_attrib ] [ < hgroup_content_fun ] [> hgroup];
    value address : star [< address_attrib ] [ < address_content_fun ] [> address];
    value article : star [< article_attrib ] [ < article_content_fun ] [> article];
    value aside : star [< aside_attrib ] [ < aside_content_fun ] [> aside];
    (** {2 Grouping content} *)
    value p : star [< p_attrib ] [ < p_content_fun ] [> p];
    value pre : star [< pre_attrib ] [ < pre_content_fun ] [> pre];
    value blockquote : star [< blockquote_attrib ] [ < blockquote_content_fun ] [> blockquote];
    value div : star [< div_attrib ] [ < div_content_fun ] [> div];
    (********************************)
    (*            In Dl             *)
    (********************************)
    (*   Zero or more groups each   *)
    (*   consisting of              *)
    (*      one or more dt element  *)
    (*      followed by             *)
    (*      one or more dd  elements*)
    (********************************)
    value dl :
      ?a: (list (attrib [ < common])) ->
        list
          (((elt [ < `Dt]) * (list (elt [ < `Dt]))) *
           ((elt [ < `Dd]) * (list (elt [ < `Dd]))))
          -> elt [ > `Dl];
    value ol : star [< ol_attrib ] [ < ol_content_fun ] [> ol];
    value ul : star [< ul_attrib ] [ < ul_content_fun ] [> ul];
    value dd : star [< dd_attrib ] [ < dd_content_fun ] [> dd];
    value dt : star [< dt_attrib ] [ < dt_content_fun ] [> dt];
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
    value li : star [< li_attrib ] [ < li_content_fun ] [> li];
    value figcaption : star [< figcaption_attrib ] [ < figcaption_content_fun ] [> figcaption];
    (********************************)
    (*          In Figure           *)
    (********************************)
    (*Either: One figcaption element*)
    (*     followed by flow content.*)
    (*Or: Flow content followed by  *)
    (*     one figcaption element.  *)
    (*Or: Flow content.             *)
    (********************************)
    value figure :
      ?figcaption: (elt [ < `Figcaption]) ->
        star [ < common] [ < flow5] [ > `Figure];
    value hr : nullary [< hr_attrib] [> hr];
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
    value rt :
      ?rp: (rp * rp) ->
        ?a: (list (attrib [ < common])) -> list (elt [ < phrasing]) -> rt;
    value rp :
      ?a: (list (attrib [ < common])) -> list (elt [ < phrasing]) -> rp;
    value ruby :
      ?a: (list (attrib [ < common])) ->
        ruby_content -> list ruby_content -> elt [ > `Ruby];
    (** {2 Semantic} *)
    value b : star [< b_attrib ] [ < b_content_fun ] [> b];
    value i : star [< i_attrib ] [ < i_content_fun ] [> i];
    value small : star [< small_attrib ] [ < small_content_fun ] [> small];
    value sub : star [< sub_attrib ] [ < sub_content_fun ] [> sub];
    value sup : star [< sup_attrib ] [ < sup_content_fun ] [> sup];
    value mark : star [< mark_attrib ] [ < mark_content_fun ] [> mark];
    value wbr : nullary [< wbr_attrib] [> wbr];
    value bdo :
      ~dir: [ < `Ltr | `Rtl] -> star [ < common] [ < phrasing] [ > `Bdo];
    value abbr : star [< abbr_attrib ] [ < abbr_content_fun ] [> abbr];
    value br : nullary [< br_attrib] [> br];
    value cite : star [< cite_attrib ] [ < cite_content_fun ] [> cite];
    value code : star [< code_attrib ] [ < code_content_fun ] [> code];
    value dfn : star [< dfn_attrib ] [ < dfn_content_fun ] [> dfn];
    value em : star [< em_attrib ] [ < em_content_fun ] [> em];
    value kbd : star [< kbd_attrib ] [ < kbd_content_fun ] [> kbd];
    value q : star [< q_attrib ] [ < q_content_fun ] [> q];
    value samp : star [< samp_attrib ] [ < samp_content_fun ] [> samp];
    value span : star [< span_attrib ] [ < span_content_fun ] [> span];
    value strong : star [< strong_attrib ] [ < strong_content_fun ] [> strong];
    value time : star [< time_attrib ] [ < time_content_fun ] [> time];
    value var : star [< var_attrib ] [ < var_content_fun ] [> var];
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
    value a : star [< a_attrib ] 'a [> `A of 'a];
    (** {2 Edit} *)
    (**********************************)
    (* del's children are transparents*)
    (**********************************)
    value del : star [< del_attrib ] 'a [> `Del of 'a];
    (**********************************)
    (* ins's children are transparents*)
    (**********************************)
    value ins : star [< ins_attrib ] 'a [> `Ins of 'a];
    (** {2 Embedded} *)
    (** About images, objects and other things like that.
          Depending on the attributes given to the element,
          the category (interactive, or not) or even the behaviour (transparent) 
          may change. You need to specify these attributes explictly for now. *)
    type expl_attrib 'a 'b;
    value expl_attrib : 'b -> expl_attrib [ = `On ] 'b;
    value expl_none : expl_attrib [ = `Off ] 'b;
    (** The usemap attrib is mandatory, and you should [expl_attrib] and [expl_none]
          to make such attributes. *)
    value img :
      ~src: uri ->
        ~alt: text ->
          ~usemap: (expl_attrib 'a idref) ->
            nullary [ < common | `Height | `Ismap | `Width] [ > `Img of 'a];
    value iframe : (*| `Srcdoc*) star [ < common | `Src | `Name | `Sandbox | `Seamless | `Width | `Height] [ < `PCDATA] [ > `Iframe];
    value object_ :
      ?params: (list (elt [ < `Param])) ->
        ~usemap: (expl_attrib 'b idref) ->
          star
            [ < common | `Data | `Form | `Mime_type | `Height | `Width
              | `Name | `Usemap
            ] 'a [ > `Object of ('a * 'b)];
    value param : nullary [< param_attrib] [> param];
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
    value embed :
      nullary [ < common | `Src | `Height | `Mime_type | `Width] [ > `Embed];
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
    value audio :
      ?srcs: (uri * (list (elt [ < `Source]))) ->
        ~controls: (expl_attrib 'b unit) ->
          star [ < common | `Preload | `Autoplay | `Loop | `Controls] 'a
            [ > `Audio of ('a * 'b)];
    value video :
      ?srcs: (uri * (list (elt [ < `Source]))) ->
        ~controls: (expl_attrib 'b unit) ->
          star
            [ < common | `Poster | `Preload | `Autoplay | `Loop | `Controls
              | `Width | `Height
            ] 'a [ > `Video of ('a * 'b)];
    value canvas : star [< canvas_attrib ] 'a [> `Canvas of 'a];
    value source : nullary [< source_attrib] [> source];
    (********************************)
    (*           In Area            *)
    (********************************)
    (* The alt, target, rel, media, *)
    (* hreflang, and type attributes*)
    (*  must be omitted if the href *)
    (*   attribute is not present.  *)
    (********************************)
    value area :
      ~alt: text ->
        nullary
          [ < common | `Alt | `Coords | `Shape | `Target | `Rel | `Media
            | `Hreflang | `Mime_type
          ] [ > `Area];
    (* XXX: SC : the current system doesn't allow
         to put <area> tag inside a map (a priori) *)
    value map : plus [< map_attrib ] 'a [> `A of 'a];
    (** {2 Tables Data} *)
    value caption : star [< caption_attrib ] [ < caption_content_fun ] [> caption];
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
    value table :
      ?caption: (elt [ < `Caption]) ->
        ?columns: (list (elt [ < `Colgroup])) ->
          ?thead: (elt [ < `Thead]) ->
            ?tfoot: (elt [ < `Tfoot]) ->
              plus [ < common | `Summary] [ < `Tr] [ > `Table];
    value tablex :
      ?caption: (elt [ < `Caption]) ->
        ?columns: (list (elt [ < `Colgroup])) ->
          ?thead: (elt [ < `Thead]) ->
            ?tfoot: (elt [ < `Tfoot]) ->
              star [ < common | `Summary] [ < `Tbody] [ > `Table];
    (********************************)
    (*          In Colgroup         *)
    (********************************)
    (*   If span attribute is:      *)
    (*       -present: Empty.       *)
    (*       -absent: Zero or more  *)
    (*                col elements. *)
    (********************************)
    value colgroup : star [< colgroup_attrib ] [ < colgroup_content_fun ] [> colgroup];
    value col : nullary [< col_attrib] [> col];
    value thead : star [< thead_attrib ] [ < thead_content_fun ] [> thead];
    value tbody : star [< tbody_attrib ] [ < tbody_content_fun ] [> tbody];
    value tfoot : star [< tfoot_attrib ] [ < tfoot_content_fun ] [> tfoot];
    value td : star [< td_attrib ] [ < td_content_fun ] [> td];
    value th : star [< th_attrib ] [ < th_content_fun ] [> th];
    (****************************************)
    (*                 In Tr                *)
    (****************************************)
    (*If the parent node is a thead element:*)
    (*      Zero or more th elements        *)
    (* Otherwise:                           *)
    (*    Zero or more td or th elements    *)
    (****************************************)
    value tr : star [< tr_attrib ] [ < tr_content_fun ] [> tr];
    (** {2 Forms} *)
    value form : plus [< form_attrib ] [ < form_content_fun ] [> form];
    value fieldset :
      ?legend: (elt [ = `Legend ]) ->
        star [ < common | `Disabled | `Form | `Name] [ < flow5] [ > `Fieldset
          ];
    value legend : star [< legend_attrib ] [ < legend_content_fun ] [> legend];
    (** Label authorizes only one  control inside them
        that should be labelled with a [for] attribute
        (although it is not necessary). Such constraints are not currently
        enforced by the type-system *)
    value label : star [< label_attrib ] [ < label_content_fun ] [> label];
    (** If the [type] attribute is not "hidden", must be considered
        as interactive. Distinction not made for now. *)
    value input : nullary [< input_attrib] [> input];
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
    value button : star [< button_attrib ] [ < button_content_fun ] [> button];
    value select : star [< select_attrib ] [ < select_content_fun ] [> select];
    value datalist :
      ?children:
        [ < `Options of list (elt [ < `Option])
          | `Phras of list (elt [ < phrasing])
        ] -> nullary [ < common] [ > `Datalist];
    value optgroup :
      ~label: text ->
        star [ < common | `Disabled | `Label] [ < `Option] [ > `Optgroup];
    value option : unary [< option_attrib ] [ < option_content_fun ] [> selectoption ];
    value textarea : unary [< textarea_attrib ] [ < textarea_content_fun ] [> textarea];
    value keygen : nullary [< keygen_attrib] [> keygen];
    value progress : star [< progress_attrib ] [ < progress_content_fun ] [> progress];
    value meter : star [< meter_attrib ] [ < meter_content_fun ] [> meter];
    value output_elt : star [< output_elt_attrib ] [ < output_elt_content_fun ] [> output_elt];
    (** {2 Data} *)
    value pcdata : string -> elt [ > `PCDATA];
    value entity : string -> elt [ > `PCDATA];
    value space : unit -> elt [ > `PCDATA];
    value cdata : string -> elt [ > `PCDATA];
    (* GK *)
    value cdata_script : string -> elt [ > `PCDATA];
    (* GK *)
    value cdata_style : string -> elt [ > `PCDATA];
    (* GK *)
    (**/**)
    value unsafe_data : string -> elt 'a;
    (**/**)
    (** {2 Interactive} *)
    value details :
      elt [ < `Summary] ->
        star [ < common | `Open] (elt [ < flow5]) [ > `Details];
    value summary : star [< summary_attrib ] [ < summary_content_fun ] [> summary];
    value command :
      ~label: text ->
        nullary
          [ < common | `Icon | `Disabled | `Checked | `Radiogroup
            | `Command_Type
          ] [ > `Command];
    value menu :
      ?child:
        [ < `Lis of list (elt [ < `Li of [ < common]])
          | `Flows of list (elt [ < flow5])
        ] -> nullary [ < common | `Label | `Menu_Type] [ > `Menu];
    (** {2 Scripting} *)
    value script : unary [< script_attrib ] [ < script_content_fun ] [> script];
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
    value noscript : plus [< noscript_attrib ] [ < noscript_content_fun ] [> noscript];
    value meta : nullary [< meta_attrib] [> meta];
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
    value style : star [< style_attrib ] [ < style_content_fun ] [> style];
    (** {2 Link} *)
    value link :
      ~rel: linktypes ->
        ~href: uri ->
          nullary
            [ < common | `Hreflang | `Media | `Rel | `Href | `Sizes
              | `Mime_type
            ] [ > `Link];
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
      [ = `HTML_v03_02 | `HTML_v04_01 | `XHTML_01_00 | `XHTML_01_01
        | `XHTML_05_00 | `Doctype of string
      ];
    value doctype : [ < doctypes] -> string;
    (** {1 Tools} *)
    value version : string;
    value standard : uri;
    (*
    val validator : uri
    val validator_icon : unit -> [>`A] elt
(** A hyperlink to the W3C validator, including the logo.
    @see <http://validator.w3.org> Validator *)
*)
    value tot : XML.elt -> elt 'a;
    value totl : list XML.elt -> list (elt 'a);
    value toelt : elt 'a -> XML.elt;
    value toeltl : list (elt 'a) -> list XML.elt;
  end;
(** An alias for XHTML5:
    @see <http://www.w3.org/TR/html5/> HTML5 *)
module type T_05_00 = T;
(* END INTERFACE *)
(* BEGIN INTERFACE

module M : T
module M_05_00 : T_05_00

   END INTERFACE *)
module M_05_00 : T_05_00 =
  struct
    type attrib 'a = XML.attrib;
    type attribs 'a = XML.attribs;
    value to_xmlattribs x = x;
    (* VB *)
    value float_attrib = XML.float_attrib;
    value int_attrib = XML.int_attrib;
    value string_attrib = XML.string_attrib;
    value uri_attrib a s = XML.string_attrib a (string_of_uri s);
    value space_sep_attrib = XML.space_sep_attrib;
    value comma_sep_attrib = XML.comma_sep_attrib;
    value event_attrib = XML.event_attrib;
    (* space-separated *)
    value length_attrib name =
      fun
      [ `Pixels p -> int_attrib name p
      | `Percent p -> string_attrib name ((string_of_int p) ^ "%") ];
    value multilength_attrib name =
      fun
      [ (#length as l) -> length_attrib name l
      | `Relative 1 -> string_attrib name "*"
      | `Relative i -> string_attrib name ((string_of_int i) ^ "*") ];
    value multilength_to_string =
      fun
      [ `Pixels p -> string_of_int p
      | `Percent p -> (string_of_int p) ^ "%"
      | `Relative 1 -> "*"
      | `Relative i -> (string_of_int i) ^ "*" ];
    value multilengths_attrib name multilengths =
      string_attrib name
        (String.concat ", " (List.map multilength_to_string multilengths));
    value linktype_to_string =
      fun
      [ `Alternate -> "alternate"
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
      | `Other t -> t ];
    value linktypes_attrib name linktypes =
      string_attrib name
        (String.concat " " (List.map linktype_to_string linktypes));
    value mediadesc_to_string =
      fun
      [ `All -> "all"
      | `Aural -> "aural"
      | `Braille -> "braille"
      | `Embossed -> "embossed"
      | `Handheld -> "handheld"
      | `Print -> "print"
      | `Projection -> "projection"
      | `Screen -> "screen"
      | `Speech -> "speech"
      | `TTY -> "tty"
      | `TV -> "tv" ];
    value mediadesc_attrib name mediadescs =
      string_attrib name
        (String.concat ", " (List.map mediadesc_to_string mediadescs));
    (* Core: *)
    value a_class = space_sep_attrib XML.class_name;
      (* class is different on client side.
         We put the value in xML.ml 
         because this file has a different implementation client side.
      *)
    value a_id = string_attrib "id";
    value a_user_data name = string_attrib ("data-" ^ name);
    value a_title = string_attrib "title";
    (* I18N: *)
    value a_xml_lang = string_attrib "xml:lang";
    (* Style: *)
    value a_style = string_attrib "style";
    (* Events: *)
    value a_onabort = event_attrib "onabort";
    value a_onafterprint = event_attrib "onafterprint";
    value a_onbeforeprint = event_attrib "onbeforeprint";
    value a_onbeforeunload = event_attrib "onbeforeunload";
    value a_onblur = event_attrib "onblur";
    value a_oncanplay = event_attrib "oncanplay";
    value a_oncanplaythrough = event_attrib "oncanplaythrough";
    value a_onchange = event_attrib "onchange";
    value a_onclick = event_attrib "onclick";
    value a_oncontextmenu = event_attrib "oncontextmenu";
    value a_ondblclick = event_attrib "ondblclick";
    value a_ondrag = event_attrib "ondrag";
    value a_ondragend = event_attrib "ondragend";
    value a_ondragenter = event_attrib "ondragenter";
    value a_ondragleave = event_attrib "ondragleave";
    value a_ondragover = event_attrib "ondragover";
    value a_ondragstart = event_attrib "ondragstart";
    value a_ondrop = event_attrib "ondrop";
    value a_ondurationchange = event_attrib "ondurationchange";
    value a_onemptied = event_attrib "onemptied";
    value a_onended = event_attrib "onended";
    value a_onerror = event_attrib "onerror";
    value a_onfocus = event_attrib "onfocus";
    value a_onformchange = event_attrib "onformchange";
    value a_onforminput = event_attrib "onforminput";
    value a_onhashchange = event_attrib "onhashchange";
    value a_oninput = event_attrib "oninput";
    value a_oninvalid = event_attrib "oninvalid";
    value a_onmousedown = event_attrib "onmousedown";
    value a_onmouseup = event_attrib "onmouseup";
    value a_onmouseover = event_attrib "onmouseover";
    value a_onmousemove = event_attrib "onmousemove";
    value a_onmouseout = event_attrib "onmouseout";
    value a_onmousewheel = event_attrib "onmousewheel";
    value a_onoffline = event_attrib "onoffline";
    value a_ononline = event_attrib "ononline";
    value a_onpause = event_attrib "onpause";
    value a_onplay = event_attrib "onplay";
    value a_onplaying = event_attrib "onplaying";
    value a_onpagehide = event_attrib "onpagehide";
    value a_onpageshow = event_attrib "onpageshow";
    value a_onpopstate = event_attrib "onpopstate";
    value a_onprogress = event_attrib "onprogress";
    value a_onratechange = event_attrib "onratechange";
    value a_onreadystatechange = event_attrib "onreadystatechange";
    value a_onredo = event_attrib "onredo";
    value a_onresize = event_attrib "onresize";
    value a_onscroll = event_attrib "onscroll";
    value a_onseeked = event_attrib "onseeked";
    value a_onseeking = event_attrib "onseeking";
    value a_onselect = event_attrib "onselect";
    value a_onshow = event_attrib "onshow";
    value a_onstalled = event_attrib "onstalled";
    value a_onstorage = event_attrib "onstorage";
    value a_onsubmit = event_attrib "onsubmit";
    value a_onsuspend = event_attrib "onsuspend";
    value a_ontimeupdate = event_attrib "ontimeupdate";
    value a_onundo = event_attrib "onundo";
    value a_onunload = event_attrib "onunload";
    value a_onvolumechange = event_attrib "onvolumechange";
    value a_onwaiting = event_attrib "onwaiting";
    value a_onkeypress = event_attrib "onkeypress";
    value a_onkeydown = event_attrib "onkeydown";
    value a_onkeyup = event_attrib "onkeyup";
    value a_onload = event_attrib "onload";
    value a_onloadeddata = event_attrib "onloadeddata";
    value a_onloadedmetadata = event_attrib "";
    value a_onloadstart = event_attrib "onloadstart";
    value a_onmessage = event_attrib "onmessage";
    (* Other Attributes *)
    value a_version = string_attrib "version";
    value a_xmlns =
      fun
      [ `W3_org_1999_xhtml ->
          string_attrib "xmlns" "http://www.w3.org/1999/xhtml" ];
    value a_manifest = uri_attrib "manifest";
    value a_cite = uri_attrib "cite";
    value a_xml_space =
      fun [ `Preserve -> string_attrib "xml:space" "preserve" ];
    value a_accesskey c = string_attrib "accesskey" (String.make 1 c);
    value a_charset = string_attrib "charset";
    value a_accept_charset = space_sep_attrib "accept-charset";
    value a_accept = space_sep_attrib "accept";
    value a_href = uri_attrib "href";
    value a_hreflang = string_attrib "hreflang";
    value a_rel = linktypes_attrib "rel";
    value a_tabindex = int_attrib "tabindex";
    value a_mime_type = string_attrib "type";
    value a_alt = string_attrib "alt";
    value a_height p = int_attrib "height" p;
    value a_src = uri_attrib "src";
    value a_width p = int_attrib "width" p;
    value a_for = string_attrib "for";
    value a_for_list = space_sep_attrib "for";
    value a_selected =
      fun [ `Selected -> string_attrib "selected" "selected" ];
    value a_text_value = string_attrib "value";
    value a_int_value = int_attrib "value";
    value a_value = string_attrib "value";
    value a_float_value = float_attrib "value";
    value a_action = uri_attrib "action";
    value a_method m =
      string_attrib "method"
        (match m with
         [ `Get -> "GET"
         | `Post -> "POST"
         | `Put -> "PUT"
         | `Delete -> "DELETE" ]);
    value a_enctype = string_attrib "enctype";
    value a_checked = fun [ `Checked -> string_attrib "checked" "checked" ];
    value a_disabled =
      fun [ `Disabled -> string_attrib "disabled" "disabled" ];
    value a_readonly =
      fun [ `Readonly -> string_attrib "readonly" "readonly" ];
    value a_maxlength = int_attrib "maxlength";
    value a_name = string_attrib "name";
    value a_autocomplete ac =
      string_attrib "autocomplete"
        (match ac with [ `On -> "on" | `Off -> "off" ]);
    value a_async = fun [ `Async -> string_attrib "async" "async" ];
    value a_autofocus =
      fun [ `Autofocus -> string_attrib "autofocus" "autofocus" ];
    value a_autoplay =
      fun [ `Autoplay -> string_attrib "autoplay" "autoplay" ];
    value a_challenge = string_attrib "challenge";
    value a_contenteditable ce =
      string_attrib "contexteditable"
        (match ce with [ `True -> "true" | `False -> "false" ]);
    value a_contextmenu = string_attrib "contextmenu";
    value a_controls =
      fun [ `Controls -> string_attrib "controls" "controls" ];
    value a_dir d =
      string_attrib "dir" (match d with [ `Ltr -> "ltr" | `Rtl -> "rtl" ]);
    value a_draggable d =
      string_attrib "draggable"
        (match d with [ `True -> "true" | `False -> "false" ]);
    value a_form = string_attrib "form";
    value a_formaction = uri_attrib "formaction";
    value a_formenctype = string_attrib "formenctype";
    value a_formmethod m =
      string_attrib "method"
        (match m with
         [ `Get -> "GET"
         | `Post -> "POST"
         | `Put -> "PUT"
         | `Delete -> "DELETE" ]);
    value a_formnovalidate =
      fun
      [ `Formnovalidate -> string_attrib "formnovalidate" "formnovalidate" ];
    value a_formtarget = string_attrib "formtarget";
    value a_hidden = fun [ `Hidden -> string_attrib "hidden" "hidden" ];
    value a_high = float_attrib "high";
    value a_icon = uri_attrib "icon"; 
    value a_ismap = fun [ `Ismap -> string_attrib "ismap" "ismap" ];
    value a_keytype = string_attrib "keytype";
    value a_list = string_attrib "list";
    value a_loop = fun [ `Loop -> string_attrib "loop" "loop" ];
    value a_low = float_attrib "low";
    value a_max = float_attrib "max";
    value a_input_max = int_attrib "max";
    value a_min = float_attrib "min";
    value a_input_min = int_attrib "min";
    value a_novalidate =
      fun [ `Novalidate -> string_attrib "novalidate" "novalidate" ];
    value a_open = fun [ `Open -> string_attrib "open" "open" ];
    value a_optimum = float_attrib "optimum";
    value a_pattern = string_attrib "pattern";
    value a_placeholder = string_attrib "placeholder";
    value a_poster = uri_attrib "poster";
    value a_preload pl =
      string_attrib "preload"
        (match pl with
         [ `None -> "none"
         | `Metadata -> "metadata"
         | `Audio -> "audio" ]);
    value a_pubdate = fun [ `Pubdate -> string_attrib "pubdate" "pubdate" ];
    value a_radiogroup = string_attrib "radiogroup";
    value a_required =
      fun [ `Required -> string_attrib "required" "required" ];
    value a_reserved =
      fun [ `Reserved -> string_attrib "reserved" "reserved" ];
    value rec a_sandbox sb =
      let rec aux sb =
        match sb with
        [ [ `AllowSameOrigin :: a ] -> [ "allow-same-origin" :: aux a ]
        | [ `AllowForms :: a ] -> [ "allow-forms" :: aux a ]
        | [ `AllowScript :: a ] -> [ "allow-script" :: aux a ]
        | [] -> [] ]
      in space_sep_attrib "sandbox" (aux sb);
    value a_spellcheck sc =
      string_attrib "spellckeck"
        (match sc with [ `True -> "true" | `False -> "false" ]);
    value a_scoped = fun [ `Scoped -> string_attrib "scoped" "scoped" ];
    value a_seamless =
      fun [ `Seamless -> string_attrib "seamless" "seamless" ];
    value a_sizes sizes =
      string_attrib "sizes"
        (String.concat " " (List.map string_of_int sizes));
    value a_span = int_attrib "span";
    (*let a_srcdoc*)
    value a_srclang = string_attrib "xml:lang";
    value a_start = int_attrib "start";
    value a_step = float_attrib "step";
    value a_wrap w =
      string_attrib "wrap"
        (match w with [ `Soft -> "soft" | `Hard -> "hard" ]);
    value a_size = int_attrib "size";
    value a_input_type it =
      string_attrib "type"
        (match it with
         [ `Url -> "url"
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
         | `Hidden -> "hidden" ]);
    value a_menu_type mt =
      string_attrib "type"
        (match mt with [ `Context -> "context" | `Toolbar -> "toolbar" ]);
    value a_command_type ct =
      string_attrib "type"
        (match ct with
         [ `Command -> "command"
         | `Checkbox -> "checkbox"
         | `Radio -> "radio" ]);
    value a_button_type bt =
      string_attrib "type"
        (match bt with
         [ `Button -> "button"
         | `Submit -> "submit"
         | `Reset -> "reset" ]);
    value a_multiple =
      fun [ `Multiple -> string_attrib "multiple" "multiple" ];
    value a_cols = int_attrib "cols";
    value a_rows = int_attrib "rows";
    value a_summary = string_attrib "summary";
    value a_align a =
      string_attrib "align"
        (match a with
         [ `Left -> "left"
         | `Right -> "right"
         | `Justify -> "justify"
         | `Char -> "char" ]);
    value a_axis = string_attrib "axis";
    value a_colspan = int_attrib "colspan";
    value a_headers = space_sep_attrib "headers";
    value a_rowspan = int_attrib "rowspan";
    value a_scope s =
      string_attrib "scope"
        (match s with
         [ `Row -> "row"
         | `Col -> "col"
         | `Rowgroup -> "rowgroup"
         | `Colgroup -> "colgroup" ]);
    value a_border = int_attrib "border";
    value a_cellpadding = length_attrib "cellpadding";
    value a_cellspacing = length_attrib "cellspacing";
    value a_datapagesize = string_attrib "datapagesize";
    value a_rules r =
      string_attrib "rules"
        (match r with
         [ `None -> "none"
         | `Groups -> "groups"
         | `Rows -> "rows"
         | `Cols -> "cols"
         | `All -> "all" ]);
    value a_char c = string_attrib "char" (String.make 1 c);
    value a_charoff = length_attrib "charoff";
    value a_data = uri_attrib "data";
    value a_codetype = string_attrib "codetype";
    value a_fs_rows mls = multilengths_attrib "rows" mls;
    value a_fs_cols mls = multilengths_attrib "cols" mls;
    value a_frameborder b =
      int_attrib "frameborder" (match b with [ `Zero -> 0 | `One -> 1 ]);
    value a_marginheight = int_attrib "marginheight";
    value a_marginwidth = int_attrib "marginwidth";
    value a_scrolling s =
      string_attrib "scrolling"
        (match s with [ `Yes -> "yes" | `No -> "no" | `Auto -> "auto" ]);
    value a_target = string_attrib "target";
    value a_content = string_attrib "content";
    value a_http_equiv = string_attrib "http-equiv";
    value a_media = mediadesc_attrib "media";
    type elt 'a = XML.elt;
    type html = elt [ = `Html ];
    (* NB: These are more general than the ones in xHTML.mli *)
    type nullary 'a 'b = ?a: (list (attrib 'a)) -> unit -> elt 'b;
    type unary 'a 'b 'c = ?a: (list (attrib 'a)) -> elt 'b -> elt 'c;
    type binary 'a 'b 'c 'd =
      ?a: (list (attrib 'a)) -> elt 'b -> elt 'c -> elt 'd;
    type tri 'b 'c 'd 'e = elt 'b -> elt 'c -> elt 'd -> elt 'e;
    type star 'a 'b 'c = ?a: (list (attrib 'a)) -> list (elt 'b) -> elt 'c;
    type plus 'a 'b 'c =
      ?a: (list (attrib 'a)) -> elt 'b -> list (elt 'b) -> elt 'c;
    value terminal tag ?a () = XML.leaf ?a tag;
    (* let nullary tag ?a () = XML.node ?a tag [] *)
    value unary tag ?a elt = XML.node ?a tag [ elt ];
    value binary tag ?a elt1 elt2 = XML.node ?a tag [ elt1; elt2 ];
    value tri tag elt1 elt2 elt3 = XML.node tag [ elt1; elt2; elt3 ];
    value star tag ?a elts = XML.node ?a tag elts;
    value plus tag ?a elt elts = XML.node ?a tag [ elt :: elts ];
    value list_of_option = fun [ Some x -> [ x ] | None -> [] ];
    value list_of_list_option = fun [ Some x -> x | None -> [] ];
    value srcs_option = fun [ Some (`Srcs s) -> s | None -> [] ];
    value phrasing_option = fun [ Some (`Phras p) -> p | None -> [] ];
    value ruby_option =
      fun [ Some (`Rt_elt r) -> r | Some (`Group g) -> g | None -> [] ];
    value body_option =
      fun [ Some (`Body b) -> b | Some (`Trs t) -> t | None -> [] ];
    value colg_option = fun [ Some (`Colgroups c) -> c | None -> [] ];
    value opts_option =
      fun [ Some (`Options o) -> o | Some (`Optgroups o) -> o | None -> [] ];
    value li_option =
      fun [ Some (`Lis l) -> l | Some (`Flows f) -> f | None -> [] ];
    value opt_option =
      fun [ Some (`Options o) -> o | Some (`Phras p) -> p | None -> [] ];
    value param_option = fun [ Some (`Params p) -> p | None -> [] ];
    value cols_option =
      fun [ Some (`Cols c) -> c | Some (`Colgroups c) -> c | None -> [] ];
    value body = star "body";
    value head = plus "head";
    value title = unary "title";
    value html = binary "html";
    value footer = star "footer";
    value header = star "header";
    value section = star "section";
    value nav = star "nav";
    value pcdata = XML.pcdata;
    value entity = XML.entity;
    value space () = entity "nbsp";
    value cdata = XML.cdata;
    value cdata_script = XML.cdata_script;
    value cdata_style = XML.cdata_style;
    value unsafe_data s = XML.encodedpcdata s;
    value unsafe_data s = XML.encodedpcdata s;
    value h1 = star "h1";
    value h2 = star "h2";
    value h3 = star "h3";
    value h4 = star "h4";
    value h5 = star "h5";
    value h6 = star "h6";
    value hgroup = plus "hgroup";
    value address = star "address";
    value blockquote = star "blockquote";
    value div = star "div";
    value p = star "p";
    value pre = star "pre";
    value abbr = star "abbr";
    value br = terminal "br";
    value cite = star "cite";
    value code = star "code";
    value dfn = star "dfn";
    value em = star "em";
    value kbd = star "kbd";
    value q = star "q";
    value samp = star "samp";
    value span = star "span";
    value strong = star "strong";
    value time = star "time";
    value var = star "var";
    value a = star "a";
    value dl ?a list =
      XML.node ?a "dl"
        (List.concat
           (List.map
              (fun ((elt, elts), (elt', elts')) ->
                 [ elt :: elts ] @ [ elt' :: elts' ])
              list));
    value ol = star "ol";
    value ul = star "ul";
    value dd = star "dd";
    value dt = star "dt";
    value li = star "li";
    value hr = terminal "hr";
    value b = star "b";
    value i = star "i";
    value small = star "small";
    value sub = star "sub";
    value sup = star "sup";
    value mark = star "mark";
    value rp ?(a = []) elts = (a, elts);
    value rt ?rp ?a elts =
      match rp with
      [ Some (a1, e1) (a2, e2) ->
          `Rpt (XML.node ~a: a1 "rp" e1) (XML.node ?a "rt" elts)
            (XML.node ~a: a2 "rp" e2)
      | None -> `Rt (XML.node ?a "rt" elts) ];
    value ruby ?a elt elts =
      let rec aux =
        fun
        [ [] -> []
        | [ (pel, `Rt e) :: l ] -> pel @ [ e :: aux l ]
        | [ (pel, `Rpt e1 e2 e3) :: l ] -> pel @ [ e1; e2; e3 :: aux l ] ]
      in XML.node ?a "ruby" (aux [ elt :: elts ]);
    value wbr = terminal "wbr";
    (* VB *)
    type shape = [ = `Rect | `Circle | `Poly | `Default ];
    value bdo ~dir ?(a = []) elts =
      XML.node ~a: [ a_dir dir :: a ] "bdo" elts;
    value a_datetime = string_attrib "datetime";
    value a_shape d =
      string_attrib "shape"
        (match d with
         [ `Rect -> "rect"
         | `Circle -> "circle"
         | `Poly -> "poly"
         | `Default -> "default" ]);
    value a_coords coords =
      string_attrib "coords"
        (String.concat "," (List.map string_of_int coords));
    value a_usemap = string_attrib "usemap";
    value a_defer = fun [ `Defer -> string_attrib "defer" "defer" ];
    value a_label = string_attrib "label";
    value area ~alt ?(a = []) () = XML.leaf ~a: [ a_alt alt :: a ] "area";
    value map = plus "map";
    value del = star "del";
    value ins = star "ins";
    value script ?(a = []) elt = XML.node ~a "script" [ elt ];
    value noscript = plus "noscript";
    value article = star "article";
    value aside = star "aside";
    value video_audio name ?srcs ~controls ?(a = []) elts =
      let a =
        match controls with
        [ Some () -> [ a_controls `Controls :: a ]
        | None -> a ] in
      let (a, children) =
        match srcs with
        [ None -> (a, elts)
        | Some uri srcs -> ([ a_src uri :: a ], (srcs @ elts)) ]
      in XML.node ~a name children;
    value audio = video_audio "audio";
    value video = video_audio "video";
    value canvas = star "canvas";
    value command ~label ?(a = []) () =
      XML.leaf ~a: [ a_label label :: a ] "command";
    value menu ?child ?a () = XML.node ?a "menu" (li_option child);
    value embed = terminal "embed";
    value source = terminal "source";
    value meter = star "meter";
    value output_elt = star "output";
    value form = plus "form";
    type input_attr =
      [ = common | `Accept | `Alt | `Autocomplete | `Autofocus | `Checked
        | `Disabled | `Form | `Formation | `Formenctype | `Formmethod
        | `Formnovalidate | `Formtarget | `Height | `List | `Input_Max
        | `Maxlength | `Input_Min | `Multiple | `Name | `Pattern
        | `Placeholder | `ReadOnly | `Required | `Size | `Src | `Step
        | `Input_Type | `Value | `Width
      ];
    value input = terminal "input";
    value keygen = terminal "keygen";
    value label = star "label";
    value option = unary "option";
    value select = star "select";
    value textarea = unary "textarea";
    value button = star "button";
    value datalist ?children ?a () =
      let children =
        match children with
        [ Some (`Options x) -> x
        | Some (`Phras x) -> x
        | None -> [] ]
      in XML.node ?a "datalist" children;
    value progress = star "proress";
    value legend = star "legend";
    value details summary ?a children =
      XML.node "details" ?a [ summary :: children ];
    value summary = star "summary";
    value fieldset ?legend ?a elts =
      XML.node ?a "fieldset" ((list_of_option legend) @ elts);
    value optgroup ~label ?(a = []) elts =
      XML.node ~a: [ a_label label :: a ] "optgroup" elts;
    value figcaption = star "figcaption";
    value figure ?figcaption ?a elts =
      XML.node ?a "figure" ((list_of_option figcaption) @ elts);
    value caption = star "caption";
    value table ?caption ?(columns = []) ?thead ?tfoot ?a elt elts =
      XML.node ?a "table"
        ((list_of_option caption) @
           (columns @
              ((list_of_option thead) @
                 ((list_of_option tfoot) @ [ elt :: elts ]))));
    value tablex ?caption ?(columns = []) ?thead ?tfoot ?a elts =
      XML.node ?a "table"
        ((list_of_option caption) @
           (columns @
              ((list_of_option thead) @ ((list_of_option tfoot) @ elts))));
    value td = star "td";
    value th = star "th";
    value tr = star "tr";
    value colgroup = star "colgroup";
    value col = terminal "col";
    value thead = star "thead";
    value tbody = star "tbody";
    value tfoot = star "tfoot";
    value iframe = star "iframe";
    value object_ ?(params = []) ~usemap ?(a = []) elts =
      let a =
        match usemap with
        [ Some idref -> [ a_usemap idref :: a ]
        | None -> a ]
      in XML.node ~a "object" (params @ elts);
    value param = terminal "param";
    type expl_attrib 'a 'b = option 'b;
    value expl_attrib x = Some x;
    value expl_none = None;
    value img ~src ~alt ~usemap ?(a = []) () =
      let li = [ a_src src; a_alt alt :: a ] in
      let a =
        match usemap with
        [ Some usemap -> [ a_usemap usemap :: li ]
        | _ -> li ]
      in XML.leaf ~a "img";
    value meta = terminal "meta";
    value style ?(a = []) elts = XML.node ~a "style" elts;
    value link ~rel ~href ?(a = []) () =
      XML.leaf ~a: [ a_rel rel; a_href href :: a ] "link";
    value base = terminal "base";
    (* VB *)
    type scripttag = [ = `Script | `Noscript ];
    type misc = scripttag;
    type heading = [ = `H1 | `H2 | `H3 | `H4 | `H5 | `H6 | `Hgroup ];
    type sectioning = [ = `Section | `Nav | `Aside | `Article ];
    type resetable = [ = `Textarea | `Select | `Output | `Keygen | `Input ];
    type submitable = [ = `Textarea | `Select | `Keygen | `Input | `Button ];
    type labelable = [ = resetable | `Progress | `Meter | `Button ];
    type formatblock =
      [ = heading | sectioning | `Pre | `P | `Header | `Footer | `Div
        | `Blockquote | `Address
      ];
    type sectionningroot =
      [ = `Td | `Figure | `Fieldset | `Details | `Body | `Blockquote
      ];
    type listed = [ = resetable | submitable | `Fieldset ];
    type formassociated = [ = listed | `Progress | `Meter | `Label ];
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
    (* a boolean at the type-level *)
    type on = [ = `On ];
    type off = [ = `Off ];
    type on_off = [ = on | off ];
    type transparent 'interactive 'noscript 'regular 'media =
      [ = `A of 'interactive | `Noscript of 'noscript | `Canvas of 'regular
        | `Map of 'regular | `Ins of 'regular | `Del of 'regular
        | `Object of ('regular * on_off) | `Audio of ('media * on_off)
        | `Video of ('media * on_off)
      ];
    type transparent_without_interactive 'a =
      [ = `Noscript of 'a | `Ins of 'a | `Del of 'a | `Object of ('a * off)
        | `Canvas of 'a | `Map of 'a | `Audio of ('a * off)
        | `Video of ('a * off)
      ];
    type transparent_without_noscript 'a =
      [ = `A of 'a | `Ins of 'a | `Del of 'a | `Canvas of 'a | `Map of 'a
        | `Object of ('a * on_off) | `Video of ('a * on_off)
        | `Audio of ('a * on_off)
      ];
    type transparent_without_media 'a =
      [ = `A of 'a | `Ins of 'a | `Del of 'a | `Map of 'a | `Canvas of 'a
        | `Object of ('a * on_off)
      ];
    (** Metadata without title *)
    type metadata_without_title =
      [ = `Style | `Script | `Noscript of [ = `Meta | `Link | `Style ]
        | `Meta | `Link | `Command | `Base
      ];
    (** Metadata contents. Used specially in <head> *)
    type metadata = [ = metadata_without_title | `Title ];
    (** Interactive contents : contents that require user-interaction 
        (Forms, link, etc.) *)
    (** Core element types are element types without transparent. *)
    type core_interactive =
      [ = `Textarea | `Select | `Menu | `Label | `Keygen | `Input
        | `Img of on | `Iframe | `Embed | `Details | `Button
      ];
    type interactive =
      [ = core_interactive | transparent_without_interactive interactive
      ];
    (** Phrasing contents is inline contents : bold text, span, and so on. *)
    type core_phrasing =
      [ = labelable | submitable | `Wbr | `Var | `Time | `Sup | `Sub
        | `Strong | `Span | `Small | `Script | `Samp | `Ruby | `Q | `Mark
        | `Label | `Kbd | `I | `Em | `Dfn | `Datalist | `Command | `Code
        | `Cite | `Br | `Bdo | `B | `Abbr | `PCDATA
      ];
    type phrasing_without_noscript =
      [ = labelable | submitable | `Wbr | `Var | `Time | `Sup | `Sub
        | `Strong | `Span | `Small | `Script | `Samp | `Ruby | `Q | `Mark
        | `Label | `Kbd | `I | `Em | `Dfn | `Datalist | `Command | `Code
        | `Cite | `Br | `Bdo | `B | `Abbr | `PCDATA
        | transparent_without_noscript phrasing_without_noscript
      ];
    type core_phrasing_without_media =
      [ = labelable | submitable | (* `Math |`Svg |*) `Img of on_off
        | `Iframe | `Embed | `Wbr | `Var | `Time | `Sup | `Sub | `Strong
        | `Span | `Small | `Script | `Samp | `Ruby | `Q | `Mark | `Label
        | `Kbd | `I | `Em | `Dfn | `Datalist | `Command | `Code | `Cite | `Br
        | `Bdo | `B | `Abbr | `PCDATA
      ];
    type phrasing_without_media =
      [ = core_phrasing_without_media
        | transparent_without_media phrasing_without_media
      ];
    type core_phrasing_without_interactive =
      [ = labelable | submitable | `Wbr | `Var | `Time | `Sup | `Sub
        | `Strong | `Span | `Small | `Script | `Samp | `Ruby | `Q | `Mark
        | `Kbd | `Img of off | `I | `Em | `Dfn | `Datalist | `Command | `Code
        | `Cite | `Br | `Bdo | `B | `Abbr | `PCDATA
      ];
    type phrasing_without_interactive =
      [ = core_phrasing_without_interactive
        | transparent_without_interactive phrasing_without_interactive
      ];
    type phrasing =
      [ =
        transparent phrasing_without_interactive phrasing_without_noscript
          phrasing phrasing_without_media
        | core_phrasing
      ];
    (** Phrasing without the interactive markups *)
    type phrasing_without_dfn =
      [ = labelable | submitable | `Wbr | `Var | `Time | `Sup | `Sub
        | `Strong | `Span | `Small | `Script | `Samp | `Ruby | `Q | `Mark
        | `Label | `Kbd | `I | `Em | `Datalist | `Command | `Code | `Cite
        | `Br | `Bdo | `B | `Abbr | `PCDATA
        | transparent phrasing_without_interactive phrasing_without_noscript
            phrasing_without_dfn phrasing_without_media
      ];
    type phrasing_without_label =
      [ = labelable | submitable | `Wbr | `Var | `Time | `Sup | `Sub
        | `Strong | `Span | `Small | `Script | `Samp | `Ruby | `Q | `Mark
        | `Kbd | `I | `Em | `Dfn | `Datalist | `Command | `Code | `Cite | `Br
        | `Bdo | `B | `Abbr | `PCDATA
        | transparent phrasing_without_interactive phrasing_without_noscript
            phrasing_without_label phrasing_without_media
      ];
    type phrasing_without_progress =
      [ = resetable | submitable | `Wbr | `Var | `Time | `Sup | `Sub
        | `Strong | `Span | `Small | `Script | `Samp | `Ruby | `Q | `Meter
        | `Mark | `Label | `Kbd | `I | `Em | `Dfn | `Datalist | `Command
        | `Code | `Cite | `Button | `Br | `Bdo | `B | `Abbr | `PCDATA
        | transparent phrasing_without_interactive phrasing_without_noscript
            phrasing_without_progress phrasing_without_media
      ];
    type phrasing_without_time =
      [ = labelable | submitable | `Wbr | `Var | `Sup | `Sub | `Strong
        | `Span | `Small | `Script | `Samp | `Ruby | `Q | `Mark | `Label
        | `Kbd | `I | `Em | `Dfn | `Datalist | `Command | `Code | `Cite | `Br
        | `Bdo | `B | `Abbr | `PCDATA
        | transparent phrasing_without_interactive phrasing_without_noscript
            phrasing_without_time phrasing_without_media
      ];
    type phrasing_without_meter =
      [ = submitable | resetable | `Progress | `Button | `Wbr | `Var | `Time
        | `Sup | `Sub | `Strong | `Span | `Small | `Script | `Samp | `Ruby
        | `Q | `Mark | `Label | `Kbd | `I | `Em | `Dfn | `Datalist | `Command
        | `Code | `Cite | `Br | `Bdo | `B | `Abbr | `PCDATA
        | transparent phrasing_without_interactive phrasing_without_noscript
            phrasing_without_meter phrasing_without_media
      ];
    (******************)
    (* Map  Ins
           Del  A         *)
    (*  in Phrasing   *)
    (* with conditions*)
    (******************)
    type core_flow5 =
      [ = core_phrasing | formassociated | formatblock | `Ul | `Table
        | `Style | `Ol | `Menu | `Hr | `Form | `Figure | `Dl
      ];
    type flow5_without_interactive =
      [ = core_flow5
        | transparent_without_interactive flow5_without_interactive
      ];
    type flow5_without_noscript =
      [ = core_flow5 | transparent_without_noscript flow5_without_noscript
      ];
    type flow5_without_media =
      [ = core_phrasing_without_media | `Textarea | `Select | `Menu | `Label
        | `Keygen | `Input | `Img of on_off | `Iframe | `Embed | `Details
        | `Button | formassociated | formatblock | `Ul | `Table | `Style
        | `Ol | `Menu | `Hr | `Form | `Figure | `Dl
        | transparent_without_media flow5_without_media
      ];
    type flow5 =
      [ = core_flow5
        | transparent flow5_without_interactive flow5_without_noscript flow5
            flow5_without_media
      ];
    type flow5_without_table =
      [ = core_phrasing | formassociated | formatblock | `Ul | `Style | `Ol
        | `Menu | `Hr | `Form | `Figure | `Dl | `Ins_flow
        | transparent flow5_without_interactive flow5_without_noscript flow5
            flow5_without_media
      ];
    type flow5_without_header_footer =
      [ = heading | sectioning | `Pre | `P | `Div | `Blockquote | `Address
        | core_phrasing | formassociated | `Ul | `Table | `Style | `Ol
        | `Menu | `Hr | `Form | `Figure | `Dl | `Ins_flow
        | transparent flow5_without_interactive flow5_without_noscript flow5
            flow5_without_media
      ];
    type flow5_without_form =
      [ = core_phrasing | formassociated | formatblock | `Ul | `Table
        | `Style | `Ol | `Menu | `Hr | `Figure | `Dl
        | transparent flow5_without_interactive flow5_without_noscript flow5
            flow5_without_media
      ];
    type flow5_without_sectioning_heading_header_footer_address =
      [ = core_phrasing | formassociated | `Pre | `P | `Div | `Blockquote
        | `Ul | `Table | `Style | `Ol | `Menu | `Hr | `Form | `Figure | `Dl
        | transparent flow5_without_interactive flow5_without_noscript flow5
            flow5_without_media
      ];
    type rt =
      [ = `Rt of elt [ = `Rt ]
        | `Rpt of ((elt [ = `Rp ]) * (elt [ = `Rt ]) * (elt [ = `Rp ]))
      ];
    type ruby_content = ((list (elt phrasing)) * rt);
    type rp = ((list (attrib common)) * (list (elt phrasing)));
    (* I/O *)
    value compose_doctype dt args =
      "<!DOCTYPE " ^
        (dt ^
           (" PUBLIC " ^
              ((String.concat " "
                  (List.map (fun a -> "\"" ^ (a ^ "\"")) args))
                 ^ ">\n")));
    type doctypes =
      [ = `HTML_v03_02 | `HTML_v04_01 | `XHTML_01_00 | `XHTML_01_01
        | `XHTML_05_00 | `Doctype of string
      ];
    value doctype =
      fun
      [ `HTML_v03_02 ->
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
      | `Doctype s -> s ];
    value no_break =
      [ "title"; "h1"; "h2"; "h3"; "h4"; "h5"; "h6"; "address"; "blockquote";
        "div"; "p"; "li"; "dd"; "dt"; "td"; "th" ];
    value preformatted = [ "pre" ];
    (* Tools *)
    value version =
      fun
      [ `XHTML_01_00 -> "XHTML 1.0"
      | `XHTML_01_01 -> "XHTML 1.1"
      | `XHTML_05_00 -> "XHTML 5.0" ];
    value standard =
      fun
      [ `XHTML_01_00 -> uri_of_string "http://www.w3.org/TR/xhtml1/"
      | `XHTML_01_01 -> uri_of_string "http://www.w3.org/TR/xhtml11/"
      | `XHTML_05_00 -> uri_of_string "http://www.w3.org/TR/xhtml5" ];
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
    value tot x = x;
    value totl x = x;
    value toelt x = x;
    value toeltl x = x;
    value xhtml_version = `XHTML_05_00;
    value version = version xhtml_version;
    value standard = standard xhtml_version;
  end;
module M = M_05_00;

