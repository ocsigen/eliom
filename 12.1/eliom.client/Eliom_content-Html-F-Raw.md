
# Module `F.Raw`

```ocaml
type +'a elt = 'a elt
```
HTML elements.

Element constructors are in section [elements](./#elements). Most elements constructors are either [nullary](./#type-nullary), [unary](./#type-unary) or [star](./#type-star), depending on the number of children they accept. Children are usually given as a list of elements. [txt](./#val-txt) is used for text.

`div [a [txt "Foo"]]` is equivalent to `<div><a>foo</a></div>`

The type variable `'a` is used to track the element's type. This allows the OCaml typechecker to check HTML validity.

For example, `div []` is of type `[> `Div] elt`. The [span](./#val-span) function only accepts children of type [`Html_types.span_content`](./../../tyxml/tyxml.functor/Html_types.md#type-span_content). Since ``Div` is not part of it. `span [div []]` will not typecheck.

Note that the concrete implementation of this type can vary. See [`Xml`](./Eliom_content-Html-F-Raw-Xml.md) for details.

```ocaml
type doc = Html_types.html elt
```
A complete HTML document.

```ocaml
type +'a attrib = 'a attrib
```
HTML attributes

Attribute constructors are in section [attributes](./#attributes) and their name starts with `a_`. Attributes are given to elements with the `~a` optional argument.

`a ~a:[a_href "ocsigen.org"] [txt "link!"]` is equivalent to `<a href="ocsigen.org">link!</a>`

Similarly to [elt](./#type-elt), attributes use the OCaml type system to enforce HTML validity.

For example [`a_href`](./#val-a_href) returns a value of type `[> `Href] attrib`. The [div](./#val-div) function only accepts attributes of type [`Html_types.div_attrib`](./../../tyxml/tyxml.functor/Html_types.md#type-div_attrib). Since ``Href` is not part of it, `div ~a:[a_href "ocsigen.org"] []` will not typecheck.

In some cases, attributes have to be disambiguated. The `max` attribute has two version, [`a_max`](./#val-a_max) and [`a_input_max`](./#val-a_input_max), depending on the element. Such disambiguated attribute will contain the name of the associated element.

```ocaml
module Xml : 
  Xml_sigs.T
    with type 'a W.t = 'a Xml.W.t
    with type 'a W.tlist = 'a Xml.W.tlist
    with type ('a, 'b) W.ft = ('a, 'b) Xml.W.ft
    with type uri = Xml.uri
    with type event_handler = Xml.event_handler
    with type mouse_event_handler = Xml.mouse_event_handler
    with type keyboard_event_handler = Xml.keyboard_event_handler
    with type touch_event_handler = Xml.touch_event_handler
    with type attrib = Xml.attrib
    with type elt = Xml.elt
```
Underlying XML data-structure

```ocaml
type 'a wrap = 'a Xml.W.t
```
`wrap` is a container for elements and values.

In most cases, `'a wrap = 'a`. For `R` modules (in eliom or js\_of\_ocaml), It will be [`React.S.t`](./../../react/react/React-S.md#type-t).

```ocaml
type 'a list_wrap = 'a Xml.W.tlist
```
`list_wrap` is a container for list of elements.

In most cases, `'a list_wrap = 'a list`. For `R` modules (in eliom or js\_of\_ocaml), It will be [`ReactiveData.RList.t`](./../../reactiveData/reactiveData/ReactiveData-RList.md#type-t).

```ocaml
type ('a, 'b) nullary = ?a:'a attrib list -> unit -> 'b elt
```
A nullary element is an element that doesn't have any children.

```ocaml
type ('a, 'b, 'c) unary = ?a:'a attrib list -> 'b elt wrap -> 'c elt
```
A unary element is an element that have exactly one children.

```ocaml
type ('a, 'b, 'c) star = ?a:'a attrib list -> 'b elt list_wrap -> 'c elt
```
A star element is an element that has any number of children, including zero.

```ocaml
module Info : Xml_sigs.Info
```
Various information about HTML, such as the doctype, ...


#### Uri

```ocaml
type uri = Xml.uri
```
```ocaml
val string_of_uri : (uri, string) Xml.W.ft
```
```ocaml
val uri_of_string : (string, uri) Xml.W.ft
```

### Attributes

```ocaml
val a_class : Html_types.nmtokens wrap -> [> `Class ] attrib
```
This attribute assigns a class name or set of class names to an element. Any number of elements may be assigned the same class name or names.

```ocaml
val a_user_data : 
  Html_types.nmtoken ->
  Html_types.text wrap ->
  [> `User_data ] attrib
```
May be used to specify custom attributes. The example given by the W3C is as follows :

```
<ol>
  <li data-length="2m11s">Beyond The Sea</li>
</ol>
```
It should be used for preprocessing ends only.

```ocaml
val a_id : Html_types.text wrap -> [> `Id ] attrib
```
This attribute assigns a name to an element. This name must be unique in a document. The text should be without any space.

```ocaml
val a_title : Html_types.text wrap -> [> `Title ] attrib
```
This attribute offers advisory information about the element for which it is set.

Values of the title attribute may be rendered by user agents in a variety of ways. For instance, visual browsers frequently display the title as a *tool tip* (a short message that appears when the pointing device pauses over an object). Audio user agents may speak the title information in a similar context.

The title attribute has an additional role when used with the `link` element to designate an external style sheet. Please consult the section on links and style sheets for details.


#### I18N

```ocaml
val a_xml_lang : Html_types.languagecode wrap -> [> `XML_lang ] attrib
```
```ocaml
val a_lang : Html_types.languagecode wrap -> [> `Lang ] attrib
```

#### Events


##### Javascript events


##### Mouse events


##### Keyboard events


#### Other attributes

```ocaml
val a_allowfullscreen : unit -> [> `Allowfullscreen ] attrib
```
```ocaml
val a_allowpaymentrequest : unit -> [> `Allowpaymentrequest ] attrib
```
```ocaml
val a_autocomplete : 
  Html_types.autocomplete_option wrap ->
  [> `Autocomplete ] attrib
```
see [https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/autocomplete](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/autocomplete) autocomplete documentation.
```ocaml
val a_async : unit -> [> `Async ] attrib
```
```ocaml
val a_autofocus : unit -> [> `Autofocus ] attrib
```
```ocaml
val a_autoplay : unit -> [> `Autoplay ] attrib
```
```ocaml
val a_muted : unit -> [> `Muted ] attrib
```
```ocaml
val a_crossorigin : 
  [< `Anonymous | `Use_credentials ] wrap ->
  [> `Crossorigin ] attrib
```
```ocaml
val a_integrity : string wrap -> [> `Integrity ] attrib
```
```ocaml
val a_mediagroup : string wrap -> [> `Mediagroup ] attrib
```
```ocaml
val a_challenge : Html_types.text wrap -> [> `Challenge ] attrib
```
```ocaml
val a_contenteditable : bool wrap -> [> `Contenteditable ] attrib
```
```ocaml
val a_contextmenu : Html_types.idref wrap -> [> `Contextmenu ] attrib
```
```ocaml
val a_controls : unit -> [> `Controls ] attrib
```
```ocaml
val a_dir : [< `Rtl | `Ltr ] wrap -> [> `Dir ] attrib
```
```ocaml
val a_draggable : bool wrap -> [> `Draggable ] attrib
```
```ocaml
val a_form : Html_types.idref wrap -> [> `Form ] attrib
```
```ocaml
val a_formaction : Xml.uri wrap -> [> `Formaction ] attrib
```
```ocaml
val a_formenctype : Html_types.contenttype wrap -> [> `Formenctype ] attrib
```
```ocaml
val a_formnovalidate : unit -> [> `Formnovalidate ] attrib
```
```ocaml
val a_formtarget : Html_types.text wrap -> [> `Formtarget ] attrib
```
```ocaml
val a_hidden : unit -> [> `Hidden ] attrib
```
```ocaml
val a_high : Html_types.float_number wrap -> [> `High ] attrib
```
```ocaml
val a_icon : Xml.uri wrap -> [> `Icon ] attrib
```
```ocaml
val a_ismap : unit -> [> `Ismap ] attrib
```
```ocaml
val a_keytype : Html_types.text wrap -> [> `Keytype ] attrib
```
```ocaml
val a_list : Html_types.idref wrap -> [> `List ] attrib
```
```ocaml
val a_loop : unit -> [> `Loop ] attrib
```
```ocaml
val a_low : Html_types.float_number wrap -> [> `High ] attrib
```
```ocaml
val a_max : Html_types.float_number wrap -> [> `Max ] attrib
```
```ocaml
val a_input_max : Html_types.number_or_datetime wrap -> [> `Input_Max ] attrib
```
```ocaml
val a_min : Html_types.float_number wrap -> [> `Min ] attrib
```
```ocaml
val a_input_min : Html_types.number_or_datetime wrap -> [> `Input_Min ] attrib
```
```ocaml
val a_inputmode : 
  [< `None | `Text | `Decimal | `Numeric | `Tel | `Search | `Email | `Url ]
    wrap ->
  [> `Inputmode ] attrib
```
see [https://developer.mozilla.org/en-US/docs/Web/HTML/Global\_attributes/inputmode](https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/inputmode) inputmode documentation.
```ocaml
val a_novalidate : unit -> [> `Novalidate ] attrib
```
```ocaml
val a_open : unit -> [> `Open ] attrib
```
```ocaml
val a_optimum : Html_types.float_number wrap -> [> `Optimum ] attrib
```
```ocaml
val a_pattern : Html_types.text wrap -> [> `Pattern ] attrib
```
```ocaml
val a_placeholder : Html_types.text wrap -> [> `Placeholder ] attrib
```
```ocaml
val a_poster : Xml.uri wrap -> [> `Poster ] attrib
```
```ocaml
val a_preload : [< `None | `Metadata | `Audio ] wrap -> [> `Preload ] attrib
```
```ocaml
val a_pubdate : unit -> [> `Pubdate ] attrib
```
```ocaml
val a_radiogroup : Html_types.text wrap -> [> `Radiogroup ] attrib
```
```ocaml
val a_referrerpolicy : 
  Html_types.referrerpolicy wrap ->
  [> `Referrerpolicy ] attrib
```
see [https://developer.mozilla.org/en-US/docs/Web/HTML/Element/iframe\#Attributes](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/iframe#Attributes) 
```ocaml
val a_required : unit -> [> `Required ] attrib
```
```ocaml
val a_reversed : unit -> [> `Reversed ] attrib
```
```ocaml
val a_sandbox : [< Html_types.sandbox_token ] list wrap -> [> `Sandbox ] attrib
```
```ocaml
val a_spellcheck : bool wrap -> [> `Spellcheck ] attrib
```
```ocaml
val a_scoped : unit -> [> `Scoped ] attrib
```
```ocaml
val a_seamless : unit -> [> `Seamless ] attrib
```
```ocaml
val a_sizes : 
  (Html_types.number * Html_types.number) list option wrap ->
  [> `Sizes ] attrib
```
```ocaml
val a_span : Html_types.number wrap -> [> `Span ] attrib
```
```ocaml
val a_srclang : Html_types.nmtoken wrap -> [> `XML_lang ] attrib
```
deprecated Use a\_xml\_lang instead.
```ocaml
type image_candidate = [ 
  | `Url of uri
  | `Url_width of uri * Html_types.number
  | `Url_pixel of uri * Html_types.float_number
 ]
```
```ocaml
val a_srcset : image_candidate list wrap -> [> `Srcset ] attrib
```
```ocaml
val a_img_sizes : Html_types.text list wrap -> [> `Img_sizes ] attrib
```
```ocaml
val a_start : Html_types.number wrap -> [> `Start ] attrib
```
```ocaml
val a_step : Html_types.float_number option wrap -> [> `Step ] attrib
```
```ocaml
val a_translate : [< `Yes | `No ] wrap -> [> `Translate ] attrib
```
see [https://developer.mozilla.org/en-US/docs/Web/HTML/Global\_attributes/translate](https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/translate) translate global attribute documentation.
```ocaml
val a_wrap : [< `Soft | `Hard ] wrap -> [> `Wrap ] attrib
```
```ocaml
val a_version : Html_types.cdata wrap -> [> `Version ] attrib
```
```ocaml
val a_xmlns : [< `W3_org_1999_xhtml ] wrap -> [> `XMLns ] attrib
```
```ocaml
val a_manifest : Xml.uri wrap -> [> `Manifest ] attrib
```
```ocaml
val a_cite : Xml.uri wrap -> [> `Cite ] attrib
```
```ocaml
val a_xml_space : [< `Default | `Preserve ] wrap -> [> `XML_space ] attrib
```
```ocaml
val a_accesskey : Html_types.character wrap -> [> `Accesskey ] attrib
```
This attribute assigns an access key to an element. An access key is a single character from the document character set. NB: authors should consider the input method of the expected reader when specifying an accesskey.

```ocaml
val a_charset : Html_types.charset wrap -> [> `Charset ] attrib
```
This attribute specifies the character encoding of the resource designated by the link. Please consult the section on character encodings for more details.

```ocaml
val a_accept_charset : Html_types.charsets wrap -> [> `Accept_charset ] attrib
```
```ocaml
val a_accept : Html_types.contenttypes wrap -> [> `Accept ] attrib
```
```ocaml
val a_href : Xml.uri wrap -> [> `Href ] attrib
```
This attribute specifies the location of a Web resource, thus defining a link between the current element (the source anchor) and the destination anchor defined by this attribute.

```ocaml
val a_hreflang : Html_types.languagecode wrap -> [> `Hreflang ] attrib
```
This attribute specifies the base language of the resource designated by href and may only be used when href is specified.

```ocaml
val a_download : string option wrap -> [> `Download ] attrib
```
```ocaml
val a_rel : Html_types.linktypes wrap -> [> `Rel ] attrib
```
This attribute describes the relationship from the current document to the anchor specified by the href attribute. The value of this attribute is a space-separated list of link types.

This attribute is used to describe a reverse link from the anchor specified by the href attribute to the current document. The value of this attribute is a space-separated list of link types.

```ocaml
val a_tabindex : Html_types.number wrap -> [> `Tabindex ] attrib
```
This attribute specifies the position of the current element in the tabbing order for the current document. This value must be a number between 0 and 32767\. User agents should ignore leading zeros.

```ocaml
val a_mime_type : Html_types.contenttype wrap -> [> `Mime_type ] attrib
```
This attribute gives an advisory hint as to the content type of the content available at the link target address. It allows user agents to opt to use a fallback mechanism rather than fetch the content if they are advised that they will get content in a content type they do not support.Authors who use this attribute take responsibility to manage the risk that it may become inconsistent with the content available at the link target address.

```ocaml
val a_datetime : Html_types.cdata wrap -> [> `Datetime ] attrib
```
```ocaml
val a_action : Xml.uri wrap -> [> `Action ] attrib
```
This attribute specifies a form processing agent. User agent behavior for a value other than an HTTP URI is undefined.

```ocaml
val a_checked : unit -> [> `Checked ] attrib
```
When the `type` attribute has the value `"radio"` or `"checkbox"`, this boolean attribute specifies that the button is on. User agents must ignore this attribute for other control types.

```ocaml
val a_cols : Html_types.number wrap -> [> `Cols ] attrib
```
This attribute specifies the visible width in average character widths. Users should be able to enter longer lines than this, so user agents should provide some means to scroll through the contents of the control when the contents extend beyond the visible area. User agents may wrap visible text lines to keep long lines visible without the need for scrolling.

```ocaml
val a_enctype : Html_types.contenttype wrap -> [> `Enctype ] attrib
```
```ocaml
val a_label_for : Html_types.idref wrap -> [> `Label_for ] attrib
```
```ocaml
val a_for : Html_types.idref wrap -> [> `Label_for ] attrib
```
deprecated Use a\_label\_for
```ocaml
val a_output_for : Html_types.idrefs wrap -> [> `Output_for ] attrib
```
```ocaml
val a_for_list : Html_types.idrefs wrap -> [> `Output_for ] attrib
```
deprecated Use a\_output\_for
```ocaml
val a_maxlength : Html_types.number wrap -> [> `Maxlength ] attrib
```
```ocaml
val a_minlength : Html_types.number wrap -> [> `Minlength ] attrib
```
```ocaml
val a_method : [< `Get | `Post ] wrap -> [> `Method ] attrib
```
```ocaml
val a_formmethod : [< `Get | `Post ] wrap -> [> `Formmethod ] attrib
```
```ocaml
val a_multiple : unit -> [> `Multiple ] attrib
```
```ocaml
val a_name : Html_types.text wrap -> [> `Name ] attrib
```
This attribute assigns the control name.

```ocaml
val a_rows : Html_types.number wrap -> [> `Rows ] attrib
```
This attribute specifies the number of visible text lines. Users should be able to enter more lines than this, so user agents should provide some means to scroll through the contents of the control when the contents extend beyond the visible area.

```ocaml
val a_selected : unit -> [> `Selected ] attrib
```
When set, this boolean attribute specifies that this option is pre-selected.

```ocaml
val a_size : Html_types.number wrap -> [> `Size ] attrib
```
```ocaml
val a_src : Xml.uri wrap -> [> `Src ] attrib
```
```ocaml
val a_input_type : 
  [< `Url
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
  | `Button ]
    wrap ->
  [> `Input_Type ] attrib
```
```ocaml
val a_text_value : Html_types.text wrap -> [> `Text_Value ] attrib
```
This attribute specifies the initial value of the control. If this attribute is not set, the initial value is set to the contents of the `option` element.

```ocaml
val a_int_value : Html_types.number wrap -> [> `Int_Value ] attrib
```
```ocaml
val a_value : Html_types.cdata wrap -> [> `Value ] attrib
```
```ocaml
val a_float_value : Html_types.float_number wrap -> [> `Float_Value ] attrib
```
```ocaml
val a_disabled : unit -> [> `Disabled ] attrib
```
```ocaml
val a_readonly : unit -> [> `ReadOnly ] attrib
```
```ocaml
val a_button_type : 
  [< `Button | `Submit | `Reset ] wrap ->
  [> `Button_Type ] attrib
```
```ocaml
val a_script_type : Html_types.script_type wrap -> [> `Script_type ] attrib
```
```ocaml
val a_command_type : 
  [< `Command | `Checkbox | `Radio ] wrap ->
  [> `Command_Type ] attrib
```
```ocaml
val a_menu_type : [< `Context | `Toolbar ] wrap -> [> `Menu_Type ] attrib
```
```ocaml
val a_label : Html_types.text wrap -> [> `Label ] attrib
```
```ocaml
val a_align : [< `Left | `Right | `Justify | `Char ] wrap -> [> `Align ] attrib
```
deprecated Use CSS text-align
```ocaml
val a_axis : Html_types.cdata wrap -> [> `Axis ] attrib
```
deprecated Not supported in HTML5
```ocaml
val a_colspan : Html_types.number wrap -> [> `Colspan ] attrib
```
```ocaml
val a_headers : Html_types.idrefs wrap -> [> `Headers ] attrib
```
```ocaml
val a_rowspan : Html_types.number wrap -> [> `Rowspan ] attrib
```
```ocaml
val a_scope : 
  [< `Row | `Col | `Rowgroup | `Colgroup ] wrap ->
  [> `Scope ] attrib
```
deprecated Not supported in HTML5
```ocaml
val a_summary : Html_types.text wrap -> [> `Summary ] attrib
```
deprecated Move content elsewhere or to a \<caption\> child
```ocaml
val a_border : Html_types.pixels wrap -> [> `Border ] attrib
```
deprecated Use CSS border and/or border-width
```ocaml
val a_rules : 
  [< `None | `Groups | `Rows | `Cols | `All ] wrap ->
  [> `Rules ] attrib
```
deprecated Use CSS border
```ocaml
val a_char : Html_types.character wrap -> [> `Char ] attrib
```
deprecated The char attribute is not supported in HTML5
```ocaml
val a_alt : Html_types.text wrap -> [> `Alt ] attrib
```
```ocaml
val a_height : Html_types.number wrap -> [> `Height ] attrib
```
```ocaml
val a_width : Html_types.number wrap -> [> `Width ] attrib
```
```ocaml
type shape = [ 
  | `Rect
  | `Circle
  | `Poly
  | `Default
 ]
```
```ocaml
val a_shape : shape wrap -> [> `Shape ] attrib
```
```ocaml
val a_coords : Html_types.numbers wrap -> [> `Coords ] attrib
```
```ocaml
val a_usemap : Html_types.idref wrap -> [> `Usemap ] attrib
```
```ocaml
val a_data : Xml.uri wrap -> [> `Data ] attrib
```
```ocaml
val a_codetype : Html_types.contenttype wrap -> [> `Codetype ] attrib
```
deprecated Not supported in HTML5
```ocaml
val a_frameborder : [< `Zero | `One ] wrap -> [> `Frameborder ] attrib
```
deprecated Use CSS border
```ocaml
val a_marginheight : Html_types.pixels wrap -> [> `Marginheight ] attrib
```
deprecated Use CSS
```ocaml
val a_marginwidth : Html_types.pixels wrap -> [> `Marginwidth ] attrib
```
deprecated Use CSS
```ocaml
val a_scrolling : [< `Yes | `No | `Auto ] wrap -> [> `Scrolling ] attrib
```
```ocaml
val a_target : Html_types.frametarget wrap -> [> `Target ] attrib
```
```ocaml
val a_content : Html_types.text wrap -> [> `Content ] attrib
```
```ocaml
val a_http_equiv : Html_types.text wrap -> [> `Http_equiv ] attrib
```
```ocaml
val a_defer : unit -> [> `Defer ] attrib
```
```ocaml
val a_media : Html_types.mediadesc wrap -> [> `Media ] attrib
```
```ocaml
val a_style : string wrap -> [> `Style_Attr ] attrib
```
```ocaml
val a_property : string wrap -> [> `Property ] attrib
```

#### ARIA support

[WAI-ARIA](https://www.w3.org/TR/wai-aria-1.1/) is a specification written by the W3C, defining a set of additional HTML attributes that can be applied to elements to provide additional semantics and improve accessibility wherever it is lacking.

See for example a [WAI-ARIA tutorial](https://developer.mozilla.org/en-US/docs/Learn/Accessibility/WAI-ARIA_basics).

```ocaml
val a_role : string list wrap -> [> `Role ] attrib
```
see [https://www.w3.org/TR/role-attribute](https://www.w3.org/TR/role-attribute) Role attribute specification
see [https://www.w3.org/TR/wai-aria-1.1/\#role\_definitions](https://www.w3.org/TR/wai-aria-1.1/#role_definitions) List of WAI-ARIA roles
```ocaml
val a_aria : string -> string list wrap -> [> `Aria ] attrib
```
Basic support for WAI-ARIA attributes: `a_aria "foo"` corresponds to an "aria-foo" attribute.

see [https://www.w3.org/TR/wai-aria-1.1/\#state\_prop\_def](https://www.w3.org/TR/wai-aria-1.1/#state_prop_def) List of WAI-ARIA attributes

### Elements

```ocaml
val txt : string wrap -> [> Html_types.txt ] elt
```
```ocaml
val html : 
  ?a:Html_types.html_attrib attrib list ->
  [< Html_types.head ] elt wrap ->
  [< Html_types.body ] elt wrap ->
  [> Html_types.html ] elt
```
```ocaml
val head : 
  ?a:Html_types.head_attrib attrib list ->
  [< Html_types.title ] elt wrap ->
  Html_types.head_content_fun elt list_wrap ->
  [> Html_types.head ] elt
```
```ocaml
val base : ([< Html_types.base_attrib ], [> Html_types.base ]) nullary
```
```ocaml
val title : 
  (Html_types.title_attrib,
    [< Html_types.title_content_fun ],
    [> Html_types.title ])
    unary
```
```ocaml
val body : 
  ([< Html_types.body_attrib ],
    [< Html_types.body_content_fun ],
    [> Html_types.body ])
    star
```
```ocaml
val svg : 
  ?a:[< Html_types.svg_attrib ] Svg.attrib list ->
  [< Html_types.svg_content ] Svg.elt list_wrap ->
  [> Html_types.svg ] elt
```

#### Section

```ocaml
val footer : 
  ([< Html_types.footer_attrib ],
    [< Html_types.footer_content_fun ],
    [> Html_types.footer ])
    star
```
```ocaml
val header : 
  ([< Html_types.header_attrib ],
    [< Html_types.header_content_fun ],
    [> Html_types.header ])
    star
```
```ocaml
val section : 
  ([< Html_types.section_attrib ],
    [< Html_types.section_content_fun ],
    [> Html_types.section ])
    star
```
```ocaml
val nav : 
  ([< Html_types.nav_attrib ],
    [< Html_types.nav_content_fun ],
    [> Html_types.nav ])
    star
```
```ocaml
val h1 : 
  ([< Html_types.h1_attrib ],
    [< Html_types.h1_content_fun ],
    [> Html_types.h1 ])
    star
```
```ocaml
val h2 : 
  ([< Html_types.h2_attrib ],
    [< Html_types.h2_content_fun ],
    [> Html_types.h2 ])
    star
```
```ocaml
val h3 : 
  ([< Html_types.h3_attrib ],
    [< Html_types.h3_content_fun ],
    [> Html_types.h3 ])
    star
```
```ocaml
val h4 : 
  ([< Html_types.h4_attrib ],
    [< Html_types.h4_content_fun ],
    [> Html_types.h4 ])
    star
```
```ocaml
val h5 : 
  ([< Html_types.h5_attrib ],
    [< Html_types.h5_content_fun ],
    [> Html_types.h5 ])
    star
```
```ocaml
val h6 : 
  ([< Html_types.h6_attrib ],
    [< Html_types.h6_content_fun ],
    [> Html_types.h6 ])
    star
```
```ocaml
val hgroup : 
  ([< Html_types.hgroup_attrib ],
    [< Html_types.hgroup_content_fun ],
    [> Html_types.hgroup ])
    star
```
```ocaml
val address : 
  ([< Html_types.address_attrib ],
    [< Html_types.address_content_fun ],
    [> Html_types.address ])
    star
```
```ocaml
val article : 
  ([< Html_types.article_attrib ],
    [< Html_types.article_content_fun ],
    [> Html_types.article ])
    star
```
```ocaml
val aside : 
  ([< Html_types.aside_attrib ],
    [< Html_types.aside_content_fun ],
    [> Html_types.aside ])
    star
```
```ocaml
val main : 
  ([< Html_types.main_attrib ],
    [< Html_types.main_content_fun ],
    [> Html_types.main ])
    star
```

#### Grouping content

```ocaml
val p : 
  ([< Html_types.p_attrib ], [< Html_types.p_content_fun ], [> Html_types.p ])
    star
```
```ocaml
val pre : 
  ([< Html_types.pre_attrib ],
    [< Html_types.pre_content_fun ],
    [> Html_types.pre ])
    star
```
```ocaml
val blockquote : 
  ([< Html_types.blockquote_attrib ],
    [< Html_types.blockquote_content_fun ],
    [> Html_types.blockquote ])
    star
```
```ocaml
val dialog : 
  ([< Html_types.dialog_attrib ],
    [< Html_types.dialog_content_fun ],
    [> Html_types.dialog ])
    star
```
see [https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dialog](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dialog) 
```ocaml
val div : 
  ([< Html_types.div_attrib ],
    [< Html_types.div_content_fun ],
    [> Html_types.div ])
    star
```
```ocaml
val dl : 
  ([< Html_types.dl_attrib ],
    [< Html_types.dl_content_fun ],
    [> Html_types.dl ])
    star
```
```ocaml
val ol : 
  ([< Html_types.ol_attrib ],
    [< Html_types.ol_content_fun ],
    [> Html_types.ol ])
    star
```
```ocaml
val ul : 
  ([< Html_types.ul_attrib ],
    [< Html_types.ul_content_fun ],
    [> Html_types.ul ])
    star
```
```ocaml
val dd : 
  ([< Html_types.dd_attrib ],
    [< Html_types.dd_content_fun ],
    [> Html_types.dd ])
    star
```
```ocaml
val dt : 
  ([< Html_types.dt_attrib ],
    [< Html_types.dt_content_fun ],
    [> Html_types.dt ])
    star
```
```ocaml
val li : 
  ([< Html_types.li_attrib ],
    [< Html_types.li_content_fun ],
    [> Html_types.li ])
    star
```
```ocaml
val figcaption : 
  ([< Html_types.figcaption_attrib ],
    [< Html_types.figcaption_content_fun ],
    [> Html_types.figcaption ])
    star
```
```ocaml
val figure : 
  ?figcaption:
    [ `Top of [< Html_types.figcaption ] elt wrap
    | `Bottom of [< Html_types.figcaption ] elt wrap ] ->
  ([< Html_types.figure_attrib ],
    [< Html_types.figure_content_fun ],
    [> Html_types.figure ])
    star
```
```ocaml
val hr : ([< Html_types.hr_attrib ], [> Html_types.hr ]) nullary
```

#### Semantic

```ocaml
val b : 
  ([< Html_types.b_attrib ], [< Html_types.b_content_fun ], [> Html_types.b ])
    star
```
```ocaml
val i : 
  ([< Html_types.i_attrib ], [< Html_types.i_content_fun ], [> Html_types.i ])
    star
```
```ocaml
val u : 
  ([< Html_types.u_attrib ], [< Html_types.u_content_fun ], [> Html_types.u ])
    star
```
```ocaml
val small : 
  ([< Html_types.small_attrib ],
    [< Html_types.small_content_fun ],
    [> Html_types.small ])
    star
```
```ocaml
val sub : 
  ([< Html_types.sub_attrib ],
    [< Html_types.sub_content_fun ],
    [> Html_types.sub ])
    star
```
```ocaml
val sup : 
  ([< Html_types.sup_attrib ],
    [< Html_types.sup_content_fun ],
    [> Html_types.sup ])
    star
```
```ocaml
val mark : 
  ([< Html_types.mark_attrib ],
    [< Html_types.mark_content_fun ],
    [> Html_types.mark ])
    star
```
```ocaml
val wbr : ([< Html_types.wbr_attrib ], [> Html_types.wbr ]) nullary
```
```ocaml
val bdo : 
  dir:[< `Ltr | `Rtl ] wrap ->
  ([< Html_types.bdo_attrib ],
    [< Html_types.bdo_content_fun ],
    [> Html_types.bdo ])
    star
```
```ocaml
val abbr : 
  ([< Html_types.abbr_attrib ],
    [< Html_types.abbr_content_fun ],
    [> Html_types.abbr ])
    star
```
```ocaml
val br : ([< Html_types.br_attrib ], [> Html_types.br ]) nullary
```
```ocaml
val cite : 
  ([< Html_types.cite_attrib ],
    [< Html_types.cite_content_fun ],
    [> Html_types.cite ])
    star
```
```ocaml
val code : 
  ([< Html_types.code_attrib ],
    [< Html_types.code_content_fun ],
    [> Html_types.code ])
    star
```
```ocaml
val dfn : 
  ([< Html_types.dfn_attrib ],
    [< Html_types.dfn_content_fun ],
    [> Html_types.dfn ])
    star
```
```ocaml
val em : 
  ([< Html_types.em_attrib ],
    [< Html_types.em_content_fun ],
    [> Html_types.em ])
    star
```
```ocaml
val kbd : 
  ([< Html_types.kbd_attrib ],
    [< Html_types.kbd_content_fun ],
    [> Html_types.kbd ])
    star
```
```ocaml
val q : 
  ([< Html_types.q_attrib ], [< Html_types.q_content_fun ], [> Html_types.q ])
    star
```
```ocaml
val samp : 
  ([< Html_types.samp_attrib ],
    [< Html_types.samp_content_fun ],
    [> Html_types.samp ])
    star
```
```ocaml
val span : 
  ([< Html_types.span_attrib ],
    [< Html_types.span_content_fun ],
    [> Html_types.span ])
    star
```
```ocaml
val strong : 
  ([< Html_types.strong_attrib ],
    [< Html_types.strong_content_fun ],
    [> Html_types.strong ])
    star
```
```ocaml
val time : 
  ([< Html_types.time_attrib ],
    [< Html_types.time_content_fun ],
    [> Html_types.time ])
    star
```
```ocaml
val var : 
  ([< Html_types.var_attrib ],
    [< Html_types.var_content_fun ],
    [> Html_types.var ])
    star
```

#### Hypertext

```ocaml
val a : ([< Html_types.a_attrib ], 'a, [> 'a Html_types.a ]) star
```

#### Edit

```ocaml
val del : ([< Html_types.del_attrib ], 'a, [> 'a Html_types.del ]) star
```
```ocaml
val ins : ([< Html_types.ins_attrib ], 'a, [> 'a Html_types.ins ]) star
```

#### Embedded

```ocaml
val img : 
  src:Xml.uri wrap ->
  alt:Html_types.text wrap ->
  ([< Html_types.img_attrib ], [> Html_types.img ]) nullary
```
```ocaml
val picture : 
  img:[< Html_types.img ] elt wrap ->
  ([< Html_types.picture_attrib ],
    [< Html_types.picture_content_fun ],
    [> Html_types.picture ])
    star
```
see [https://developer.mozilla.org/en-US/docs/Web/HTML/Element/picture](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/picture) Picture element documentation on MDN
```ocaml
val iframe : 
  ([< Html_types.iframe_attrib ],
    [< Html_types.iframe_content_fun ],
    [> Html_types.iframe ])
    star
```
```ocaml
val object_ : 
  ?params:[< Html_types.param ] elt list_wrap ->
  ([< Html_types.object__attrib ], 'a, [> `Object of 'a ]) star
```
```ocaml
val param : ([< Html_types.param_attrib ], [> Html_types.param ]) nullary
```
```ocaml
val embed : ([< Html_types.embed_attrib ], [> Html_types.embed ]) nullary
```
```ocaml
val audio : 
  ?src:Xml.uri wrap ->
  ?srcs:[< Html_types.source ] elt list_wrap ->
  ([< Html_types.audio_attrib ], 'a, [> 'a Html_types.audio ]) star
```
```ocaml
val video : 
  ?src:Xml.uri wrap ->
  ?srcs:[< Html_types.source ] elt list_wrap ->
  ([< Html_types.video_attrib ], 'a, [> 'a Html_types.video ]) star
```
```ocaml
val canvas : 
  ([< Html_types.canvas_attrib ], 'a, [> 'a Html_types.canvas ]) star
```
```ocaml
val source : ([< Html_types.source_attrib ], [> Html_types.source ]) nullary
```
```ocaml
val area : 
  alt:Html_types.text wrap ->
  ([< Html_types.common
   | `Alt
   | `Coords
   | `Shape
   | `Target
   | `Rel
   | `Media
   | `Hreflang
   | `Mime_type ],
    [> Html_types.area ])
    nullary
```
```ocaml
val map : ([< Html_types.map_attrib ], 'a, [> 'a Html_types.map ]) star
```

#### Tables Data

```ocaml
val caption : 
  ([< Html_types.caption_attrib ],
    [< Html_types.caption_content_fun ],
    [> Html_types.caption ])
    star
```
```ocaml
val table : 
  ?caption:[< Html_types.caption ] elt wrap ->
  ?columns:[< Html_types.colgroup ] elt list_wrap ->
  ?thead:[< Html_types.thead ] elt wrap ->
  ?tfoot:[< Html_types.tfoot ] elt wrap ->
  ([< Html_types.table_attrib ],
    [< Html_types.table_content_fun ],
    [> Html_types.table ])
    star
```
```ocaml
val tablex : 
  ?caption:[< Html_types.caption ] elt wrap ->
  ?columns:[< Html_types.colgroup ] elt list_wrap ->
  ?thead:[< Html_types.thead ] elt wrap ->
  ?tfoot:[< Html_types.tfoot ] elt wrap ->
  ([< Html_types.tablex_attrib ],
    [< Html_types.tablex_content_fun ],
    [> Html_types.tablex ])
    star
```
```ocaml
val colgroup : 
  ([< Html_types.colgroup_attrib ],
    [< Html_types.colgroup_content_fun ],
    [> Html_types.colgroup ])
    star
```
```ocaml
val col : ([< Html_types.col_attrib ], [> Html_types.col ]) nullary
```
```ocaml
val thead : 
  ([< Html_types.thead_attrib ],
    [< Html_types.thead_content_fun ],
    [> Html_types.thead ])
    star
```
```ocaml
val tbody : 
  ([< Html_types.tbody_attrib ],
    [< Html_types.tbody_content_fun ],
    [> Html_types.tbody ])
    star
```
```ocaml
val tfoot : 
  ([< Html_types.tfoot_attrib ],
    [< Html_types.tfoot_content_fun ],
    [> Html_types.tfoot ])
    star
```
```ocaml
val td : 
  ([< Html_types.td_attrib ],
    [< Html_types.td_content_fun ],
    [> Html_types.td ])
    star
```
```ocaml
val th : 
  ([< Html_types.th_attrib ],
    [< Html_types.th_content_fun ],
    [> Html_types.th ])
    star
```
```ocaml
val tr : 
  ([< Html_types.tr_attrib ],
    [< Html_types.tr_content_fun ],
    [> Html_types.tr ])
    star
```

#### Forms

```ocaml
val form : 
  ([< Html_types.form_attrib ],
    [< Html_types.form_content_fun ],
    [> Html_types.form ])
    star
```
```ocaml
val fieldset : 
  ?legend:[< Html_types.legend ] elt wrap ->
  ([< Html_types.fieldset_attrib ],
    [< Html_types.fieldset_content_fun ],
    [> Html_types.fieldset ])
    star
```
```ocaml
val legend : 
  ([< Html_types.legend_attrib ],
    [< Html_types.legend_content_fun ],
    [> Html_types.legend ])
    star
```
```ocaml
val label : 
  ([< Html_types.label_attrib ],
    [< Html_types.label_content_fun ],
    [> Html_types.label ])
    star
```
Label authorizes only one control inside them that should be labelled with a `for` attribute (although it is not necessary). Such constraints are not currently enforced by the type-system

```ocaml
val input : ([< Html_types.input_attrib ], [> Html_types.input ]) nullary
```
```ocaml
val button : 
  ([< Html_types.button_attrib ],
    [< Html_types.button_content_fun ],
    [> Html_types.button ])
    star
```
```ocaml
val select : 
  ([< Html_types.select_attrib ],
    [< Html_types.select_content_fun ],
    [> Html_types.select ])
    star
```
```ocaml
val datalist : 
  ?children:
    [< `Options of [< Html_types.selectoption ] elt list_wrap
    | `Phras of [< Html_types.phrasing ] elt list_wrap ] ->
  ([< Html_types.datalist_attrib ], [> Html_types.datalist ]) nullary
```
```ocaml
val optgroup : 
  label:Html_types.text wrap ->
  ([< Html_types.optgroup_attrib ],
    [< Html_types.optgroup_content_fun ],
    [> Html_types.optgroup ])
    star
```
```ocaml
val option : 
  ([< Html_types.option_attrib ],
    [< Html_types.option_content_fun ],
    [> Html_types.selectoption ])
    unary
```
```ocaml
val textarea : 
  ([< Html_types.textarea_attrib ],
    [< Html_types.textarea_content_fun ],
    [> Html_types.textarea ])
    unary
```
```ocaml
val keygen : ([< Html_types.keygen_attrib ], [> Html_types.keygen ]) nullary
```
```ocaml
val progress : 
  ([< Html_types.progress_attrib ],
    [< Html_types.progress_content_fun ],
    [> Html_types.progress ])
    star
```
```ocaml
val meter : 
  ([< Html_types.meter_attrib ],
    [< Html_types.meter_content_fun ],
    [> Html_types.meter ])
    star
```
```ocaml
val output_elt : 
  ([< Html_types.output_elt_attrib ],
    [< Html_types.output_elt_content_fun ],
    [> Html_types.output_elt ])
    star
```

#### Data

```ocaml
val entity : string -> [> Html_types.txt ] elt
```
`entity "foo"` is the HTML entity `&foo;`. Both numerical and named form are allowed.

see [http://www.w3schools.com/html/html\_entities.asp](http://www.w3schools.com/html/html_entities.asp) A tutorial on HTML entities.
see [https://www.w3.org/TR/html5/syntax.html\#named-character-references](https://www.w3.org/TR/html5/syntax.html#named-character-references) The list of HTML entities.
```ocaml
val space : unit -> [> Html_types.txt ] elt
```
```ocaml
val cdata : string -> [> Html_types.txt ] elt
```
```ocaml
val cdata_script : string -> [> Html_types.txt ] elt
```
```ocaml
val cdata_style : string -> [> Html_types.txt ] elt
```

#### Interactive

```ocaml
val details : 
  [< Html_types.summary ] elt wrap ->
  ([< Html_types.details_attrib ],
    [< Html_types.details_content_fun ],
    [> Html_types.details ])
    star
```
```ocaml
val summary : 
  ([< Html_types.summary_attrib ],
    [< Html_types.summary_content_fun ],
    [> Html_types.summary ])
    star
```
```ocaml
val command : 
  label:Html_types.text wrap ->
  ([< Html_types.command_attrib ], [> Html_types.command ]) nullary
```
```ocaml
val menu : 
  ?children:
    [< `Lis of [< `Li of [< Html_types.common ] ] elt list_wrap
    | `Flows of [< Html_types.flow5 ] elt list_wrap ] ->
  ([< Html_types.menu_attrib ], [> Html_types.menu ]) nullary
```

#### Scripting

```ocaml
val script : 
  ([< Html_types.script_attrib ],
    [< Html_types.script_content_fun ],
    [> Html_types.script ])
    unary
```
```ocaml
val noscript : 
  ([< Html_types.noscript_attrib ],
    [< Html_types.noscript_content_fun ],
    [> Html_types.noscript ])
    star
```
```ocaml
val template : 
  ([< Html_types.template_attrib ],
    [< Html_types.template_content_fun ],
    [> Html_types.template ])
    star
```
see [https://developer.mozilla.org/en-US/docs/Web/HTML/Element/template](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/template) Template element documentation on MDN
```ocaml
val meta : ([< Html_types.meta_attrib ], [> Html_types.meta ]) nullary
```

#### Style Sheets

```ocaml
val style : 
  ([< Html_types.style_attrib ],
    [< Html_types.style_content_fun ],
    [> Html_types.style ])
    star
```
```ocaml
val link : 
  rel:Html_types.linktypes wrap ->
  href:Xml.uri wrap ->
  ([< Html_types.link_attrib ], [> Html_types.link ]) nullary
```

#### Ruby

```ocaml
val rt : 
  ([< Html_types.rt_attrib ],
    [< Html_types.rt_content_fun ],
    [> Html_types.rt ])
    star
```
```ocaml
val rp : 
  ([< Html_types.rp_attrib ],
    [< Html_types.rp_content_fun ],
    [> Html_types.rp ])
    star
```
```ocaml
val ruby : 
  ([< Html_types.ruby_attrib ],
    [< Html_types.ruby_content_fun ],
    [> Html_types.ruby ])
    star
```

#### Deprecated

```ocaml
val pcdata : string wrap -> [> Html_types.pcdata ] elt
```
deprecated Use txt instead

### Conversion with untyped representation

WARNING: These functions do not ensure HTML or SVG validity\! You should always explicitly given an appropriate type to the output.

```ocaml
val of_seq : Xml_stream.signal Seq.t -> 'a elt list_wrap
```
`import signal` converts the given XML signal into Tyxml elements. It can be used with HTML and SVG parsing libraries, such as Markup.

raises [`Xml_stream.Malformed_stream`](./../../tyxml/tyxml.functor/Xml_stream.md#exception-Malformed_stream) if the stream is malformed.
```ocaml
val tot : Xml.elt -> 'a elt
```
```ocaml
val totl : Xml.elt list_wrap -> 'a elt list_wrap
```
```ocaml
val toelt : 'a elt -> Xml.elt
```
```ocaml
val toeltl : 'a elt list_wrap -> Xml.elt list_wrap
```
```ocaml
val doc_toelt : doc -> Xml.elt
```
```ocaml
val to_xmlattribs : 'a attrib list -> Xml.attrib list
```
```ocaml
val to_attrib : Xml.attrib -> 'a attrib
```
```ocaml
module Unsafe : sig ... end
```
Unsafe features.

```ocaml
val a_onabort : string -> [> `OnAbort ] attrib
```
```ocaml
val a_onafterprint : string -> [> `OnAfterPrint ] attrib
```
```ocaml
val a_onbeforeprint : string -> [> `OnBeforePrint ] attrib
```
```ocaml
val a_onbeforeunload : string -> [> `OnBeforeUnload ] attrib
```
```ocaml
val a_onblur : string -> [> `OnBlur ] attrib
```
```ocaml
val a_oncanplay : string -> [> `OnCanPlay ] attrib
```
```ocaml
val a_oncanplaythrough : string -> [> `OnCanPlayThrough ] attrib
```
```ocaml
val a_onchange : string -> [> `OnChange ] attrib
```
```ocaml
val a_onclose : string -> [> `OnClose ] attrib
```
```ocaml
val a_ondurationchange : string -> [> `OnDurationChange ] attrib
```
```ocaml
val a_onemptied : string -> [> `OnEmptied ] attrib
```
```ocaml
val a_onended : string -> [> `OnEnded ] attrib
```
```ocaml
val a_onerror : string -> [> `OnError ] attrib
```
```ocaml
val a_onfocus : string -> [> `OnFocus ] attrib
```
```ocaml
val a_onformchange : string -> [> `OnFormChange ] attrib
```
```ocaml
val a_onforminput : string -> [> `OnFormInput ] attrib
```
```ocaml
val a_onhashchange : string -> [> `OnHashChange ] attrib
```
```ocaml
val a_oninput : string -> [> `OnInput ] attrib
```
```ocaml
val a_oninvalid : string -> [> `OnInvalid ] attrib
```
```ocaml
val a_onmousewheel : string -> [> `OnMouseWheel ] attrib
```
```ocaml
val a_onoffline : string -> [> `OnOffLine ] attrib
```
```ocaml
val a_ononline : string -> [> `OnOnLine ] attrib
```
```ocaml
val a_onpause : string -> [> `OnPause ] attrib
```
```ocaml
val a_onplay : string -> [> `OnPlay ] attrib
```
```ocaml
val a_onplaying : string -> [> `OnPlaying ] attrib
```
```ocaml
val a_onpagehide : string -> [> `OnPageHide ] attrib
```
```ocaml
val a_onpageshow : string -> [> `OnPageShow ] attrib
```
```ocaml
val a_onpopstate : string -> [> `OnPopState ] attrib
```
```ocaml
val a_onprogress : string -> [> `OnProgress ] attrib
```
```ocaml
val a_onratechange : string -> [> `OnRateChange ] attrib
```
```ocaml
val a_onreadystatechange : string -> [> `OnReadyStateChange ] attrib
```
```ocaml
val a_onredo : string -> [> `OnRedo ] attrib
```
```ocaml
val a_onresize : string -> [> `OnResize ] attrib
```
```ocaml
val a_onscroll : string -> [> `OnScroll ] attrib
```
```ocaml
val a_onseeked : string -> [> `OnSeeked ] attrib
```
```ocaml
val a_onseeking : string -> [> `OnSeeking ] attrib
```
```ocaml
val a_onselect : string -> [> `OnSelect ] attrib
```
```ocaml
val a_onshow : string -> [> `OnShow ] attrib
```
```ocaml
val a_onstalled : string -> [> `OnStalled ] attrib
```
```ocaml
val a_onstorage : string -> [> `OnStorage ] attrib
```
```ocaml
val a_onsubmit : string -> [> `OnSubmit ] attrib
```
```ocaml
val a_onsuspend : string -> [> `OnSuspend ] attrib
```
```ocaml
val a_ontimeupdate : string -> [> `OnTimeUpdate ] attrib
```
```ocaml
val a_onundo : string -> [> `OnUndo ] attrib
```
```ocaml
val a_onunload : string -> [> `OnUnload ] attrib
```
```ocaml
val a_onvolumechange : string -> [> `OnVolumeChange ] attrib
```
```ocaml
val a_onwaiting : string -> [> `OnWaiting ] attrib
```
```ocaml
val a_onload : string -> [> `OnLoad ] attrib
```
```ocaml
val a_onloadeddata : string -> [> `OnLoadedData ] attrib
```
```ocaml
val a_onloadedmetadata : string -> [> `OnLoadedMetaData ] attrib
```
```ocaml
val a_onloadstart : string -> [> `OnLoadStart ] attrib
```
```ocaml
val a_onmessage : string -> [> `OnMessage ] attrib
```
```ocaml
val a_onclick : string -> [> `OnClick ] attrib
```
```ocaml
val a_oncontextmenu : string -> [> `OnContextMenu ] attrib
```
```ocaml
val a_ondblclick : string -> [> `OnDblClick ] attrib
```
```ocaml
val a_ondrag : string -> [> `OnDrag ] attrib
```
```ocaml
val a_ondragend : string -> [> `OnDragEnd ] attrib
```
```ocaml
val a_ondragenter : string -> [> `OnDragEnter ] attrib
```
```ocaml
val a_ondragleave : string -> [> `OnDragLeave ] attrib
```
```ocaml
val a_ondragover : string -> [> `OnDragOver ] attrib
```
```ocaml
val a_ondragstart : string -> [> `OnDragStart ] attrib
```
```ocaml
val a_ondrop : string -> [> `OnDrop ] attrib
```
```ocaml
val a_onmousedown : string -> [> `OnMouseDown ] attrib
```
```ocaml
val a_onmouseup : string -> [> `OnMouseUp ] attrib
```
```ocaml
val a_onmouseover : string -> [> `OnMouseOver ] attrib
```
```ocaml
val a_onmousemove : string -> [> `OnMouseMove ] attrib
```
```ocaml
val a_onmouseout : string -> [> `OnMouseOut ] attrib
```
```ocaml
val a_ontouchstart : string -> [> `OnTouchStart ] attrib
```
```ocaml
val a_ontouchend : string -> [> `OnTouchEnd ] attrib
```
```ocaml
val a_ontouchmove : string -> [> `OnTouchMove ] attrib
```
```ocaml
val a_ontouchcancel : string -> [> `OnTouchCancel ] attrib
```
```ocaml
val a_onkeypress : string -> [> `OnKeyPress ] attrib
```
```ocaml
val a_onkeydown : string -> [> `OnKeyDown ] attrib
```
```ocaml
val a_onkeyup : string -> [> `OnKeyUp ] attrib
```