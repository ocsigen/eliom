
# Module `D.Raw`

```ocaml
type +'a elt = 'a elt
```
SVG elements.

Element constructors are in section [Elements](./#elements). Most elements constructors are either [nullary](./#type-nullary), [unary](./#type-unary) or [star](./#type-star), depending on the number of children they accept. Children are usually given as a list of elements. [txt](./#val-txt) is used for text.

The type variable `'a` is used to track the element's type. This allows the OCaml typechecker to check SVG validity.

Note that the concrete implementation of this type can vary. See [`Xml`](./Eliom_content-Svg-D-Raw-Xml.md) for details.

```ocaml
type doc = [ `Svg ] elt
```
A complete SVG document.

```ocaml
type +'a attrib = 'a attrib
```
SVG attributes

Attribute constructors are in section [Attributes](./#attributes) and their name starts with `a_`. Attributes are given to elements with the `~a` optional argument.

Similarly to [elt](./#type-elt), attributes use the OCaml type system to enforce Html validity.

In some cases, attributes have to be disambiguated. The `max` attribute has two version, [`a_fill`](./#val-a_fill) and [`a_animation_fill`](./#val-a_animation_fill), depending on the element. Such disambiguated attribute will contain the name of the associated element.

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
`list_wrap` is a containre for list of elements.

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
Various information about SVG, such as the doctype, ...


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
val a_version : string wrap -> [> `Version ] attrib
```
deprecated Removed in SVG2
```ocaml
val a_baseProfile : string wrap -> [> `BaseProfile ] attrib
```
deprecated Removed in SVG2
```ocaml
val a_x : Svg_types.coord wrap -> [> `X ] attrib
```
```ocaml
val a_y : Svg_types.coord wrap -> [> `Y ] attrib
```
```ocaml
val a_width : Svg_types.Unit.length wrap -> [> `Width ] attrib
```
```ocaml
val a_height : Svg_types.Unit.length wrap -> [> `Height ] attrib
```
```ocaml
val a_preserveAspectRatio : string wrap -> [> `PreserveAspectRatio ] attrib
```
```ocaml
val a_contentScriptType : string wrap -> [> `ContentScriptType ] attrib
```
deprecated Removed in SVG2
```ocaml
val a_contentStyleType : string wrap -> [> `ContentStyleType ] attrib
```
deprecated Removed in SVG2
```ocaml
val a_zoomAndPan : [< `Disable | `Magnify ] wrap -> [> `ZoomAndSpan ] attrib
```
```ocaml
val a_href : Svg_types.iri wrap -> [> `Xlink_href ] attrib
```
```ocaml
val a_xlink_href : Svg_types.iri wrap -> [> `Xlink_href ] attrib
```
deprecated Use a\_href
```ocaml
val a_requiredFeatures : 
  Svg_types.spacestrings wrap ->
  [> `RequiredFeatures ] attrib
```
deprecated Removed in SVG2
```ocaml
val a_requiredExtensions : 
  Svg_types.spacestrings wrap ->
  [> `RequiredExtension ] attrib
```
```ocaml
val a_systemLanguage : 
  Svg_types.commastrings wrap ->
  [> `SystemLanguage ] attrib
```
```ocaml
val a_externalRessourcesRequired : 
  bool wrap ->
  [> `ExternalRessourcesRequired ] attrib
```
```ocaml
val a_id : string wrap -> [> `Id ] attrib
```
```ocaml
val a_user_data : string -> string wrap -> [> `User_data ] attrib
```
```ocaml
val a_xml_base : Svg_types.iri wrap -> [> `Xml_Base ] attrib
```
deprecated Removed in SVG2
```ocaml
val a_xml_lang : Svg_types.iri wrap -> [> `Xml_Lang ] attrib
```
```ocaml
val a_xml_space : [< `Default | `Preserve ] wrap -> [> `Xml_Space ] attrib
```
deprecated Use CSS white-space
```ocaml
val a_type : string wrap -> [> `Type ] attrib
```
```ocaml
val a_media : Svg_types.commastrings wrap -> [> `Media ] attrib
```
```ocaml
val a_xlink_title : string wrap -> [> `Title ] attrib
```
deprecated Use a child title element
```ocaml
val a_class : Svg_types.spacestrings wrap -> [> `Class ] attrib
```
```ocaml
val a_style : string wrap -> [> `Style ] attrib
```
```ocaml
val a_transform : Svg_types.transforms wrap -> [> `Transform ] attrib
```
```ocaml
val a_viewBox : Svg_types.fourfloats wrap -> [> `ViewBox ] attrib
```
```ocaml
val a_d : string wrap -> [> `D ] attrib
```
```ocaml
val a_pathLength : float wrap -> [> `PathLength ] attrib
```
```ocaml
val a_rx : Svg_types.Unit.length wrap -> [> `Rx ] attrib
```
```ocaml
val a_ry : Svg_types.Unit.length wrap -> [> `Ry ] attrib
```
```ocaml
val a_cx : Svg_types.Unit.length wrap -> [> `Cx ] attrib
```
```ocaml
val a_cy : Svg_types.Unit.length wrap -> [> `Cy ] attrib
```
```ocaml
val a_r : Svg_types.Unit.length wrap -> [> `R ] attrib
```
```ocaml
val a_x1 : Svg_types.coord wrap -> [> `X1 ] attrib
```
```ocaml
val a_y1 : Svg_types.coord wrap -> [> `Y1 ] attrib
```
```ocaml
val a_x2 : Svg_types.coord wrap -> [> `X2 ] attrib
```
```ocaml
val a_y2 : Svg_types.coord wrap -> [> `Y2 ] attrib
```
```ocaml
val a_points : Svg_types.coords wrap -> [> `Points ] attrib
```
```ocaml
val a_x_list : Svg_types.lengths wrap -> [> `X_list ] attrib
```
```ocaml
val a_y_list : Svg_types.lengths wrap -> [> `Y_list ] attrib
```
```ocaml
val a_dx : Svg_types.number wrap -> [> `Dx ] attrib
```
```ocaml
val a_dy : Svg_types.number wrap -> [> `Dy ] attrib
```
```ocaml
val a_dx_list : Svg_types.lengths wrap -> [> `Dx_list ] attrib
```
```ocaml
val a_dy_list : Svg_types.lengths wrap -> [> `Dy_list ] attrib
```
```ocaml
val a_lengthAdjust : 
  [< `Spacing | `SpacingAndGlyphs ] wrap ->
  [> `LengthAdjust ] attrib
```
```ocaml
val a_textLength : Svg_types.Unit.length wrap -> [> `TextLength ] attrib
```
```ocaml
val a_text_anchor : 
  [< `Start | `Middle | `End | `Inherit ] wrap ->
  [> `Text_Anchor ] attrib
```
```ocaml
val a_text_decoration : 
  [< `None | `Underline | `Overline | `Line_through | `Blink | `Inherit ] wrap ->
  [> `Text_Decoration ] attrib
```
```ocaml
val a_text_rendering : 
  [< `Auto
  | `OptimizeSpeed
  | `OptimizeLegibility
  | `GeometricPrecision
  | `Inherit ]
    wrap ->
  [> `Text_Rendering ] attrib
```
```ocaml
val a_rotate : Svg_types.numbers wrap -> [> `Rotate ] attrib
```
```ocaml
val a_startOffset : Svg_types.Unit.length wrap -> [> `StartOffset ] attrib
```
```ocaml
val a_method : [< `Align | `Stretch ] wrap -> [> `Method ] attrib
```
```ocaml
val a_spacing : [< `Auto | `Exact ] wrap -> [> `Spacing ] attrib
```
```ocaml
val a_glyphRef : string wrap -> [> `GlyphRef ] attrib
```
```ocaml
val a_format : string wrap -> [> `Format ] attrib
```
```ocaml
val a_markerUnits : 
  [< `StrokeWidth | `UserSpaceOnUse ] wrap ->
  [> `MarkerUnits ] attrib
```
```ocaml
val a_refX : Svg_types.coord wrap -> [> `RefX ] attrib
```
```ocaml
val a_refY : Svg_types.coord wrap -> [> `RefY ] attrib
```
```ocaml
val a_markerWidth : Svg_types.Unit.length wrap -> [> `MarkerWidth ] attrib
```
```ocaml
val a_markerHeight : Svg_types.Unit.length wrap -> [> `MarkerHeight ] attrib
```
```ocaml
val a_orient : Svg_types.Unit.angle option wrap -> [> `Orient ] attrib
```
```ocaml
val a_local : string wrap -> [> `Local ] attrib
```
```ocaml
val a_rendering_intent : 
  [< `Auto
  | `Perceptual
  | `Relative_colorimetric
  | `Saturation
  | `Absolute_colorimetric ]
    wrap ->
  [> `Rendering_Indent ] attrib
```
```ocaml
val a_gradientUnits : 
  [< `UserSpaceOnUse | `ObjectBoundingBox ] wrap ->
  [ `GradientUnits ] attrib
```
```ocaml
val a_gradientTransform : 
  Svg_types.transforms wrap ->
  [> `GradientTransform ] attrib
```
```ocaml
val a_spreadMethod : 
  [< `Pad | `Reflect | `Repeat ] wrap ->
  [> `SpreadMethod ] attrib
```
```ocaml
val a_fx : Svg_types.coord wrap -> [> `Fx ] attrib
```
```ocaml
val a_fy : Svg_types.coord wrap -> [> `Fy ] attrib
```
```ocaml
val a_offset : 
  [< `Number of Svg_types.number | `Percentage of Svg_types.percentage ] wrap ->
  [> `Offset ] attrib
```
```ocaml
val a_patternUnits : 
  [< `UserSpaceOnUse | `ObjectBoundingBox ] wrap ->
  [> `PatternUnits ] attrib
```
```ocaml
val a_patternContentUnits : 
  [< `UserSpaceOnUse | `ObjectBoundingBox ] wrap ->
  [> `PatternContentUnits ] attrib
```
```ocaml
val a_patternTransform : 
  Svg_types.transforms wrap ->
  [> `PatternTransform ] attrib
```
```ocaml
val a_clipPathUnits : 
  [< `UserSpaceOnUse | `ObjectBoundingBox ] wrap ->
  [> `ClipPathUnits ] attrib
```
```ocaml
val a_maskUnits : 
  [< `UserSpaceOnUse | `ObjectBoundingBox ] wrap ->
  [> `MaskUnits ] attrib
```
```ocaml
val a_maskContentUnits : 
  [< `UserSpaceOnUse | `ObjectBoundingBox ] wrap ->
  [> `MaskContentUnits ] attrib
```
```ocaml
val a_primitiveUnits : 
  [< `UserSpaceOnUse | `ObjectBoundingBox ] wrap ->
  [> `PrimitiveUnits ] attrib
```
```ocaml
val a_filterRes : 
  Svg_types.number_optional_number wrap ->
  [> `FilterResUnits ] attrib
```
```ocaml
val a_result : string wrap -> [> `Result ] attrib
```
```ocaml
val a_in : 
  [< `SourceGraphic
  | `SourceAlpha
  | `BackgroundImage
  | `BackgroundAlpha
  | `FillPaint
  | `StrokePaint
  | `Ref of string ]
    wrap ->
  [> `In ] attrib
```
```ocaml
val a_in2 : 
  [< `SourceGraphic
  | `SourceAlpha
  | `BackgroundImage
  | `BackgroundAlpha
  | `FillPaint
  | `StrokePaint
  | `Ref of string ]
    wrap ->
  [> `In2 ] attrib
```
```ocaml
val a_azimuth : float wrap -> [> `Azimuth ] attrib
```
```ocaml
val a_elevation : float wrap -> [> `Elevation ] attrib
```
```ocaml
val a_pointsAtX : float wrap -> [> `PointsAtX ] attrib
```
```ocaml
val a_pointsAtY : float wrap -> [> `PointsAtY ] attrib
```
```ocaml
val a_pointsAtZ : float wrap -> [> `PointsAtZ ] attrib
```
```ocaml
val a_specularExponent : float wrap -> [> `SpecularExponent ] attrib
```
```ocaml
val a_specularConstant : float wrap -> [> `SpecularConstant ] attrib
```
```ocaml
val a_limitingConeAngle : float wrap -> [> `LimitingConeAngle ] attrib
```
```ocaml
val a_mode : 
  [< `Normal | `Multiply | `Screen | `Darken | `Lighten ] wrap ->
  [> `Mode ] attrib
```
```ocaml
val a_feColorMatrix_type : 
  [< `Matrix | `Saturate | `HueRotate | `LuminanceToAlpha ] wrap ->
  [> `Typefecolor ] attrib
```
```ocaml
val a_values : Svg_types.numbers wrap -> [> `Values ] attrib
```
```ocaml
val a_transfer_type : 
  [< `Identity | `Table | `Discrete | `Linear | `Gamma ] wrap ->
  [> `Type_transfert ] attrib
```
```ocaml
val a_tableValues : Svg_types.numbers wrap -> [> `TableValues ] attrib
```
```ocaml
val a_intercept : Svg_types.number wrap -> [> `Intercept ] attrib
```
```ocaml
val a_amplitude : Svg_types.number wrap -> [> `Amplitude ] attrib
```
```ocaml
val a_exponent : Svg_types.number wrap -> [> `Exponent ] attrib
```
```ocaml
val a_transfer_offset : Svg_types.number wrap -> [> `Offset_transfer ] attrib
```
```ocaml
val a_feComposite_operator : 
  [< `Over | `In | `Out | `Atop | `Xor | `Arithmetic ] wrap ->
  [> `OperatorComposite ] attrib
```
```ocaml
val a_k1 : Svg_types.number wrap -> [> `K1 ] attrib
```
```ocaml
val a_k2 : Svg_types.number wrap -> [> `K2 ] attrib
```
```ocaml
val a_k3 : Svg_types.number wrap -> [> `K3 ] attrib
```
```ocaml
val a_k4 : Svg_types.number wrap -> [> `K4 ] attrib
```
```ocaml
val a_order : Svg_types.number_optional_number wrap -> [> `Order ] attrib
```
```ocaml
val a_kernelMatrix : Svg_types.numbers wrap -> [> `KernelMatrix ] attrib
```
```ocaml
val a_divisor : Svg_types.number wrap -> [> `Divisor ] attrib
```
```ocaml
val a_bias : Svg_types.number wrap -> [> `Bias ] attrib
```
```ocaml
val a_kernelUnitLength : 
  Svg_types.number_optional_number wrap ->
  [> `KernelUnitLength ] attrib
```
```ocaml
val a_targetX : int wrap -> [> `TargetX ] attrib
```
```ocaml
val a_targetY : int wrap -> [> `TargetY ] attrib
```
```ocaml
val a_edgeMode : [< `Duplicate | `Wrap | `None ] wrap -> [> `TargetY ] attrib
```
```ocaml
val a_preserveAlpha : bool wrap -> [> `TargetY ] attrib
```
```ocaml
val a_surfaceScale : Svg_types.number wrap -> [> `SurfaceScale ] attrib
```
```ocaml
val a_diffuseConstant : Svg_types.number wrap -> [> `DiffuseConstant ] attrib
```
```ocaml
val a_scale : Svg_types.number wrap -> [> `Scale ] attrib
```
```ocaml
val a_xChannelSelector : 
  [< `R | `G | `B | `A ] wrap ->
  [> `XChannelSelector ] attrib
```
```ocaml
val a_yChannelSelector : 
  [< `R | `G | `B | `A ] wrap ->
  [> `YChannelSelector ] attrib
```
```ocaml
val a_stdDeviation : 
  Svg_types.number_optional_number wrap ->
  [> `StdDeviation ] attrib
```
```ocaml
val a_feMorphology_operator : 
  [< `Erode | `Dilate ] wrap ->
  [> `OperatorMorphology ] attrib
```
```ocaml
val a_radius : Svg_types.number_optional_number wrap -> [> `Radius ] attrib
```
```ocaml
val a_baseFrenquency : 
  Svg_types.number_optional_number wrap ->
  [> `BaseFrequency ] attrib
```
```ocaml
val a_numOctaves : int wrap -> [> `NumOctaves ] attrib
```
```ocaml
val a_seed : Svg_types.number wrap -> [> `Seed ] attrib
```
```ocaml
val a_stitchTiles : [< `Stitch | `NoStitch ] wrap -> [> `StitchTiles ] attrib
```
```ocaml
val a_feTurbulence_type : 
  [< `FractalNoise | `Turbulence ] wrap ->
  [> `TypeStitch ] attrib
```
```ocaml
val a_xlink_show : [< `New | `Replace ] wrap -> [> `Xlink_show ] attrib
```
deprecated Removed in SVG2
```ocaml
val a_xlink_actuate : 
  [< `OnRequest | `OnLoad | `Other | `None ] wrap ->
  [> `Xlink_actuate ] attrib
```
deprecated Removed in SVG2
```ocaml
val a_target : string wrap -> [> `Xlink_target ] attrib
```
```ocaml
val a_viewTarget : string wrap -> [> `ViewTarget ] attrib
```
deprecated Removed in SVG2
```ocaml
val a_attributeName : string wrap -> [> `AttributeName ] attrib
```
```ocaml
val a_attributeType : 
  [< `CSS | `XML | `Auto ] wrap ->
  [> `AttributeType ] attrib
```
```ocaml
val a_begin : string wrap -> [> `Begin ] attrib
```
```ocaml
val a_dur : string wrap -> [> `Dur ] attrib
```
```ocaml
val a_min : string wrap -> [> `Min ] attrib
```
```ocaml
val a_max : string wrap -> [> `Max ] attrib
```
```ocaml
val a_restart : 
  [< `Always | `WhenNotActive | `Never ] wrap ->
  [> `Restart ] attrib
```
```ocaml
val a_repeatCount : string wrap -> [> `RepeatCount ] attrib
```
```ocaml
val a_repeatDur : string wrap -> [> `RepeatDur ] attrib
```
```ocaml
val a_fill : Svg_types.paint wrap -> [> `Fill ] attrib
```
```ocaml
val a_animation_fill : 
  [< `Freeze | `Remove ] wrap ->
  [> `Fill_Animation ] attrib
```
```ocaml
val a_fill_rule : Svg_types.fill_rule wrap -> [> `Fill_rule ] attrib
```
```ocaml
val a_calcMode : 
  [< `Discrete | `Linear | `Paced | `Spline ] wrap ->
  [> `CalcMode ] attrib
```
```ocaml
val a_animation_values : Svg_types.strings wrap -> [> `Valuesanim ] attrib
```
```ocaml
val a_keyTimes : Svg_types.strings wrap -> [> `KeyTimes ] attrib
```
```ocaml
val a_keySplines : Svg_types.strings wrap -> [> `KeySplines ] attrib
```
```ocaml
val a_from : string wrap -> [> `From ] attrib
```
```ocaml
val a_to : string wrap -> [> `To ] attrib
```
```ocaml
val a_by : string wrap -> [> `By ] attrib
```
```ocaml
val a_additive : [< `Replace | `Sum ] wrap -> [> `Additive ] attrib
```
```ocaml
val a_accumulate : [< `None | `Sum ] wrap -> [> `Accumulate ] attrib
```
```ocaml
val a_keyPoints : Svg_types.numbers_semicolon wrap -> [> `KeyPoints ] attrib
```
```ocaml
val a_path : string wrap -> [> `Path ] attrib
```
```ocaml
val a_animateTransform_type : 
  [ `Translate | `Scale | `Rotate | `SkewX | `SkewY ] wrap ->
  [ `Typeanimatetransform ] attrib
```
```ocaml
val a_horiz_origin_x : Svg_types.number wrap -> [> `HorizOriginX ] attrib
```
```ocaml
val a_horiz_origin_y : Svg_types.number wrap -> [> `HorizOriginY ] attrib
```
```ocaml
val a_horiz_adv_x : Svg_types.number wrap -> [> `HorizAdvX ] attrib
```
```ocaml
val a_vert_origin_x : Svg_types.number wrap -> [> `VertOriginX ] attrib
```
```ocaml
val a_vert_origin_y : Svg_types.number wrap -> [> `VertOriginY ] attrib
```
```ocaml
val a_vert_adv_y : Svg_types.number wrap -> [> `VertAdvY ] attrib
```
```ocaml
val a_unicode : string wrap -> [> `Unicode ] attrib
```
```ocaml
val a_glyph_name : string wrap -> [> `glyphname ] attrib
```
```ocaml
val a_orientation : [< `H | `V ] wrap -> [> `Orientation ] attrib
```
```ocaml
val a_arabic_form : 
  [< `Initial | `Medial | `Terminal | `Isolated ] wrap ->
  [> `Arabicform ] attrib
```
```ocaml
val a_lang : string wrap -> [> `Lang ] attrib
```
```ocaml
val a_u1 : string wrap -> [> `U1 ] attrib
```
```ocaml
val a_u2 : string wrap -> [> `U2 ] attrib
```
```ocaml
val a_g1 : string wrap -> [> `G1 ] attrib
```
```ocaml
val a_g2 : string wrap -> [> `G2 ] attrib
```
```ocaml
val a_k : string wrap -> [> `K ] attrib
```
```ocaml
val a_font_family : string wrap -> [> `Font_Family ] attrib
```
```ocaml
val a_font_style : string wrap -> [> `Font_Style ] attrib
```
```ocaml
val a_font_variant : string wrap -> [> `Font_Variant ] attrib
```
```ocaml
val a_font_weight : string wrap -> [> `Font_Weight ] attrib
```
```ocaml
val a_font_stretch : string wrap -> [> `Font_Stretch ] attrib
```
```ocaml
val a_font_size : string wrap -> [> `Font_Size ] attrib
```
```ocaml
val a_unicode_range : string wrap -> [> `UnicodeRange ] attrib
```
```ocaml
val a_units_per_em : string wrap -> [> `UnitsPerEm ] attrib
```
```ocaml
val a_stemv : Svg_types.number wrap -> [> `Stemv ] attrib
```
```ocaml
val a_stemh : Svg_types.number wrap -> [> `Stemh ] attrib
```
```ocaml
val a_slope : Svg_types.number wrap -> [> `Slope ] attrib
```
```ocaml
val a_cap_height : Svg_types.number wrap -> [> `CapHeight ] attrib
```
```ocaml
val a_x_height : Svg_types.number wrap -> [> `XHeight ] attrib
```
```ocaml
val a_accent_height : Svg_types.number wrap -> [> `AccentHeight ] attrib
```
```ocaml
val a_ascent : Svg_types.number wrap -> [> `Ascent ] attrib
```
```ocaml
val a_widths : string wrap -> [> `Widths ] attrib
```
```ocaml
val a_bbox : string wrap -> [> `Bbox ] attrib
```
```ocaml
val a_ideographic : Svg_types.number wrap -> [> `Ideographic ] attrib
```
```ocaml
val a_alphabetic : Svg_types.number wrap -> [> `Alphabetic ] attrib
```
```ocaml
val a_mathematical : Svg_types.number wrap -> [> `Mathematical ] attrib
```
```ocaml
val a_hanging : Svg_types.number wrap -> [> `Hanging ] attrib
```
```ocaml
val a_videographic : Svg_types.number wrap -> [> `VIdeographic ] attrib
```
```ocaml
val a_v_alphabetic : Svg_types.number wrap -> [> `VAlphabetic ] attrib
```
```ocaml
val a_v_mathematical : Svg_types.number wrap -> [> `VMathematical ] attrib
```
```ocaml
val a_v_hanging : Svg_types.number wrap -> [> `VHanging ] attrib
```
```ocaml
val a_underline_position : 
  Svg_types.number wrap ->
  [> `UnderlinePosition ] attrib
```
```ocaml
val a_underline_thickness : 
  Svg_types.number wrap ->
  [> `UnderlineThickness ] attrib
```
```ocaml
val a_strikethrough_position : 
  Svg_types.number wrap ->
  [> `StrikethroughPosition ] attrib
```
```ocaml
val a_strikethrough_thickness : 
  Svg_types.number wrap ->
  [> `StrikethroughThickness ] attrib
```
```ocaml
val a_overline_position : 
  Svg_types.number wrap ->
  [> `OverlinePosition ] attrib
```
```ocaml
val a_overline_thickness : 
  Svg_types.number wrap ->
  [> `OverlineThickness ] attrib
```
```ocaml
val a_string : string wrap -> [> `String ] attrib
```
```ocaml
val a_name : string wrap -> [> `Name ] attrib
```
```ocaml
val a_alignment_baseline : 
  [< `Auto
  | `Baseline
  | `Before_edge
  | `Text_before_edge
  | `Middle
  | `Central
  | `After_edge
  | `Text_after_edge
  | `Ideographic
  | `Alphabetic
  | `Hanging
  | `Mathematical
  | `Inherit ]
    wrap ->
  [> `Alignment_Baseline ] attrib
```
```ocaml
val a_dominant_baseline : 
  [< `Auto
  | `Use_script
  | `No_change
  | `Reset_size
  | `Ideographic
  | `Alphabetic
  | `Hanging
  | `Mathematical
  | `Central
  | `Middle
  | `Text_after_edge
  | `Text_before_edge
  | `Inherit ]
    wrap ->
  [> `Dominant_Baseline ] attrib
```
```ocaml
val a_stop_color : Svg_types.color wrap -> [> `Stop_Color ] attrib
```
```ocaml
val a_stop_opacity : Svg_types.number wrap -> [> `Stop_Opacity ] attrib
```
```ocaml
val a_stroke : Svg_types.paint wrap -> [> `Stroke ] attrib
```
```ocaml
val a_stroke_width : Svg_types.Unit.length wrap -> [> `Stroke_Width ] attrib
```
```ocaml
val a_stroke_linecap : 
  [< `Butt | `Round | `Square ] wrap ->
  [> `Stroke_Linecap ] attrib
```
```ocaml
val a_stroke_linejoin : 
  [< `Miter | `Round | `Bever ] wrap ->
  [> `Stroke_Linejoin ] attrib
```
```ocaml
val a_stroke_miterlimit : float wrap -> [> `Stroke_Miterlimit ] attrib
```
```ocaml
val a_stroke_dasharray : 
  Svg_types.Unit.length list wrap ->
  [> `Stroke_Dasharray ] attrib
```
```ocaml
val a_stroke_dashoffset : 
  Svg_types.Unit.length wrap ->
  [> `Stroke_Dashoffset ] attrib
```
```ocaml
val a_stroke_opacity : float wrap -> [> `Stroke_Opacity ] attrib
```

### Events


#### Javascript events


#### Javascript mouse events


### Elements

```ocaml
val txt : string wrap -> [> Svg_types.txt ] elt
```
```ocaml
val svg : 
  ([< Svg_types.svg_attr ], [< Svg_types.svg_content ], [> Svg_types.svg ])
    star
```
```ocaml
val g : 
  ([< Svg_types.g_attr ], [< Svg_types.g_content ], [> Svg_types.g ]) star
```
```ocaml
val defs : 
  ([< Svg_types.defs_attr ], [< Svg_types.defs_content ], [> Svg_types.defs ])
    star
```
```ocaml
val desc : 
  ([< Svg_types.desc_attr ], [< Svg_types.desc_content ], [> Svg_types.desc ])
    unary
```
```ocaml
val title : 
  ([< Svg_types.title_attr ],
    [< Svg_types.title_content ],
    [> Svg_types.title ])
    unary
```
```ocaml
val symbol : 
  ([< Svg_types.symbol_attr ],
    [< Svg_types.symbol_content ],
    [> Svg_types.symbol ])
    star
```
```ocaml
val use : 
  ([< Svg_types.use_attr ], [< Svg_types.use_content ], [> Svg_types.use ])
    star
```
```ocaml
val image : 
  ([< Svg_types.image_attr ],
    [< Svg_types.image_content ],
    [> Svg_types.image ])
    star
```
```ocaml
val switch : 
  ([< Svg_types.switch_attr ],
    [< Svg_types.switch_content ],
    [> Svg_types.switch ])
    star
```
```ocaml
val style : 
  ([< Svg_types.style_attr ],
    [< Svg_types.style_content ],
    [> Svg_types.style ])
    unary
```
```ocaml
val path : 
  ([< Svg_types.path_attr ], [< Svg_types.path_content ], [> Svg_types.path ])
    star
```
```ocaml
val rect : 
  ([< Svg_types.rect_attr ], [< Svg_types.rect_content ], [> Svg_types.rect ])
    star
```
```ocaml
val circle : 
  ([< Svg_types.circle_attr ],
    [< Svg_types.circle_content ],
    [> Svg_types.circle ])
    star
```
```ocaml
val ellipse : 
  ([< Svg_types.ellipse_attr ],
    [< Svg_types.ellipse_content ],
    [> Svg_types.ellipse ])
    star
```
```ocaml
val line : 
  ([< Svg_types.line_attr ], [< Svg_types.line_content ], [> Svg_types.line ])
    star
```
```ocaml
val polyline : 
  ([< Svg_types.polyline_attr ],
    [< Svg_types.polyline_content ],
    [> Svg_types.polyline ])
    star
```
```ocaml
val polygon : 
  ([< Svg_types.polygon_attr ],
    [< Svg_types.polygon_content ],
    [> Svg_types.polygon ])
    star
```
```ocaml
val text : 
  ([< Svg_types.text_attr ], [< Svg_types.text_content ], [> Svg_types.text ])
    star
```
```ocaml
val tspan : 
  ([< Svg_types.tspan_attr ],
    [< Svg_types.tspan_content ],
    [> Svg_types.tspan ])
    star
```
```ocaml
val tref : 
  ([< Svg_types.tref_attr ], [< Svg_types.tref_content ], [> Svg_types.tref ])
    star
```
deprecated Removed in SVG2
```ocaml
val textPath : 
  ([< Svg_types.textpath_attr ],
    [< Svg_types.textpath_content ],
    [> Svg_types.textpath ])
    star
```
```ocaml
val altGlyph : 
  ([< Svg_types.altglyph_attr ],
    [< Svg_types.altglyph_content ],
    [> Svg_types.altglyph ])
    unary
```
deprecated Removed in SVG2
```ocaml
type altglyphdef_content = [ 
  | `Ref of Svg_types.glyphref elt list
  | `Item of Svg_types.altglyphitem elt list
 ]
```
```ocaml
val altGlyphDef : 
  ([< Svg_types.altglyphdef_attr ],
    [< altglyphdef_content ],
    [> Svg_types.altglyphdef ])
    unary
```
deprecated Removed in SVG2
```ocaml
val altGlyphItem : 
  ([< Svg_types.altglyphitem_attr ],
    [< Svg_types.altglyphitem_content ],
    [> Svg_types.altglyphitem ])
    star
```
deprecated Removed in SVG2
```ocaml
val glyphRef : ([< Svg_types.glyphref_attr ], [> Svg_types.glyphref ]) nullary
```
deprecated Removed in SVG2
```ocaml
val marker : 
  ([< Svg_types.marker_attr ],
    [< Svg_types.marker_content ],
    [> Svg_types.marker ])
    star
```
```ocaml
val color_profile : 
  ([< Svg_types.colorprofile_attr ],
    [< Svg_types.colorprofile_content ],
    [> Svg_types.colorprofile ])
    star
```
deprecated Removed in SVG2
```ocaml
val linearGradient : 
  ([< Svg_types.lineargradient_attr ],
    [< Svg_types.lineargradient_content ],
    [> Svg_types.lineargradient ])
    star
```
```ocaml
val radialGradient : 
  ([< Svg_types.radialgradient_attr ],
    [< Svg_types.radialgradient_content ],
    [> Svg_types.radialgradient ])
    star
```
```ocaml
val stop : 
  ([< Svg_types.stop_attr ], [< Svg_types.stop_content ], [> Svg_types.stop ])
    star
```
```ocaml
val pattern : 
  ([< Svg_types.pattern_attr ],
    [< Svg_types.pattern_content ],
    [> Svg_types.pattern ])
    star
```
```ocaml
val clipPath : 
  ([< Svg_types.clippath_attr ],
    [< Svg_types.clippath_content ],
    [> Svg_types.clippath ])
    star
```
```ocaml
val filter : 
  ([< Svg_types.filter_attr ],
    [< Svg_types.filter_content ],
    [> Svg_types.filter ])
    star
```
```ocaml
val feDistantLight : 
  ([< Svg_types.fedistantlight_attr ],
    [< Svg_types.fedistantlight_content ],
    [> Svg_types.fedistantlight ])
    star
```
```ocaml
val fePointLight : 
  ([< Svg_types.fepointlight_attr ],
    [< Svg_types.fepointlight_content ],
    [> Svg_types.fepointlight ])
    star
```
```ocaml
val feSpotLight : 
  ([< Svg_types.fespotlight_attr ],
    [< Svg_types.fespotlight_content ],
    [> Svg_types.fespotlight ])
    star
```
```ocaml
val feBlend : 
  ([< Svg_types.feblend_attr ],
    [< Svg_types.feblend_content ],
    [> Svg_types.feblend ])
    star
```
```ocaml
val feColorMatrix : 
  ([< Svg_types.fecolormatrix_attr ],
    [< Svg_types.fecolormatrix_content ],
    [> Svg_types.fecolormatrix ])
    star
```
```ocaml
val feComponentTransfer : 
  ([< Svg_types.fecomponenttransfer_attr ],
    [< Svg_types.fecomponenttransfer_content ],
    [> Svg_types.fecomponenttransfer ])
    star
```
```ocaml
val feFuncA : 
  ([< Svg_types.fefunca_attr ],
    [< Svg_types.fefunca_content ],
    [> Svg_types.fefunca ])
    star
```
```ocaml
val feFuncG : 
  ([< Svg_types.fefuncg_attr ],
    [< Svg_types.fefuncg_content ],
    [> Svg_types.fefuncg ])
    star
```
```ocaml
val feFuncB : 
  ([< Svg_types.fefuncb_attr ],
    [< Svg_types.fefuncb_content ],
    [> Svg_types.fefuncb ])
    star
```
```ocaml
val feFuncR : 
  ([< Svg_types.fefuncr_attr ],
    [< Svg_types.fefuncr_content ],
    [> Svg_types.fefuncr ])
    star
```
```ocaml
val feComposite : 
  ([< Svg_types.fecomposite_attr ],
    [< Svg_types.fecomposite_content ],
    [> Svg_types.fecomposite ])
    star
```
```ocaml
val feConvolveMatrix : 
  ([< Svg_types.feconvolvematrix_attr ],
    [< Svg_types.feconvolvematrix_content ],
    [> Svg_types.feconvolvematrix ])
    star
```
```ocaml
val feDiffuseLighting : 
  ([< Svg_types.fediffuselighting_attr ],
    [< Svg_types.fediffuselighting_content ],
    [> Svg_types.fediffuselighting ])
    star
```
```ocaml
val feDisplacementMap : 
  ([< Svg_types.fedisplacementmap_attr ],
    [< Svg_types.fedisplacementmap_content ],
    [> Svg_types.fedisplacementmap ])
    star
```
```ocaml
val feFlood : 
  ([< Svg_types.feflood_attr ],
    [< Svg_types.feflood_content ],
    [> Svg_types.feflood ])
    star
```
```ocaml
val feGaussianBlur : 
  ([< Svg_types.fegaussianblur_attr ],
    [< Svg_types.fegaussianblur_content ],
    [> Svg_types.fegaussianblur ])
    star
```
```ocaml
val feImage : 
  ([< Svg_types.feimage_attr ],
    [< Svg_types.feimage_content ],
    [> Svg_types.feimage ])
    star
```
```ocaml
val feMerge : 
  ([< Svg_types.femerge_attr ],
    [< Svg_types.femerge_content ],
    [> Svg_types.femerge ])
    star
```
```ocaml
val feMorphology : 
  ([< Svg_types.femorphology_attr ],
    [< Svg_types.femorphology_content ],
    [> Svg_types.femorphology ])
    star
```
```ocaml
val feOffset : 
  ([< Svg_types.feoffset_attr ],
    [< Svg_types.feoffset_content ],
    [> Svg_types.feoffset ])
    star
```
```ocaml
val feSpecularLighting : 
  ([< Svg_types.fespecularlighting_attr ],
    [< Svg_types.fespecularlighting_content ],
    [> Svg_types.fespecularlighting ])
    star
```
```ocaml
val feTile : 
  ([< Svg_types.fetile_attr ],
    [< Svg_types.fetile_content ],
    [> Svg_types.fetile ])
    star
```
```ocaml
val feTurbulence : 
  ([< Svg_types.feturbulence_attr ],
    [< Svg_types.feturbulence_content ],
    [> Svg_types.feturbulence ])
    star
```
```ocaml
val cursor : 
  ([< Svg_types.cursor_attr ],
    [< Svg_types.cursor_content ],
    [> Svg_types.cursor ])
    star
```
```ocaml
val a : 
  ([< Svg_types.a_attr ], [< Svg_types.a_content ], [> Svg_types.a ]) star
```
```ocaml
val view : 
  ([< Svg_types.view_attr ], [< Svg_types.view_content ], [> Svg_types.view ])
    star
```
```ocaml
val script : 
  ([< Svg_types.script_attr ],
    [< Svg_types.script_content ],
    [> Svg_types.script ])
    unary
```
```ocaml
val animate : 
  ([< Svg_types.animate_attr ],
    [< Svg_types.animate_content ],
    [> Svg_types.animate ])
    star
```
```ocaml
val set : 
  ([< Svg_types.set_attr ], [< Svg_types.set_content ], [> Svg_types.set ])
    star
```
```ocaml
val animateMotion : 
  ([< Svg_types.animatemotion_attr ],
    [< Svg_types.animatemotion_content ],
    [> Svg_types.animatemotion ])
    star
```
```ocaml
val mpath : 
  ([< Svg_types.mpath_attr ],
    [< Svg_types.mpath_content ],
    [> Svg_types.mpath ])
    star
```
```ocaml
val animateColor : 
  ([< Svg_types.animatecolor_attr ],
    [< Svg_types.animatecolor_content ],
    [> Svg_types.animatecolor ])
    star
```
```ocaml
val animateTransform : 
  ([< Svg_types.animatetransform_attr ],
    [< Svg_types.animatetransform_content ],
    [> Svg_types.animatetransform ])
    star
```
```ocaml
val font : 
  ([< Svg_types.font_attr ], [< Svg_types.font_content ], [> Svg_types.font ])
    star
```
deprecated Removed in SVG2
```ocaml
val glyph : 
  ([< Svg_types.glyph_attr ],
    [< Svg_types.glyph_content ],
    [> Svg_types.glyph ])
    star
```
deprecated Removed in SVG2
```ocaml
val missing_glyph : 
  ([< Svg_types.missingglyph_attr ],
    [< Svg_types.missingglyph_content ],
    [> Svg_types.missingglyph ])
    star
```
deprecated Removed in SVG2
```ocaml
val hkern : ([< Svg_types.hkern_attr ], [> Svg_types.hkern ]) nullary
```
deprecated Removed in SVG2
```ocaml
val vkern : ([< Svg_types.vkern_attr ], [> Svg_types.vkern ]) nullary
```
deprecated Removed in SVG2
```ocaml
val font_face : 
  ([< Svg_types.font_face_attr ], [> Svg_types.font_face ]) nullary
```
deprecated Removed in SVG2
```ocaml
val font_face_src : 
  ([< Svg_types.font_face_src_attr ],
    [< Svg_types.font_face_src_content ],
    [> Svg_types.font_face_src ])
    star
```
deprecated Removed in SVG2
```ocaml
val font_face_uri : 
  ([< Svg_types.font_face_uri_attr ],
    [< Svg_types.font_face_uri_content ],
    [> Svg_types.font_face_uri ])
    star
```
deprecated Removed in SVG2
```ocaml
val font_face_format : 
  ([< Svg_types.font_face_format_attr ], [> Svg_types.font_face_format ])
    nullary
```
deprecated Removed in SVG2
```ocaml
val font_face_name : 
  ([< Svg_types.font_face_name_attr ], [> Svg_types.font_face_name ]) nullary
```
deprecated Removed in SVG2
```ocaml
val metadata : 
  ?a:Svg_types.metadata_attr attrib list ->
  Xml.elt list_wrap ->
  [> Svg_types.metadata ] elt
```
```ocaml
val foreignObject : 
  ?a:Svg_types.foreignobject_attr attrib list ->
  Xml.elt list_wrap ->
  [> Svg_types.foreignobject ] elt
```

#### Deprecated

```ocaml
val pcdata : string wrap -> [> Svg_types.txt ] elt
```
deprecated Use txt instead
```ocaml
val animation : 
  ([< Svg_types.animate_attr ],
    [< Svg_types.animate_content ],
    [> Svg_types.animate ])
    star
```
deprecated Use animate instead

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
val a_onactivate : string -> [> `OnActivate ] attrib
```
```ocaml
val a_onbegin : string -> [> `OnBegin ] attrib
```
```ocaml
val a_onend : string -> [> `OnEnd ] attrib
```
```ocaml
val a_onerror : string -> [> `OnError ] attrib
```
```ocaml
val a_onfocusin : string -> [> `OnFocusIn ] attrib
```
```ocaml
val a_onfocusout : string -> [> `OnFocusOut ] attrib
```
```ocaml
val a_onload : string -> [> `OnLoad ] attrib
```
deprecated Removed in SVG2
```ocaml
val a_onrepeat : string -> [> `OnRepeat ] attrib
```
```ocaml
val a_onresize : string -> [> `OnResize ] attrib
```
```ocaml
val a_onscroll : string -> [> `OnScroll ] attrib
```
```ocaml
val a_onunload : string -> [> `OnUnload ] attrib
```
```ocaml
val a_onzoom : string -> [> `OnZoom ] attrib
```
```ocaml
val a_onclick : string -> [> `OnClick ] attrib
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
val a_onmouseout : string -> [> `OnMouseOut ] attrib
```
```ocaml
val a_onmousemove : string -> [> `OnMouseMove ] attrib
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