
(** Type instantiations for SVG *)

(** This module defines basic data types for data, attributes
    and element occuring in SVG documents.
    It is based on the specification available at http://www.w3.org/TR/SVG/.

    This module is experimental, it may lack of some attributes,
    and the interface is very low level and do not take deeply into account
    the needs of SVG elements. *)

module M :
  sig
    open Svgtypes
    open Unit
      
    (** {1 Abstraction over XML's types *)
    type 'a attrib
    
    type 'a attribs
    
    type +'a elt
    
    type +'a elts
    
    type ('a, 'b) nullary = ?a: (('a attrib) list) -> unit -> 'b elt
    
    type ('a, 'b, 'c) unary = ?a: (('a attrib) list) -> 'b elt -> 'c elt
    
    type ('a, 'b, 'c) star =
      ?a: (('a attrib) list) -> ('b elt) list -> 'c elt
    
    type ('a, 'b, 'c) plus =
      ?a: (('a attrib) list) -> 'b elt -> ('b elt) list -> 'c elt
    
    (* to be processed by a script *)
    type altglyphdef_content =
      [ | `Ref of (glyphref elt) list | `Item of (altglyphitem elt) list
      ]
    
    (** {1 attributes } *)
    val a_version : string -> [ | `Version ] attrib
      
    val a_baseprofile : string -> [ | `BaseProfile ] attrib
      
    val a_x : coord -> [ | `X ] attrib
      
    val a_y : coord -> [ | `Y ] attrib
      
    val a_width : Unit.length -> [ | `Width ] attrib
      
    val a_height : Unit.length -> [ | `Height ] attrib
      
    val a_preserveaspectratio : string -> [ | `PreserveAspectRatio ] attrib
      
    val a_contentscripttype : string -> [ | `ContentScriptType ] attrib
      
    val a_contentstyletype : string -> [ | `ContentStyleType ] attrib
      
    val a_zoomAndPan : [ | `Disable | `Magnify ] -> [ | `ZoomAndSpan ] attrib
      
    val a_xlink_href : iri -> [ | `Xlink_href ] attrib
      
    val a_requiredfeatures : spacestrings -> [ | `RequiredFeatures ] attrib
      
    val a_requiredextensions :
      spacestrings -> [ | `RequiredExtension ] attrib
      
    val a_systemlanguage : commastrings -> [ | `SystemLanguage ] attrib
      
    val a_externalressourcesrequired :
      bool -> [ | `ExternalRessourcesRequired ] attrib
      
    val a_id : string -> [ | `Id ] attrib
      
    val a_xml_base : iri -> [ | `Xml_Base ] attrib
      
    val a_xml_lang : iri -> [ | `Xml_Lang ] attrib
      
    val a_xml_space : [ | `Default | `Preserve ] -> [ | `Xml_Space ] attrib
      
    val a_type : string -> [ | `Type ] attrib
      
    val a_media : commastrings -> [ | `Media ] attrib
      
    val a_title : string -> [ | `Title ] attrib
      
    val a_class : spacestrings -> [ | `Class ] attrib
      
    val a_style : string -> [ | `Style ] attrib
      
    val a_transform : transform -> [ | `Transform ] attrib
      
    val a_viewbox : fourfloats -> [ | `Viewbox ] attrib
      
    val a_d : string -> [ | `D ] attrib
      
    val a_pathlength : float -> [ | `PathLength ] attrib
      
    (* XXX: better language support *)
    val a_rx : Unit.length -> [ | `Rx ] attrib
      
    val a_ry : Unit.length -> [ | `Ry ] attrib
      
    val a_cx : Unit.length -> [ | `Cx ] attrib
      
    val a_cy : Unit.length -> [ | `Cy ] attrib
      
    val a_r : Unit.length -> [ | `R ] attrib
      
    val a_x1 : coord -> [ | `X1 ] attrib
      
    val a_y1 : coord -> [ | `Y1 ] attrib
      
    val a_x2 : coord -> [ | `X2 ] attrib
      
    val a_y2 : coord -> [ | `Y2 ] attrib
      
    val a_points : coords -> [ | `Points ] attrib
      
    val a_x_list : lengths -> [ | `X__list ] attrib
      
    val a_y_list : lengths -> [ | `Y__list ] attrib
      
    val a_dx : lengths -> [ | `Dx ] attrib
      
    val a_dy : lengths -> [ | `Dy ] attrib
      
    val a_dx_single : Unit.length -> [ | `Dx__single ] attrib
      
    val a_dy_single : Unit.length -> [ | `Dy__single ] attrib
      
    val a_dx_number : number -> [ | `Dx__number ] attrib
      
    val a_dy_number : number -> [ | `Dy__number ] attrib
      
    val a_lengthadjust :
      [ | `Spacing | `SpacingAndGlyphs ] -> [ | `LengthAdjust ] attrib
      
    val a_textlength : Unit.length -> [ | `TextLength ] attrib
      
    val a_rotate : numbers -> [ | `Rotate ] attrib
      
    val a_startoffset : Unit.length -> [ | `StartOffset ] attrib
      
    val a_method : [ | `Align | `Stretch ] -> [ | `Method ] attrib
      
    val a_spacing : [ | `Auto | `Exact ] -> [ | `Spacing ] attrib
      
    val a_glyphref : string -> [ | `GlyphRef ] attrib
      
    val a_format : string -> [ | `Format ] attrib
      
    val a_markerunits :
      [ | `StrokeWidth | `UserSpaceOnUse ] -> [ | `MarkerUnits ] attrib
      
    val a_refx : coord -> [ | `RefX ] attrib
      
    val a_refy : coord -> [ | `RefY ] attrib
      
    val a_markerwidth : Unit.length -> [ | `MarkerWidth ] attrib
      
    val a_markerheight : Unit.length -> [ | `MarkerHeight ] attrib
      
    val a_orient : [ | `Auto | `Angle of angle ] -> [ | `Orient ] attrib
      
    val a_local : string -> [ | `Local ] attrib
      
    val a_renderingindent :
      [
        | `Auto
        | `Perceptual
        | `Relative_colorimetric
        | `Saturation
        | `Absolute_colorimetric ] -> [ | `Rendering_Indent ] attrib
      
    val a_gradientunits :
      [ | `UserSpaceOnUse | `ObjectBoundingBox ] ->
        [ | `GradientUnits ] attrib
      
    val a_gradienttransform : transforms -> [ | `Gradient_Transform ] attrib
      
    val a_spreadmethod :
      [ | `Pad | `Reflect | `Repeat ] -> [ | `SpreadMethod ] attrib
      
    val a_fx : coord -> [ | `Fx ] attrib
      
    val a_fy : coord -> [ | `Fy ] attrib
      
    val a_offset :
      [ | `Number of number | `Percentage of percentage ] ->
        [ | `Offset ] attrib
      
    val a_patternunits :
      [ | `UserSpaceOnUse | `ObjectBoundingBox ] ->
        [ | `PatternUnits ] attrib
      
    val a_patterncontentunits :
      [ | `UserSpaceOnUse | `ObjectBoundingBox ] ->
        [ | `PatternContentUnits ] attrib
      
    val a_patterntransform : transforms -> [ | `PatternTransform ] attrib
      
    val a_clippathunits :
      [ | `UserSpaceOnUse | `ObjectBoundingBox ] ->
        [ | `ClipPathUnits ] attrib
      
    val a_maskunits :
      [ | `UserSpaceOnUse | `ObjectBoundingBox ] -> [ | `MaskUnits ] attrib
      
    val a_maskcontentunits :
      [ | `UserSpaceOnUse | `ObjectBoundingBox ] ->
        [ | `MaskContentUnits ] attrib
      
    val a_primitiveunits :
      [ | `UserSpaceOnUse | `ObjectBoundingBox ] ->
        [ | `PrimitiveUnits ] attrib
      
    val a_filterres : number_optional_number -> [ | `FilterResUnits ] attrib
      
    val a_result : string -> [ | `Result ] attrib
      
    val a_in :
      [
        | `SourceGraphic
        | `SourceAlpha
        | `BackgroundImage
        | `BackgroundAlpha
        | `FillPaint
        | `StrokePaint
        | `Ref of string ] -> [ | `In ] attrib
      
    val a_in2 :
      [
        | `SourceGraphic
        | `SourceAlpha
        | `BackgroundImage
        | `BackgroundAlpha
        | `FillPaint
        | `StrokePaint
        | `Ref of string ] -> [ | `In2 ] attrib
      
    val a_aizmuth : float -> [ | `Azimuth ] attrib
      
    val a_elevation : float -> [ | `Elevation ] attrib
      
    val a_pointatx : float -> [ | `PointsAtX ] attrib
      
    val a_pointaty : float -> [ | `PointsAtY ] attrib
      
    val a_pointatz : float -> [ | `PointsAtZ ] attrib
      
    val a_specularexponent : float -> [ | `SpecularExponent ] attrib
      
    val a_specularconstant : float -> [ | `SpecularConstant ] attrib
      
    val a_limitingconeangle : float -> [ | `LimitingConeAngle ] attrib
      
    val a_mode :
      [ | `Normal | `Multiply | `Screen | `Darken | `Lighten ] ->
        [ | `Mode ] attrib
      
    val a_typefecolor :
      [ | `Matrix | `Saturate | `HueRotate | `LuminanceToAlpha ] ->
        [ | `Type__fecolor ] attrib
      
    val a_values : numbers -> [ | `Values ] attrib
      
    val a_transferttype :
      [ | `Identity | `Table | `Discrete | `Linear | `Gamma ] ->
        [ | `Type__transfert ] attrib
      
    val a_tablevalues : numbers -> [ | `TableValues ] attrib
      
    val a_slope : number -> [ | `Slope ] attrib
      
    val a_intercept : number -> [ | `Intercept ] attrib
      
    val a_amplitude : number -> [ | `Amplitude ] attrib
      
    val a_exponent : number -> [ | `Exponent ] attrib
      
    val a_offsettransfer : number -> [ | `Offset__transfer ] attrib
      
    val a_operator :
      [ | `Over | `In | `Out | `Atop | `Xor | `Arithmetic ] ->
        [ | `Operator ] attrib
      
    val a_k1 : number -> [ | `K1 ] attrib
      
    val a_k2 : number -> [ | `K2 ] attrib
      
    val a_k3 : number -> [ | `K3 ] attrib
      
    val a_k4 : number -> [ | `K4 ] attrib
      
    val a_order : number_optional_number -> [ | `Order ] attrib
      
    val a_kernelmatrix : numbers -> [ | `KernelMatrix ] attrib
      
    val a_divisor : number -> [ | `Divisor ] attrib
      
    val a_bias : number -> [ | `Bias ] attrib
      
    val a_kernelunitlength :
      number_optional_number -> [ | `KernelUnitLength ] attrib
      
    val a_targetX : int -> [ | `TargetX ] attrib
      
    val a_targetY : int -> [ | `TargetY ] attrib
      
    val a_edgemode :
      [ | `Duplicate | `Wrap | `None ] -> [ | `TargetY ] attrib
      
    val a_preservealpha : bool -> [ | `TargetY ] attrib
      
    val a_surfacescale : number -> [ | `SurfaceScale ] attrib
      
    val a_diffuseconstant : number -> [ | `DiffuseConstant ] attrib
      
    val a_scale : number -> [ | `Scale ] attrib
      
    val a_xchannelselector :
      [ | `R | `G | `B | `A ] -> [ | `XChannelSelector ] attrib
      
    val a_ychannelselector :
      [ | `R | `G | `B | `A ] -> [ | `YChannelSelector ] attrib
      
    val a_stddeviation : number_optional_number -> [ | `StdDeviation ] attrib
      
    val a_operatormorphology :
      [ | `Erode | `Dilate ] -> [ | `OperatorMorphology ] attrib
      
    val a_radius : number_optional_number -> [ | `Radius ] attrib
      
    val a_basefrenquency :
      number_optional_number -> [ | `BaseFrequency ] attrib
      
    val a_numoctaves : int -> [ | `NumOctaves ] attrib
      
    val a_seed : number -> [ | `Seed ] attrib
      
    val a_stitchtiles :
      [ | `Stitch | `NoStitch ] -> [ | `StitchTiles ] attrib
      
    val a_stitchtype :
      [ | `FractalNoise | `Turbulence ] -> [ | `TypeStitch ] attrib
      
    val a_xlinkshow : [ | `New | `Replace ] -> [ | `Xlink_show ] attrib
      
    val a_xlinkactuate : [ | `OnRequest ] -> [ | `Xlink_actuate ] attrib
      
    val a_target : string -> [ | `Xlink_target ] attrib
      
    val a_viewtarget : string -> [ | `ViewTarget ] attrib
      
    val a_attributename : string -> [ | `AttributeName ] attrib
      
    val a_attributetype :
      [ | `CSS | `XML | `Auto ] -> [ | `AttributeType ] attrib
      
    val a_begin : string -> [ | `Begin ] attrib
      
    val a_dur : string -> [ | `Dur ] attrib
      
    (* XXX*)
    val a_min : string -> [ | `Min ] attrib
      
    (* XXX *)
    val a_max : string -> [ | `Max ] attrib
      
    (* XXX *)
    val a_restart :
      [ | `Always | `WhenNotActive | `Never ] -> [ | `Restart ] attrib
      
    val a_repeatcount : string -> [ | `RepeatCount ] attrib
      
    (* XXX *)
    val a_repeatdur : string -> [ | `RepeatDur ] attrib
      
    (* XXX *)
    val a_fill : [ | `Freeze | `Remove ] -> [ | `Fill ] attrib
      
    val a_calcmode :
      [ | `Discrete | `Linear | `Paced | `Spline ] -> [ | `CalcMode ] attrib
      
    val a_values_anim : strings -> [ | `Values__anim ] attrib
      
    val a_keytimes : strings -> [ | `KeyTimes ] attrib
      
    val a_keysplines : strings -> [ | `KeySplines ] attrib
      
    val a_from : string -> [ | `From ] attrib
      
    val a_to : string -> [ | `To ] attrib
      
    val a_by : string -> [ | `By ] attrib
      
    val a_additive : [ | `Replace | `Sum ] -> [ | `Additive ] attrib
      
    val a_accumulate : [ | `None | `Sum ] -> [ | `Accumulate ] attrib
      
    val a_keypoints : numbers_semicolon -> [ | `KeyPoints ] attrib
      
    val a_path : string -> [ | `Path ] attrib
      
    val a_typeanimatecolor :
      [ | `Translate | `Scale | `Rotate | `SkewX | `SkewY ] ->
        [ | `Type__animatecolor ] attrib
      
    val a_horiz_origin_x : number -> [ | `Horiz___origin___x ] attrib
      
    val a_horiz_origin_y : number -> [ | `Horiz___origin___y ] attrib
      
    val a_horiz_adv_x : number -> [ | `Horiz___adv___x ] attrib
      
    val a_vert_origin_x : number -> [ | `Vert___origin___x ] attrib
      
    val a_vert_origin_y : number -> [ | `Vert___origin___y ] attrib
      
    val a_vert_adv_y : number -> [ | `Vert___adv___y ] attrib
      
    val a_unicode : string -> [ | `Unicode ] attrib
      
    val a_glyphname : string -> [ | `glyphname ] attrib
      
    val a_orientation : [ | `H | `V ] -> [ | `Orientation ] attrib
      
    val a_arabicform :
      [ | `Initial | `Medial | `Terminal | `Isolated ] ->
        [ | `Arabic___form ] attrib
      
    val a_lang : string -> [ | `Lang ] attrib
      
    val a_u1 : string -> [ | `U1 ] attrib
      
    val a_u2 : string -> [ | `U2 ] attrib
      
    val a_g1 : string -> [ | `G1 ] attrib
      
    val a_g2 : string -> [ | `G2 ] attrib
      
    val a_k : string -> [ | `K ] attrib
      
    val a_fontfamily : string -> [ | `Font___Family ] attrib
      
    val a_fontstyle : string -> [ | `Font___Style ] attrib
      
    val a_fontvariant : string -> [ | `Font___Variant ] attrib
      
    val a_fontweight : string -> [ | `Font___Weight ] attrib
      
    val a_fontstretch : string -> [ | `Font___Stretch ] attrib
      
    val a_fontsize : string -> [ | `Font___Size ] attrib
      
    val a_unicoderange : string -> [ | `Unicode___Range ] attrib
      
    val a_unitsperem : string -> [ | `Units___Per___Em ] attrib
      
    val a_stemv : number -> [ | `Stemv ] attrib
      
    val a_stemh : number -> [ | `Stemh ] attrib
      
    val a_slope : number -> [ | `Slope ] attrib
      
    val a_capheight : number -> [ | `Cap___Height ] attrib
      
    val a_xheight : number -> [ | `X___Height ] attrib
      
    val a_accentheight : number -> [ | `Accent___Height ] attrib
      
    val a_ascent : number -> [ | `Ascent ] attrib
      
    val a_widths : string -> [ | `Widths ] attrib
      
    val a_bbox : string -> [ | `Bbox ] attrib
      
    val a_ideographic : number -> [ | `Ideographic ] attrib
      
    val a_alphabetic : number -> [ | `Alphabetic ] attrib
      
    val a_mathematical : number -> [ | `Mathematical ] attrib
      
    val a_hanging : number -> [ | `Hanging ] attrib
      
    val a_videographic : number -> [ | `V___Ideographic ] attrib
      
    val a_valphabetic : number -> [ | `V___Alphabetic ] attrib
      
    val a_vmathematical : number -> [ | `V___Mathematical ] attrib
      
    val a_vhanging : number -> [ | `V___Hanging ] attrib
      
    val a_underlineposition : number -> [ | `Underline___Position ] attrib
      
    val a_underlinethickness : number -> [ | `Underline___Thickness ] attrib
      
    val a_strikethroughposition :
      number -> [ | `Strikethrough___Position ] attrib
      
    val a_strikethroughthickness :
      number -> [ | `Strikethrough___Thickness ] attrib
      
    val a_overlineposition : number -> [ | `Overline___Position ] attrib
      
    val a_overlinethickness : number -> [ | `Overline___Thickness ] attrib
      
    val a_string : string -> [ | `String ] attrib
      
    val a_name : string -> [ | `Name ] attrib
      
    val a_onabort : string -> [ | `Onabort ] attrib
      
    val a_onactivate : string -> [ | `Onactivate ] attrib
      
    val a_onbegin : string -> [ | `Onbegin ] attrib
      
    val a_onclick : string -> [ | `Onclick ] attrib
      
    val a_onend : string -> [ | `Onend ] attrib
      
    val a_onerror : string -> [ | `Onerror ] attrib
      
    val a_onfocusin : string -> [ | `Onfocusin ] attrib
      
    val a_onfocusout : string -> [ | `Onfocusout ] attrib
      
    val a_onload : string -> [ | `Onload ] attrib
      
    val a_onmousedown : string -> [ | `Onmousdown ] attrib
      
    val a_onmouseup : string -> [ | `Onmouseup ] attrib
      
    val a_onmouseover : string -> [ | `Onmouseover ] attrib
      
    val a_onmouseout : string -> [ | `Onmouseout ] attrib
      
    val a_onmousemove : string -> [ | `Onmousemove ] attrib
      
    val a_onrepeat : string -> [ | `Onrepeat ] attrib
      
    val a_onresize : string -> [ | `Onresize ] attrib
      
    val a_onscroll : string -> [ | `Onscroll ] attrib
      
    val a_onunload : string -> [ | `Onunload ] attrib
      
    val a_onzoom : string -> [ | `Onzoom ] attrib
      
    val metadata :
      ?a: ((metadata_attr attrib) list) -> XML.elt list -> [> | metadata] elt
      
    val foreignobject :
      ?a: ((foreignobject_attr attrib) list) ->
        XML.elt list -> [> | foreignobject] elt
      
    (** {1 Elements} *)
    (* generated *)
    val svg : ([< | svg_attr], [< | svg_content], [> | svg]) star
      
    val g : ([< | g_attr], [< | g_content], [> | g]) star
      
    val defs : ([< | defs_attr], [< | defs_content], [> | defs]) star
      
    val desc : ([< | desc_attr], [< | desc_content], [> | desc]) unary
      
    val title : ([< | title_attr], [< | title_content], [> | title]) unary
      
    val symbol : ([< | symbol_attr], [< | symbol_content], [> | symbol]) star
      
    val use : ([< | use_attr], [< | use_content], [> | use]) star
      
    val image : ([< | image_attr], [< | image_content], [> | image]) star
      
    val switch : ([< | switch_attr], [< | switch_content], [> | switch]) star
      
    val style : ([< | style_attr], [< | style_content], [> | style]) unary
      
    val path : ([< | path_attr], [< | path_content], [> | path]) star
      
    val rect : ([< | rect_attr], [< | rect_content], [> | rect]) star
      
    val circle : ([< | circle_attr], [< | circle_content], [> | circle]) star
      
    val ellipse :
      ([< | ellipse_attr], [< | ellipse_content], [> | ellipse]) star
      
    val line : ([< | line_attr], [< | line_content], [> | line]) star
      
    val polyline :
      ([< | polyline_attr], [< | polyline_content], [> | polyline]) star
      
    val polygon :
      ([< | polygon_attr], [< | polygon_content], [> | polygon]) star
      
    val text : ([< | text_attr], [< | text_content], [> | text]) star
      
    val tspan : ([< | tspan_attr], [< | tspan_content], [> | tspan]) star
      
    val tref : ([< | tref_attr], [< | tref_content], [> | tref]) star
      
    val textpath :
      ([< | textpath_attr], [< | textpath_content], [> | textpath]) star
      
    val altglyph :
      ([< | altglyph_attr], [< | altglyph_content], [> | altglyph]) unary
      
    val altglyphdef :
      ([< | altglyphdef_attr], [< | altglyphdef_content], [> | altglyphdef])
        unary
      
    val altglyphitem :
      ([< | altglyphitem_attr], [< | altglyphitem_content], [> | altglyphitem
        ]) plus
      
    val glyphref : ([< | glyphref_attr], [> | glyphref]) nullary
      
    val marker : ([< | marker_attr], [< | marker_content], [> | marker]) star
      
    val colorprofile :
      ([< | colorprofile_attr], [< | colorprofile_content], [> | colorprofile
        ]) star
      
    val lineargradient :
      ([< | lineargradient_attr], [< | lineargradient_content],
        [> | lineargradient]) star
      
    val radialgradient :
      ([< | radialgradient_attr], [< | radialgradient_content],
        [> | radialgradient]) star
      
    val gradientstop :
      ([< | gradientstop_attr], [< | gradientstop_content], [> | gradientstop
        ]) star
      
    val pattern :
      ([< | pattern_attr], [< | pattern_content], [> | pattern]) star
      
    val clippath :
      ([< | clippath_attr], [< | clippath_content], [> | clippath]) star
      
    val filter : ([< | filter_attr], [< | filter_content], [> | filter]) star
      
    val fedistantlight :
      ([< | fedistantlight_attr], [< | fedistantlight_content],
        [> | fedistantlight]) star
      
    val fepointlight :
      ([< | fepointlight_attr], [< | fepointlight_content], [> | fepointlight
        ]) star
      
    val fespotlight :
      ([< | fespotlight_attr], [< | fespotlight_content], [> | fespotlight])
        star
      
    val feblend :
      ([< | feblend_attr], [< | feblend_content], [> | feblend]) star
      
    val fecolormatrix :
      ([< | fecolormatrix_attr], [< | fecolormatrix_content],
        [> | fecolormatrix]) star
      
    val fecomponenttransfer :
      ([< | fecomponenttransfer_attr], [< | fecomponenttransfer_content],
        [> | fecomponenttransfer]) star
      
    val fefunca :
      ([< | fefunca_attr], [< | fefunca_content], [> | fefunca]) star
      
    val fefuncg :
      ([< | fefuncg_attr], [< | fefuncg_content], [> | fefuncg]) star
      
    val fefuncb :
      ([< | fefuncb_attr], [< | fefuncb_content], [> | fefuncb]) star
      
    val fefuncr :
      ([< | fefuncr_attr], [< | fefuncr_content], [> | fefuncr]) star
      
    val fecomposite :
      ([< | fecomposite_attr], [< | fecomposite_content], [> | fecomposite])
        star
      
    val feconvolvematrix :
      ([< | feconvolvematrix_attr], [< | feconvolvematrix_content],
        [> | feconvolvematrix]) star
      
    val fediffuselighting :
      ([< | fediffuselighting_attr], [< | fediffuselighting_content],
        [> | fediffuselighting]) star
      
    val fedisplacementmap :
      ([< | fedisplacementmap_attr], [< | fedisplacementmap_content],
        [> | fedisplacementmap]) star
      
    val feflood :
      ([< | feflood_attr], [< | feflood_content], [> | feflood]) star
      
    val fegaussianblur :
      ([< | fegaussianblur_attr], [< | fegaussianblur_content],
        [> | fegaussianblur]) star
      
    val feimage :
      ([< | feimage_attr], [< | feimage_content], [> | feimage]) star
      
    val femerge :
      ([< | femerge_attr], [< | femerge_content], [> | femerge]) star
      
    val femorphology :
      ([< | femorphology_attr], [< | femorphology_content], [> | femorphology
        ]) star
      
    val feoffset :
      ([< | feoffset_attr], [< | feoffset_content], [> | feoffset]) star
      
    val fespecularlighting :
      ([< | fespecularlighting_attr], [< | fespecularlighting_content],
        [> | fespecularlighting]) star
      
    val fetile : ([< | fetile_attr], [< | fetile_content], [> | fetile]) star
      
    val feturbulence :
      ([< | feturbulence_attr], [< | feturbulence_content], [> | feturbulence
        ]) star
      
    val cursor : ([< | cursor_attr], [< | cursor_content], [> | cursor]) star
      
    val a : ([< | a_attr], [< | a_content], [> | a]) star
      
    val view : ([< | view_attr], [< | view_content], [> | view]) star
      
    val script :
      ([< | script_attr], [< | script_content], [> | script]) unary
      
    val animation :
      ([< | animation_attr], [< | animation_content], [> | animation]) star
      
    val set : ([< | set_attr], [< | set_content], [> | set]) star
      
    val animatemotion :
      ([< | animatemotion_attr], [< | animatemotion_content],
        [> | animatemotion]) star
      
    val mpath : ([< | mpath_attr], [< | mpath_content], [> | mpath]) star
      
    val animatecolor :
      ([< | animatecolor_attr], [< | animatecolor_content], [> | animatecolor
        ]) star
      
    val animatetransform :
      ([< | animatetransform_attr], [< | animatetransform_content],
        [> | animatetransform]) star
      
    val font : ([< | font_attr], [< | font_content], [> | font]) star
      
    val glyph : ([< | glyph_attr], [< | glyph_content], [> | glyph]) star
      
    val missingglyph :
      ([< | missingglyph_attr], [< | missingglyph_content], [> | missingglyph
        ]) star
      
    val hkern : ([< | hkern_attr], [> | hkern]) nullary
      
    val vkern : ([< | vkern_attr], [> | vkern]) nullary
      
    val fontface : ([< | fontface_attr], [> | fontface]) nullary
      
    val fontfacesrc :
      ([< | fontfacesrc_attr], [< | fontfacesrc_content], [> | fontfacesrc])
        star
      
    val fontfaceuri :
      ([< | fontfaceuri_attr], [< | fontfaceuri_content], [> | fontfaceuri])
        star
      
    val fontfaceformat :
      ([< | fontfaceformat_attr], [> | fontfaceformat]) nullary
      
    val fontfacename : ([< | fontfacename_attr], [> | fontfacename]) nullary
      
    val tot : XML.elt -> 'a elt
      
    val totl : XML.elt list -> ('a elt) list
      
    val toelt : 'a elt -> XML.elt
      
    val toeltl : ('a elt) list -> XML.elt list
    
    val to_xmlattribs : ('a attrib) list -> XML.attrib list
  end
