(* sVG.ml module
   based on http://www.w3.org/TR/SVG *)
(* BEGIN INTERFACE *)

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
(* END INTERFACE *)
  = struct
    include Uri
    open Svgtypes
    open Unit
      
    type 'a attrib = XML.attrib
    
    type 'a attribs = XML.attribs
    
    type +'a elt = XML.elt
    
    type +'a elts = XML.elt list
    
    type ('a, 'b) nullary = ?a: (('a attrib) list) -> unit -> 'b elt
    
    type ('a, 'b, 'c) unary = ?a: (('a attrib) list) -> 'b elt -> 'c elt
    
    type ('a, 'b, 'c) star =
      ?a: (('a attrib) list) -> ('b elt) list -> 'c elt
    
    type ('a, 'b, 'c) plus =
      ?a: (('a attrib) list) -> 'b elt -> ('b elt) list -> 'c elt
    
    let tot x = x
      
    let totl x = x
      
    let toelt x = x
      
    let toeltl x = x

    let to_xmlattribs x = x
    let nullary tag ?a () = XML.node ?a tag []
      
    let unary tag ?a elt = XML.node ?a tag [ elt ]
      
    let star tag ?a elts = XML.node ?a tag elts
      
    let plus tag ?a elt elts = XML.node ?a tag (elt :: elts)
      
    type altglyphdef_content =
      [ | `Ref of (glyphref elt) list | `Item of (altglyphitem elt) list
      ]
    
    let string_of_string s = s
      
    let to_xmlattribs x = x
      
    let float_attrib = XML.float_attrib
      
    let int_attrib = XML.int_attrib
      
    let string_attrib = XML.string_attrib
      
    let uri_attrib a s = XML.string_attrib a (string_of_uri s)
      
    let user_attrib f name v = XML.string_attrib name (f v)
      
    let metadata ?a children = XML.node ?a "metadata" children
      
    let foreignobject ?a children = XML.node ?a "foreignObject" children
      
    (* generated *)
    let a_version = user_attrib string_of_string "version"
      
    let a_baseprofile = user_attrib string_of_string "baseProfile"
      
    let a_x = user_attrib string_of_coord "x"
      
    let a_y = user_attrib string_of_coord "y"
      
    let a_width = user_attrib string_of_length "width"
      
    let a_height = user_attrib string_of_length "height"
      
    let a_preserveaspectratio =
      user_attrib string_of_string "preserveAspectRatio"
      
    let a_contentscripttype =
      user_attrib string_of_string "contentScriptType"
      
    let a_contentstyletype = user_attrib string_of_string "contentStyleType"
      
    let a_zoomAndPan =
      user_attrib (function | `Disable -> "disable" | `Magnify -> "magnify")
        "zoomAndSpan"
      
    let a_xlink_href = user_attrib string_of_iri "xlink:href"
      
    let a_requiredfeatures =
      user_attrib string_of_spacestrings "requiredFeatures"
      
    let a_requiredextensions =
      user_attrib string_of_spacestrings "requiredExtension"
      
    let a_systemlanguage =
      user_attrib string_of_commastrings "systemLanguage"
      
    let a_externalressourcesrequired =
      user_attrib string_of_bool "externalRessourcesRequired"
      
    let a_id = user_attrib string_of_string "id"
      
    let a_xml_base = user_attrib string_of_iri "xml:base"
      
    let a_xml_lang = user_attrib string_of_iri "xml:lang"
      
    let a_xml_space =
      user_attrib
        (function | `Default -> "default" | `Preserve -> "preserve")
        "xml:space"
      
    let a_type = user_attrib string_of_string "type"
      
    let a_media = user_attrib string_of_commastrings "media"
      
    let a_title = user_attrib string_of_string "title"
      
    let a_class = user_attrib string_of_spacestrings "class"
      
    let a_style = user_attrib string_of_string "style"
      
    let a_transform = user_attrib string_of_transform "transform"
      
    let a_viewbox = user_attrib string_of_fourfloats "viewbox"
      
    let a_d = user_attrib string_of_string "d"
      
    let a_pathlength = user_attrib string_of_float "pathLength"
      
    let a_rx = user_attrib string_of_length "rx"
      
    let a_ry = user_attrib string_of_length "ry"
      
    let a_cx = user_attrib string_of_length "cx"
      
    let a_cy = user_attrib string_of_length "cy"
      
    let a_r = user_attrib string_of_length "r"
      
    let a_x1 = user_attrib string_of_coord "x1"
      
    let a_y1 = user_attrib string_of_coord "y1"
      
    let a_x2 = user_attrib string_of_coord "x2"
      
    let a_y2 = user_attrib string_of_coord "y2"
      
    let a_points = user_attrib string_of_coords "points"
      
    let a_x_list = user_attrib string_of_lengths "x"
      
    let a_y_list = user_attrib string_of_lengths "y"
      
    let a_dx = user_attrib string_of_lengths "dx"
      
    let a_dy = user_attrib string_of_lengths "dy"
      
    let a_dx_single = user_attrib string_of_length "dx"
      
    let a_dy_single = user_attrib string_of_length "dy"
      
    let a_dx_number = user_attrib string_of_number "dx"
      
    let a_dy_number = user_attrib string_of_number "dy"
      
    let a_lengthadjust =
      user_attrib
        (function
         | `Spacing -> "spacing"
         | `SpacingAndGlyphs -> "spacingAndGlyphs")
        "lengthAdjust"
      
    let a_textlength = user_attrib string_of_length "textLength"
      
    let a_rotate = user_attrib string_of_numbers "rotate"
      
    let a_startoffset = user_attrib string_of_length "startOffset"
      
    let a_method =
      user_attrib (function | `Align -> "align" | `Stretch -> "stretch")
        "method"
      
    let a_spacing =
      user_attrib (function | `Auto -> "auto" | `Exact -> "exact") "spacing"
      
    let a_glyphref = user_attrib string_of_string "glyphRef"
      
    let a_format = user_attrib string_of_string "format"
      
    let a_markerunits =
      user_attrib
        (function
         | `StrokeWidth -> "strokeWidth"
         | `UserSpaceOnUse -> "userSpaceOnUse")
        "markerUnits"
      
    let a_refx = user_attrib string_of_coord "refX"
      
    let a_refy = user_attrib string_of_coord "refY"
      
    let a_markerwidth = user_attrib string_of_length "markerWidth"
      
    let a_markerheight = user_attrib string_of_length "markerHeight"
      
    let a_orient =
      user_attrib
        (function | `Auto -> "auto" | `Angle __svg -> string_of_angle __svg)
        "orient"
      
    let a_local = user_attrib string_of_string "local"
      
    let a_string = user_attrib string_of_string "name"
      
    let a_renderingindent =
      user_attrib
        (function
         | `Auto -> "auto"
         | `Perceptual -> "perceptual"
         | `Relative_colorimetric -> "relative_colorimetric"
         | `Saturation -> "saturation"
         | `Absolute_colorimetric -> "absolute_colorimetric")
        "rendering:indent"
      
    let a_gradientunits =
      user_attrib
        (function
         | `UserSpaceOnUse -> "userSpaceOnUse"
         | `ObjectBoundingBox -> "objectBoundingBox")
        "gradientUnits"
      
    let a_gradienttransform =
      user_attrib string_of_transforms "gradient:transform"
      
    let a_spreadmethod =
      user_attrib
        (function
         | `Pad -> "pad"
         | `Reflect -> "reflect"
         | `Repeat -> "repeat")
        "spreadMethod"
      
    let a_fx = user_attrib string_of_coord "fx"
      
    let a_fy = user_attrib string_of_coord "fy"
      
    let a_offset =
      user_attrib
        (function
         | `Number __svg -> string_of_number __svg
         | `Percentage __svg -> string_of_percentage __svg)
        "offset"
      
    let a_patternunits =
      user_attrib
        (function
         | `UserSpaceOnUse -> "userSpaceOnUse"
         | `ObjectBoundingBox -> "objectBoundingBox")
        "patternUnits"
      
    let a_patterncontentunits =
      user_attrib
        (function
         | `UserSpaceOnUse -> "userSpaceOnUse"
         | `ObjectBoundingBox -> "objectBoundingBox")
        "patternContentUnits"
      
    let a_patterntransform =
      user_attrib string_of_transforms "patternTransform"
      
    let a_clippathunits =
      user_attrib
        (function
         | `UserSpaceOnUse -> "userSpaceOnUse"
         | `ObjectBoundingBox -> "objectBoundingBox")
        "clipPathUnits"
      
    let a_maskunits =
      user_attrib
        (function
         | `UserSpaceOnUse -> "userSpaceOnUse"
         | `ObjectBoundingBox -> "objectBoundingBox")
        "maskUnits"
      
    let a_maskcontentunits =
      user_attrib
        (function
         | `UserSpaceOnUse -> "userSpaceOnUse"
         | `ObjectBoundingBox -> "objectBoundingBox")
        "maskContentUnits"
      
    let a_primitiveunits =
      user_attrib
        (function
         | `UserSpaceOnUse -> "userSpaceOnUse"
         | `ObjectBoundingBox -> "objectBoundingBox")
        "primitiveUnits"
      
    let a_filterres =
      user_attrib string_of_number_optional_number "filterResUnits"
      
    let a_result = user_attrib string_of_string "result"
      
    let a_in =
      user_attrib
        (function
         | `SourceGraphic -> "sourceGraphic"
         | `SourceAlpha -> "sourceAlpha"
         | `BackgroundImage -> "backgroundImage"
         | `BackgroundAlpha -> "backgroundAlpha"
         | `FillPaint -> "fillPaint"
         | `StrokePaint -> "strokePaint"
         | `Ref __svg -> string_of_string __svg)
        "in"
      
    let a_in2 =
      user_attrib
        (function
         | `SourceGraphic -> "sourceGraphic"
         | `SourceAlpha -> "sourceAlpha"
         | `BackgroundImage -> "backgroundImage"
         | `BackgroundAlpha -> "backgroundAlpha"
         | `FillPaint -> "fillPaint"
         | `StrokePaint -> "strokePaint"
         | `Ref __svg -> string_of_string __svg)
        "in2"
      
    let a_aizmuth = user_attrib string_of_float "azimuth"
      
    let a_elevation = user_attrib string_of_float "elevation"
      
    let a_pointatx = user_attrib string_of_float "pointsAtX"
      
    let a_pointaty = user_attrib string_of_float "pointsAtY"
      
    let a_pointatz = user_attrib string_of_float "pointsAtZ"
      
    let a_specularexponent = user_attrib string_of_float "specularExponent"
      
    let a_specularconstant = user_attrib string_of_float "specularConstant"
      
    let a_limitingconeangle = user_attrib string_of_float "limitingConeAngle"
      
    let a_mode =
      user_attrib
        (function
         | `Normal -> "normal"
         | `Multiply -> "multiply"
         | `Screen -> "screen"
         | `Darken -> "darken"
         | `Lighten -> "lighten")
        "mode"
      
    let a_typefecolor =
      user_attrib
        (function
         | `Matrix -> "matrix"
         | `Saturate -> "saturate"
         | `HueRotate -> "hueRotate"
         | `LuminanceToAlpha -> "luminanceToAlpha")
        "type"
      
    let a_values = user_attrib string_of_numbers "values"
      
    let a_transferttype =
      user_attrib
        (function
         | `Identity -> "identity"
         | `Table -> "table"
         | `Discrete -> "discrete"
         | `Linear -> "linear"
         | `Gamma -> "gamma")
        "type"
      
    let a_tablevalues = user_attrib string_of_numbers "tableValues"
      
    let a_slope = user_attrib string_of_number "slope"
      
    let a_intercept = user_attrib string_of_number "intercept"
      
    let a_amplitude = user_attrib string_of_number "amplitude"
      
    let a_exponent = user_attrib string_of_number "exponent"
      
    let a_offsettransfer = user_attrib string_of_number "offset"
      
    let a_operator =
      user_attrib
        (function
         | `Over -> "over"
         | `In -> "in"
         | `Out -> "out"
         | `Atop -> "atop"
         | `Xor -> "xor"
         | `Arithmetic -> "arithmetic")
        "operator"
      
    let a_k1 = user_attrib string_of_number "k1"
      
    let a_k2 = user_attrib string_of_number "k2"
      
    let a_k3 = user_attrib string_of_number "k3"
      
    let a_k4 = user_attrib string_of_number "k4"
      
    let a_order = user_attrib string_of_number_optional_number "order"
      
    let a_kernelmatrix = user_attrib string_of_numbers "kernelMatrix"
      
    let a_divisor = user_attrib string_of_number "divisor"
      
    let a_bias = user_attrib string_of_number "bias"
      
    let a_kernelunitlength =
      user_attrib string_of_number_optional_number "kernelUnitLength"
      
    let a_targetX = user_attrib string_of_int "targetX"
      
    let a_targetY = user_attrib string_of_int "targetY"
      
    let a_edgemode =
      user_attrib
        (function
         | `Duplicate -> "duplicate"
         | `Wrap -> "wrap"
         | `None -> "none")
        "targetY"
      
    let a_preservealpha = user_attrib string_of_bool "targetY"
      
    let a_surfacescale = user_attrib string_of_number "surfaceScale"
      
    let a_diffuseconstant = user_attrib string_of_number "diffuseConstant"
      
    let a_scale = user_attrib string_of_number "scale"
      
    let a_xchannelselector =
      user_attrib (function | `R -> "r" | `G -> "g" | `B -> "b" | `A -> "a")
        "xChannelSelector"
      
    let a_ychannelselector =
      user_attrib (function | `R -> "r" | `G -> "g" | `B -> "b" | `A -> "a")
        "yChannelSelector"
      
    let a_stddeviation =
      user_attrib string_of_number_optional_number "stdDeviation"
      
    let a_operatormorphology =
      user_attrib (function | `Erode -> "erode" | `Dilate -> "dilate")
        "operatorMorphology"
      
    let a_radius = user_attrib string_of_number_optional_number "radius"
      
    let a_basefrenquency =
      user_attrib string_of_number_optional_number "baseFrequency"
      
    let a_numoctaves = user_attrib string_of_int "numOctaves"
      
    let a_seed = user_attrib string_of_number "seed"
      
    let a_stitchtiles =
      user_attrib (function | `Stitch -> "stitch" | `NoStitch -> "noStitch")
        "stitchTiles"
      
    let a_stitchtype =
      user_attrib
        (function
         | `FractalNoise -> "fractalNoise"
         | `Turbulence -> "turbulence")
        "typeStitch"
      
    let a_xlinkshow =
      user_attrib (function | `New -> "new" | `Replace -> "replace")
        "xlink:show"
      
    let a_xlinkactuate =
      user_attrib (function | `OnRequest -> "onRequest") "xlink:actuate"
      
    let a_target = user_attrib string_of_string "xlink:target"
      
    let a_viewtarget = user_attrib string_of_string "viewTarget"
      
    let a_attributename = user_attrib string_of_string "attributeName"
      
    let a_attributetype =
      user_attrib
        (function | `CSS -> "cSS" | `XML -> "xML" | `Auto -> "auto")
        "attributeType"
      
    let a_begin = user_attrib string_of_string "begin"
      
    let a_dur = user_attrib string_of_string "dur"
      
    let a_min = user_attrib string_of_string "min"
      
    let a_max = user_attrib string_of_string "max"
      
    let a_restart =
      user_attrib
        (function
         | `Always -> "always"
         | `WhenNotActive -> "whenNotActive"
         | `Never -> "never")
        "restart"
      
    let a_repeatcount = user_attrib string_of_string "repeatCount"
      
    let a_repeatdur = user_attrib string_of_string "repeatDur"
      
    let a_fill =
      user_attrib (function | `Freeze -> "freeze" | `Remove -> "remove")
        "fill"
      
    let a_calcmode =
      user_attrib
        (function
         | `Discrete -> "discrete"
         | `Linear -> "linear"
         | `Paced -> "paced"
         | `Spline -> "spline")
        "calcMode"
      
    let a_values_anim = user_attrib string_of_strings "values"
      
    let a_keytimes = user_attrib string_of_strings "keyTimes"
      
    let a_keysplines = user_attrib string_of_strings "keySplines"
      
    let a_from = user_attrib string_of_string "from"
      
    let a_to = user_attrib string_of_string "to"
      
    let a_by = user_attrib string_of_string "by"
      
    let a_additive =
      user_attrib (function | `Replace -> "replace" | `Sum -> "sum")
        "additive"
      
    let a_accumulate =
      user_attrib (function | `None -> "none" | `Sum -> "sum") "accumulate"
      
    let a_keypoints = user_attrib string_of_numbers_semicolon "keyPoints"
      
    let a_path = user_attrib string_of_string "path"
      
    let a_typeanimatecolor =
      user_attrib
        (function
         | `Translate -> "translate"
         | `Scale -> "scale"
         | `Rotate -> "rotate"
         | `SkewX -> "skewX"
         | `SkewY -> "skewY")
        "type"
      
    let a_horiz_origin_x = user_attrib string_of_number "horiz-origin-x"
      
    let a_horiz_origin_y = user_attrib string_of_number "horiz-origin-y"
      
    let a_horiz_adv_x = user_attrib string_of_number "horiz-adv-x"
      
    let a_vert_origin_x = user_attrib string_of_number "vert-origin-x"
      
    let a_vert_origin_y = user_attrib string_of_number "vert-origin-y"
      
    let a_vert_adv_y = user_attrib string_of_number "vert-adv-y"
      
    let a_unicode = user_attrib string_of_string "unicode"
      
    let a_glyphname = user_attrib string_of_string "glyphname"
      
    let a_orientation =
      user_attrib (function | `H -> "h" | `V -> "v") "orientation"
      
    let a_arabicform =
      user_attrib
        (function
         | `Initial -> "initial"
         | `Medial -> "medial"
         | `Terminal -> "terminal"
         | `Isolated -> "isolated")
        "arabic-form"
      
    let a_lang = user_attrib string_of_string "lang"
      
    let a_u1 = user_attrib string_of_string "u1"
      
    let a_u2 = user_attrib string_of_string "u2"
      
    let a_g1 = user_attrib string_of_string "g1"
      
    let a_g2 = user_attrib string_of_string "g2"
      
    let a_k = user_attrib string_of_string "k"
      
    let a_fontfamily = user_attrib string_of_string "font-family"
      
    let a_fontstyle = user_attrib string_of_string "font-style"
      
    let a_fontvariant = user_attrib string_of_string "font-variant"
      
    let a_fontweight = user_attrib string_of_string "font-weight"
      
    let a_fontstretch = user_attrib string_of_string "font-stretch"
      
    let a_fontsize = user_attrib string_of_string "font-size"
      
    let a_unicoderange = user_attrib string_of_string "unicode-range"
      
    let a_unitsperem = user_attrib string_of_string "units-per-em"
      
    let a_stemv = user_attrib string_of_number "stemv"
      
    let a_stemh = user_attrib string_of_number "stemh"
      
    let a_slope = user_attrib string_of_number "slope"
      
    let a_capheight = user_attrib string_of_number "cap-height"
      
    let a_xheight = user_attrib string_of_number "x-height"
      
    let a_accentheight = user_attrib string_of_number "accent-height"
      
    let a_ascent = user_attrib string_of_number "ascent"
      
    let a_widths = user_attrib string_of_string "widths"
      
    let a_bbox = user_attrib string_of_string "bbox"
      
    let a_ideographic = user_attrib string_of_number "ideographic"
      
    let a_alphabetic = user_attrib string_of_number "alphabetic"
      
    let a_mathematical = user_attrib string_of_number "mathematical"
      
    let a_hanging = user_attrib string_of_number "hanging"
      
    let a_videographic = user_attrib string_of_number "v-ideographic"
      
    let a_valphabetic = user_attrib string_of_number "v-alphabetic"
      
    let a_vmathematical = user_attrib string_of_number "v-mathematical"
      
    let a_vhanging = user_attrib string_of_number "v-hanging"
      
    let a_underlineposition =
      user_attrib string_of_number "underline-position"
      
    let a_underlinethickness =
      user_attrib string_of_number "underline-thickness"
      
    let a_strikethroughposition =
      user_attrib string_of_number "strikethrough-position"
      
    let a_strikethroughthickness =
      user_attrib string_of_number "strikethrough-thickness"
      
    let a_overlineposition = user_attrib string_of_number "overline-position"
      
    let a_overlinethickness =
      user_attrib string_of_number "overline-thickness"
      
    let a_string = user_attrib string_of_string "string"
      
    let a_name = user_attrib string_of_string "name"
      
    let a_onabort = user_attrib string_of_string "onabort"
      
    let a_onactivate = user_attrib string_of_string "onactivate"
      
    let a_onbegin = user_attrib string_of_string "onbegin"
      
    let a_onclick = user_attrib string_of_string "onclick"
      
    let a_onend = user_attrib string_of_string "onend"
      
    let a_onerror = user_attrib string_of_string "onerror"
      
    let a_onfocusin = user_attrib string_of_string "onfocusin"
      
    let a_onfocusout = user_attrib string_of_string "onfocusout"
      
    let a_onload = user_attrib string_of_string "onload"
      
    let a_onmousedown = user_attrib string_of_string "onmousdown"
      
    let a_onmouseup = user_attrib string_of_string "onmouseup"
      
    let a_onmouseover = user_attrib string_of_string "onmouseover"
      
    let a_onmouseout = user_attrib string_of_string "onmouseout"
      
    let a_onmousemove = user_attrib string_of_string "onmousemove"
      
    let a_onrepeat = user_attrib string_of_string "onrepeat"
      
    let a_onresize = user_attrib string_of_string "onresize"
      
    let a_onscroll = user_attrib string_of_string "onscroll"
      
    let a_onunload = user_attrib string_of_string "onunload"
      
    let a_onzoom = user_attrib string_of_string "onzoom"
      
    (* also generated *)
    let svg = star "svg"
      
    let g = star "g"
      
    let defs = star "defs"
      
    let desc = unary "desc"
      
    let title = unary "title"
      
    let symbol = star "symbol"
      
    let use = star "use"
      
    let image = star "image"
      
    let switch = star "switch"
      
    let style = unary "style"
      
    let path = star "path"
      
    let rect = star "rect"
      
    let circle = star "circle"
      
    let ellipse = star "ellipse"
      
    let line = star "line"
      
    let polyline = star "polyline"
      
    let polygon = star "polygon"
      
    let text = star "text"
      
    let tspan = star "tspan"
      
    let tref = star "tref"
      
    let textpath = star "textPath"
      
    let altglyph = unary "altGlyph"
      
    let altglyphdef = unary "altGlyphDef"
      
    let altglyphitem = plus "altGlyphItem"
      
    let glyphref = nullary "glyphRef];"
      
    let marker = star "marker"
      
    let colorprofile = star "colorProfile"
      
    let lineargradient = star "linear-gradient"
      
    let radialgradient = star "radial-gradient"
      
    let gradientstop = star "gradient-stop"
      
    let pattern = star "pattern"
      
    let clippath = star "clipPath"
      
    let filter = star "filter"
      
    let fedistantlight = star "feDistantLight"
      
    let fepointlight = star "fePointLight"
      
    let fespotlight = star "feSpotLight"
      
    let feblend = star "feBlend"
      
    let fecolormatrix = star "feColorMatrix"
      
    let fecomponenttransfer = star "feComponentTransfer"
      
    let fefunca = star "feFuncA"
      
    let fefuncg = star "feFuncA"
      
    let fefuncb = star "feFuncA"
      
    let fefuncr = star "feFuncA"
      
    let fecomposite = star "(*"
      
    let feconvolvematrix = star "feConvolveMatrix"
      
    let fediffuselighting = star "(*"
      
    let fedisplacementmap = star "feDisplacementMap];"
      
    let feflood = star "(*"
      
    let fegaussianblur = star "];"
      
    let feimage = star "(*"
      
    let femerge = star "feMerge"
      
    let femorphology = star "feMorphology"
      
    let feoffset = star "feOffset"
      
    let fespecularlighting = star "feSpecularLighting"
      
    let fetile = star "feTile"
      
    let feturbulence = star "feTurbulence"
      
    let cursor = star "(*"
      
    let a = star "a"
      
    let view = star "view"
      
    let script = unary "script"
      
    let animation = star "(*"
      
    let set = star "set"
      
    let animatemotion = star "animateMotion"
      
    let mpath = star "mpath"
      
    let animatecolor = star "animateColor"
      
    let animatetransform = star "animateTransform"
      
    let font = star "font"
      
    let glyph = star "glyph"
      
    let missingglyph = star "missingGlyph"
      
    let hkern = nullary "hkern"
      
    let vkern = nullary "vkern"
      
    let fontface = nullary "fontFace"
      
    let fontfacesrc = star "font-face-src"
      
    let fontfaceuri = star "font-face-uri"
      
    let fontfaceformat = nullary "font-face-uri"
      
    let fontfacename = nullary "font-face-name"
      
  end
  

