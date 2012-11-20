(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2012 Vincent Balat, Benedikt Becker
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
open Eliom_content_core

module Xml = Xml

module Svg = struct

  module F = Svg.F
  module D = Svg.D
  module Id = Svg.Id

  type 'a elt = 'a F.elt
  type 'a attrib = 'a F.attrib
  type uri = F.uri

end

module Html5 = struct

  module F = struct
    include Html5.F
    include Eliom_registration_base.Html5_forms.F
    let raw_form = form
    let form = get_form
    let input = string_input
    let select = string_select
  end

  module D = struct
    include Html5.D
    include Eliom_registration_base.Html5_forms.D
    let raw_form = form
    let form = get_form
    let input = string_input
    let select = string_select
  end

  type 'a elt = 'a F.elt
  type 'a attrib = 'a F.attrib
  type uri = F.uri

  module Custom_data = Eliom_content_core.Html5.Custom_data

  module Of_dom = Eliom_content_core.Html5.Of_dom

  module To_dom = struct

    open Eliom_client

    let of_element elt = rebuild_node "of_element" elt
    let of_node elt = rebuild_node "of_node" elt

    let of_heading elt = rebuild_node "of_heading" elt

    let of_a elt = rebuild_node "of_a" elt
    let of_abbr elt = rebuild_node "of_abbr" elt
    let of_acronym elt = rebuild_node "of_acronym" elt
    let of_address elt = rebuild_node "of_address" elt
    let of_applet elt = rebuild_node "of_applet" elt
    let of_area elt = rebuild_node "of_area" elt
    let of_article elt = rebuild_node "of_article" elt
    let of_aside elt = rebuild_node "of_aside" elt
    let of_audio elt = rebuild_node "of_audio" elt
    let of_b elt = rebuild_node "of_b" elt
    let of_base elt = rebuild_node "of_base" elt
    let of_basefont elt = rebuild_node "of_basefont" elt
    let of_bdi elt = rebuild_node "of_bdi" elt
    let of_bdo elt = rebuild_node "of_bdo" elt
    let of_big elt = rebuild_node "of_big" elt
    let of_blockquote elt = rebuild_node "of_blockquote" elt
    let of_body elt = rebuild_node "of_body" elt
    let of_br elt = rebuild_node "of_br" elt
    let of_button elt = rebuild_node "of_button" elt
    let of_canvas elt = rebuild_node "of_canvas" elt
    let of_caption elt = rebuild_node "of_caption" elt
    let of_center elt = rebuild_node "of_center" elt
    let of_cite elt = rebuild_node "of_cite" elt
    let of_code elt = rebuild_node "of_code" elt
    let of_col elt = rebuild_node "of_col" elt
    let of_colgroup elt = rebuild_node "of_colgroup" elt
    let of_command elt = rebuild_node "of_command" elt
    let of_datalist elt = rebuild_node "of_datalist" elt
    let of_dd elt = rebuild_node "of_dd" elt
    let of_del elt = rebuild_node "of_del" elt
    let of_details elt = rebuild_node "of_details" elt
    let of_dfn elt = rebuild_node "of_dfn" elt
    let of_dir elt = rebuild_node "of_dir" elt
    let of_div elt = rebuild_node "of_div" elt
    let of_dl elt = rebuild_node "of_dl" elt
    let of_dt elt = rebuild_node "of_dt" elt
    let of_em elt = rebuild_node "of_em" elt
    let of_embed elt = rebuild_node "of_embed" elt
    let of_fieldset elt = rebuild_node "of_fieldset" elt
    let of_figcaption elt = rebuild_node "of_figcaption" elt
    let of_figure elt = rebuild_node "of_figure" elt
    let of_font elt = rebuild_node "of_font" elt
    let of_footer elt = rebuild_node "of_footer" elt
    let of_form elt = rebuild_node "of_form" elt
    let of_frame elt = rebuild_node "of_frame" elt
    let of_frameset elt = rebuild_node "of_frameset" elt
    let of_h1 elt = rebuild_node "of_h1" elt
    let of_h2 elt = rebuild_node "of_h2" elt
    let of_h3 elt = rebuild_node "of_h3" elt
    let of_h4 elt = rebuild_node "of_h4" elt
    let of_h5 elt = rebuild_node "of_h5" elt
    let of_h6 elt = rebuild_node "of_h6" elt
    let of_head elt = rebuild_node "of_head" elt
    let of_header elt = rebuild_node "of_header" elt
    let of_hgroup elt = rebuild_node "of_hgroup" elt
    let of_hr elt = rebuild_node "of_hr" elt
    let of_html elt = rebuild_node "of_html" elt
    let of_i elt = rebuild_node "of_i" elt
    let of_iframe elt = rebuild_node "of_iframe" elt
    let of_img elt = rebuild_node "of_img" elt
    let of_input elt = rebuild_node "of_input" elt
    let of_ins elt = rebuild_node "of_ins" elt
    let of_keygen elt = rebuild_node "of_keygen" elt
    let of_kbd elt = rebuild_node "of_kbd" elt
    let of_label elt = rebuild_node "of_label" elt
    let of_legend elt = rebuild_node "of_legend" elt
    let of_li elt = rebuild_node "of_li" elt
    let of_link elt = rebuild_node "of_link" elt
    let of_map elt = rebuild_node "of_map" elt
    let of_mark elt = rebuild_node "of_mark" elt
    let of_menu elt = rebuild_node "of_menu" elt
    let of_meta elt = rebuild_node "of_meta" elt
    let of_meter elt = rebuild_node "of_meter" elt
    let of_nav elt = rebuild_node "of_nav" elt
    let of_noframes elt = rebuild_node "of_noframes" elt
    let of_noscript elt = rebuild_node "of_noscript" elt
    let of_object elt = rebuild_node "of_object" elt
    let of_ol elt = rebuild_node "of_ol" elt
    let of_optgroup elt = rebuild_node "of_optgroup" elt
    let of_option elt = rebuild_node "of_option" elt
    let of_output elt = rebuild_node "of_output" elt
    let of_p elt = rebuild_node "of_p" elt
    let of_param elt = rebuild_node "of_param" elt
    let of_pre elt = rebuild_node "of_pre" elt
    let of_progress elt = rebuild_node "of_progress" elt
    let of_q elt = rebuild_node "of_q" elt
    let of_rp elt = rebuild_node "of_rp" elt
    let of_rt elt = rebuild_node "of_rt" elt
    let of_ruby elt = rebuild_node "of_ruby" elt
    let of_s elt = rebuild_node "of_s" elt
    let of_samp elt = rebuild_node "of_samp" elt
    let of_script elt = rebuild_node "of_script" elt
    let of_section elt = rebuild_node "of_section" elt
    let of_select elt = rebuild_node "of_select" elt
    let of_small elt = rebuild_node "of_small" elt
    let of_source elt = rebuild_node "of_source" elt
    let of_span elt = rebuild_node "of_span" elt
    let of_strike elt = rebuild_node "of_strike" elt
    let of_strong elt = rebuild_node "of_strong" elt
    let of_style elt = rebuild_node "of_style" elt
    let of_sub elt = rebuild_node "of_sub" elt
    let of_summary elt = rebuild_node "of_summary" elt
    let of_sup elt = rebuild_node "of_sup" elt
    let of_table elt = rebuild_node "of_table" elt
    let of_tbody elt = rebuild_node "of_tbody" elt
    let of_td elt = rebuild_node "of_td" elt
    let of_textarea elt = rebuild_node "of_textarea" elt
    let of_tfoot elt = rebuild_node "of_tfoot" elt
    let of_th elt = rebuild_node "of_th" elt
    let of_thead elt = rebuild_node "of_thead" elt
    let of_time elt = rebuild_node "of_time" elt
    let of_title elt = rebuild_node "of_title" elt
    let of_tr elt = rebuild_node "of_tr" elt
    let of_track elt = rebuild_node "of_track" elt
    let of_tt elt = rebuild_node "of_tt" elt
    let of_u elt = rebuild_node "of_u" elt
    let of_ul elt = rebuild_node "of_ul" elt
    let of_var elt = rebuild_node "of_var" elt
    let of_video elt = rebuild_node "of_video" elt
    let of_wbr elt = rebuild_node "of_wbr" elt

    let of_pcdata elt = rebuild_node "of_pcdata" elt
  end

  module Id = struct

    include Html5.Id

    let get_element' id =
      let id = string_of_id id in
      let node = Eliom_client.getElementById id in
      Js.Opt.case
        (Dom_html.CoerceTo.element node)
        (fun () -> failwith (Printf.sprintf "Non element node (%s)" id))
        (fun x -> x)

    let get_element id =
      Of_dom.of_element (get_element' id)
  end

  module Manip = struct
    let get_node elt = (To_dom.of_element elt :> Dom.node Js.t)
    let get_unique_node context (elt: 'a Html5.elt) : Dom.node Js.t =
      match Xml.get_node (Html5.D.toelt elt) with
      | Xml.DomNode node -> node
      | Xml.TyXMLNode desc ->
        let elt' = Html5.D.toelt elt in
          match Xml.get_node_id elt' with
          | Xml.NoId ->
            Eliom_lib.error_any (Eliom_client.rebuild_node' elt')
              "Cannot call %s on an element with functional semantics"
              context
          | _ -> get_node elt

    let get_unique_elt name elt : Dom_html.element Js.t =
      Js.Opt.case
        (Dom_html.CoerceTo.element (get_unique_node name elt))
        (fun () ->
          Eliom_lib.error_any (Eliom_client.rebuild_node' (Html5.F.toelt elt))
            "Cannot call %s on a node which is not an element"
            name)
        id

    let raw_appendChild ?before node elt2 =
      match before with
      | None -> ignore(node##appendChild(get_node elt2))
      | Some elt3 ->
          let node3 = get_unique_node "appendChild" elt3 in
          ignore(node##insertBefore(get_node elt2, Js.some node3))


    let raw_appendChilds ?before node elts =
      match before with
      | None ->
          List.iter (fun elt2 -> ignore(node##appendChild(get_node elt2))) elts
      | Some elt3 ->
          let node3 = get_unique_node "appendChild" elt3 in
          List.iter (fun elt2 -> ignore(node##insertBefore(get_node elt2, Js.some node3))) elts

    let raw_removeChild node1 elt2 =
      let node2 = get_unique_node "removeChild" elt2 in
      ignore(node1##removeChild(node2))

    let raw_replaceChild node1 elt2 elt3 =
      let node2 = get_unique_node "replaceChild" elt2 in
      ignore(node1##replaceChild(node2, get_node elt3))

    let raw_removeAllChild node =
      let childrens = Dom.list_of_nodeList (node##childNodes) in
      List.iter (fun c -> ignore(node##removeChild(c))) childrens

    let raw_replaceAllChild node elts =
      raw_removeAllChild node;
      List.iter (fun elt -> ignore(node##appendChild(get_node elt))) elts

    let appendChild ?before elt1 elt2 =
      let node = get_unique_node "appendChild" elt1 in
      raw_appendChild ?before node elt2

    let appendChilds ?before elt1 elts =
      let node = get_unique_node "appendChilds" elt1 in
      raw_appendChilds ?before node elts

    let removeChild elt1 elt2 =
      let node1 = get_unique_node "removeChild" elt1 in
      raw_removeChild node1 elt2

    let replaceChild elt1 elt2 elt3 =
      let node1 = get_unique_node "replaceChild" elt1 in
      raw_replaceChild node1 elt2 elt3

    let removeAllChild elt =
      let node = get_unique_node "removeAllChild" elt in
      raw_removeAllChild node

    let replaceAllChild elt elts =
      let node = get_unique_node "replaceAllChild" elt in
      raw_replaceAllChild node elts

    let childNodes elt =
      let node = get_unique_node "childNodes" elt in
      Dom.list_of_nodeList (node##childNodes)

    let rec filterElements nodes = match nodes with
      | [] -> []
      | node :: nodes ->
        let elts = filterElements nodes in
        Js.Opt.case
          (Dom.CoerceTo.element node)
          (fun () -> elts)
          (fun elt -> elt :: elts)

    let childElements elt =
      let node = get_unique_node "childElements" elt in
      filterElements (Dom.list_of_nodeList (node##childNodes))

    let raw_addEventListener ?(capture = false) node event handler =
      Dom_html.addEventListener node event
        (Dom_html.full_handler (fun n e -> Js.bool (handler (Html5.D.tot (Xml.make_dom (n :> Dom.node Js.t))) e)))
        (Js.bool capture)

    let addEventListener ?capture target event handler =
      let node = get_unique_elt "addEventListener" target in
      raw_addEventListener ?capture node event handler

    module Named = struct

      let appendChild ?before id1 elt2 =
        let node = Id.get_element' id1 in
        raw_appendChild ?before node elt2

      let appendChilds ?before id1 elts =
        let node = Id.get_element' id1 in
        raw_appendChilds ?before node elts

      let removeChild id1 elt2 =
        let node1 = Id.get_element' id1 in
        raw_removeChild node1 elt2

      let replaceChild id1 elt2 elt3 =
        let node1 = Id.get_element' id1 in
        raw_replaceChild node1 elt2 elt3

      let removeAllChild id =
        let node = Id.get_element' id in
        raw_removeAllChild node

      let replaceAllChild id elts =
        let node = Id.get_element' id in
        raw_replaceAllChild node elts

      let addEventListener ?capture id event handler =
        let node = Id.get_element' id in
        raw_addEventListener ?capture node event handler

    end

    let scrollIntoView ?(bottom = false) elt =
      let elt = get_unique_elt "Css.background" elt in
      elt##scrollIntoView(Js.bool (not bottom))

    module Css = struct
      let background elt =
        let elt = get_unique_elt "Css.background" elt in
        Js.to_bytestring (elt##style##background)
      let backgroundAttachment elt =
        let elt = get_unique_elt "Css.backgroundAttachment" elt in
        Js.to_bytestring (elt##style##backgroundAttachment)
      let backgroundColor elt =
        let elt = get_unique_elt "Css.backgroundColor" elt in
        Js.to_bytestring (elt##style##backgroundColor)
      let backgroundImage elt =
        let elt = get_unique_elt "Css.backgroundImage" elt in
        Js.to_bytestring (elt##style##backgroundImage)
      let backgroundPosition elt =
        let elt = get_unique_elt "Css.backgroundPosition" elt in
        Js.to_bytestring (elt##style##backgroundPosition)
      let backgroundRepeat elt =
        let elt = get_unique_elt "Css.backgroundRepeat" elt in
        Js.to_bytestring (elt##style##backgroundRepeat)
      let border elt =
        let elt = get_unique_elt "Css.border" elt in
        Js.to_bytestring (elt##style##border)
      let borderBottom elt =
        let elt = get_unique_elt "Css.borderBottom" elt in
        Js.to_bytestring (elt##style##borderBottom)
      let borderBottomColor elt =
        let elt = get_unique_elt "Css.borderBottomColor" elt in
        Js.to_bytestring (elt##style##borderBottomColor)
      let borderBottomStyle elt =
        let elt = get_unique_elt "Css.borderBottomStyle" elt in
        Js.to_bytestring (elt##style##borderBottomStyle)
      let borderBottomWidth elt =
        let elt = get_unique_elt "Css.borderBottomWidth" elt in
        Js.to_bytestring (elt##style##borderBottomWidth)
      let borderCollapse elt =
        let elt = get_unique_elt "Css.borderCollapse" elt in
        Js.to_bytestring (elt##style##borderCollapse)
      let borderColor elt =
        let elt = get_unique_elt "Css.borderColor" elt in
        Js.to_bytestring (elt##style##borderColor)
      let borderLeft elt =
        let elt = get_unique_elt "Css.borderLeft" elt in
        Js.to_bytestring (elt##style##borderLeft)
      let borderLeftColor elt =
        let elt = get_unique_elt "Css.borderLeftColor" elt in
        Js.to_bytestring (elt##style##borderLeftColor)
      let borderLeftStyle elt =
        let elt = get_unique_elt "Css.borderLeftStyle" elt in
        Js.to_bytestring (elt##style##borderLeftStyle)
      let borderLeftWidth elt =
        let elt = get_unique_elt "Css.borderLeftWidth" elt in
        Js.to_bytestring (elt##style##borderLeftWidth)
      let borderRight elt =
        let elt = get_unique_elt "Css.borderRight" elt in
        Js.to_bytestring (elt##style##borderRight)
      let borderRightColor elt =
        let elt = get_unique_elt "Css.borderRightColor" elt in
        Js.to_bytestring (elt##style##borderRightColor)
      let borderRightStyle elt =
        let elt = get_unique_elt "Css.borderRightStyle" elt in
        Js.to_bytestring (elt##style##borderRightStyle)
      let borderRightWidth elt =
        let elt = get_unique_elt "Css.borderRightWidth" elt in
        Js.to_bytestring (elt##style##borderRightWidth)
      let borderSpacing elt =
        let elt = get_unique_elt "Css.borderSpacing" elt in
        Js.to_bytestring (elt##style##borderSpacing)
      let borderStyle elt =
        let elt = get_unique_elt "Css.borderStyle" elt in
        Js.to_bytestring (elt##style##borderStyle)
      let borderTop elt =
        let elt = get_unique_elt "Css.borderTop" elt in
        Js.to_bytestring (elt##style##borderTop)
      let borderTopColor elt =
        let elt = get_unique_elt "Css.borderTopColor" elt in
        Js.to_bytestring (elt##style##borderTopColor)
      let borderTopStyle elt =
        let elt = get_unique_elt "Css.borderTopStyle" elt in
        Js.to_bytestring (elt##style##borderTopStyle)
      let borderTopWidth elt =
        let elt = get_unique_elt "Css.borderTopWidth" elt in
        Js.to_bytestring (elt##style##borderTopWidth)
      let borderWidth elt =
        let elt = get_unique_elt "Css.borderWidth" elt in
        Js.to_bytestring (elt##style##borderWidth)
      let bottom elt =
        let elt = get_unique_elt "Css.bottom" elt in
        Js.to_bytestring (elt##style##bottom)
      let captionSide elt =
        let elt = get_unique_elt "Css.captionSide" elt in
        Js.to_bytestring (elt##style##captionSide)
      let clear elt =
        let elt = get_unique_elt "Css.clear" elt in
        Js.to_bytestring (elt##style##clear)
      let clip elt =
        let elt = get_unique_elt "Css.clip" elt in
        Js.to_bytestring (elt##style##clip)
      let color elt =
        let elt = get_unique_elt "Css.color" elt in
        Js.to_bytestring (elt##style##color)
      let content elt =
        let elt = get_unique_elt "Css.content" elt in
        Js.to_bytestring (elt##style##content)
      let counterIncrement elt =
        let elt = get_unique_elt "Css.counterIncrement" elt in
        Js.to_bytestring (elt##style##counterIncrement)
      let counterReset elt =
        let elt = get_unique_elt "Css.counterReset" elt in
        Js.to_bytestring (elt##style##counterReset)
      let cssFloat elt =
        let elt = get_unique_elt "Css.cssFloat" elt in
        Js.to_bytestring (elt##style##cssFloat)
      let cssText elt =
        let elt = get_unique_elt "Css.cssText" elt in
        Js.to_bytestring (elt##style##cssText)
      let cursor elt =
        let elt = get_unique_elt "Css.cursor" elt in
        Js.to_bytestring (elt##style##cursor)
      let direction elt =
        let elt = get_unique_elt "Css.direction" elt in
        Js.to_bytestring (elt##style##direction)
      let display elt =
        let elt = get_unique_elt "Css.display" elt in
        Js.to_bytestring (elt##style##display)
      let emptyCells elt =
        let elt = get_unique_elt "Css.emptyCells" elt in
        Js.to_bytestring (elt##style##emptyCells)
      let font elt =
        let elt = get_unique_elt "Css.font" elt in
        Js.to_bytestring (elt##style##font)
      let fontFamily elt =
        let elt = get_unique_elt "Css.fontFamily" elt in
        Js.to_bytestring (elt##style##fontFamily)
      let fontSize elt =
        let elt = get_unique_elt "Css.fontSize" elt in
        Js.to_bytestring (elt##style##fontSize)
      let fontStyle elt =
        let elt = get_unique_elt "Css.fontStyle" elt in
        Js.to_bytestring (elt##style##fontStyle)
      let fontVariant elt =
        let elt = get_unique_elt "Css.fontVariant" elt in
        Js.to_bytestring (elt##style##fontVariant)
      let fontWeight elt =
        let elt = get_unique_elt "Css.fontWeight" elt in
        Js.to_bytestring (elt##style##fontWeight)
      let height elt =
        let elt = get_unique_elt "Css.height" elt in
        Js.to_bytestring (elt##style##height)
      let left elt =
        let elt = get_unique_elt "Css.left" elt in
        Js.to_bytestring (elt##style##left)
      let letterSpacing elt =
        let elt = get_unique_elt "Css.letterSpacing" elt in
        Js.to_bytestring (elt##style##letterSpacing)
      let lineHeight elt =
        let elt = get_unique_elt "Css.lineHeight" elt in
        Js.to_bytestring (elt##style##lineHeight)
      let listStyle elt =
        let elt = get_unique_elt "Css.listStyle" elt in
        Js.to_bytestring (elt##style##listStyle)
      let listStyleImage elt =
        let elt = get_unique_elt "Css.listStyleImage" elt in
        Js.to_bytestring (elt##style##listStyleImage)
      let listStylePosition elt =
        let elt = get_unique_elt "Css.listStylePosition" elt in
        Js.to_bytestring (elt##style##listStylePosition)
      let listStyleType elt =
        let elt = get_unique_elt "Css.listStyleType" elt in
        Js.to_bytestring (elt##style##listStyleType)
      let margin elt =
        let elt = get_unique_elt "Css.margin" elt in
        Js.to_bytestring (elt##style##margin)
      let marginBottom elt =
        let elt = get_unique_elt "Css.marginBottom" elt in
        Js.to_bytestring (elt##style##marginBottom)
      let marginLeft elt =
        let elt = get_unique_elt "Css.marginLeft" elt in
        Js.to_bytestring (elt##style##marginLeft)
      let marginRight elt =
        let elt = get_unique_elt "Css.marginRight" elt in
        Js.to_bytestring (elt##style##marginRight)
      let marginTop elt =
        let elt = get_unique_elt "Css.marginTop" elt in
        Js.to_bytestring (elt##style##marginTop)
      let maxHeight elt =
        let elt = get_unique_elt "Css.maxHeight" elt in
        Js.to_bytestring (elt##style##maxHeight)
      let maxWidth elt =
        let elt = get_unique_elt "Css.maxWidth" elt in
        Js.to_bytestring (elt##style##maxWidth)
      let minHeight elt =
        let elt = get_unique_elt "Css.minHeight" elt in
        Js.to_bytestring (elt##style##minHeight)
      let minWidth elt =
        let elt = get_unique_elt "Css.minWidth" elt in
        Js.to_bytestring (elt##style##minWidth)
      let opacity elt =
        let elt = get_unique_elt "Css.opacity" elt in
        Option.map Js.to_bytestring (Js.Optdef.to_option (elt##style##opacity))
      let outline elt =
        let elt = get_unique_elt "Css.outline" elt in
        Js.to_bytestring (elt##style##outline)
      let outlineColor elt =
        let elt = get_unique_elt "Css.outlineColor" elt in
        Js.to_bytestring (elt##style##outlineColor)
      let outlineOffset elt =
        let elt = get_unique_elt "Css.outlineOffset" elt in
        Js.to_bytestring (elt##style##outlineOffset)
      let outlineStyle elt =
        let elt = get_unique_elt "Css.outlineStyle" elt in
        Js.to_bytestring (elt##style##outlineStyle)
      let outlineWidth elt =
        let elt = get_unique_elt "Css.outlineWidth" elt in
        Js.to_bytestring (elt##style##outlineWidth)
      let overflow elt =
        let elt = get_unique_elt "Css.overflow" elt in
        Js.to_bytestring (elt##style##overflow)
      let overflowX elt =
        let elt = get_unique_elt "Css.overflowX" elt in
        Js.to_bytestring (elt##style##overflowX)
      let overflowY elt =
        let elt = get_unique_elt "Css.overflowY" elt in
        Js.to_bytestring (elt##style##overflowY)
      let padding elt =
        let elt = get_unique_elt "Css.padding" elt in
        Js.to_bytestring (elt##style##padding)
      let paddingBottom elt =
        let elt = get_unique_elt "Css.paddingBottom" elt in
        Js.to_bytestring (elt##style##paddingBottom)
      let paddingLeft elt =
        let elt = get_unique_elt "Css.paddingLeft" elt in
        Js.to_bytestring (elt##style##paddingLeft)
      let paddingRight elt =
        let elt = get_unique_elt "Css.paddingRight" elt in
        Js.to_bytestring (elt##style##paddingRight)
      let paddingTop elt =
        let elt = get_unique_elt "Css.paddingTop" elt in
        Js.to_bytestring (elt##style##paddingTop)
      let pageBreakAfter elt =
        let elt = get_unique_elt "Css.pageBreakAfter" elt in
        Js.to_bytestring (elt##style##pageBreakAfter)
      let pageBreakBefore elt =
        let elt = get_unique_elt "Css.pageBreakBefore" elt in
        Js.to_bytestring (elt##style##pageBreakBefore)
      let position elt =
        let elt = get_unique_elt "Css.position" elt in
        Js.to_bytestring (elt##style##position)
      let right elt =
        let elt = get_unique_elt "Css.right" elt in
        Js.to_bytestring (elt##style##right)
      let tableLayout elt =
        let elt = get_unique_elt "Css.tableLayout" elt in
        Js.to_bytestring (elt##style##tableLayout)
      let textAlign elt =
        let elt = get_unique_elt "Css.textAlign" elt in
        Js.to_bytestring (elt##style##textAlign)
      let textDecoration elt =
        let elt = get_unique_elt "Css.textDecoration" elt in
        Js.to_bytestring (elt##style##textDecoration)
      let textIndent elt =
        let elt = get_unique_elt "Css.textIndent" elt in
        Js.to_bytestring (elt##style##textIndent)
      let textTransform elt =
        let elt = get_unique_elt "Css.textTransform" elt in
        Js.to_bytestring (elt##style##textTransform)
      let top elt =
        let elt = get_unique_elt "Css.top" elt in
        Js.to_bytestring (elt##style##top)
      let verticalAlign elt =
        let elt = get_unique_elt "Css.verticalAlign" elt in
        Js.to_bytestring (elt##style##verticalAlign)
      let visibility elt =
        let elt = get_unique_elt "Css.visibility" elt in
        Js.to_bytestring (elt##style##visibility)
      let whiteSpace elt =
        let elt = get_unique_elt "Css.whiteSpace" elt in
        Js.to_bytestring (elt##style##whiteSpace)
      let width elt =
        let elt = get_unique_elt "Css.width" elt in
        Js.to_bytestring (elt##style##width)
      let wordSpacing elt =
        let elt = get_unique_elt "Css.wordSpacing" elt in
        Js.to_bytestring (elt##style##wordSpacing)
      let zIndex elt =
        let elt = get_unique_elt "Css.zIndex" elt in
        Js.to_bytestring (elt##style##zIndex)
    end

    module SetCss = struct
      let background elt v =
        let elt = get_unique_elt "SetCss.background" elt in
        elt##style##background <- Js.bytestring v
      let backgroundAttachment elt v =
        let elt = get_unique_elt "SetCss.backgroundAttachment" elt in
        elt##style##backgroundAttachment <- Js.bytestring v
      let backgroundColor elt v =
        let elt = get_unique_elt "SetCss.backgroundColor" elt in
        elt##style##backgroundColor <- Js.bytestring v
      let backgroundImage elt v =
        let elt = get_unique_elt "SetCss.backgroundImage" elt in
        elt##style##backgroundImage <- Js.bytestring v
      let backgroundPosition elt v =
        let elt = get_unique_elt "SetCss.backgroundPosition" elt in
        elt##style##backgroundPosition <- Js.bytestring v
      let backgroundRepeat elt v =
        let elt = get_unique_elt "SetCss.backgroundRepeat" elt in
        elt##style##backgroundRepeat <- Js.bytestring v
      let border elt v =
        let elt = get_unique_elt "SetCss.border" elt in
        elt##style##border <- Js.bytestring v
      let borderBottom elt v =
        let elt = get_unique_elt "SetCss.borderBottom" elt in
        elt##style##borderBottom <- Js.bytestring v
      let borderBottomColor elt v =
        let elt = get_unique_elt "SetCss.borderBottomColor" elt in
        elt##style##borderBottomColor <- Js.bytestring v
      let borderBottomStyle elt v =
        let elt = get_unique_elt "SetCss.borderBottomStyle" elt in
        elt##style##borderBottomStyle <- Js.bytestring v
      let borderBottomWidth elt v =
        let elt = get_unique_elt "SetCss.borderBottomWidth" elt in
        elt##style##borderBottomWidth <- Js.bytestring v
      let borderCollapse elt v =
        let elt = get_unique_elt "SetCss.borderCollapse" elt in
        elt##style##borderCollapse <- Js.bytestring v
      let borderColor elt v =
        let elt = get_unique_elt "SetCss.borderColor" elt in
        elt##style##borderColor <- Js.bytestring v
      let borderLeft elt v =
        let elt = get_unique_elt "SetCss.borderLeft" elt in
        elt##style##borderLeft <- Js.bytestring v
      let borderLeftColor elt v =
        let elt = get_unique_elt "SetCss.borderLeftColor" elt in
        elt##style##borderLeftColor <- Js.bytestring v
      let borderLeftStyle elt v =
        let elt = get_unique_elt "SetCss.borderLeftStyle" elt in
        elt##style##borderLeftStyle <- Js.bytestring v
      let borderLeftWidth elt v =
        let elt = get_unique_elt "SetCss.borderLeftWidth" elt in
        elt##style##borderLeftWidth <- Js.bytestring v
      let borderRight elt v =
        let elt = get_unique_elt "SetCss.borderRight" elt in
        elt##style##borderRight <- Js.bytestring v
      let borderRightColor elt v =
        let elt = get_unique_elt "SetCss.borderRightColor" elt in
        elt##style##borderRightColor <- Js.bytestring v
      let borderRightStyle elt v =
        let elt = get_unique_elt "SetCss.borderRightStyle" elt in
        elt##style##borderRightStyle <- Js.bytestring v
      let borderRightWidth elt v =
        let elt = get_unique_elt "SetCss.borderRightWidth" elt in
        elt##style##borderRightWidth <- Js.bytestring v
      let borderSpacing elt v =
        let elt = get_unique_elt "SetCss.borderSpacing" elt in
        elt##style##borderSpacing <- Js.bytestring v
      let borderStyle elt v =
        let elt = get_unique_elt "SetCss.borderStyle" elt in
        elt##style##borderStyle <- Js.bytestring v
      let borderTop elt v =
        let elt = get_unique_elt "SetCss.borderTop" elt in
        elt##style##borderTop <- Js.bytestring v
      let borderTopColor elt v =
        let elt = get_unique_elt "SetCss.borderTopColor" elt in
        elt##style##borderTopColor <- Js.bytestring v
      let borderTopStyle elt v =
        let elt = get_unique_elt "SetCss.borderTopStyle" elt in
        elt##style##borderTopStyle <- Js.bytestring v
      let borderTopWidth elt v =
        let elt = get_unique_elt "SetCss.borderTopWidth" elt in
        elt##style##borderTopWidth <- Js.bytestring v
      let borderWidth elt v =
        let elt = get_unique_elt "SetCss.borderWidth" elt in
        elt##style##borderWidth <- Js.bytestring v
      let bottom elt v =
        let elt = get_unique_elt "SetCss.bottom" elt in
        elt##style##bottom <- Js.bytestring v
      let captionSide elt v =
        let elt = get_unique_elt "SetCss.captionSide" elt in
        elt##style##captionSide <- Js.bytestring v
      let clear elt v =
        let elt = get_unique_elt "SetCss.clear" elt in
        elt##style##clear <- Js.bytestring v
      let clip elt v =
        let elt = get_unique_elt "SetCss.clip" elt in
        elt##style##clip <- Js.bytestring v
      let color elt v =
        let elt = get_unique_elt "SetCss.color" elt in
        elt##style##color <- Js.bytestring v
      let content elt v =
        let elt = get_unique_elt "SetCss.content" elt in
        elt##style##content <- Js.bytestring v
      let counterIncrement elt v =
        let elt = get_unique_elt "SetCss.counterIncrement" elt in
        elt##style##counterIncrement <- Js.bytestring v
      let counterReset elt v =
        let elt = get_unique_elt "SetCss.counterReset" elt in
        elt##style##counterReset <- Js.bytestring v
      let cssFloat elt v =
        let elt = get_unique_elt "SetCss.cssFloat" elt in
        elt##style##cssFloat <- Js.bytestring v
      let cssText elt v =
        let elt = get_unique_elt "SetCss.cssText" elt in
        elt##style##cssText <- Js.bytestring v
      let cursor elt v =
        let elt = get_unique_elt "SetCss.cursor" elt in
        elt##style##cursor <- Js.bytestring v
      let direction elt v =
        let elt = get_unique_elt "SetCss.direction" elt in
        elt##style##direction <- Js.bytestring v
      let display elt v =
        let elt = get_unique_elt "SetCss.display" elt in
        elt##style##display <- Js.bytestring v
      let emptyCells elt v =
        let elt = get_unique_elt "SetCss.emptyCells" elt in
        elt##style##emptyCells <- Js.bytestring v
      let font elt v =
        let elt = get_unique_elt "SetCss.font" elt in
        elt##style##font <- Js.bytestring v
      let fontFamily elt v =
        let elt = get_unique_elt "SetCss.fontFamily" elt in
        elt##style##fontFamily <- Js.bytestring v
      let fontSize elt v =
        let elt = get_unique_elt "SetCss.fontSize" elt in
        elt##style##fontSize <- Js.bytestring v
      let fontStyle elt v =
        let elt = get_unique_elt "SetCss.fontStyle" elt in
        elt##style##fontStyle <- Js.bytestring v
      let fontVariant elt v =
        let elt = get_unique_elt "SetCss.fontVariant" elt in
        elt##style##fontVariant <- Js.bytestring v
      let fontWeight elt v =
        let elt = get_unique_elt "SetCss.fontWeight" elt in
        elt##style##fontWeight <- Js.bytestring v
      let height elt v =
        let elt = get_unique_elt "SetCss.height" elt in
        elt##style##height <- Js.bytestring v
      let left elt v =
        let elt = get_unique_elt "SetCss.left" elt in
        elt##style##left <- Js.bytestring v
      let letterSpacing elt v =
        let elt = get_unique_elt "SetCss.letterSpacing" elt in
        elt##style##letterSpacing <- Js.bytestring v
      let lineHeight elt v =
        let elt = get_unique_elt "SetCss.lineHeight" elt in
        elt##style##lineHeight <- Js.bytestring v
      let listStyle elt v =
        let elt = get_unique_elt "SetCss.listStyle" elt in
        elt##style##listStyle <- Js.bytestring v
      let listStyleImage elt v =
        let elt = get_unique_elt "SetCss.listStyleImage" elt in
        elt##style##listStyleImage <- Js.bytestring v
      let listStylePosition elt v =
        let elt = get_unique_elt "SetCss.listStylePosition" elt in
        elt##style##listStylePosition <- Js.bytestring v
      let listStyleType elt v =
        let elt = get_unique_elt "SetCss.listStyleType" elt in
        elt##style##listStyleType <- Js.bytestring v
      let margin elt v =
        let elt = get_unique_elt "SetCss.margin" elt in
        elt##style##margin <- Js.bytestring v
      let marginBottom elt v =
        let elt = get_unique_elt "SetCss.marginBottom" elt in
        elt##style##marginBottom <- Js.bytestring v
      let marginLeft elt v =
        let elt = get_unique_elt "SetCss.marginLeft" elt in
        elt##style##marginLeft <- Js.bytestring v
      let marginRight elt v =
        let elt = get_unique_elt "SetCss.marginRight" elt in
        elt##style##marginRight <- Js.bytestring v
      let marginTop elt v =
        let elt = get_unique_elt "SetCss.marginTop" elt in
        elt##style##marginTop <- Js.bytestring v
      let maxHeight elt v =
        let elt = get_unique_elt "SetCss.maxHeight" elt in
        elt##style##maxHeight <- Js.bytestring v
      let maxWidth elt v =
        let elt = get_unique_elt "SetCss.maxWidth" elt in
        elt##style##maxWidth <- Js.bytestring v
      let minHeight elt v =
        let elt = get_unique_elt "SetCss.minHeight" elt in
        elt##style##minHeight <- Js.bytestring v
      let minWidth elt v =
        let elt = get_unique_elt "SetCss.minWidth" elt in
        elt##style##minWidth <- Js.bytestring v
      let opacity elt v =
        let elt = get_unique_elt "SetCss.opacity" elt in
        elt##style##opacity <- match v with None -> Js.undefined | Some v -> Js.def (Js.bytestring v)
      let outline elt v =
        let elt = get_unique_elt "SetCss.outline" elt in
        elt##style##outline <- Js.bytestring v
      let outlineColor elt v =
        let elt = get_unique_elt "SetCss.outlineColor" elt in
        elt##style##outlineColor <- Js.bytestring v
      let outlineOffset elt v =
        let elt = get_unique_elt "SetCss.outlineOffset" elt in
        elt##style##outlineOffset <- Js.bytestring v
      let outlineStyle elt v =
        let elt = get_unique_elt "SetCss.outlineStyle" elt in
        elt##style##outlineStyle <- Js.bytestring v
      let outlineWidth elt v =
        let elt = get_unique_elt "SetCss.outlineWidth" elt in
        elt##style##outlineWidth <- Js.bytestring v
      let overflow elt v =
        let elt = get_unique_elt "SetCss.overflow" elt in
        elt##style##overflow <- Js.bytestring v
      let overflowX elt v =
        let elt = get_unique_elt "SetCss.overflowX" elt in
        elt##style##overflowX <- Js.bytestring v
      let overflowY elt v =
        let elt = get_unique_elt "SetCss.overflowY" elt in
        elt##style##overflowY <- Js.bytestring v
      let padding elt v =
        let elt = get_unique_elt "SetCss.padding" elt in
        elt##style##padding <- Js.bytestring v
      let paddingBottom elt v =
        let elt = get_unique_elt "SetCss.paddingBottom" elt in
        elt##style##paddingBottom <- Js.bytestring v
      let paddingLeft elt v =
        let elt = get_unique_elt "SetCss.paddingLeft" elt in
        elt##style##paddingLeft <- Js.bytestring v
      let paddingRight elt v =
        let elt = get_unique_elt "SetCss.paddingRight" elt in
        elt##style##paddingRight <- Js.bytestring v
      let paddingTop elt v =
        let elt = get_unique_elt "SetCss.paddingTop" elt in
        elt##style##paddingTop <- Js.bytestring v
      let pageBreakAfter elt v =
        let elt = get_unique_elt "SetCss.pageBreakAfter" elt in
        elt##style##pageBreakAfter <- Js.bytestring v
      let pageBreakBefore elt v =
        let elt = get_unique_elt "SetCss.pageBreakBefore" elt in
        elt##style##pageBreakBefore <- Js.bytestring v
      let position elt v =
        let elt = get_unique_elt "SetCss.position" elt in
        elt##style##position <- Js.bytestring v
      let right elt v =
        let elt = get_unique_elt "SetCss.right" elt in
        elt##style##right <- Js.bytestring v
      let tableLayout elt v =
        let elt = get_unique_elt "SetCss.tableLayout" elt in
        elt##style##tableLayout <- Js.bytestring v
      let textAlign elt v =
        let elt = get_unique_elt "SetCss.textAlign" elt in
        elt##style##textAlign <- Js.bytestring v
      let textDecoration elt v =
        let elt = get_unique_elt "SetCss.textDecoration" elt in
        elt##style##textDecoration <- Js.bytestring v
      let textIndent elt v =
        let elt = get_unique_elt "SetCss.textIndent" elt in
        elt##style##textIndent <- Js.bytestring v
      let textTransform elt v =
        let elt = get_unique_elt "SetCss.textTransform" elt in
        elt##style##textTransform <- Js.bytestring v
      let top elt v =
        let elt = get_unique_elt "SetCss.top" elt in
        elt##style##top <- Js.bytestring v
      let verticalAlign elt v =
        let elt = get_unique_elt "SetCss.verticalAlign" elt in
        elt##style##verticalAlign <- Js.bytestring v
      let visibility elt v =
        let elt = get_unique_elt "SetCss.visibility" elt in
        elt##style##visibility <- Js.bytestring v
      let whiteSpace elt v =
        let elt = get_unique_elt "SetCss.whiteSpace" elt in
        elt##style##whiteSpace <- Js.bytestring v
      let width elt v =
        let elt = get_unique_elt "SetCss.width" elt in
        elt##style##width <- Js.bytestring v
      let wordSpacing elt v =
        let elt = get_unique_elt "SetCss.wordSpacing" elt in
        elt##style##wordSpacing <- Js.bytestring v
      let zIndex elt v =
        let elt = get_unique_elt "SetCss.zIndex" elt in
        elt##style##zIndex <- Js.bytestring v
    end
  end
end
