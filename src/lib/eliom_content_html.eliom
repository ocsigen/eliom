[%%client.start]

type uri = string
type 'a elt = 'a Eliom_content_html_raw.elt
type 'a attrib = 'a Eliom_content_html_raw.attrib
type 'a form_param = 'a Eliom_form.param
module Xml = Eliom_content_xml.Xml
module F = Eliom_content_html_f
module D = Eliom_content_html_d
module R = Eliom_content_html_r

module C = struct
  let node ?init x = x
  let attr ?init x = x
end

module Custom_data = struct

  type 'a t = {
    name : string;
    to_string : 'a -> string;
    of_string : string -> 'a;
    default : 'a option;
  }

  let create ~name ?default ~to_string ~of_string () =
    { name ; of_string ; to_string; default }

  let create_json ~name ?default typ =
    { name ;
      of_string = Eliom_lib.of_json ~typ ;
      to_string = Eliom_lib.to_json ~typ;
      default }

  let attrib custom_data value =
    F.a_user_data
      custom_data.name
      (custom_data.to_string value)

  let attribute_name name =
    "data-"^name

  let get_dom (element : Dom_html.element Js.t) custom_data =
    Js.Opt.case
      (element##(getAttribute (Js.string (attribute_name custom_data.name))))
      (fun () ->
         match custom_data.default with
           | Some value -> value
           | None -> raise Not_found)
      (fun str -> custom_data.of_string (Js.to_string str))

  let set_dom element custom_data value =
    element##(setAttribute (Js.string (attribute_name custom_data.name))
                          (Js.string (custom_data.to_string value)))

end

module Of_dom = Tyxml_cast.MakeOf(struct
    type 'a elt = 'a F.elt
    let elt node = Eliom_content_html_f.tot (Xml.make_dom node)
  end)

module To_dom = Tyxml_cast.MakeTo(struct
    type 'a elt = 'a F.elt
    let elt x = Js.Unsafe.coerce (Eliom_client_core.rebuild_node "n/a" x)
  end)

module Id = struct
  type 'a id = string (* FIXME invariant type parameter ? *)
  let new_elt_id: ?global:bool -> unit -> 'a id = Xml.make_node_name
  let new_global_elt_id () = new_elt_id ()
  let create_named_elt ~(id : 'a id) elt =
    D.tot (Xml.make_process_node ~id (D.toelt elt))
  let create_global_elt elt =
    D.tot (Xml.make_process_node (D.toelt elt))
  let create_request_elt ?reset:(reset = true) elt =
    D.tot (Xml.make_request_node ~reset (D.toelt elt))
  let string_of_id x = x

  let get_element' id =
    let id = string_of_id id in
    let node = Eliom_client_core.getElementById id in
    Js.Opt.case
      (Dom_html.CoerceTo.element node)
      (fun () -> failwith (Printf.sprintf "Non element node (%s)" id))
      (fun x -> x)

  let get_element id =
    try Some (Of_dom.of_element (get_element' id))
    with Failure _ -> None
end

module Manip = struct

  include
    Eliom_content_manip.Make(F)(To_dom)(Of_dom)
      (struct
        type 'a id = 'a Id.id
        let get_element' id = (Id.get_element' id :> Dom.node Js.t)
      end)
      (struct
        let content_ns = `HTML5
      end)

  let raw_addEventListener ?(capture = false) node event handler =
    Dom_html.addEventListener node event
      (Dom_html.full_handler (fun n e -> Js.bool (handler (F.tot (Xml.make_dom (n :> Dom.node Js.t))) e)))
      (Js.bool capture)

  let addEventListener ?capture target event handler =
    let node = get_unique_elt "addEventListener" target in
    raw_addEventListener ?capture node event handler

  module Named = struct
    include RawNamed

    let addEventListener ?capture id event handler =
      let node = Id.get_element' id in
      raw_addEventListener ?capture node event handler
  end

  let appendToBody ?before elt2 =
    let body = (Of_dom.of_body Dom_html.window##.document##.body) in
    appendChild ?before body elt2

  let get_unique_elt_input name elt : Dom_html.inputElement Js.t =
    Js.Opt.case
      (Js.Opt.bind (Dom_html.CoerceTo.element (get_unique_node name elt)) Dom_html.CoerceTo.input)
      (fun () -> failwith (Printf.sprintf "Non element node (%s)" name))
      Eliom_lib.id

  let get_unique_elt_select name elt : Dom_html.selectElement Js.t =
    Js.Opt.case
      (Js.Opt.bind (Dom_html.CoerceTo.element (get_unique_node name elt)) Dom_html.CoerceTo.select)
      (fun () -> failwith (Printf.sprintf "Non element node (%s)" name))
      Eliom_lib.id

  let get_unique_elt_textarea name elt : Dom_html.textAreaElement Js.t =
    Js.Opt.case
      (Js.Opt.bind (Dom_html.CoerceTo.element (get_unique_node name elt)) Dom_html.CoerceTo.textarea)
      (fun () -> failwith (Printf.sprintf "Non element node (%s)" name))
      Eliom_lib.id

  let get_unique_elt_img name elt : Dom_html.imageElement Js.t =
    Js.Opt.case
      (Js.Opt.bind (Dom_html.CoerceTo.element (get_unique_node name elt)) Dom_html.CoerceTo.img)
      (fun () -> failwith (Printf.sprintf "Non element node (%s)" name))
      Eliom_lib.id

  let scrollIntoView ?(bottom = false) elt =
    let elt = get_unique_elt "Css.background" elt in
    elt##(scrollIntoView (Js.bool (not bottom)))

  module Elt = struct
    let body () = Of_dom.of_body (Dom_html.window##.document##.body)
  end

  module Ev = struct
    type ('a,'b) ev = 'a elt -> ('b Js.t -> bool) -> unit
    type ('a,'b) ev_unit = 'a elt -> ('b Js.t -> unit) -> unit
    let bool_cb f = Dom_html.handler (fun e -> Js.bool (f e))
    let onkeyup elt f =
      let elt = get_unique_elt "Ev.onkeyup" elt in
      elt##.onkeyup := (bool_cb f)
    let onkeydown elt f =
      let elt = get_unique_elt "Ev.onkeydown" elt in
      elt##.onkeydown := (bool_cb f)
    let onmouseup elt f =
      let elt = get_unique_elt "Ev.onmouseup" elt in
      elt##.onmouseup := (bool_cb f)
    let onmousedown elt f =
      let elt = get_unique_elt "Ev.onmousedown" elt in
      elt##.onmousedown := (bool_cb f)
    let onmouseout elt f =
      let elt = get_unique_elt "Ev.onmouseout" elt in
      elt##.onmouseout := (bool_cb f)
    let onmouseover elt f =
      let elt = get_unique_elt "Ev.onmouseover" elt in
      elt##.onmouseover := (bool_cb f)
    let onclick elt f =
      let elt = get_unique_elt "Ev.onclick" elt in
      elt##.onclick := (bool_cb f)
    let ondblclick elt f =
      let elt = get_unique_elt "Ev.ondblclick" elt in
      elt##.ondblclick := (bool_cb f)
    let onload elt f =
      let elt = get_unique_elt_img "Ev.onload" elt in
      elt##.onload := (bool_cb f)
    let onerror elt f =
      let elt = get_unique_elt_img "Ev.onerror" elt in
      elt##.onerror := (bool_cb f)
    let onabort elt f =
      let elt = get_unique_elt_img "Ev.onabort" elt in
      elt##.onabort := (bool_cb f)
    let onfocus elt f =
      let elt = get_unique_elt_input "Ev.onfocus" elt in
      elt##.onfocus := (bool_cb f)
    let onblur elt f =
      let elt = get_unique_elt_input "Ev.onblur" elt in
      elt##.onblur := (bool_cb f)
    let onfocus_textarea elt f =
      let elt = get_unique_elt_textarea "Ev.onfocus" elt in
      elt##.onfocus := (bool_cb f)
    let onblur_textarea elt f =
      let elt = get_unique_elt_textarea "Ev.onblur" elt in
      elt##.onblur := (bool_cb f)
    let onscroll elt f =
      let elt = get_unique_elt "Ev.onscroll" elt in
      elt##.onscroll := (bool_cb f)
    let onreturn elt f =
      let f ev =
        let key = ev##.keyCode in
        if key = Keycode.return then f ev;
        true in
      onkeydown elt f
    let onchange elt f =
      let elt = get_unique_elt_input "Ev.onchange" elt in
      elt##.onchange := (bool_cb f)
    let onchange_select elt f =
      let elt = get_unique_elt_select "Ev.onchange_select" elt in
      elt##.onchange := (bool_cb f)
  end

  module Attr = struct
    let clientWidth elt =
      let elt = get_unique_elt "Attr.clientWidth" elt in
      elt##.clientWidth
    let clientHeight elt =
      let elt = get_unique_elt "Attr.clientHeight" elt in
      elt##.clientHeight
    let offsetWidth elt =
      let elt = get_unique_elt "Attr.offsetWidth" elt in
      elt##.offsetWidth
    let offsetHeight elt =
      let elt = get_unique_elt "Attr.offsetHeight" elt in
      elt##.offsetHeight
    let clientLeft elt =
      let elt = get_unique_elt "Attr.clientLeft" elt in
      elt##.clientLeft
    let clientTop elt =
      let elt = get_unique_elt "Attr.clientTop" elt in
      elt##.clientTop
  end

  module Css = struct
    let background elt =
      let elt = get_unique_elt "Css.background" elt in
      Js.to_bytestring (elt##.style##.background)
    let backgroundAttachment elt =
      let elt = get_unique_elt "Css.backgroundAttachment" elt in
      Js.to_bytestring (elt##.style##.backgroundAttachment)
    let backgroundColor elt =
      let elt = get_unique_elt "Css.backgroundColor" elt in
      Js.to_bytestring (elt##.style##.backgroundColor)
    let backgroundImage elt =
      let elt = get_unique_elt "Css.backgroundImage" elt in
      Js.to_bytestring (elt##.style##.backgroundImage)
    let backgroundPosition elt =
      let elt = get_unique_elt "Css.backgroundPosition" elt in
      Js.to_bytestring (elt##.style##.backgroundPosition)
    let backgroundRepeat elt =
      let elt = get_unique_elt "Css.backgroundRepeat" elt in
      Js.to_bytestring (elt##.style##.backgroundRepeat)
    let border elt =
      let elt = get_unique_elt "Css.border" elt in
      Js.to_bytestring (elt##.style##.border)
    let borderBottom elt =
      let elt = get_unique_elt "Css.borderBottom" elt in
      Js.to_bytestring (elt##.style##.borderBottom)
    let borderBottomColor elt =
      let elt = get_unique_elt "Css.borderBottomColor" elt in
      Js.to_bytestring (elt##.style##.borderBottomColor)
    let borderBottomStyle elt =
      let elt = get_unique_elt "Css.borderBottomStyle" elt in
      Js.to_bytestring (elt##.style##.borderBottomStyle)
    let borderBottomWidth elt =
      let elt = get_unique_elt "Css.borderBottomWidth" elt in
      Js.to_bytestring (elt##.style##.borderBottomWidth)
    let borderBottomWidthPx elt =
      let elt = get_unique_elt "Css.borderBottomWidthPx" elt in
      Js.parseInt (elt##.style##.borderBottomWidth)
    let borderCollapse elt =
      let elt = get_unique_elt "Css.borderCollapse" elt in
      Js.to_bytestring (elt##.style##.borderCollapse)
    let borderColor elt =
      let elt = get_unique_elt "Css.borderColor" elt in
      Js.to_bytestring (elt##.style##.borderColor)
    let borderLeft elt =
      let elt = get_unique_elt "Css.borderLeft" elt in
      Js.to_bytestring (elt##.style##.borderLeft)
    let borderLeftColor elt =
      let elt = get_unique_elt "Css.borderLeftColor" elt in
      Js.to_bytestring (elt##.style##.borderLeftColor)
    let borderLeftStyle elt =
      let elt = get_unique_elt "Css.borderLeftStyle" elt in
      Js.to_bytestring (elt##.style##.borderLeftStyle)
    let borderLeftWidth elt =
      let elt = get_unique_elt "Css.borderLeftWidth" elt in
      Js.to_bytestring (elt##.style##.borderLeftWidth)
    let borderLeftWidthPx elt =
      let elt = get_unique_elt "Css.borderLeftWidthPx" elt in
      Js.parseInt (elt##.style##.borderLeftWidth)
    let borderRight elt =
      let elt = get_unique_elt "Css.borderRight" elt in
      Js.to_bytestring (elt##.style##.borderRight)
    let borderRightColor elt =
      let elt = get_unique_elt "Css.borderRightColor" elt in
      Js.to_bytestring (elt##.style##.borderRightColor)
    let borderRightStyle elt =
      let elt = get_unique_elt "Css.borderRightStyle" elt in
      Js.to_bytestring (elt##.style##.borderRightStyle)
    let borderRightWidth elt =
      let elt = get_unique_elt "Css.borderRightWidth" elt in
      Js.to_bytestring (elt##.style##.borderRightWidth)
    let borderRightWidthPx elt =
      let elt = get_unique_elt "Css.borderRightWidthPx" elt in
      Js.parseInt (elt##.style##.borderRightWidth)
    let borderSpacing elt =
      let elt = get_unique_elt "Css.borderSpacing" elt in
      Js.to_bytestring (elt##.style##.borderSpacing)
    let borderStyle elt =
      let elt = get_unique_elt "Css.borderStyle" elt in
      Js.to_bytestring (elt##.style##.borderStyle)
    let borderTop elt =
      let elt = get_unique_elt "Css.borderTop" elt in
      Js.to_bytestring (elt##.style##.borderTop)
    let borderTopColor elt =
      let elt = get_unique_elt "Css.borderTopColor" elt in
      Js.to_bytestring (elt##.style##.borderTopColor)
    let borderTopStyle elt =
      let elt = get_unique_elt "Css.borderTopStyle" elt in
      Js.to_bytestring (elt##.style##.borderTopStyle)
    let borderTopWidth elt =
      let elt = get_unique_elt "Css.borderTopWidth" elt in
      Js.to_bytestring (elt##.style##.borderTopWidth)
    let borderTopWidthPx elt =
      let elt = get_unique_elt "Css.borderTopWidthPx" elt in
      Js.parseInt (elt##.style##.borderTopWidth)
    let borderWidth elt =
      let elt = get_unique_elt "Css.borderWidth" elt in
      Js.to_bytestring (elt##.style##.borderWidth)
    let borderWidthPx elt =
      let elt = get_unique_elt "Css.borderWidthPx" elt in
      Js.parseInt (elt##.style##.borderWidth)
    let bottom elt =
      let elt = get_unique_elt "Css.bottom" elt in
      Js.to_bytestring (elt##.style##.bottom)
    let captionSide elt =
      let elt = get_unique_elt "Css.captionSide" elt in
      Js.to_bytestring (elt##.style##.captionSide)
    let clear elt =
      let elt = get_unique_elt "Css.clear" elt in
      Js.to_bytestring (elt##.style##.clear)
    let clip elt =
      let elt = get_unique_elt "Css.clip" elt in
      Js.to_bytestring (elt##.style##.clip)
    let color elt =
      let elt = get_unique_elt "Css.color" elt in
      Js.to_bytestring (elt##.style##.color)
    let content elt =
      let elt = get_unique_elt "Css.content" elt in
      Js.to_bytestring (elt##.style##.content)
    let counterIncrement elt =
      let elt = get_unique_elt "Css.counterIncrement" elt in
      Js.to_bytestring (elt##.style##.counterIncrement)
    let counterReset elt =
      let elt = get_unique_elt "Css.counterReset" elt in
      Js.to_bytestring (elt##.style##.counterReset)
    let cssFloat elt =
      let elt = get_unique_elt "Css.cssFloat" elt in
      Js.to_bytestring (elt##.style##.cssFloat)
    let cssText elt =
      let elt = get_unique_elt "Css.cssText" elt in
      Js.to_bytestring (elt##.style##.cssText)
    let cursor elt =
      let elt = get_unique_elt "Css.cursor" elt in
      Js.to_bytestring (elt##.style##.cursor)
    let direction elt =
      let elt = get_unique_elt "Css.direction" elt in
      Js.to_bytestring (elt##.style##.direction)
    let display elt =
      let elt = get_unique_elt "Css.display" elt in
      Js.to_bytestring (elt##.style##.display)
    let emptyCells elt =
      let elt = get_unique_elt "Css.emptyCells" elt in
      Js.to_bytestring (elt##.style##.emptyCells)
    let font elt =
      let elt = get_unique_elt "Css.font" elt in
      Js.to_bytestring (elt##.style##.font)
    let fontFamily elt =
      let elt = get_unique_elt "Css.fontFamily" elt in
      Js.to_bytestring (elt##.style##.fontFamily)
    let fontSize elt =
      let elt = get_unique_elt "Css.fontSize" elt in
      Js.to_bytestring (elt##.style##.fontSize)
    let fontStyle elt =
      let elt = get_unique_elt "Css.fontStyle" elt in
      Js.to_bytestring (elt##.style##.fontStyle)
    let fontVariant elt =
      let elt = get_unique_elt "Css.fontVariant" elt in
      Js.to_bytestring (elt##.style##.fontVariant)
    let fontWeight elt =
      let elt = get_unique_elt "Css.fontWeight" elt in
      Js.to_bytestring (elt##.style##.fontWeight)
    let height elt =
      let elt = get_unique_elt "Css.height" elt in
      Js.to_bytestring (elt##.style##.height)
    let heightPx elt =
      let elt = get_unique_elt "Css.heightPx" elt in
      Js.parseInt (elt##.style##.height)
    let left elt =
      let elt = get_unique_elt "Css.left" elt in
      Js.to_bytestring (elt##.style##.left)
    let leftPx elt =
      let elt = get_unique_elt "Css.leftPx" elt in
      Js.parseInt (elt##.style##.left)
    let letterSpacing elt =
      let elt = get_unique_elt "Css.letterSpacing" elt in
      Js.to_bytestring (elt##.style##.letterSpacing)
    let lineHeight elt =
      let elt = get_unique_elt "Css.lineHeight" elt in
      Js.to_bytestring (elt##.style##.lineHeight)
    let listStyle elt =
      let elt = get_unique_elt "Css.listStyle" elt in
      Js.to_bytestring (elt##.style##.listStyle)
    let listStyleImage elt =
      let elt = get_unique_elt "Css.listStyleImage" elt in
      Js.to_bytestring (elt##.style##.listStyleImage)
    let listStylePosition elt =
      let elt = get_unique_elt "Css.listStylePosition" elt in
      Js.to_bytestring (elt##.style##.listStylePosition)
    let listStyleType elt =
      let elt = get_unique_elt "Css.listStyleType" elt in
      Js.to_bytestring (elt##.style##.listStyleType)
    let margin elt =
      let elt = get_unique_elt "Css.margin" elt in
      Js.to_bytestring (elt##.style##.margin)
    let marginBottom elt =
      let elt = get_unique_elt "Css.marginBottom" elt in
      Js.to_bytestring (elt##.style##.marginBottom)
    let marginBottomPx elt =
      let elt = get_unique_elt "Css.marginBottomPx" elt in
      Js.parseInt (elt##.style##.marginBottom)
    let marginLeft elt =
      let elt = get_unique_elt "Css.marginLeft" elt in
      Js.to_bytestring (elt##.style##.marginLeft)
    let marginLeftPx elt =
      let elt = get_unique_elt "Css.marginLeftPx" elt in
      Js.parseInt (elt##.style##.marginLeft)
    let marginRight elt =
      let elt = get_unique_elt "Css.marginRight" elt in
      Js.to_bytestring (elt##.style##.marginRight)
    let marginRightPx elt =
      let elt = get_unique_elt "Css.marginRightPx" elt in
      Js.parseInt (elt##.style##.marginRight)
    let marginTop elt =
      let elt = get_unique_elt "Css.marginTop" elt in
      Js.to_bytestring (elt##.style##.marginTop)
    let marginTopPx elt =
      let elt = get_unique_elt "Css.marginTopPx" elt in
      Js.parseInt (elt##.style##.marginTop)
    let maxHeight elt =
      let elt = get_unique_elt "Css.maxHeight" elt in
      Js.to_bytestring (elt##.style##.maxHeight)
    let maxHeightPx elt =
      let elt = get_unique_elt "Css.maxHeightPx" elt in
      Js.parseInt (elt##.style##.maxHeight)
    let maxWidth elt =
      let elt = get_unique_elt "Css.maxWidth" elt in
      Js.to_bytestring (elt##.style##.maxWidth)
    let maxWidthPx elt =
      let elt = get_unique_elt "Css.maxWidthPx" elt in
      Js.parseInt (elt##.style##.maxWidth)
    let minHeight elt =
      let elt = get_unique_elt "Css.minHeight" elt in
      Js.to_bytestring (elt##.style##.minHeight)
    let minHeightPx elt =
      let elt = get_unique_elt "Css.minHeightPx" elt in
      Js.parseInt (elt##.style##.minHeight)
    let minWidth elt =
      let elt = get_unique_elt "Css.minWidth" elt in
      Js.to_bytestring (elt##.style##.minWidth)
    let minWidthPx elt =
      let elt = get_unique_elt "Css.minWidthPx" elt in
      Js.parseInt (elt##.style##.minWidth)
    let opacity elt =
      let elt = get_unique_elt "Css.opacity" elt in
      Eliom_lib.Option.map
        Js.to_bytestring (Js.Optdef.to_option (elt##.style##.opacity))
    let outline elt =
      let elt = get_unique_elt "Css.outline" elt in
      Js.to_bytestring (elt##.style##.outline)
    let outlineColor elt =
      let elt = get_unique_elt "Css.outlineColor" elt in
      Js.to_bytestring (elt##.style##.outlineColor)
    let outlineOffset elt =
      let elt = get_unique_elt "Css.outlineOffset" elt in
      Js.to_bytestring (elt##.style##.outlineOffset)
    let outlineStyle elt =
      let elt = get_unique_elt "Css.outlineStyle" elt in
      Js.to_bytestring (elt##.style##.outlineStyle)
    let outlineWidth elt =
      let elt = get_unique_elt "Css.outlineWidth" elt in
      Js.to_bytestring (elt##.style##.outlineWidth)
    let overflow elt =
      let elt = get_unique_elt "Css.overflow" elt in
      Js.to_bytestring (elt##.style##.overflow)
    let overflowX elt =
      let elt = get_unique_elt "Css.overflowX" elt in
      Js.to_bytestring (elt##.style##.overflowX)
    let overflowY elt =
      let elt = get_unique_elt "Css.overflowY" elt in
      Js.to_bytestring (elt##.style##.overflowY)
    let padding elt =
      let elt = get_unique_elt "Css.padding" elt in
      Js.to_bytestring (elt##.style##.padding)
    let paddingBottom elt =
      let elt = get_unique_elt "Css.paddingBottom" elt in
      Js.to_bytestring (elt##.style##.paddingBottom)
    let paddingBottomPx elt =
      let elt = get_unique_elt "Css.paddingBottomPx" elt in
      Js.parseInt (elt##.style##.paddingBottom)
    let paddingLeft elt =
      let elt = get_unique_elt "Css.paddingLeft" elt in
      Js.to_bytestring (elt##.style##.paddingLeft)
    let paddingLeftPx elt =
      let elt = get_unique_elt "Css.paddingLeftPx" elt in
      Js.parseInt (elt##.style##.paddingLeft)
    let paddingRight elt =
      let elt = get_unique_elt "Css.paddingRight" elt in
      Js.to_bytestring (elt##.style##.paddingRight)
    let paddingRightPx elt =
      let elt = get_unique_elt "Css.paddingRightPx" elt in
      Js.parseInt (elt##.style##.paddingRight)
    let paddingTop elt =
      let elt = get_unique_elt "Css.paddingTop" elt in
      Js.to_bytestring (elt##.style##.paddingTop)
    let paddingTopPx elt =
      let elt = get_unique_elt "Css.paddingTopPx" elt in
      Js.parseInt (elt##.style##.paddingTop)
    let pageBreakAfter elt =
      let elt = get_unique_elt "Css.pageBreakAfter" elt in
      Js.to_bytestring (elt##.style##.pageBreakAfter)
    let pageBreakBefore elt =
      let elt = get_unique_elt "Css.pageBreakBefore" elt in
      Js.to_bytestring (elt##.style##.pageBreakBefore)
    let position elt =
      let elt = get_unique_elt "Css.position" elt in
      Js.to_bytestring (elt##.style##.position)
    let right elt =
      let elt = get_unique_elt "Css.right" elt in
      Js.to_bytestring (elt##.style##.right)
    let rightPx elt =
      let elt = get_unique_elt "Css.rightPx" elt in
      Js.parseInt (elt##.style##.right)
    let tableLayout elt =
      let elt = get_unique_elt "Css.tableLayout" elt in
      Js.to_bytestring (elt##.style##.tableLayout)
    let textAlign elt =
      let elt = get_unique_elt "Css.textAlign" elt in
      Js.to_bytestring (elt##.style##.textAlign)
    let textDecoration elt =
      let elt = get_unique_elt "Css.textDecoration" elt in
      Js.to_bytestring (elt##.style##.textDecoration)
    let textIndent elt =
      let elt = get_unique_elt "Css.textIndent" elt in
      Js.to_bytestring (elt##.style##.textIndent)
    let textTransform elt =
      let elt = get_unique_elt "Css.textTransform" elt in
      Js.to_bytestring (elt##.style##.textTransform)
    let top elt =
      let elt = get_unique_elt "Css.top" elt in
      Js.to_bytestring (elt##.style##.top)
    let topPx elt =
      let elt = get_unique_elt "Css.topPx" elt in
      Js.parseInt (elt##.style##.top)
    let verticalAlign elt =
      let elt = get_unique_elt "Css.verticalAlign" elt in
      Js.to_bytestring (elt##.style##.verticalAlign)
    let visibility elt =
      let elt = get_unique_elt "Css.visibility" elt in
      Js.to_bytestring (elt##.style##.visibility)
    let whiteSpace elt =
      let elt = get_unique_elt "Css.whiteSpace" elt in
      Js.to_bytestring (elt##.style##.whiteSpace)
    let width elt =
      let elt = get_unique_elt "Css.width" elt in
      Js.to_bytestring (elt##.style##.width)
    let widthPx elt =
      let elt = get_unique_elt "Css.widthPx" elt in
      Js.parseInt (elt##.style##.width)
    let wordSpacing elt =
      let elt = get_unique_elt "Css.wordSpacing" elt in
      Js.to_bytestring (elt##.style##.wordSpacing)
    let zIndex elt =
      let elt = get_unique_elt "Css.zIndex" elt in
      Js.to_bytestring (elt##.style##.zIndex)
  end

  module SetCss = struct
    let background elt v =
      let elt = get_unique_elt "SetCss.background" elt in
      elt##.style##.background := Js.bytestring v
    let backgroundAttachment elt v =
      let elt = get_unique_elt "SetCss.backgroundAttachment" elt in
      elt##.style##.backgroundAttachment := Js.bytestring v
    let backgroundColor elt v =
      let elt = get_unique_elt "SetCss.backgroundColor" elt in
      elt##.style##.backgroundColor := Js.bytestring v
    let backgroundImage elt v =
      let elt = get_unique_elt "SetCss.backgroundImage" elt in
      elt##.style##.backgroundImage := Js.bytestring v
    let backgroundPosition elt v =
      let elt = get_unique_elt "SetCss.backgroundPosition" elt in
      elt##.style##.backgroundPosition := Js.bytestring v
    let backgroundRepeat elt v =
      let elt = get_unique_elt "SetCss.backgroundRepeat" elt in
      elt##.style##.backgroundRepeat := Js.bytestring v
    let border elt v =
      let elt = get_unique_elt "SetCss.border" elt in
      elt##.style##.border := Js.bytestring v
    let borderBottom elt v =
      let elt = get_unique_elt "SetCss.borderBottom" elt in
      elt##.style##.borderBottom := Js.bytestring v
    let borderBottomColor elt v =
      let elt = get_unique_elt "SetCss.borderBottomColor" elt in
      elt##.style##.borderBottomColor := Js.bytestring v
    let borderBottomStyle elt v =
      let elt = get_unique_elt "SetCss.borderBottomStyle" elt in
      elt##.style##.borderBottomStyle := Js.bytestring v
    let borderBottomWidth elt v =
      let elt = get_unique_elt "SetCss.borderBottomWidth" elt in
      elt##.style##.borderBottomWidth := Js.bytestring v
    let borderBottomWidthPx elt v = borderBottomWidth elt (Printf.sprintf "%dpx" v)
    let borderCollapse elt v =
      let elt = get_unique_elt "SetCss.borderCollapse" elt in
      elt##.style##.borderCollapse := Js.bytestring v
    let borderColor elt v =
      let elt = get_unique_elt "SetCss.borderColor" elt in
      elt##.style##.borderColor := Js.bytestring v
    let borderLeft elt v =
      let elt = get_unique_elt "SetCss.borderLeft" elt in
      elt##.style##.borderLeft := Js.bytestring v
    let borderLeftColor elt v =
      let elt = get_unique_elt "SetCss.borderLeftColor" elt in
      elt##.style##.borderLeftColor := Js.bytestring v
    let borderLeftStyle elt v =
      let elt = get_unique_elt "SetCss.borderLeftStyle" elt in
      elt##.style##.borderLeftStyle := Js.bytestring v
    let borderLeftWidth elt v =
      let elt = get_unique_elt "SetCss.borderLeftWidth" elt in
      elt##.style##.borderLeftWidth := Js.bytestring v
    let borderLeftWidthPx elt v = borderLeftWidth elt (Printf.sprintf "%dpx" v)
    let borderRight elt v =
      let elt = get_unique_elt "SetCss.borderRight" elt in
      elt##.style##.borderRight := Js.bytestring v
    let borderRightColor elt v =
      let elt = get_unique_elt "SetCss.borderRightColor" elt in
      elt##.style##.borderRightColor := Js.bytestring v
    let borderRightStyle elt v =
      let elt = get_unique_elt "SetCss.borderRightStyle" elt in
      elt##.style##.borderRightStyle := Js.bytestring v
    let borderRightWidth elt v =
      let elt = get_unique_elt "SetCss.borderRightWidth" elt in
      elt##.style##.borderRightWidth := Js.bytestring v
    let borderRightWidthPx elt v = borderRightWidth elt (Printf.sprintf "%dpx" v)
    let borderSpacing elt v =
      let elt = get_unique_elt "SetCss.borderSpacing" elt in
      elt##.style##.borderSpacing := Js.bytestring v
    let borderStyle elt v =
      let elt = get_unique_elt "SetCss.borderStyle" elt in
      elt##.style##.borderStyle := Js.bytestring v
    let borderTop elt v =
      let elt = get_unique_elt "SetCss.borderTop" elt in
      elt##.style##.borderTop := Js.bytestring v
    let borderTopColor elt v =
      let elt = get_unique_elt "SetCss.borderTopColor" elt in
      elt##.style##.borderTopColor := Js.bytestring v
    let borderTopStyle elt v =
      let elt = get_unique_elt "SetCss.borderTopStyle" elt in
      elt##.style##.borderTopStyle := Js.bytestring v
    let borderTopWidth elt v =
      let elt = get_unique_elt "SetCss.borderTopWidth" elt in
      elt##.style##.borderTopWidth := Js.bytestring v
    let borderTopWidthPx elt v = borderTopWidth elt (Printf.sprintf "%dpx" v)
    let borderWidth elt v =
      let elt = get_unique_elt "SetCss.borderWidth" elt in
      elt##.style##.borderWidth := Js.bytestring v
    let bottom elt v =
      let elt = get_unique_elt "SetCss.bottom" elt in
      elt##.style##.bottom := Js.bytestring v
    let bottomPx elt v = bottom elt (Printf.sprintf "%dpx" v)
    let captionSide elt v =
      let elt = get_unique_elt "SetCss.captionSide" elt in
      elt##.style##.captionSide := Js.bytestring v
    let clear elt v =
      let elt = get_unique_elt "SetCss.clear" elt in
      elt##.style##.clear := Js.bytestring v
    let clip elt v =
      let elt = get_unique_elt "SetCss.clip" elt in
      elt##.style##.clip := Js.bytestring v
    let color elt v =
      let elt = get_unique_elt "SetCss.color" elt in
      elt##.style##.color := Js.bytestring v
    let content elt v =
      let elt = get_unique_elt "SetCss.content" elt in
      elt##.style##.content := Js.bytestring v
    let counterIncrement elt v =
      let elt = get_unique_elt "SetCss.counterIncrement" elt in
      elt##.style##.counterIncrement := Js.bytestring v
    let counterReset elt v =
      let elt = get_unique_elt "SetCss.counterReset" elt in
      elt##.style##.counterReset := Js.bytestring v
    let cssFloat elt v =
      let elt = get_unique_elt "SetCss.cssFloat" elt in
      elt##.style##.cssFloat := Js.bytestring v
    let cssText elt v =
      let elt = get_unique_elt "SetCss.cssText" elt in
      elt##.style##.cssText := Js.bytestring v
    let cursor elt v =
      let elt = get_unique_elt "SetCss.cursor" elt in
      elt##.style##.cursor := Js.bytestring v
    let direction elt v =
      let elt = get_unique_elt "SetCss.direction" elt in
      elt##.style##.direction := Js.bytestring v
    let display elt v =
      let elt = get_unique_elt "SetCss.display" elt in
      elt##.style##.display := Js.bytestring v
    let emptyCells elt v =
      let elt = get_unique_elt "SetCss.emptyCells" elt in
      elt##.style##.emptyCells := Js.bytestring v
    let font elt v =
      let elt = get_unique_elt "SetCss.font" elt in
      elt##.style##.font := Js.bytestring v
    let fontFamily elt v =
      let elt = get_unique_elt "SetCss.fontFamily" elt in
      elt##.style##.fontFamily := Js.bytestring v
    let fontSize elt v =
      let elt = get_unique_elt "SetCss.fontSize" elt in
      elt##.style##.fontSize := Js.bytestring v
    let fontStyle elt v =
      let elt = get_unique_elt "SetCss.fontStyle" elt in
      elt##.style##.fontStyle := Js.bytestring v
    let fontVariant elt v =
      let elt = get_unique_elt "SetCss.fontVariant" elt in
      elt##.style##.fontVariant := Js.bytestring v
    let fontWeight elt v =
      let elt = get_unique_elt "SetCss.fontWeight" elt in
      elt##.style##.fontWeight := Js.bytestring v
    let height elt v =
      let elt = get_unique_elt "SetCss.height" elt in
      elt##.style##.height := Js.bytestring v
    let heightPx elt v = height elt (Printf.sprintf "%dpx" v)
    let left elt v =
      let elt = get_unique_elt "SetCss.left" elt in
      elt##.style##.left := Js.bytestring v
    let leftPx elt v = left elt (Printf.sprintf "%dpx" v)
    let letterSpacing elt v =
      let elt = get_unique_elt "SetCss.letterSpacing" elt in
      elt##.style##.letterSpacing := Js.bytestring v
    let lineHeight elt v =
      let elt = get_unique_elt "SetCss.lineHeight" elt in
      elt##.style##.lineHeight := Js.bytestring v
    let listStyle elt v =
      let elt = get_unique_elt "SetCss.listStyle" elt in
      elt##.style##.listStyle := Js.bytestring v
    let listStyleImage elt v =
      let elt = get_unique_elt "SetCss.listStyleImage" elt in
      elt##.style##.listStyleImage := Js.bytestring v
    let listStylePosition elt v =
      let elt = get_unique_elt "SetCss.listStylePosition" elt in
      elt##.style##.listStylePosition := Js.bytestring v
    let listStyleType elt v =
      let elt = get_unique_elt "SetCss.listStyleType" elt in
      elt##.style##.listStyleType := Js.bytestring v
    let margin elt v =
      let elt = get_unique_elt "SetCss.margin" elt in
      elt##.style##.margin := Js.bytestring v
    let marginBottom elt v =
      let elt = get_unique_elt "SetCss.marginBottom" elt in
      elt##.style##.marginBottom := Js.bytestring v
    let marginBottomPx elt v = marginBottom elt (Printf.sprintf "%dpx" v)
    let marginLeft elt v =
      let elt = get_unique_elt "SetCss.marginLeft" elt in
      elt##.style##.marginLeft := Js.bytestring v
    let marginLeftPx elt v = marginLeft elt (Printf.sprintf "%dpx" v)
    let marginRight elt v =
      let elt = get_unique_elt "SetCss.marginRight" elt in
      elt##.style##.marginRight := Js.bytestring v
    let marginRightPx elt v = marginRight elt (Printf.sprintf "%dpx" v)
    let marginTop elt v =
      let elt = get_unique_elt "SetCss.marginTop" elt in
      elt##.style##.marginTop := Js.bytestring v
    let marginTopPx elt v = marginTop elt (Printf.sprintf "%dpx" v)
    let maxHeight elt v =
      let elt = get_unique_elt "SetCss.maxHeight" elt in
      elt##.style##.maxHeight := Js.bytestring v
    let maxHeightPx elt v = maxHeight elt (Printf.sprintf "%dpx" v)
    let maxWidth elt v =
      let elt = get_unique_elt "SetCss.maxWidth" elt in
      elt##.style##.maxWidth := Js.bytestring v
    let maxWidthPx elt v = maxWidth elt (Printf.sprintf "%dpx" v)
    let minHeight elt v =
      let elt = get_unique_elt "SetCss.minHeight" elt in
      elt##.style##.minHeight := Js.bytestring v
    let minHeightPx elt v = minHeight elt (Printf.sprintf "%dpx" v)
    let minWidth elt v =
      let elt = get_unique_elt "SetCss.minWidth" elt in
      elt##.style##.minWidth := Js.bytestring v
    let minWidthPx elt v = minWidth elt (Printf.sprintf "%dpx" v)
    let opacity elt v =
      let elt = get_unique_elt "SetCss.opacity" elt in
      elt##.style##.opacity := Js.def (Js.bytestring v)
    let outline elt v =
      let elt = get_unique_elt "SetCss.outline" elt in
      elt##.style##.outline := Js.bytestring v
    let outlineColor elt v =
      let elt = get_unique_elt "SetCss.outlineColor" elt in
      elt##.style##.outlineColor := Js.bytestring v
    let outlineOffset elt v =
      let elt = get_unique_elt "SetCss.outlineOffset" elt in
      elt##.style##.outlineOffset := Js.bytestring v
    let outlineStyle elt v =
      let elt = get_unique_elt "SetCss.outlineStyle" elt in
      elt##.style##.outlineStyle := Js.bytestring v
    let outlineWidth elt v =
      let elt = get_unique_elt "SetCss.outlineWidth" elt in
      elt##.style##.outlineWidth := Js.bytestring v
    let overflow elt v =
      let elt = get_unique_elt "SetCss.overflow" elt in
      elt##.style##.overflow := Js.bytestring v
    let overflowX elt v =
      let elt = get_unique_elt "SetCss.overflowX" elt in
      elt##.style##.overflowX := Js.bytestring v
    let overflowY elt v =
      let elt = get_unique_elt "SetCss.overflowY" elt in
      elt##.style##.overflowY := Js.bytestring v
    let padding elt v =
      let elt = get_unique_elt "SetCss.padding" elt in
      elt##.style##.padding := Js.bytestring v
    let paddingBottom elt v =
      let elt = get_unique_elt "SetCss.paddingBottom" elt in
      elt##.style##.paddingBottom := Js.bytestring v
    let paddingBottomPx elt v = paddingBottom elt (Printf.sprintf "%dpx" v)
    let paddingLeft elt v =
      let elt = get_unique_elt "SetCss.paddingLeft" elt in
      elt##.style##.paddingLeft := Js.bytestring v
    let paddingLeftPx elt v = paddingLeft elt (Printf.sprintf "%dpx" v)
    let paddingRight elt v =
      let elt = get_unique_elt "SetCss.paddingRight" elt in
      elt##.style##.paddingRight := Js.bytestring v
    let paddingRightPx elt v = paddingRight elt (Printf.sprintf "%dpx" v)
    let paddingTop elt v =
      let elt = get_unique_elt "SetCss.paddingTop" elt in
      elt##.style##.paddingTop := Js.bytestring v
    let paddingTopPx elt v = paddingTop elt (Printf.sprintf "%dpx" v)
    let pageBreakAfter elt v =
      let elt = get_unique_elt "SetCss.pageBreakAfter" elt in
      elt##.style##.pageBreakAfter := Js.bytestring v
    let pageBreakBefore elt v =
      let elt = get_unique_elt "SetCss.pageBreakBefore" elt in
      elt##.style##.pageBreakBefore := Js.bytestring v
    let position elt v =
      let elt = get_unique_elt "SetCss.position" elt in
      elt##.style##.position := Js.bytestring v
    let right elt v =
      let elt = get_unique_elt "SetCss.right" elt in
      elt##.style##.right := Js.bytestring v
    let rightPx elt v = right elt (Printf.sprintf "%dpx" v)
    let tableLayout elt v =
      let elt = get_unique_elt "SetCss.tableLayout" elt in
      elt##.style##.tableLayout := Js.bytestring v
    let textAlign elt v =
      let elt = get_unique_elt "SetCss.textAlign" elt in
      elt##.style##.textAlign := Js.bytestring v
    let textDecoration elt v =
      let elt = get_unique_elt "SetCss.textDecoration" elt in
      elt##.style##.textDecoration := Js.bytestring v
    let textIndent elt v =
      let elt = get_unique_elt "SetCss.textIndent" elt in
      elt##.style##.textIndent := Js.bytestring v
    let textTransform elt v =
      let elt = get_unique_elt "SetCss.textTransform" elt in
      elt##.style##.textTransform := Js.bytestring v
    let top elt v =
      let elt = get_unique_elt "SetCss.top" elt in
      elt##.style##.top := Js.bytestring v
    let topPx elt v = top elt (Printf.sprintf "%dpx" v)
    let verticalAlign elt v =
      let elt = get_unique_elt "SetCss.verticalAlign" elt in
      elt##.style##.verticalAlign := Js.bytestring v
    let visibility elt v =
      let elt = get_unique_elt "SetCss.visibility" elt in
      elt##.style##.visibility := Js.bytestring v
    let whiteSpace elt v =
      let elt = get_unique_elt "SetCss.whiteSpace" elt in
      elt##.style##.whiteSpace := Js.bytestring v
    let width elt v =
      let elt = get_unique_elt "SetCss.width" elt in
      elt##.style##.width := Js.bytestring v
    let widthPx elt v = width elt (Printf.sprintf "%dpx" v)
    let wordSpacing elt v =
      let elt = get_unique_elt "SetCss.wordSpacing" elt in
      elt##.style##.wordSpacing := Js.bytestring v
    let zIndex elt v =
      let elt = get_unique_elt "SetCss.zIndex" elt in
      elt##.style##.zIndex := Js.bytestring v
  end
end

let set_classes_of_elt elt = F.tot (Xml.set_classes_of_elt (F.toelt elt))

[%%shared
type boxed
external boxed : 'a Eliom_client_value.t
  -> boxed Eliom_client_value.t
  = "%identity"
external unboxed : boxed Eliom_client_value.t
  -> 'a Eliom_client_value.t
  = "%identity"
]

[%%server.start]

type uri = Eliom_content_xml.Xml.uri
type +'a elt = 'a Eliom_content_html_raw.elt
type +'a attrib = 'a Eliom_content_html_raw.attrib
type 'a form_param = 'a Eliom_form.param
module F = Eliom_content_html_f
module D = Eliom_content_html_d
module R = Eliom_content_html_r

module Id = struct
  type 'a id = string (* FIXME invariant type parameter ? *)
  let new_elt_id: ?global:bool -> unit -> 'a id =
    fun ?(global=true) () -> Eliom_content_xml.Xml.make_node_name ~global ()
  let create_named_elt ~(id : 'a id) elt =
    D.tot (Eliom_content_xml.Xml.make_process_node ~id (D.toelt elt))
  let create_global_elt elt =
    D.tot (Eliom_content_xml.Xml.make_process_node (D.toelt elt))
  let create_request_elt ?reset elt =
    D.tot (Eliom_content_xml.Xml.make_request_node ?reset (D.toelt elt))
  let have_id name elt =
    Eliom_content_xml.Xml.get_node_id (D.toelt elt) =
    Eliom_content_xml.Xml.ProcessId name
end

module Custom_data = struct

  type 'a t = {
    name : string;
    to_string : 'a -> string;
    of_string : string -> 'a;
    default : 'a option;
  }

  let create ~name ?default ~to_string ~of_string () =
    { name ; of_string ; to_string; default }

  let create_json ~name ?default typ =
    { name ;
      of_string = Eliom_lib.of_json ~typ ;
      to_string = Eliom_lib.to_json ~typ;
      default }

  let attrib custom_data value =
    F.a_user_data
      custom_data.name
      (custom_data.to_string value)
end

module Printer = Xml_print.Make_typed_fmt(Eliom_content_xml.Xml)(F)

module C = struct
  let node ?(init=D.Unsafe.node "span" []) x =
    let dummy_elt = D.toelt init in
    (* We need to box / unbox the client_value to convince eliom it's not polymorphic *)
    let client_boxed : boxed Eliom_client_value.t = boxed x in
    let _ = [%client (
        let dummy_dom =
          To_dom.of_element
            (D.tot ~%((dummy_elt : Eliom_content_xml.Xml.elt))) in
        let client_boxed = ~%client_boxed in
        let real = To_dom.of_element (unboxed client_boxed) in
        Js.Opt.iter
          (dummy_dom##.parentNode)
          (fun parent -> Dom.replaceChild parent real dummy_dom)
      : unit)] in
    init

(*XXXXXXXXXXXXXX
  let attr ?init x =
    Eliom_content_html_raw.F.to_attrib
      (Eliom_content_xml.Xml.client_attrib ?init x])
*)
end
