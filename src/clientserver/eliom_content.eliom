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


{client{

  include Eliom_content_

  open Html5
  open Html5.F

  let force_link = ()

}}




{server{

module type Forms = "sigs/eliom_forms.mli"

module Xml = Eliom_content_.Xml

module Xml_react = struct

  module Xmld = Eliom_content_core.Xmld

  type 'a wrap = 'a React.signal client_value
  type econtent = Xmld.econtent
  type ename = Xmld.ename
  type elt = Xmld.elt
  type attrib = Xmld.attrib
  type event_handler = Xml.event_handler
  type aname = Xmld.aname
  type uri = Xmld.uri

  let empty = Xmld.empty
  let comment = Xmld.comment
  let entity = Xmld.entity
  let leaf = Xmld.leaf

  let node ?a name
      (children_signal_client_value
       : 'a list React.signal client_value) =
    let n = Xmld.node ?a name [] in
    let _ = {unit{
        ignore (React.S.map
                  (fun l -> Html5.Manip.replaceChildren
                      (Html5.F.tot %((n : elt)))
                      (Html5.F.totl l))
                %((children_signal_client_value : elt list React.signal client_value)))
      }}
    in
    n

  let lazy_node ?a name children_signal_client_value =
    let n = Xmld.lazy_node
        ?a name (Eliom_lazy.from_val [])
    in
    let _ = {unit{
        ignore (React.S.map
                  (fun l ->
                     Html5.Manip.replaceChildren
                       (Html5.F.tot %((n : elt)))
                       (Html5.F.totl l))
                %((children_signal_client_value : elt list React.signal client_value)))
      }}
    in
    n

  let pcdata signal_client_value =
    let n = Xmld.node "span" [] in
    let _ = {unit{
        let n = To_dom.of_element (Html5.F.tot %((n : elt))) in
        let current = ref n in
        ignore (React.S.map
                  (fun s ->
                     let n3 = To_dom.of_element (pcdata s) in
                     Js.Opt.iter
                       ((!current)##parentNode)
                       (fun parent -> parent##replaceChild(n3, !current) );
                     current := n3
                  )
                  %((signal_client_value : string React.signal client_value)))
      }}
    in
    n

  let encodedpcdata = pcdata


  let cdata_style = Xmld.cdata_style
  let cdata_script = Xmld.cdata_script
  let cdata = Xmld.cdata
  let uris_attrib n v = Xmld.uris_attrib n []
  let uri_attrib n v = Xmld.uri_attrib n (Xmld.uri_of_string "")
  let event_handler_attrib n v = failwith "NI3"
  let comma_sep_attrib n v = Xmld.comma_sep_attrib n []
  let space_sep_attrib n v = Xmld.space_sep_attrib n []
  let string_attrib n v  = Xmld.string_attrib n ""
  let int_attrib n v = Xmld.int_attrib n 0
  let float_attrib n v = Xmld.float_attrib n 0.0
  let uri_of_string = Xmld.uri_of_string
  let string_of_uri = Xmld.string_of_uri

  let make_request_node = Xmld.make_request_node
  let make = Xmld.make
  let make_lazy = Xmld.make_lazy

end

module W_react = struct
  type 'a t = 'a React.signal Eliom_pervasives.client_value
(****!!!! client values cannot be polymorphic :'( ****)
  let return x =
    Obj.magic {unit React.signal{ Lwt_react.S.return (Obj.magic %((Obj.magic x : unit))) }}
  let bind x f =
    Obj.magic {unit React.signal{ Lwt_react.S.bind (Obj.magic %((Obj.magic x : unit))) (Obj.magic %((Obj.magic f : unit))) }}
  let fmap x f = Obj.magic {unit React.signal{ React.S.map (Obj.magic %((Obj.magic x : unit))) (Obj.magic %((Obj.magic f : unit))) }}
  let fmap2 x y f = Obj.magic {unit React.signal{ React.S.l2 (Obj.magic %((Obj.magic x : unit))) (Obj.magic %((Obj.magic y : unit))) (Obj.magic %((Obj.magic f : unit))) }}
  let fmap3 x y z f = Obj.magic {unit React.signal{ React.S.l3 (Obj.magic %((Obj.magic x : unit))) (Obj.magic %((Obj.magic y : unit))) (Obj.magic %((Obj.magic z : unit))) (Obj.magic %((Obj.magic f : unit))) }}
  let fmap4 x y z t f = Obj.magic {unit React.signal{ React.S.l4 (Obj.magic %((Obj.magic x : unit)))  (Obj.magic %((Obj.magic y : unit))) (Obj.magic %((Obj.magic z : unit))) (Obj.magic %((Obj.magic t : unit))) (Obj.magic %((Obj.magic f : unit)))}}
  let fmap5 x y z t u f = Obj.magic {unit React.signal{ React.S.l5 (Obj.magic %((Obj.magic x : unit)))  (Obj.magic %((Obj.magic y : unit))) (Obj.magic %((Obj.magic z : unit))) (Obj.magic %((Obj.magic t : unit))) (Obj.magic %((Obj.magic u : unit))) (Obj.magic %((Obj.magic f : unit)))}}
(*****!!!!*****)
end

module Svg = struct
  include Eliom_content_.Svg

  module R = struct

    module Raw = Svg_f.MakeWrapped
        (W_react)
        (Xml_react)

    include Raw

    module Eliom_xml = Eliom_content_.Xml

    let a_onabort ev = Raw.a_onabort (Eliom_xml.event_handler ev)
    let a_onclick
        (ev : (Dom_html.mouseEvent Js.t -> unit) Eliom_lib.client_value) =
      (* Typed by the syntax extension. *)
      Raw.a_onclick (Eliom_xml.event_handler (Obj.magic ev))
    let a_onmousedown
        (ev : (Dom_html.mouseEvent Js.t -> unit) Eliom_lib.client_value) =
      (* Typed by the syntax extension. *)
      Raw.a_onmousedown (Eliom_xml.event_handler (Obj.magic ev))
    let a_onmouseup
        (ev : (Dom_html.mouseEvent Js.t -> unit) Eliom_lib.client_value) =
      (* Typed by the syntax extension. *)
      Raw.a_onmouseup (Eliom_xml.event_handler (Obj.magic ev))
    let a_onmouseover
        (ev : (Dom_html.mouseEvent Js.t -> unit) Eliom_lib.client_value) =
      (* Typed by the syntax extension. *)
      Raw.a_onmouseover (Eliom_xml.event_handler (Obj.magic ev))
    let a_onmousemove
        (ev : (Dom_html.mouseEvent Js.t -> unit) Eliom_lib.client_value) =
      (* Typed by the syntax extension. *)
      Raw.a_onmousemove (Eliom_xml.event_handler (Obj.magic ev))

    let a_onmouseout
        (ev : (Dom_html.mouseEvent Js.t -> unit) Eliom_lib.client_value) =
      (* Typed by the syntax extension. *)
      Raw.a_onmouseout (Eliom_xml.event_handler (Obj.magic ev))

    let a_onscroll ev = a_onscroll (Eliom_xml.event_handler ev)
    let a_onload ev = a_onload (Eliom_xml.event_handler ev)
    let a_onresize ev = a_onresize (Eliom_xml.event_handler ev)

  end

end

module Html_text = Eliom_content_.Html_text

module Html5 = struct

  include Eliom_content_.Html5

  (** On server side, module R generates client side reactive nodes,
     that is, nodes that react to client side reactive signals. *)
  (*VVV Missing: Eliom forms *)
  module R = struct

    module Raw = Html5_f.MakeWrapped(W_react)(Xml_react)(Svg.R.Raw)

    include Raw

    type ('a, 'b, 'c) lazy_plus =
      ?a: (('a attrib) list) -> 'b elt Eliom_lazy.request -> ('b elt) list Eliom_lazy.request -> 'c elt

    let lazy_form ?(a = []) elt1 elts =
      failwith "NI4"
      (* tot (Xml'.lazy_node ~a:(to_xmlattribs a) "form" *)
      (*        (Eliom_lazy.from_fun *)
      (*           (fun () -> *)
      (*              toelt (Eliom_lazy.force elt1) *)
      (*              :: toeltl (Eliom_lazy.force elts)))) *)

    module Eliom_xml = Eliom_content_.Xml
    let a_onabort ev = Raw.a_onabort (Eliom_xml.event_handler ev)
    let a_onafterprint ev = Raw.a_onafterprint (Eliom_xml.event_handler ev)
    let a_onbeforeprint ev = Raw.a_onbeforeprint (Eliom_xml.event_handler ev)
    let a_onbeforeunload ev = Raw.a_onbeforeunload (Eliom_xml.event_handler ev)
    let a_onblur ev = Raw.a_onblur (Eliom_xml.event_handler ev)
    let a_oncanplay ev = Raw.a_oncanplay (Eliom_xml.event_handler ev)
    let a_oncanplaythrough ev = Raw.a_oncanplaythrough (Eliom_xml.event_handler ev)
    let a_onchange ev = Raw.a_onchange (Eliom_xml.event_handler ev)
    let a_onclick (ev : (Dom_html.mouseEvent Js.t -> unit) Eliom_lib.client_value) =
      Raw.a_onclick (Eliom_xml.event_handler (Obj.magic ev)) (* Typed by the syntax extension. *)
    let a_oncontextmenu (ev : (Dom_html.mouseEvent Js.t -> unit) Eliom_lib.client_value) =
      Raw.a_oncontextmenu (Eliom_xml.event_handler (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_ondblclick (ev : (Dom_html.mouseEvent Js.t -> unit) Eliom_lib.client_value) =
      Raw.a_ondblclick (Eliom_xml.event_handler (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_ondrag (ev : (Dom_html.mouseEvent Js.t -> unit) Eliom_lib.client_value) =
      Raw.a_ondrag (Eliom_xml.event_handler (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_ondragend (ev : (Dom_html.mouseEvent Js.t -> unit) Eliom_lib.client_value) =
      Raw.a_ondragend (Eliom_xml.event_handler (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_ondragenter (ev : (Dom_html.mouseEvent Js.t -> unit) Eliom_lib.client_value) =
      Raw.a_ondragenter (Eliom_xml.event_handler (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_ondragleave (ev : (Dom_html.mouseEvent Js.t -> unit) Eliom_lib.client_value) =
      Raw.a_ondragleave (Eliom_xml.event_handler (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_ondragover (ev : (Dom_html.mouseEvent Js.t -> unit) Eliom_lib.client_value) =
      Raw.a_ondragover (Eliom_xml.event_handler (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_ondragstart (ev : (Dom_html.mouseEvent Js.t -> unit) Eliom_lib.client_value) =
      Raw.a_ondragstart (Eliom_xml.event_handler (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_ondrop (ev : (Dom_html.mouseEvent Js.t -> unit) Eliom_lib.client_value) =
      Raw.a_ondrop (Eliom_xml.event_handler (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_ondurationchange ev = Raw.a_ondurationchange (Eliom_xml.event_handler ev)
    let a_onemptied ev = Raw.a_onemptied (Eliom_xml.event_handler ev)
    let a_onended ev = Raw.a_onended (Eliom_xml.event_handler ev)
    let a_onerror ev = Raw.a_onerror (Eliom_xml.event_handler ev)
    let a_onfocus ev = Raw.a_onfocus (Eliom_xml.event_handler ev)
    let a_onformchange ev = Raw.a_onformchange (Eliom_xml.event_handler ev)
    let a_onforminput ev = Raw.a_onforminput (Eliom_xml.event_handler ev)
    let a_onhashchange ev = Raw.a_onhashchange (Eliom_xml.event_handler ev)
    let a_oninput ev = Raw.a_oninput (Eliom_xml.event_handler ev)
    let a_oninvalid ev = Raw.a_oninvalid (Eliom_xml.event_handler ev)
    let a_onmousedown (ev : (Dom_html.mouseEvent Js.t -> unit) Eliom_lib.client_value) =
      Raw.a_onmousedown (Eliom_xml.event_handler (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_onmouseup (ev : (Dom_html.mouseEvent Js.t -> unit) Eliom_lib.client_value) =
      Raw.a_onmouseup (Eliom_xml.event_handler (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_onmouseover (ev : (Dom_html.mouseEvent Js.t -> unit) Eliom_lib.client_value) =
      Raw.a_onmouseover (Eliom_xml.event_handler (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_onmousemove (ev : (Dom_html.mouseEvent Js.t -> unit) Eliom_lib.client_value) =
      Raw.a_onmousemove (Eliom_xml.event_handler (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_onmouseout (ev : (Dom_html.mouseEvent Js.t -> unit) Eliom_lib.client_value) =
      Raw.a_onmouseout (Eliom_xml.event_handler (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_onmousewheel ev = Raw.a_onmousewheel (Eliom_xml.event_handler ev)
    let a_onoffline ev = Raw.a_onoffline (Eliom_xml.event_handler ev)
    let a_ononline ev = Raw.a_ononline (Eliom_xml.event_handler ev)
    let a_onpause ev = Raw.a_onpause (Eliom_xml.event_handler ev)
    let a_onplay ev = Raw.a_onplay (Eliom_xml.event_handler ev)
    let a_onplaying ev = Raw.a_onplaying (Eliom_xml.event_handler ev)
    let a_onpagehide ev = Raw.a_onpagehide (Eliom_xml.event_handler ev)
    let a_onpageshow ev = Raw.a_onpageshow (Eliom_xml.event_handler ev)
    let a_onpopstate ev = Raw.a_onpopstate (Eliom_xml.event_handler ev)
    let a_onprogress ev = Raw.a_onprogress (Eliom_xml.event_handler ev)
    let a_onratechange ev = Raw.a_onratechange (Eliom_xml.event_handler ev)
    let a_onreadystatechange ev = Raw.a_onreadystatechange (Eliom_xml.event_handler ev)
    let a_onredo ev = Raw.a_onredo (Eliom_xml.event_handler ev)
    let a_onresize ev = Raw.a_onresize (Eliom_xml.event_handler ev)
    let a_onscroll ev = Raw.a_onscroll (Eliom_xml.event_handler ev)
    let a_onseeked ev = Raw.a_onseeked (Eliom_xml.event_handler ev)
    let a_onseeking ev = Raw.a_onseeking (Eliom_xml.event_handler ev)
    let a_onselect ev = Raw.a_onselect (Eliom_xml.event_handler ev)
    let a_onshow ev = Raw.a_onshow (Eliom_xml.event_handler ev)
    let a_onstalled ev = Raw.a_onstalled (Eliom_xml.event_handler ev)
    let a_onstorage ev = Raw.a_onstorage (Eliom_xml.event_handler ev)
    let a_onsubmit ev = Raw.a_onsubmit (Eliom_xml.event_handler ev)
    let a_onsuspend ev = Raw.a_onsuspend (Eliom_xml.event_handler ev)
    let a_ontimeupdate ev = Raw.a_ontimeupdate (Eliom_xml.event_handler ev)
    let a_onundo ev = Raw.a_onundo (Eliom_xml.event_handler ev)
    let a_onunload ev = Raw.a_onunload (Eliom_xml.event_handler ev)
    let a_onvolumechange ev = Raw.a_onvolumechange (Eliom_xml.event_handler ev)
    let a_onwaiting ev = Raw.a_onwaiting (Eliom_xml.event_handler ev)
    let a_onkeypress (ev : (Dom_html.keyboardEvent Js.t -> unit) Eliom_lib.client_value) =
      Raw.a_onkeypress (Eliom_xml.event_handler (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_onkeydown (ev : (Dom_html.keyboardEvent Js.t -> unit) Eliom_lib.client_value) =
      Raw.a_onkeydown (Eliom_xml.event_handler (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_onkeyup (ev : (Dom_html.keyboardEvent Js.t -> unit) Eliom_lib.client_value) =
      Raw.a_onkeyup (Eliom_xml.event_handler (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_onload ev = Raw.a_onload (Eliom_xml.event_handler ev)
    let a_onloadeddata ev = Raw.a_onloadeddata (Eliom_xml.event_handler ev)
    let a_onloadedmetadata ev = Raw.a_onloadedmetadata (Eliom_xml.event_handler ev)
    let a_onloadstart ev = Raw.a_onloadstart (Eliom_xml.event_handler ev)
    let a_onmessage ev = Raw.a_onmessage (Eliom_xml.event_handler ev)

    let node _ = failwith "NInode"

  end


end


}}
