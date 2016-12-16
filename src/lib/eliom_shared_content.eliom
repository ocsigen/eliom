(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2015
 * Vasilis Papavasileiou
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

open%shared Js_of_ocaml

[%%server
open Eliom_shared
let local_value s = React.S.value s |> Value.local
]

module Xml = struct

  module W = struct

    include React.S

    type (-'a, 'b) ft = unit -> ('a -> 'b) Eliom_shared.Value.t

    type 'a tlist = 'a ReactiveData.RList.t

    let return = const
    let append = ReactiveData.RList.concat
    let singleton = ReactiveData.RList.singleton_s
    let cons a l = append (singleton a) l
    let nil () = fst (ReactiveData.RList.create [])
    let fmap f a = React.S.map (f ()) a
    let map f l = ReactiveData.RList.map (f ()) l

  end

  type 'a wrap = 'a W.t

  type 'a list_wrap ='a W.tlist

  type uri = Eliom_content_xml.Xml.uri

  let string_of_uri () =
    [%shared  Eliom_content_xml.Xml.string_of_uri ]

  let uri_of_string () =
    [%shared  Eliom_content_xml.Xml.uri_of_string ]

  type aname = Eliom_content_xml.Xml.aname

  type ename = Eliom_content_xml.Xml.ename

  type event_handler =
    (Dom_html.event Js.t -> unit) Eliom_client_value.t

  type mouse_event_handler =
    (Dom_html.mouseEvent Js.t -> unit) Eliom_client_value.t

  type keyboard_event_handler =
    (Dom_html.keyboardEvent Js.t -> unit) Eliom_client_value.t

  type touch_event_handler =
    (Dom_html.touchEvent Js.t -> unit) Eliom_client_value.t

  (* attributes *)

  type attrib = Eliom_content_xml.Xml.attrib

  let float_attrib name s =
    let init =
      local_value s |> Eliom_content_xml.Xml.float_attrib name
    in
     [%client  Eliom_content_xml.Xml_wed.float_attrib ~%name ~%s ] |>
    Eliom_content_xml.Xml.client_attrib ~init

  let int_attrib name s =
    let init =
      local_value s |> Eliom_content_xml.Xml.int_attrib name
    in
     [%client  Eliom_content_xml.Xml_wed.int_attrib ~%name ~%s ] |>
    Eliom_content_xml.Xml.client_attrib ~init

  let string_attrib name s =
    let init =
      local_value s |> Eliom_content_xml.Xml.string_attrib name
    in
     [%client  Eliom_content_xml.Xml_wed.string_attrib ~%name ~%s ] |>
    Eliom_content_xml.Xml.client_attrib ~init

  let space_sep_attrib name s =
    let init =
      local_value s |> Eliom_content_xml.Xml.space_sep_attrib name
    in
     [%client  Eliom_content_xml.Xml_wed.space_sep_attrib ~%name ~%s ] |>
    Eliom_content_xml.Xml.client_attrib ~init

  let comma_sep_attrib name s =
    let init =
      local_value s |> Eliom_content_xml.Xml.comma_sep_attrib name
    in
     [%client  Eliom_content_xml.Xml_wed.comma_sep_attrib ~%name ~%s ] |>
    Eliom_content_xml.Xml.client_attrib ~init

  let uri_attrib name s =
    let init =
      local_value s |> Eliom_content_xml.Xml.uri_attrib name
    in
     [%client  Eliom_content_xml.Xml_wed.uri_attrib ~%name ~%s ] |>
    Eliom_content_xml.Xml.client_attrib ~init

  let uris_attrib name s =
    let init =
      local_value s |> Eliom_content_xml.Xml.uris_attrib name
    in
     [%client  Eliom_content_xml.Xml_wed.uris_attrib ~%name ~%s ] |>
    Eliom_content_xml.Xml.client_attrib ~init

  let event_handler_attrib =
    Eliom_content_xml.Xml.event_handler_attrib

  let keyboard_event_handler_attrib =
    Eliom_content_xml.Xml.keyboard_event_handler_attrib

  let touch_event_handler_attrib =
    Eliom_content_xml.Xml.touch_event_handler_attrib

  let mouse_event_handler_attrib =
    Eliom_content_xml.Xml.mouse_event_handler_attrib

  (* elements *)

  type elt = Eliom_content_xml.Xml.elt

  let empty = Eliom_content_xml.Xml.empty

  let comment = Eliom_content_xml.Xml.comment

  let name_node e =
    Eliom_content_xml.Xml.make_request_node e

  let leaf ?(a : attrib list option) name =
    Eliom_content_xml.Xml.leaf ?a name |> name_node

  let pcdata s =
    let e =
      let s = local_value s in
      Eliom_content_xml.Xml.(node "span" [pcdata s]) |> name_node
    and synced = React.S.synced s in
    let _ = [%client (
      let (>>!) = Js.Opt.iter in
      let update =
        let e = Eliom_client_core.rebuild_node' `HTML5 ~%e in
        fun x ->
          Js.Opt.case (e##.firstChild)
            (fun () ->
               Dom.appendChild e
                 (Dom_html.document##(createTextNode (Js.string x))))
            (fun e ->
               Dom.CoerceTo.text e >>! fun e ->
               e##.data := Js.string x)
      in
      if not ~%synced then update (React.S.value ~%s);
      React.S.changes ~%s |> React.E.map update |> ignore;
    : unit)] in
    e

  let encodedpcdata = pcdata

  let cdata = Eliom_content_xml.Xml.cdata

  let cdata_script = Eliom_content_xml.Xml.cdata_script

  let cdata_style = Eliom_content_xml.Xml.cdata_style

  let entity = Eliom_content_xml.Xml.entity

  let node_aux ns ?a name l =
    let e =
      ReactiveData.RList.value l |>
      Value.local |>
      Eliom_content_xml.Xml.node ?a name |>
      name_node
    in
    let _ = [%client (
      let f = Eliom_client_core.rebuild_node' ~%ns in
      let e = f ~%e
      and l = ReactiveData.RList.map f ~%l in
      Js_of_ocaml_tyxml.Tyxml_js.Util.update_children e l
    : unit)] in
    e

  let node = node_aux `HTML5

end

[%%shared
module Raw_wrapped_functions_svg =
  Svg_f.Wrapped_functions(Eliom_content_xml.Xml)
]

module Svg = struct

  module Wrapped_functions :

    Svg_sigs.Wrapped_functions with module Xml = Xml =

  struct

    module Xml = Xml

    let string_of_alignment_baseline () =
      [%shared  Raw_wrapped_functions_svg.string_of_alignment_baseline ]

    let string_of_big_variant () =
      [%shared  Raw_wrapped_functions_svg.string_of_big_variant ]

    let string_of_bool () =
      [%shared  Raw_wrapped_functions_svg.string_of_bool ]

    let string_of_coords () =
      [%shared  Raw_wrapped_functions_svg.string_of_coords ]

    let string_of_dominant_baseline () =
      [%shared  Raw_wrapped_functions_svg.string_of_dominant_baseline ]

    let string_of_fourfloats () =
      [%shared  Raw_wrapped_functions_svg.string_of_fourfloats ]

    let string_of_in_value () =
      [%shared  Raw_wrapped_functions_svg.string_of_in_value ]

    let string_of_int () =
      [%shared  Raw_wrapped_functions_svg.string_of_int ]

    let string_of_length () =
      [%shared  Raw_wrapped_functions_svg.string_of_length ]

    let string_of_lengths () =
      [%shared  Raw_wrapped_functions_svg.string_of_lengths ]

    let string_of_number () =
      [%shared  Raw_wrapped_functions_svg.string_of_number ]

    let string_of_number_optional_number () =
      [%shared
         Raw_wrapped_functions_svg.string_of_number_optional_number ]

    let string_of_numbers () =
      [%shared  Raw_wrapped_functions_svg.string_of_numbers ]

    let string_of_numbers_semicolon () =
      [%shared  Raw_wrapped_functions_svg.string_of_numbers_semicolon ]

    let string_of_offset () =
      [%shared  Raw_wrapped_functions_svg.string_of_offset ]

    let string_of_orient () =
      [%shared  Raw_wrapped_functions_svg.string_of_orient ]

    let string_of_paint () =
      [%shared  Raw_wrapped_functions_svg.string_of_paint ]

    let string_of_strokedasharray () =
      [%shared  Raw_wrapped_functions_svg.string_of_strokedasharray ]

    let string_of_transform () =
      [%shared  Raw_wrapped_functions_svg.string_of_transform ]

    let string_of_transforms () =
      [%shared  Raw_wrapped_functions_svg.string_of_transforms ]

  end

  module Xml = struct
    include Xml
    let node = node_aux `SVG
  end

  module R = struct

    (* Same as the HTML version, with Html -> SVG and `HTML5 ->
       `SVG. Hard to functorize. Make sure they stay synced! *)
    let node s =
      let e =
        local_value s |>
        Eliom_content_xml.Xml.make_request_node ~reset:false
      and synced = React.S.synced s in
      let _ = [%client (
        let s =
          Eliom_shared.React.S.map
            (fun s ->
              Eliom_content_xml.Xml.make_request_node ~reset:false s |>
                Eliom_client_core.rebuild_node' `SVG)
            ~%s
        in
        let f =
          let replace e' e =
            let f p = Dom.replaceChild p e' e in
            Js.Opt.iter (e##.parentNode) f |> ignore
          and e = Eliom_client_core.rebuild_node' `SVG ~%e in
          fun e' ->
            replace e' e;
            React.S.diff replace s |> ignore
        in
        if ~%synced then
          React.(S.changes s |> E.once |> E.map f) |> ignore
        else
          f (React.S.value s) |> ignore
      : unit)] in
      e

    include Svg_f.Make_with_wrapped_functions(Xml)(Wrapped_functions)

  end

end ;;

[%%shared
module Raw_wrapped_functions =
  Html_f.Wrapped_functions(Eliom_content_xml.Xml)
]

module Html = struct

  module Wrapped_functions :

    Html_sigs.Wrapped_functions with module Xml = Xml =

  struct

    module Xml = Xml

    type image_candidate =
      [ `Url of Xml.uri
      | `Url_width of Xml.uri * Html_types.number
      | `Url_pixel of Xml.uri * Html_types.float_number ]

    let onoff_of_bool () =
      [%shared  Raw_wrapped_functions.onoff_of_bool ]

    let string_of_big_variant () =
      [%shared  Raw_wrapped_functions.string_of_big_variant ]

    let string_of_bool () =
      [%shared  Raw_wrapped_functions.string_of_bool ]

    let string_of_character () =
      [%shared  Raw_wrapped_functions.string_of_character ]

    let string_of_input_type () =
      [%shared  Raw_wrapped_functions.string_of_input_type ]

    let string_of_linktypes () =
      [%shared  Raw_wrapped_functions.string_of_linktypes ]

    let string_of_mediadesc () =
      [%shared  Raw_wrapped_functions.string_of_mediadesc ]

    let string_of_number_or_datetime () =
      [%shared  Raw_wrapped_functions.string_of_number_or_datetime ]

    let string_of_numbers () =
      [%shared  Raw_wrapped_functions.string_of_numbers ]

    let string_of_sandbox () =
      [%shared  Raw_wrapped_functions.string_of_sandbox ]

    let string_of_sizes () =
      [%shared  Raw_wrapped_functions.string_of_sizes ]

    let string_of_srcset () =
      [%shared  Raw_wrapped_functions.string_of_srcset ]

    let string_of_step () =
      [%shared  Raw_wrapped_functions.string_of_step ]

    let unoption_string () =
      [%shared  Raw_wrapped_functions.unoption_string ]

  end

  module R (Svg : Svg_sigs.T
            with type 'a Xml.W.t = 'a Eliom_shared.React.S.t
             and type 'a Xml.W.tlist = 'a Eliom_shared.ReactiveData.RList.t
             and type ('a, 'b) Xml.W.ft =
                   unit -> ('a -> 'b) Eliom_shared.Value.t
             and type Xml.uri = Eliom_content_xml.Xml.uri
             and type Xml.event_handler =
                   (Dom_html.event Js.t -> unit) Eliom_client_value.t
             and type Xml.mouse_event_handler =
                   (Dom_html.mouseEvent Js.t -> unit) Eliom_client_value.t
             and type Xml.keyboard_event_handler =
                   (Dom_html.keyboardEvent Js.t -> unit) Eliom_client_value.t
             and type Xml.touch_event_handler =
                   (Dom_html.touchEvent Js.t -> unit) Eliom_client_value.t
             and type Xml.elt = Eliom_content_xml.Xml.elt
             and type Xml.attrib = Eliom_content_xml.Xml.attrib) = struct

    (* Same as the SVG version, with Svg -> Html and `SVG ->
       `HTML5. Hard to functorize. Make sure they stay synced! *)
    let node s =
      let e =
        local_value s |>
        Eliom_content_xml.Xml.make_request_node ~reset:false
      and synced = React.S.synced s in
      let _ = [%client (
        let s =
          Eliom_shared.React.S.map
            (fun s ->
              Eliom_content_xml.Xml.make_request_node ~reset:false s |>
                Eliom_client_core.rebuild_node' `HTML5)
            ~%s
        in
        let f =
          let replace e' e =
            let f p = Dom.replaceChild p e' e in
            Js.Opt.iter (e##.parentNode) f |> ignore
          and e = Eliom_client_core.rebuild_node' `HTML5 ~%e in
          fun e' ->
            replace e' e;
            React.S.diff replace s |> ignore
        in
        if ~%synced then
          React.(S.changes s |> E.once |> E.map f) |> ignore
        else
          f (React.S.value s) |> ignore
      : unit)] in
      e

    let filter_attrib a s =
      let init = (*ZZZZZZ if local_value s then Some (Eliom_content_html_raw.Unsafe.to_xmlattrib a) else *) None
      and c =
        [%client
            Eliom_content_html_raw.Unsafe.to_xmlattrib
            (Eliom_content_html_r.filter_attrib ~%a ~%s) ]
      in
      Eliom_content_xml.Xml.client_attrib ?init c

    include
      Html_f.Make_with_wrapped_functions(Xml)(Wrapped_functions)(Svg)

    let pcdata x = txt x |> Unsafe.coerce_elt

  end

end
