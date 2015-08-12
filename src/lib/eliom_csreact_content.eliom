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

{client{

module Xml = struct

  type elt =  Eliom_content_core.Xml.elt

end

}}

module Xml = struct

  module W = struct

    include Eliom_csreact.SharedReact.S

    type (-'a, 'b) ft = ('a -> 'b) Eliom_lib.shared_value

    let return = const

    let append = Eliom_csreact.SharedReactiveData.RList.concat

    let cons a l =
      Eliom_csreact.SharedReactiveData.RList.(concat (singleton_s a) l)

    let singleton = Eliom_csreact.SharedReactiveData.RList.singleton_s

    let nil () =
      Eliom_csreact.SharedReactiveData.RList.make [] |> fst

    let fmap f a =
      Eliom_csreact.SharedReact.S.map f a

    type 'a tlist = 'a Eliom_csreact.SharedReactiveData.RList.t

    let map _ _ =
      if true then assert false;
      Eliom_csreact.SharedReactiveData.RList.make [] |> fst

  end

  type 'a wrap = 'a W.t

  type 'a list_wrap ='a W.tlist

  type uri = Eliom_content_core.Xml.uri

  let string_of_uri = Eliom_content_core.Xml.string_of_uri

  let uri_of_string = Eliom_content_core.Xml.uri_of_string

  type aname = Eliom_content_core.Xml.aname

  type ename = Eliom_content_core.Xml.ename

  type event_handler =
    (Dom_html.event Js.t -> unit) client_value

  type mouse_event_handler =
    (Dom_html.mouseEvent Js.t -> unit) client_value

  type keyboard_event_handler =
    (Dom_html.keyboardEvent Js.t -> unit) client_value

  (* attributes *)

  type attrib = Eliom_content_core.Xml.attrib

  let local_value s =
    Eliom_csreact.SharedReact.S.value s |>
    Eliom_lib.Shared.local

  let float_attrib name s =
    let init =
      local_value s |> Eliom_content_core.Xml.float_attrib name
    in
    {{ Eliom_content_core.Xml_wed.float_attrib %name %s }} |>
    Eliom_content_core.Xml.client_attrib ~init

  let int_attrib name s =
    let init =
      local_value s |> Eliom_content_core.Xml.int_attrib name
    in
    {{ Eliom_content_core.Xml_wed.int_attrib %name %s }} |>
    Eliom_content_core.Xml.client_attrib ~init

  let string_attrib name s =
    let init =
      local_value s |> Eliom_content_core.Xml.string_attrib name
    in
    {{ Eliom_content_core.Xml_wed.string_attrib %name %s }} |>
    Eliom_content_core.Xml.client_attrib ~init

  let space_sep_attrib name s =
    let init =
      local_value s |> Eliom_content_core.Xml.space_sep_attrib name
    in
    {{ Eliom_content_core.Xml_wed.space_sep_attrib %name %s }} |>
    Eliom_content_core.Xml.client_attrib ~init

  let comma_sep_attrib name s =
    let init =
      local_value s |> Eliom_content_core.Xml.comma_sep_attrib name
    in
    {{ Eliom_content_core.Xml_wed.comma_sep_attrib %name %s }} |>
    Eliom_content_core.Xml.client_attrib ~init

  let uri_attrib name s =
    let init =
      local_value s |> Eliom_content_core.Xml.uri_attrib name
    in
    {{ Eliom_content_core.Xml_wed.uri_attrib %name %s }} |>
    Eliom_content_core.Xml.client_attrib ~init

  let uris_attrib name s =
    let init =
      local_value s |> Eliom_content_core.Xml.uris_attrib name
    in
    {{ Eliom_content_core.Xml_wed.uris_attrib %name %s }} |>
    Eliom_content_core.Xml.client_attrib ~init

  let event_handler_attrib =
    Eliom_content_core.Xml.event_handler_attrib

  let keyboard_event_handler_attrib =
    Eliom_content_core.Xml.keyboard_event_handler_attrib

  let mouse_event_handler_attrib =
    Eliom_content_core.Xml.mouse_event_handler_attrib

  (* elements *)

  type elt = Eliom_content_core.Xml.elt

  let empty = Eliom_content_core.Xml.empty

  let comment = Eliom_content_core.Xml.comment

  let name_node e =
    let id =
      String.sub
        (Ocsigen_lib.make_cryptographic_safe_string ())
        0 12
    in
    Eliom_content_core.Xml.make_process_node ~id e

  let leaf ?(a : attrib list option) name =
    Eliom_content_core.Xml.leaf ?a name |> name_node

  let pcdata s =
    let e =
      let s =
        Eliom_csreact.SharedReact.S.value s |>
        Eliom_lib.Shared.local
      in
      Eliom_content_core.Xml.(node "span" [pcdata s]) |> name_node
    in
    let _ = {unit{
      let (>>!) = Js.Opt.iter in
      let e = Eliom_client.rebuild_node' `HTML5 %e in
      e##firstChild >>! fun e ->
      Dom.CoerceTo.text e >>! fun e ->
      React.E.map
        (fun x -> e##data <- Js.string x)
        (React.S.changes %s) |>
      ignore;
    }} in
    e

  let encodedpcdata = pcdata

  let cdata = Eliom_content_core.Xml.cdata

  let cdata_script = Eliom_content_core.Xml.cdata_script

  let cdata_style = Eliom_content_core.Xml.cdata_style

  let entity = Eliom_content_core.Xml.entity

  let node ?a name l =
    let e =
      Eliom_csreact.SharedReactiveData.RList.value l |>
      Eliom_lib.Shared.local |>
      Eliom_content_core.Xml.node ?a name |>
      name_node
    in
    let _ = {unit{
      let f _ =
        let f = Eliom_client.rebuild_node' `HTML5 in
        let e = f %e
        and l = Eliom_csreact.SharedReactiveData.RList.map f %l in
        Tyxml_js.Util.update_children e l
      and e = ReactiveData.RList.event %l |> React.E.once in
      React.E.map f e |> ignore
    }} in
    e

end

module Svg = struct

  module Conv :

    Svg_sigs.Conv with type (-'a, 'b) ft = ('a, 'b) Xml.W.ft =

  struct

    type (-'a, 'b) ft = ('a, 'b) Xml.W.ft

    let string_of_alignment_baseline =
      Eliom_lib.create_shared_value
        Svg_f.Conv.string_of_alignment_baseline
        {{Svg_f.Conv.string_of_alignment_baseline}}

    let string_of_big_variant =
      Eliom_lib.create_shared_value
        Svg_f.Conv.string_of_big_variant
        {{Svg_f.Conv.string_of_big_variant}}

    let string_of_bool =
      Eliom_lib.create_shared_value
        Svg_f.Conv.string_of_bool
        {{Svg_f.Conv.string_of_bool}}

    let string_of_coords =
      Eliom_lib.create_shared_value
        Svg_f.Conv.string_of_coords
        {{Svg_f.Conv.string_of_coords}}

    let string_of_dominant_baseline =
      Eliom_lib.create_shared_value
        Svg_f.Conv.string_of_dominant_baseline
        {{Svg_f.Conv.string_of_dominant_baseline}}

    let string_of_fourfloats =
      Eliom_lib.create_shared_value
        Svg_f.Conv.string_of_fourfloats
        {{Svg_f.Conv.string_of_fourfloats}}

    let string_of_in_value =
      Eliom_lib.create_shared_value
        Svg_f.Conv.string_of_in_value
        {{Svg_f.Conv.string_of_in_value}}

    let string_of_int =
      Eliom_lib.create_shared_value
        Svg_f.Conv.string_of_int
        {{Svg_f.Conv.string_of_int}}

    let string_of_length =
      Eliom_lib.create_shared_value
        Svg_f.Conv.string_of_length
        {{Svg_f.Conv.string_of_length}}

    let string_of_lengths =
      Eliom_lib.create_shared_value
        Svg_f.Conv.string_of_lengths
        {{Svg_f.Conv.string_of_lengths}}

    let string_of_number =
      Eliom_lib.create_shared_value
        Svg_f.Conv.string_of_number
        {{Svg_f.Conv.string_of_number}}

    let string_of_number_optional_number =
      Eliom_lib.create_shared_value
        Svg_f.Conv.string_of_number_optional_number
        {{Svg_f.Conv.string_of_number_optional_number}}

    let string_of_numbers =
      Eliom_lib.create_shared_value
        Svg_f.Conv.string_of_numbers
        {{Svg_f.Conv.string_of_numbers}}

    let string_of_numbers_semicolon =
      Eliom_lib.create_shared_value
        Svg_f.Conv.string_of_numbers_semicolon
        {{Svg_f.Conv.string_of_numbers_semicolon}}

    let string_of_offset =
      Eliom_lib.create_shared_value
        Svg_f.Conv.string_of_offset
        {{Svg_f.Conv.string_of_offset}}

    let string_of_orient =
      Eliom_lib.create_shared_value
        Svg_f.Conv.string_of_orient
        {{Svg_f.Conv.string_of_orient}}

    let string_of_paint =
      Eliom_lib.create_shared_value
        Svg_f.Conv.string_of_paint
        {{Svg_f.Conv.string_of_paint}}

    let string_of_strokedasharray =
      Eliom_lib.create_shared_value
        Svg_f.Conv.string_of_strokedasharray
        {{Svg_f.Conv.string_of_strokedasharray}}

    let string_of_transform =
      Eliom_lib.create_shared_value
        Svg_f.Conv.string_of_transform
        {{Svg_f.Conv.string_of_transform}}

    let string_of_transforms =
      Eliom_lib.create_shared_value
        Svg_f.Conv.string_of_transforms
        {{Svg_f.Conv.string_of_transforms}}

  end

  module R = Eliom_content_core.Svg.Make(Xml)(Conv)

end

module Html5 = struct

  module Conv :

    Html5_sigs.Conv with type (-'a, 'b) ft = ('a, 'b) Xml.W.ft =

  struct

    type (-'a, 'b) ft = ('a, 'b) Xml.W.ft

    let string_of_big_variant =
      Eliom_lib.create_shared_value
        Html5_f.Conv.string_of_big_variant
        {{Html5_f.Conv.string_of_big_variant}}

    let string_of_bool =
      Eliom_lib.create_shared_value
        Html5_f.Conv.string_of_bool
        {{Html5_f.Conv.string_of_bool}}

    let string_of_character =
      Eliom_lib.create_shared_value
        Html5_f.Conv.string_of_character
        {{Html5_f.Conv.string_of_character}}

    let string_of_input_type =
      Eliom_lib.create_shared_value
        Html5_f.Conv.string_of_input_type
        {{Html5_f.Conv.string_of_input_type}}

    let string_of_linktypes =
      Eliom_lib.create_shared_value
        Html5_f.Conv.string_of_linktypes
        {{Html5_f.Conv.string_of_linktypes}}

    let string_of_mediadesc =
      Eliom_lib.create_shared_value
        Html5_f.Conv.string_of_mediadesc
        {{Html5_f.Conv.string_of_mediadesc}}

    let string_of_multilength =
      Eliom_lib.create_shared_value
        Html5_f.Conv.string_of_multilength
        {{Html5_f.Conv.string_of_multilength}}

    let string_of_multilengths =
      Eliom_lib.create_shared_value
        Html5_f.Conv.string_of_multilengths
        {{Html5_f.Conv.string_of_multilengths}}

    let string_of_numbers =
      Eliom_lib.create_shared_value
        Html5_f.Conv.string_of_numbers
        {{Html5_f.Conv.string_of_numbers}}

    let string_of_sandbox =
      Eliom_lib.create_shared_value
        Html5_f.Conv.string_of_sandbox
        {{Html5_f.Conv.string_of_sandbox}}

    let string_of_sizes =
      Eliom_lib.create_shared_value
        Html5_f.Conv.string_of_sizes
        {{Html5_f.Conv.string_of_sizes}}

    let string_of_step =
      Eliom_lib.create_shared_value
        Html5_f.Conv.string_of_step
        {{Html5_f.Conv.string_of_step}}

    let unoption_string =
      Eliom_lib.create_shared_value
        Html5_f.Conv.unoption_string
        {{Html5_f.Conv.unoption_string}}

  end

  module R = struct
    include Eliom_content_core.Html5.Make(Xml)(Conv)(Svg.R)
    let pcdata x = pcdata x |> Unsafe.coerce_elt
  end

end
