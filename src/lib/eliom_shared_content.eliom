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

let local_value s = React.S.value s |> Value.local]

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
  type 'a list_wrap = 'a W.tlist
  type uri = Eliom_content_core.Xml.uri

  let string_of_uri () = [%shared Eliom_content_core.Xml.string_of_uri]
  let uri_of_string () = [%shared Eliom_content_core.Xml.uri_of_string]

  type aname = Eliom_content_core.Xml.aname
  type ename = Eliom_content_core.Xml.ename
  type event_handler = (Dom_html.event Js.t -> unit) Eliom_client_value.t

  type mouse_event_handler =
    (Dom_html.mouseEvent Js.t -> unit) Eliom_client_value.t

  type keyboard_event_handler =
    (Dom_html.keyboardEvent Js.t -> unit) Eliom_client_value.t

  type touch_event_handler =
    (Dom_html.touchEvent Js.t -> unit) Eliom_client_value.t

  (* attributes *)

  type attrib = Eliom_content_core.Xml.attrib

  let float_attrib name s =
    let init = local_value s |> Eliom_content_core.Xml.float_attrib name in
    [%client Eliom_content_core.Xml_wed.float_attrib ~%name ~%s]
    |> Eliom_content_core.Xml.client_attrib ~init

  let int_attrib name s =
    let init = local_value s |> Eliom_content_core.Xml.int_attrib name in
    [%client Eliom_content_core.Xml_wed.int_attrib ~%name ~%s]
    |> Eliom_content_core.Xml.client_attrib ~init

  let string_attrib name s =
    let init = local_value s |> Eliom_content_core.Xml.string_attrib name in
    [%client Eliom_content_core.Xml_wed.string_attrib ~%name ~%s]
    |> Eliom_content_core.Xml.client_attrib ~init

  let space_sep_attrib name s =
    let init = local_value s |> Eliom_content_core.Xml.space_sep_attrib name in
    [%client Eliom_content_core.Xml_wed.space_sep_attrib ~%name ~%s]
    |> Eliom_content_core.Xml.client_attrib ~init

  let comma_sep_attrib name s =
    let init = local_value s |> Eliom_content_core.Xml.comma_sep_attrib name in
    [%client Eliom_content_core.Xml_wed.comma_sep_attrib ~%name ~%s]
    |> Eliom_content_core.Xml.client_attrib ~init

  let uri_attrib name s =
    let init = local_value s |> Eliom_content_core.Xml.uri_attrib name in
    [%client Eliom_content_core.Xml_wed.uri_attrib ~%name ~%s]
    |> Eliom_content_core.Xml.client_attrib ~init

  let uris_attrib name s =
    let init = local_value s |> Eliom_content_core.Xml.uris_attrib name in
    [%client Eliom_content_core.Xml_wed.uris_attrib ~%name ~%s]
    |> Eliom_content_core.Xml.client_attrib ~init

  let event_handler_attrib = Eliom_content_core.Xml.event_handler_attrib

  let keyboard_event_handler_attrib =
    Eliom_content_core.Xml.keyboard_event_handler_attrib

  let touch_event_handler_attrib =
    Eliom_content_core.Xml.touch_event_handler_attrib

  let mouse_event_handler_attrib =
    Eliom_content_core.Xml.mouse_event_handler_attrib

  (* elements *)

  type elt = Eliom_content_core.Xml.elt

  let empty = Eliom_content_core.Xml.empty
  let comment = Eliom_content_core.Xml.comment
  let name_node e = Eliom_content_core.Xml.make_request_node e

  let leaf ?(a : attrib list option) name =
    Eliom_content_core.Xml.leaf ?a name |> name_node

  let pcdata s =
    let e =
      let s = local_value s in
      Eliom_content_core.Xml.(node "span" [pcdata s]) |> name_node
    and synced = React.S.synced s in
    let _ =
      [%client
        (let ( >>! ) = Js.Opt.iter in
         let e = Eliom_client_core.rebuild_node' `HTML5 ~%e in
         let update x =
           Js.Opt.case e##.firstChild
             (fun () ->
                Dom.appendChild e
                  Dom_html.document##(createTextNode (Js.string x)))
             (fun e -> Dom.CoerceTo.text e >>! fun e -> e##.data := Js.string x)
         in
         if not ~%synced then update (React.S.value ~%s);
         Eliom_lib.Dom_reference.retain e
           ~keep:(React.S.changes ~%s |> React.E.map update)
         : unit)]
    in
    e

  let encodedpcdata = pcdata
  let cdata = Eliom_content_core.Xml.cdata
  let cdata_script = Eliom_content_core.Xml.cdata_script
  let cdata_style = Eliom_content_core.Xml.cdata_style
  let entity = Eliom_content_core.Xml.entity

  let node_aux ns ?a name l =
    let e =
      ReactiveData.RList.value l |> Value.local
      |> Eliom_content_core.Xml.node ?a name
      |> name_node
    in
    let _ =
      [%client.unsafe
        (let f = Eliom_client_core.rebuild_node' ~%ns in
         let e = f ~%e and l = ReactiveData.RList.map f ~%l in
         Js_of_ocaml_tyxml.Tyxml_js.Util.update_children e l
         : unit)]
    in
    e

  let node = node_aux `HTML5
end

[%%shared
module Raw_wrapped_functions_svg =
  Svg_f.Wrapped_functions (Eliom_content_core.Xml)]

module Svg = struct
  module Wrapped_functions : Svg_sigs.Wrapped_functions with module Xml = Xml =
  struct
    module Xml = Xml

    let string_of_alignment_baseline () =
      ([%shared Raw_wrapped_functions_svg.string_of_alignment_baseline]
        : (Svg_types.alignment_baseline -> string) Eliom_shared.Value.t
        :> ([< Svg_types.alignment_baseline] -> string) Eliom_shared.Value.t)

    let string_of_big_variant () =
      ([%shared Raw_wrapped_functions_svg.string_of_big_variant]
        : (Svg_types.big_variant -> string) Eliom_shared.Value.t
        :> ([< Svg_types.big_variant] -> string) Eliom_shared.Value.t)

    let string_of_bool () = [%shared Raw_wrapped_functions_svg.string_of_bool]

    let string_of_coords () =
      [%shared Raw_wrapped_functions_svg.string_of_coords]

    let string_of_dominant_baseline () =
      ([%shared Raw_wrapped_functions_svg.string_of_dominant_baseline]
        : (Svg_types.dominant_baseline -> string) Eliom_shared.Value.t
        :> ([< Svg_types.dominant_baseline] -> string) Eliom_shared.Value.t)

    let string_of_fill_rule () =
      ([%shared Raw_wrapped_functions_svg.string_of_fill_rule]
        : (Svg_types.fill_rule -> string) Eliom_shared.Value.t
        :> ([< Svg_types.fill_rule] -> string) Eliom_shared.Value.t)

    let string_of_fourfloats () =
      [%shared Raw_wrapped_functions_svg.string_of_fourfloats]

    let string_of_in_value () =
      ([%shared Raw_wrapped_functions_svg.string_of_in_value]
        : (Svg_types.in_value -> string) Eliom_shared.Value.t
        :> ([< Svg_types.in_value] -> string) Eliom_shared.Value.t)

    let string_of_int () = [%shared Raw_wrapped_functions_svg.string_of_int]

    let string_of_length () =
      [%shared Raw_wrapped_functions_svg.string_of_length]

    let string_of_lengths () =
      [%shared Raw_wrapped_functions_svg.string_of_lengths]

    let string_of_number () =
      [%shared Raw_wrapped_functions_svg.string_of_number]

    let string_of_number_optional_number () =
      [%shared Raw_wrapped_functions_svg.string_of_number_optional_number]

    let string_of_numbers () =
      [%shared Raw_wrapped_functions_svg.string_of_numbers]

    let string_of_numbers_semicolon () =
      [%shared Raw_wrapped_functions_svg.string_of_numbers_semicolon]

    let string_of_offset () =
      ([%shared Raw_wrapped_functions_svg.string_of_offset]
        : (Svg_types.offset -> string) Eliom_shared.Value.t
        :> ([< Svg_types.offset] -> string) Eliom_shared.Value.t)

    let string_of_orient () =
      [%shared Raw_wrapped_functions_svg.string_of_orient]

    let string_of_paint () =
      ([%shared Raw_wrapped_functions_svg.string_of_paint]
        : (Svg_types.paint -> string) Eliom_shared.Value.t
        :> ([< Svg_types.paint] -> string) Eliom_shared.Value.t)

    let string_of_strokedasharray () =
      [%shared Raw_wrapped_functions_svg.string_of_strokedasharray]

    let string_of_transform () =
      [%shared Raw_wrapped_functions_svg.string_of_transform]

    let string_of_transforms () =
      [%shared Raw_wrapped_functions_svg.string_of_transforms]
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
        local_value s |> Eliom_content_core.Svg.D.toelt
        |> Eliom_content_core.Xml.make_request_node ~reset:false
      and synced = React.S.synced s in
      let _ =
        [%client.unsafe
          (let s =
             Eliom_shared.React.S.map
               (fun s ->
                  Eliom_content_core.Svg.(
                    Id.create_request_elt s ~reset:false |> D.toelt)
                  |> Eliom_client_core.rebuild_node' `SVG)
               ~%s
           in
           let key = Eliom_lib.Dom_reference.new_key () in
           let e = Eliom_client_core.rebuild_node' `SVG ~%e in
           let f =
             let replace e' e =
               Eliom_lib.Dom_reference.transfer ~key ~src:e ~dst:e';
               let f p = Dom.replaceChild p e' e in
               Js.Opt.iter e##.parentNode f
             in
             fun e' ->
               replace e' e;
               Eliom_lib.Dom_reference.retain ~key e'
                 ~keep:(React.S.diff replace s)
           in
           if ~%synced
           then
             Eliom_lib.Dom_reference.retain ~key e
               ~keep:React.(S.changes s |> E.once |> E.map f)
           else f (React.S.value s)
           : unit)]
      in
      e |> Eliom_content_core.Svg.D.tot

    include Eliom_content_core.Svg.Make (Xml) (Wrapped_functions)
  end
end

[%%shared
module Raw_wrapped_functions = Html_f.Wrapped_functions (Eliom_content_core.Xml)]

module Html = struct
  module Wrapped_functions : Html_sigs.Wrapped_functions with module Xml = Xml =
  struct
    module Xml = Xml

    type image_candidate =
      [ `Url of Xml.uri
      | `Url_width of Xml.uri * Html_types.number
      | `Url_pixel of Xml.uri * Html_types.float_number ]

    let onoff_of_bool () = [%shared Raw_wrapped_functions.onoff_of_bool]

    let string_of_autocomplete () =
      [%shared Raw_wrapped_functions.string_of_autocomplete]

    let string_of_big_variant () =
      ([%shared Raw_wrapped_functions.string_of_big_variant]
        : (Html_types.big_variant -> string) Eliom_shared.Value.t
        :> ([< Html_types.big_variant] -> string) Eliom_shared.Value.t)

    let string_of_bool () = [%shared Raw_wrapped_functions.string_of_bool]

    let string_of_character () =
      [%shared Raw_wrapped_functions.string_of_character]

    let string_of_input_type () =
      ([%shared Raw_wrapped_functions.string_of_input_type]
        : (Html_types.input_type -> string) Eliom_shared.Value.t
        :> ([< Html_types.input_type] -> string) Eliom_shared.Value.t)

    let string_of_script_type () =
      ([%shared Raw_wrapped_functions.string_of_script_type]
        : (Html_types.script_type -> string) Eliom_shared.Value.t
        :> ([< Html_types.script_type] -> string) Eliom_shared.Value.t)

    let string_of_linktypes () =
      ([%shared Raw_wrapped_functions.string_of_linktypes]
        : (Html_types.linktype list -> string) Eliom_shared.Value.t
        :> ([< Html_types.linktype] list -> string) Eliom_shared.Value.t)

    let string_of_mediadesc () =
      ([%shared Raw_wrapped_functions.string_of_mediadesc]
        : (Html_types.mediadesc_token list -> string) Eliom_shared.Value.t
        :> ([< Html_types.mediadesc_token] list -> string) Eliom_shared.Value.t)

    let string_of_number_or_datetime () =
      ([%shared Raw_wrapped_functions.string_of_number_or_datetime]
        : (Html_types.number_or_datetime -> string) Eliom_shared.Value.t
        :> ([< Html_types.number_or_datetime] -> string) Eliom_shared.Value.t)

    let string_of_numbers () = [%shared Raw_wrapped_functions.string_of_numbers]

    let string_of_sandbox () =
      ([%shared Raw_wrapped_functions.string_of_sandbox]
        : (Html_types.sandbox_token list -> string) Eliom_shared.Value.t
        :> ([< Html_types.sandbox_token] list -> string) Eliom_shared.Value.t)

    let string_of_sizes () = [%shared Raw_wrapped_functions.string_of_sizes]

    let string_of_srcset () =
      ([%shared Raw_wrapped_functions.string_of_srcset]
        : (Raw_wrapped_functions.image_candidate list -> string)
            Eliom_shared.Value.t
        :> ([< Raw_wrapped_functions.image_candidate] list -> string)
             Eliom_shared.Value.t)

    let string_of_step () = [%shared Raw_wrapped_functions.string_of_step]
    let unoption_string () = [%shared Raw_wrapped_functions.unoption_string]

    let string_of_referrerpolicy () =
      ([%shared Raw_wrapped_functions.string_of_referrerpolicy]
        : (Html_types.referrerpolicy -> string) Eliom_shared.Value.t
        :> ([< Html_types.referrerpolicy] -> string) Eliom_shared.Value.t)
  end

  module R = struct
    (* Same as the SVG version, with Svg -> Html and `SVG ->
       `HTML5. Hard to functorize. Make sure they stay synced! *)
    let node s =
      let e =
        local_value s |> Eliom_content_core.Html.D.toelt
        |> Eliom_content_core.Xml.make_request_node ~reset:false
      and synced = React.S.synced s in
      let _ =
        [%client.unsafe
          (let s =
             Eliom_shared.React.S.map
               (fun s ->
                  Eliom_content_core.Html.(
                    Id.create_request_elt s ~reset:false |> D.toelt)
                  |> Eliom_client_core.rebuild_node' `HTML5)
               ~%s
           in
           let key = Eliom_lib.Dom_reference.new_key () in
           let e = Eliom_client_core.rebuild_node' `HTML5 ~%e in
           let f =
             let replace e' e =
               Eliom_lib.Dom_reference.transfer ~key ~src:e ~dst:e';
               let f p = Dom.replaceChild p e' e in
               Js.Opt.iter e##.parentNode f
             in
             fun e' ->
               replace e' e;
               Eliom_lib.Dom_reference.retain ~key e'
                 ~keep:(React.S.diff replace s)
           in
           if ~%synced
           then
             Eliom_lib.Dom_reference.retain ~key e
               ~keep:React.(S.changes s |> E.once |> E.map f)
           else f (React.S.value s)
           : unit)]
      in
      e |> Eliom_content_core.Html.D.tot

    let filter_attrib a s =
      let init = if local_value s then Some a else None
      and c =
        [%client.unsafe Eliom_content_core.Html.R.filter_attrib ~%a ~%s]
      in
      Eliom_content_core.Html.D.client_attrib ?init c

    include Eliom_content_core.Html.Make (Xml) (Wrapped_functions) (Svg.R)

    let pcdata x = txt x |> Unsafe.coerce_elt
  end
end
