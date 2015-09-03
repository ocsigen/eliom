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

{server{ open Eliom_shared }}

module Xml = struct

  module W = struct

    include React.S

    type (-'a, 'b) ft = unit -> ('a -> 'b) Eliom_lib.shared_value

    let return = const

    let append = ReactiveData.RList.concat

    let cons a l =
      ReactiveData.RList.(concat (singleton_s a) l)

    let singleton = ReactiveData.RList.singleton_s

    let nil () =
      ReactiveData.RList.make [] |> fst

    let fmap f a =
      React.S.map (f ()) a

    type 'a tlist = 'a ReactiveData.RList.t

    let map _ _ =
      if true then assert false;
      ReactiveData.RList.make [] |> fst

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
    React.S.value s |> Value.local

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
    Eliom_content_core.Xml.make_request_node e

  let leaf ?(a : attrib list option) name =
    Eliom_content_core.Xml.leaf ?a name |> name_node

  let pcdata s =
    let e =
      let s = local_value s in
      Eliom_content_core.Xml.(node "span" [pcdata s]) |> name_node
    and synced = React.S.synced s in
    let _ = {unit{
      let (>>!) = Js.Opt.iter in
      let update =
        let e = Eliom_client.rebuild_node' `HTML5 %e in
        fun x ->
          e##firstChild >>! fun e ->
          Dom.CoerceTo.text e >>! fun e ->
          e##data <- Js.string x
      in
      if not %synced then update (React.S.value %s);
      React.S.changes %s |> React.E.map update |> ignore;
    }} in
    e

  let encodedpcdata = pcdata

  let cdata = Eliom_content_core.Xml.cdata

  let cdata_script = Eliom_content_core.Xml.cdata_script

  let cdata_style = Eliom_content_core.Xml.cdata_style

  let entity = Eliom_content_core.Xml.entity

  let node ?a name l =
    let e =
      ReactiveData.RList.value l |>
      Value.local |>
      Eliom_content_core.Xml.node ?a name |>
      name_node
    and synced = ReactiveData.RList.synced l in
    let _ = {unit{
      let f () =
        let f = Eliom_client.rebuild_node' `HTML5 in
        let e = f %e
        and l = ReactiveData.RList.map f %l in
        Tyxml_js.Util.update_children e l
      in
      if %synced then
        ReactiveData.RList.event %l |>
        React.E.once |>
        React.E.map (fun _ -> f ()) |> ignore
      else
        f ()
    }} in
    e

end

module Svg = struct

  module Wrapped_functions :

    Svg_sigs.Wrapped_functions
    with type (-'a, 'b) ft = ('a, 'b) Xml.W.ft =

  struct

    type (-'a, 'b) ft = ('a, 'b) Xml.W.ft

    let string_of_alignment_baseline () =
      {shared#{ Svg_f.Wrapped_functions.string_of_alignment_baseline }}

    let string_of_big_variant () =
      {shared#{ Svg_f.Wrapped_functions.string_of_big_variant }}

    let string_of_bool () =
      {shared#{ Svg_f.Wrapped_functions.string_of_bool }}

    let string_of_coords () =
      {shared#{ Svg_f.Wrapped_functions.string_of_coords }}

    let string_of_dominant_baseline () =
      {shared#{ Svg_f.Wrapped_functions.string_of_dominant_baseline }}

    let string_of_fourfloats () =
      {shared#{ Svg_f.Wrapped_functions.string_of_fourfloats }}

    let string_of_in_value () =
      {shared#{ Svg_f.Wrapped_functions.string_of_in_value }}

    let string_of_int () =
      {shared#{ Svg_f.Wrapped_functions.string_of_int }}

    let string_of_length () =
      {shared#{ Svg_f.Wrapped_functions.string_of_length }}

    let string_of_lengths () =
      {shared#{ Svg_f.Wrapped_functions.string_of_lengths }}

    let string_of_number () =
      {shared#{ Svg_f.Wrapped_functions.string_of_number }}

    let string_of_number_optional_number () =
      {shared#{
         Svg_f.Wrapped_functions.string_of_number_optional_number }}

    let string_of_numbers () =
      {shared#{ Svg_f.Wrapped_functions.string_of_numbers }}

    let string_of_numbers_semicolon () =
      {shared#{ Svg_f.Wrapped_functions.string_of_numbers_semicolon }}

    let string_of_offset () =
      {shared#{ Svg_f.Wrapped_functions.string_of_offset }}

    let string_of_orient () =
      {shared#{ Svg_f.Wrapped_functions.string_of_orient }}

    let string_of_paint () =
      {shared#{ Svg_f.Wrapped_functions.string_of_paint }}

    let string_of_strokedasharray () =
      {shared#{ Svg_f.Wrapped_functions.string_of_strokedasharray }}

    let string_of_transform () =
      {shared#{ Svg_f.Wrapped_functions.string_of_transform }}

    let string_of_transforms () =
      {shared#{ Svg_f.Wrapped_functions.string_of_transforms }}

  end

  module R = struct

    (* Same as the HTML version, with Html5 -> SVG and `HTML5 ->
       `SVG. We can in principle functorize, but probably not worth
       the trouble. Make sure they stay synced! *)
    let node s =
      let e =
        React.S.value s |>
        Value.local |>
        Eliom_content_core.Svg.D.toelt |>
        Eliom_content_core.Xml.make_request_node ~reset:false
      and synced = React.S.synced s in
      let _ = {unit{
        let replace e e' =
          let e =
            Eliom_content_core.Svg.D.toelt e |>
            Eliom_client.rebuild_node' `SVG
          and e' =
            Eliom_content_core.Svg.D.toelt e' |>
            Eliom_client.rebuild_node' `SVG
          in
          let f p = Dom.replaceChild p e' e in
          Js.Opt.iter (e##parentNode) f
        and s =
          let f =
            Eliom_content_core.Svg.Id.create_request_elt
              ~reset:false
          in
          React.S.map f %s
        in
        let f =
          let e = Eliom_content_core.Svg.D.tot %e in
          fun e' ->
            replace e e';
            let ev = React.S.diff (fun e' e -> e, e') s
            and f (e, e') = replace e e' in
            React.E.map f ev |> ignore
        in
        if %synced then
          let ev = React.(S.changes s |> E.once) in
          React.E.map f ev |> ignore
        else
          f (React.S.value s)
      }} in
      e |> Eliom_content_core.Svg.D.tot

    include Eliom_content_core.Svg.Make(Xml)(Wrapped_functions)

  end

end

module Html5 = struct

  module Wrapped_functions :

    Html5_sigs.Wrapped_functions
    with type (-'a, 'b) ft = ('a, 'b) Xml.W.ft =

  struct

    type (-'a, 'b) ft = ('a, 'b) Xml.W.ft

    let string_of_big_variant () =
      {shared#{ Html5_f.Wrapped_functions.string_of_big_variant }}

    let string_of_bool () =
      {shared#{ Html5_f.Wrapped_functions.string_of_bool }}

    let string_of_character () =
      {shared#{ Html5_f.Wrapped_functions.string_of_character }}

    let string_of_input_type () =
      {shared#{ Html5_f.Wrapped_functions.string_of_input_type }}

    let string_of_linktypes () =
      {shared#{ Html5_f.Wrapped_functions.string_of_linktypes }}

    let string_of_mediadesc () =
      {shared#{ Html5_f.Wrapped_functions.string_of_mediadesc }}

    let string_of_multilength () =
      {shared#{ Html5_f.Wrapped_functions.string_of_multilength }}

    let string_of_multilengths () =
      {shared#{ Html5_f.Wrapped_functions.string_of_multilengths }}

    let string_of_numbers () =
      {shared#{ Html5_f.Wrapped_functions.string_of_numbers }}

    let string_of_sandbox () =
      {shared#{ Html5_f.Wrapped_functions.string_of_sandbox }}

    let string_of_sizes () =
      {shared#{ Html5_f.Wrapped_functions.string_of_sizes }}

    let string_of_step () =
      {shared#{ Html5_f.Wrapped_functions.string_of_step }}

    let unoption_string () =
      {shared#{ Html5_f.Wrapped_functions.unoption_string }}

  end

  module R = struct

    (* Same as the SVG version, with Svg -> Html5 and `SVG ->
       `HTML5. We can in principle functorize, but probably not worth
       the trouble. Make sure they stay synced! *)
    let node s =
      let e =
        React.S.value s |>
        Value.local |>
        Eliom_content_core.Html5.D.toelt |>
        Eliom_content_core.Xml.make_request_node ~reset:false
      and synced = React.S.synced s in
      let _ = {unit{
        let replace e e' =
          let e =
            Eliom_content_core.Html5.D.toelt e |>
            Eliom_client.rebuild_node' `HTML5
          and e' =
            Eliom_content_core.Html5.D.toelt e' |>
            Eliom_client.rebuild_node' `HTML5
          in
          let f p = Dom.replaceChild p e' e in
          Js.Opt.iter (e##parentNode) f
        and s =
          let f =
            Eliom_content_core.Html5.Id.create_request_elt
              ~reset:false
          in
          React.S.map f %s
        in
        let f =
          let e = Eliom_content_core.Html5.D.tot %e in
          fun e' ->
            replace e e';
            let ev = React.S.diff (fun e' e -> e, e') s
            and f (e, e') = replace e e' in
            React.E.map f ev |> ignore
        in
        if %synced then
          let ev = React.(S.changes s |> E.once) in
          React.E.map f ev |> ignore
        else
          f (React.S.value s)
      }} in
      e |> Eliom_content_core.Html5.D.tot

    let filter_attrib a s =
      let init =
        if
          React.S.value s |>
          Value.local
        then
          Some a
        else
          None
      and c = {{ Eliom_content_core.Html5.R.filter_attrib %a %s }} in
      Eliom_content_core.Html5.D.client_attrib ?init c

    include
      Eliom_content_core.Html5.Make(Xml)(Wrapped_functions)(Svg.R)

    let pcdata x = pcdata x |> Unsafe.coerce_elt

  end

end
