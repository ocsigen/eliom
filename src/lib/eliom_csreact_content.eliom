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

{shared{

module Conv_ = struct

  type (-'a, 'b) ft = ('a -> 'b) Eliom_lib.shared_value

  let string_of_sandbox_token :
    [< Html5_types.sandbox_token ] -> string =
    function
    | `AllowForms -> "allow-forms"
    | `AllowPointerLock -> "allow-pointer-lock"
    | `AllowPopups -> "allow-popups"
    | `AllowTopNavigation -> "allow-top-navigation"
    | `AllowSameOrigin -> "allow-same-origin"
    | `AllowScript -> "allow-script"

  let string_of_multilength :
    [< Html5_types.multilength ] -> string =
    function
    | `Percent p -> (string_of_int p) ^ "%"
    | `Pixels p -> string_of_int p
    | `Relative 1 -> "*"
    | `Relative i -> (string_of_int i) ^ "*"

  let string_of_linktype :
    [< Html5_types.linktype ] -> string =
    function
    | `Alternate -> "alternate"
    | `Archives -> "archives"
    | `Author -> "author"
    | `Bookmark -> "bookmark"
    | `External -> "external"
    | `First -> "first"
    | `Help -> "help"
    | `Icon -> "icon"
    | `Index -> "index"
    | `Last -> "last"
    | `License -> "license"
    | `Next -> "next"
    | `Nofollow -> "nofollow"
    | `Noreferrer -> "noreferrer"
    | `Pingback -> "pingback"
    | `Prefetch -> "prefetch"
    | `Prev -> "prev"
    | `Search -> "search"
    | `Stylesheet -> "stylesheet"
    | `Sidebar -> "sidebar"
    | `Tag -> "tag"
    | `Up -> "up"
    | `Other s -> s

  let string_of_mediadesc_token :
    [< Html5_types.mediadesc_token ] -> string =
    function
    | `All -> "all"
    | `Aural -> "aural"
    | `Braille -> "braille"
    | `Embossed -> "embossed"
    | `Handheld -> "handheld"
    | `Print -> "print"
    | `Projection -> "projection"
    | `Screen -> "screen"
    | `Speech -> "speech"
    | `TTY -> "tty"
    | `TV -> "tv"
    | `Raw_mediadesc s -> s

  let string_of_variant :
    [< Html5_types.big_variant] -> string =
    function
    | `Anonymous -> "anonymous"
    | `Async -> "async"
    | `Autofocus -> "autofocus"
    | `Autoplay -> "autoplay"
    | `Checked -> "checked"
    | `Defer -> "defer"
    | `Disabled -> "disabled"
    | `Muted -> "muted"
    | `Off -> "off"
    | `On -> "on"
    | `ReadOnly -> "readonly"
    | `Rect -> "rect"
    | `Selected -> "selected"
    | `Use_credentials -> "use-credentials"
    | `W3_org_1999_xhtml -> "http://www.w3.org/1999/xhtml"
    | `All -> "all"
    | `Preserve -> "preserve"
    | `Default -> "default"
    | `Controls -> "controls"
    | `Ltr -> "ltr"
    | `Rtl -> "rtl"
    | `Get -> "GET"
    | `Post -> "POST"
    | `Put -> "PUT"
    | `Delete -> "DELETE"
    | `Formnovalidate -> "formnovalidate"
    | `Hidden -> "hidden"
    | `Ismap -> "ismap"
    | `Loop -> "loop"
    | `Novalidate -> "novalidate"
    | `Open -> "open"
    | `None -> "none"
    | `Metadata -> "metadata"
    | `Audio -> "audio"
    | `Pubdate -> "pubdate"
    | `Required -> "required"
    | `Reversed -> "reserved"
    | `Scoped -> "scoped"
    | `Seamless -> "seamless"
    | `Soft -> "soft"
    | `Hard -> "hard"
    | `Context -> "context"
    | `Toolbar -> "toolbar"
    | `Command -> "command"
    | `Checkbox -> "checkbox"
    | `Radio -> "radio"
    | `Multiple -> "multiple"
    | `Left -> "left"
    | `Right -> "right"
    | `Justify -> "justify"
    | `Char -> "char"
    | `Row -> "row"
    | `Col -> "col"
    | `Rowgroup -> "rowgroup"
    | `Colgroup -> "colgroup"
    | `Groups -> "groups"
    | `Rows -> "rows"
    | `Cols -> "cols"
    | `Zero -> "0"
    | `One -> "1"
    | `Yes -> "yes"
    | `No -> "no"
    | `Auto -> "auto"
    | `Circle -> "circle"
    | `Poly -> "poly"

  let string_of_input_type :
    [< Html5_types.input_type ] -> string =
    function
    | `Button -> "button"
    | `Checkbox -> "checkbox"
    | `Color -> "color"
    | `Date -> "date"
    | `Datetime -> "datetime"
    | `Datetime_local -> "datetime-local"
    | `Email -> "email"
    | `File -> "file"
    | `Hidden -> "hidden"
    | `Image -> "image"
    | `Month -> "month"
    | `Number -> "number"
    | `Password -> "password"
    | `Radio -> "radio"
    | `Range -> "range"
    | `Reset -> "reset"
    | `Search -> "search"
    | `Submit -> "submit"
    | `Tel -> "tel"
    | `Text -> "text"
    | `Time -> "time"
    | `Url -> "url"
    | `Week -> "week"

  let string_of_character = String.make 1

  let string_of_number = string_of_int

  let string_of_bool = string_of_bool

  let unoption_string = function
    | Some x -> x
    | None -> ""

  let string_of_step = function
    | Some x -> string_of_float x
    | None -> "any"

  let string_of_sizes = function
    | `Sizes l ->
      String.concat " "
        (List.map
           (fun (x, y) -> Printf.sprintf "%dx%d" x y)
           l)
    | `Any ->
      "any"

  let string_of_sandbox l =
    String.concat " " (List.map string_of_sandbox_token l)

  let string_of_numbers l =
    String.concat "," (List.map string_of_number l)

  let string_of_multilengths l =
    String.concat ", " (List.map string_of_multilength l)

  let string_of_mediadesc l =
    String.concat ", " (List.map string_of_mediadesc_token l)

  let string_of_linktypes l =
    String.concat " " (List.map string_of_linktype l)

end }}

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

    let append = Eliom_csreact.SharedReactiveData.RList.append

    let cons = Eliom_csreact.SharedReactiveData.RList.cons

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

  let float_attrib name s =
    let init =
      Eliom_csreact.(Shared.local s |> FakeReact.S.value) |>
      Eliom_content_core.Xml.float_attrib name
    in
    {{ Eliom_content_core.Xml_wed.float_attrib %name %s }} |>
    Eliom_content_core.Xml.client_attrib ~init

  let int_attrib name s =
    let init =
      Eliom_csreact.(Shared.local s |> FakeReact.S.value) |>
      Eliom_content_core.Xml.int_attrib name
    in
    {{ Eliom_content_core.Xml_wed.int_attrib %name %s }} |>
    Eliom_content_core.Xml.client_attrib ~init

  let string_attrib name s =
    let init =
      Eliom_csreact.(Shared.local s |> FakeReact.S.value) |>
      Eliom_content_core.Xml.string_attrib name
    in
    {{ Eliom_content_core.Xml_wed.string_attrib %name %s }} |>
    Eliom_content_core.Xml.client_attrib ~init

  let space_sep_attrib name s =
    let init =
      Eliom_csreact.(Shared.local s |> FakeReact.S.value) |>
      Eliom_content_core.Xml.space_sep_attrib name
    in
    {{ Eliom_content_core.Xml_wed.space_sep_attrib %name %s }} |>
    Eliom_content_core.Xml.client_attrib ~init

  let comma_sep_attrib name s =
    let init =
      Eliom_csreact.(Shared.local s |> FakeReact.S.value) |>
      Eliom_content_core.Xml.comma_sep_attrib name
    in
    {{ Eliom_content_core.Xml_wed.comma_sep_attrib %name %s }} |>
    Eliom_content_core.Xml.client_attrib ~init

  let uri_attrib name s =
    let init =
      Eliom_csreact.(Shared.local s |> FakeReact.S.value) |>
      Eliom_content_core.Xml.uri_attrib name
    in
    {{ Eliom_content_core.Xml_wed.uri_attrib %name %s }} |>
    Eliom_content_core.Xml.client_attrib ~init

  let uris_attrib name s =
    let init =
      Eliom_csreact.(Shared.local s |> FakeReact.S.value) |>
      Eliom_content_core.Xml.uris_attrib name
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
      let s = Eliom_csreact.(Shared.local (SharedReact.S.value s)) in
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
      Eliom_csreact.Shared.local l |>
      Eliom_csreact.FakeReactiveData.RList.value |>
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

module Conv :

  Html5_sigs.Conv with type (-'a, 'b) ft = ('a, 'b) Xml.W.ft =

struct

  type (-'a, 'b) ft = ('a, 'b) Xml.W.ft

  let string_of_sizes =
    Eliom_lib.create_shared_value
      Conv_.string_of_sizes {{Conv_.string_of_sizes}}

  let string_of_input_type =
    Eliom_lib.create_shared_value
      Conv_.string_of_input_type {{Conv_.string_of_input_type}}

  let string_of_mediadesc_token =
    Eliom_lib.create_shared_value
      Conv_.string_of_mediadesc_token
      {{Conv_.string_of_mediadesc_token}}

  let string_of_variant =
    Eliom_lib.create_shared_value
      Conv_.string_of_variant {{Conv_.string_of_variant}}

  let string_of_linktype =
    Eliom_lib.create_shared_value
      Conv_.string_of_linktype {{Conv_.string_of_linktype}}

  let string_of_sandbox_token =
    Eliom_lib.create_shared_value
      Conv_.string_of_sandbox_token {{Conv_.string_of_sandbox_token}}

  let string_of_multilength =
    Eliom_lib.create_shared_value
      Conv_.string_of_multilength {{Conv_.string_of_multilength}}

  let string_of_character =
    Eliom_lib.create_shared_value
      Conv_.string_of_character {{Conv_.string_of_character}}

  let string_of_number =
    Eliom_lib.create_shared_value
      Conv_.string_of_number {{Conv_.string_of_number}}

  let string_of_bool =
    Eliom_lib.create_shared_value
      Conv_.string_of_bool {{Conv_.string_of_bool}}

  let string_of_step =
    Eliom_lib.create_shared_value
      Conv_.string_of_step {{Conv_.string_of_step}}

  let unoption_string =
    Eliom_lib.create_shared_value
      Conv_.unoption_string {{Conv_.unoption_string}}

  let string_of_sizes =
    Eliom_lib.create_shared_value
      Conv_.string_of_sizes {{Conv_.string_of_sizes}}

  let string_of_sandbox =
    Eliom_lib.create_shared_value
      Conv_.string_of_sandbox {{Conv_.string_of_sandbox}}

  let string_of_numbers =
    Eliom_lib.create_shared_value
      Conv_.string_of_numbers {{Conv_.string_of_numbers}}

  let string_of_multilengths =
    Eliom_lib.create_shared_value
      Conv_.string_of_multilengths {{Conv_.string_of_multilengths}}

  let string_of_mediadesc =
    Eliom_lib.create_shared_value
      Conv_.string_of_mediadesc {{Conv_.string_of_mediadesc}}

  let string_of_linktypes =
    Eliom_lib.create_shared_value
      Conv_.string_of_linktypes {{Conv_.string_of_linktypes}}

end ;;

module Html5 = struct

  module R = struct
    include Eliom_content_core.Html5.Make_NoSVG(Xml)(Conv)
    let pcdata x = pcdata x |> Unsafe.coerce_elt
  end

end
