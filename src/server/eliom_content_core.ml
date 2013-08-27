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

(* This the core of [Eliom_content] without its dependencies to [Eliom_service] et al.
   Its name is not [Eliom_content_base] because this would suggest the sharing
   between server and client. *)

(*****************************************************************************)

module Xml = struct

  include RawXML

  type econtent =
    | Empty
    | Comment of string
    | EncodedPCDATA of string
    | PCDATA of string
    | Entity of string
    | Leaf of ename * attrib list
    | Node of ename * attrib list * elt list
  and recontent =
    | RELazy of econtent Eliom_lazy.request
    | RE of econtent
  and elt' = {
    recontent : recontent;
    node_id : node_id;
    unwrapper_mark: Eliom_wrap.unwrapper;
  }
  (** Values of type [elt] are wrapped values of type [elt']. *)
  and elt = {
    elt : elt';
    wrapper_mark : elt Eliom_wrap.wrapper
  }

  let content { elt } = match elt.recontent with
    | RE e -> e
    | RELazy e -> Eliom_lazy.force e

  module Node_id_set = Set.Make (struct type t = node_id let compare : t -> t -> int = compare end)
  let node_ids_in_content = ref Node_id_set.empty
  let wrapper_mark =
    Eliom_wrap.create_wrapper
      (fun { elt } ->
        if Node_id_set.mem elt.node_id !node_ids_in_content then
          { elt with recontent = RE Empty }
        else elt)
  let wrap page value = 
    let node_ids = ref [] in
    let rec collect_node_ids ({ elt = { node_id }} as elt) =
      if node_id <> NoId then
        node_ids := node_id :: !node_ids;
      match content elt with
        | Empty | Comment _ | EncodedPCDATA _
        | PCDATA _ | Entity _ | Leaf _ -> ()
        | Node (_, _, children) -> List.iter collect_node_ids children
    in
    collect_node_ids page;
    node_ids_in_content := List.fold_right Node_id_set.add !node_ids Node_id_set.empty;
    let res = Eliom_wrap.wrap value in
    node_ids_in_content := Node_id_set.empty;
    res

  let rcontent { elt } = elt.recontent

  let get_node_id { elt } = elt.node_id

  let tyxml_unwrap_id = Eliom_wrap.id_of_int tyxml_unwrap_id_int

  let make elt =
    { elt =
        { recontent = RE elt;
          node_id = NoId;
          unwrapper_mark = Eliom_wrap.create_unwrapper tyxml_unwrap_id };
      wrapper_mark }

  let make_lazy elt =
    { elt = 
        { recontent = RELazy elt;
          node_id = NoId;
          unwrapper_mark = Eliom_wrap.create_unwrapper tyxml_unwrap_id };
      wrapper_mark }

  let empty () = make Empty

  let comment c = make (Comment c)
  let pcdata d = make (PCDATA d)
  let encodedpcdata d = make (EncodedPCDATA d)
  let entity e = make (Entity e)

  let leaf ?(a = []) name =  make (Leaf (name, a))
  let node ?(a = []) name children = make (Node (name, a, children))
  let lazy_node ?(a = []) name children =
    make_lazy (Eliom_lazy.from_fun (fun () -> (Node (name, a, Eliom_lazy.force children))))

  let caml_event_handler cf =
    let crypto = make_cryptographic_safe_string () in
    CE_registered_closure (crypto, Eliom_lib.client_value_server_repr cf)

  let event_handler cf =
    Caml (caml_event_handler cf)

  let cdata s = (* GK *)
    (* For security reasons, we do not allow "]]>" inside CDATA
       (as this string is to be considered as the end of the cdata)
     *)
    let s' = "\n<![CDATA[\n"^
      (Netstring_pcre.global_replace
         (Netstring_pcre.regexp_string "]]>") "" s)
      ^"\n]]>\n" in
    encodedpcdata s'

  let cdata_script s = (* GK *)
    (* For security reasons, we do not allow "]]>" inside CDATA
       (as this string is to be considered as the end of the cdata)
     *)
    let s' = "\n//<![CDATA[\n"^
      (Netstring_pcre.global_replace
         (Netstring_pcre.regexp_string "]]>") "" s)
      ^"\n//]]>\n" in
    encodedpcdata s'

  let cdata_style s = (* GK *)
    (* For security reasons, we do not allow "]]>" inside CDATA
       (as this string is to be considered as the end of the cdata)
     *)
    let s' = "\n/* <![CDATA[ */\n"^
      (Netstring_pcre.global_replace
         (Netstring_pcre.regexp_string "]]>") "" s)
      ^"\n/* ]]> */\n" in
    encodedpcdata s'

  let make_node_name ~global () =
    (if global then "global_" else "")
    ^ "server_" ^ make_cryptographic_safe_string ()

  let make_process_node ?(id = make_node_name ~global:true ()) elt' =
    { elt' with elt = { elt'.elt with node_id = ProcessId id } }

  let make_request_node elt' =
    { elt' with elt = { elt'.elt with node_id = RequestId (make_node_name ~global:false ()) } }

  (** Ref tree *)

  let cons_attrib att acc = match racontent att with
    | RACamlEventHandler (CE_registered_closure (closure_id, cv)) ->
      ClosureMap.add closure_id cv acc
    | _ -> acc

  let make_event_handler_table elt =
    let rec aux closure_acc elt =
      let make attribs =
        List.fold_right cons_attrib attribs closure_acc
      in
      match content elt with
        | Empty | EncodedPCDATA _ | PCDATA _
        | Entity _ | Comment _  -> closure_acc
        | Leaf (_, attribs) -> make attribs
        | Node (_, attribs, elts) ->
          List.fold_left aux (make attribs) elts
    in
    aux ClosureMap.empty elt

  let set_classes node_id = function
    | Empty
    | Comment _
    | EncodedPCDATA _
    | PCDATA _
    | Entity _ as e -> e
    | Leaf (ename, attribs) ->
      Leaf (ename, filter_class_attribs node_id attribs)
    | Node (ename, attribs, sons) ->
      Node (ename, filter_class_attribs node_id attribs, sons)

  let content { elt } =
    let c = match elt.recontent with
      | RE e -> e
      | RELazy e -> Eliom_lazy.force e
    in
    set_classes elt.node_id c

end

module Eliom_xml = Xml

module Svg = struct

  module D = struct

    module Raw = Svg_f.Make(struct

        include Xml

        let make elt = make_request_node (make elt)
        let make_lazy elt = make_request_node (make_lazy elt)

        let empty () = make Empty

        let comment c = make (Comment c)
        let pcdata d = make (PCDATA d)
        let encodedpcdata d = make (EncodedPCDATA d)
        let entity e = make (Entity e)

        let leaf ?(a = []) name =  make (Leaf (name, a))
        let node ?(a = []) name children = make (Node (name, a, children))
        let lazy_node ?(a = []) name children =
          make_lazy (Eliom_lazy.from_fun (fun () -> (Node (name, a, Eliom_lazy.force children))))

      end)

    include Raw

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

  module F = struct

    module Raw = Svg_f.Make(Xml)

      include Raw


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

  type +'a elt = 'a F.elt
  type +'a attrib = 'a F.attrib
  type uri = F.uri

  module Id = struct
    type 'a id = string (* FIXME invariant type parameter ? *)
    let new_elt_id: ?global:bool -> unit -> 'a id =
      fun ?(global=true) () -> Xml.make_node_name ~global ()
    let create_named_elt ~(id : 'a id) elt =
      D.tot (Xml.make_process_node ~id (D.toelt elt))
    let create_global_elt elt =
      D.tot (Xml.make_process_node (D.toelt elt))
  end

  module Printer = Xml_print.Make_typed_simple(Xml)(F)

end

module Html5 = struct

  module D = struct

    (* This is [Eliom_content.Xml] adapted such that request nodes are produced *)
    module Xml' = struct
      include Eliom_xml

      let make elt = make_request_node (make elt)
      let make_lazy elt = make_request_node (make_lazy elt)

      let empty () = make Empty

      let comment c = make (Comment c)
      let pcdata d = make (PCDATA d)
      let encodedpcdata d = make (EncodedPCDATA d)
      let entity e = make (Entity e)

      let leaf ?(a = []) name =  make (Leaf (name, a))
      let node ?(a = []) name children = make (Node (name, a, children))
      let lazy_node ?(a = []) name children =
        make_lazy (Eliom_lazy.from_fun (fun () -> (Node (name, a, Eliom_lazy.force children))))

    end

    module Raw = Html5_f.Make(Xml')(Svg.D.Raw)

    include Raw

    type ('a, 'b, 'c) lazy_plus =
      ?a: (('a attrib) list) -> 'b elt Eliom_lazy.request -> ('b elt) list Eliom_lazy.request -> 'c elt

    let lazy_form ?(a = []) elt1 elts =
      tot (Xml'.lazy_node ~a:(to_xmlattribs a) "form"
             (Eliom_lazy.from_fun
                (fun () ->
                  toelt (Eliom_lazy.force elt1)
                  :: toeltl (Eliom_lazy.force elts))))
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

  end

  module F = struct

    module Raw = Html5_f.Make(Xml)(Svg.F.Raw)
    include Raw

    type ('a, 'b, 'c) lazy_plus =
      ?a: (('a attrib) list) -> 'b elt Eliom_lazy.request -> ('b elt) list Eliom_lazy.request -> 'c elt

    let lazy_form ?(a = []) elt1 elts =
      tot (Eliom_xml.lazy_node ~a:(to_xmlattribs a) "form"
             (Eliom_lazy.from_fun
                (fun () ->
                  toelt (Eliom_lazy.force elt1)
                  :: toeltl (Eliom_lazy.force elts))))

    let a_onabort ev = a_onabort (Eliom_xml.event_handler ev)
    let a_onafterprint ev = a_onafterprint (Eliom_xml.event_handler ev)
    let a_onbeforeprint ev = a_onbeforeprint (Eliom_xml.event_handler ev)
    let a_onbeforeunload ev = a_onbeforeunload (Eliom_xml.event_handler ev)
    let a_onblur ev = a_onblur (Eliom_xml.event_handler ev)
    let a_oncanplay ev = a_oncanplay (Eliom_xml.event_handler ev)
    let a_oncanplaythrough ev = a_oncanplaythrough (Eliom_xml.event_handler ev)
    let a_onchange ev = a_onchange (Eliom_xml.event_handler ev)
    let a_onclick (ev : (Dom_html.mouseEvent Js.t -> unit) Eliom_lib.client_value) =
      a_onclick (Eliom_xml.event_handler (Obj.magic ev)) (* Typed by the syntax extension. *)
    let a_oncontextmenu (ev : (Dom_html.mouseEvent Js.t -> unit) Eliom_lib.client_value) =
      a_oncontextmenu (Eliom_xml.event_handler (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_ondblclick (ev : (Dom_html.mouseEvent Js.t -> unit) Eliom_lib.client_value) =
      a_ondblclick (Eliom_xml.event_handler (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_ondrag (ev : (Dom_html.mouseEvent Js.t -> unit) Eliom_lib.client_value) =
      a_ondrag (Eliom_xml.event_handler (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_ondragend (ev : (Dom_html.mouseEvent Js.t -> unit) Eliom_lib.client_value) =
      a_ondragend (Eliom_xml.event_handler (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_ondragenter (ev : (Dom_html.mouseEvent Js.t -> unit) Eliom_lib.client_value) =
      a_ondragenter (Eliom_xml.event_handler (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_ondragleave (ev : (Dom_html.mouseEvent Js.t -> unit) Eliom_lib.client_value) =
      a_ondragleave (Eliom_xml.event_handler (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_ondragover (ev : (Dom_html.mouseEvent Js.t -> unit) Eliom_lib.client_value) =
      a_ondragover (Eliom_xml.event_handler (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_ondragstart (ev : (Dom_html.mouseEvent Js.t -> unit) Eliom_lib.client_value) =
      a_ondragstart (Eliom_xml.event_handler (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_ondrop (ev : (Dom_html.mouseEvent Js.t -> unit) Eliom_lib.client_value) =
      a_ondrop (Eliom_xml.event_handler (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_ondurationchange ev = a_ondurationchange (Eliom_xml.event_handler ev)
    let a_onemptied ev = a_onemptied (Eliom_xml.event_handler ev)
    let a_onended ev = a_onended (Eliom_xml.event_handler ev)
    let a_onerror ev = a_onerror (Eliom_xml.event_handler ev)
    let a_onfocus ev = a_onfocus (Eliom_xml.event_handler ev)
    let a_onformchange ev = a_onformchange (Eliom_xml.event_handler ev)
    let a_onforminput ev = a_onforminput (Eliom_xml.event_handler ev)
    let a_onhashchange ev = a_onhashchange (Eliom_xml.event_handler ev)
    let a_oninput ev = a_oninput (Eliom_xml.event_handler ev)
    let a_oninvalid ev = a_oninvalid (Eliom_xml.event_handler ev)
    let a_onmousedown (ev : (Dom_html.mouseEvent Js.t -> unit) Eliom_lib.client_value) =
      a_onmousedown (Eliom_xml.event_handler (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_onmouseup (ev : (Dom_html.mouseEvent Js.t -> unit) Eliom_lib.client_value) =
      a_onmouseup (Eliom_xml.event_handler (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_onmouseover (ev : (Dom_html.mouseEvent Js.t -> unit) Eliom_lib.client_value) =
      a_onmouseover (Eliom_xml.event_handler (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_onmousemove (ev : (Dom_html.mouseEvent Js.t -> unit) Eliom_lib.client_value) =
      a_onmousemove (Eliom_xml.event_handler (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_onmouseout (ev : (Dom_html.mouseEvent Js.t -> unit) Eliom_lib.client_value) =
      a_onmouseout (Eliom_xml.event_handler (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_onmousewheel ev = a_onmousewheel (Eliom_xml.event_handler ev)
    let a_onoffline ev = a_onoffline (Eliom_xml.event_handler ev)
    let a_ononline ev = a_ononline (Eliom_xml.event_handler ev)
    let a_onpause ev = a_onpause (Eliom_xml.event_handler ev)
    let a_onplay ev = a_onplay (Eliom_xml.event_handler ev)
    let a_onplaying ev = a_onplaying (Eliom_xml.event_handler ev)
    let a_onpagehide ev = a_onpagehide (Eliom_xml.event_handler ev)
    let a_onpageshow ev = a_onpageshow (Eliom_xml.event_handler ev)
    let a_onpopstate ev = a_onpopstate (Eliom_xml.event_handler ev)
    let a_onprogress ev = a_onprogress (Eliom_xml.event_handler ev)
    let a_onratechange ev = a_onratechange (Eliom_xml.event_handler ev)
    let a_onreadystatechange ev = a_onreadystatechange (Eliom_xml.event_handler ev)
    let a_onredo ev = a_onredo (Eliom_xml.event_handler ev)
    let a_onresize ev = a_onresize (Eliom_xml.event_handler ev)
    let a_onscroll ev = a_onscroll (Eliom_xml.event_handler ev)
    let a_onseeked ev = a_onseeked (Eliom_xml.event_handler ev)
    let a_onseeking ev = a_onseeking (Eliom_xml.event_handler ev)
    let a_onselect ev = a_onselect (Eliom_xml.event_handler ev)
    let a_onshow ev = a_onshow (Eliom_xml.event_handler ev)
    let a_onstalled ev = a_onstalled (Eliom_xml.event_handler ev)
    let a_onstorage ev = a_onstorage (Eliom_xml.event_handler ev)
    let a_onsubmit ev = a_onsubmit (Eliom_xml.event_handler ev)
    let a_onsuspend ev = a_onsuspend (Eliom_xml.event_handler ev)
    let a_ontimeupdate ev = a_ontimeupdate (Eliom_xml.event_handler ev)
    let a_onundo ev = a_onundo (Eliom_xml.event_handler ev)
    let a_onunload ev = a_onunload (Eliom_xml.event_handler ev)
    let a_onvolumechange ev = a_onvolumechange (Eliom_xml.event_handler ev)
    let a_onwaiting ev = a_onwaiting (Eliom_xml.event_handler ev)
    let a_onkeypress (ev : (Dom_html.keyboardEvent Js.t -> unit) Eliom_lib.client_value) =
      a_onkeypress (Eliom_xml.event_handler (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_onkeydown (ev : (Dom_html.keyboardEvent Js.t -> unit) Eliom_lib.client_value) =
      a_onkeydown (Eliom_xml.event_handler (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_onkeyup (ev : (Dom_html.keyboardEvent Js.t -> unit) Eliom_lib.client_value) =
      a_onkeyup (Eliom_xml.event_handler (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_onload ev = a_onload (Eliom_xml.event_handler ev)
    let a_onloadeddata ev = a_onloadeddata (Eliom_xml.event_handler ev)
    let a_onloadedmetadata ev = a_onloadedmetadata (Eliom_xml.event_handler ev)
    let a_onloadstart ev = a_onloadstart (Eliom_xml.event_handler ev)
    let a_onmessage ev = a_onmessage (Eliom_xml.event_handler ev)

  end

  type +'a elt = 'a F.elt
  type +'a attrib = 'a F.attrib
  type uri = F.uri

  module Id = struct
    type 'a id = string (* FIXME invariant type parameter ? *)
    let new_elt_id: ?global:bool -> unit -> 'a id =
      fun ?(global=true) () -> Xml.make_node_name ~global ()
    let create_named_elt ~(id : 'a id) elt =
      D.tot (Xml.make_process_node ~id (D.toelt elt))
    let create_global_elt elt =
      D.tot (Xml.make_process_node (D.toelt elt))
    let have_id name elt = Xml.get_node_id (D.toelt elt) = Xml.ProcessId name
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
      { name ; of_string = of_json ~typ ; to_string = to_json ~typ; default }

    let attrib custom_data value =
      F.a_user_data
        custom_data.name
        (custom_data.to_string value)
  end

  module Printer = Xml_print.Make_typed_simple(Xml)(F)

end

