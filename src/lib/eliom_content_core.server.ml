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
  include Eliom_runtime.RawXML
  module W = Xml_wrap.NoWrap
  type 'a wrap = 'a
  type 'a list_wrap = 'a list

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
    let rec collect_node_ids ({elt} as elt') =
      let {node_id} = elt in
      if node_id <> NoId then
        node_ids := node_id :: !node_ids;
      match content elt' with
        | Empty | Comment _ | EncodedPCDATA _
        | PCDATA _ | Entity _ | Leaf _ -> ()
        | Node (_, _, children) -> List.iter collect_node_ids children
    in
    collect_node_ids page;
    node_ids_in_content := List.fold_right Node_id_set.add !node_ids Node_id_set.empty;
    let res = Eliom_wrap.wrap value in
    node_ids_in_content := Node_id_set.empty;
    res

  let get_node_id { elt } = elt.node_id

  let tyxml_unwrap_id =
    Eliom_wrap.id_of_int Eliom_runtime.tyxml_unwrap_id_int

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

  type event_handler = (Dom_html.event Js.t -> unit) Eliom_client_value.t
  type mouse_event_handler = (Dom_html.mouseEvent Js.t -> unit) Eliom_client_value.t
  type keyboard_event_handler = (Dom_html.keyboardEvent Js.t -> unit) Eliom_client_value.t

  let make_cryptographic_safe_string () =
    (* FIX: we should directly produce a string of the right length *)
    String.sub (make_cryptographic_safe_string ()) 0 12

  let caml_event_handler cf =
    let crypto = make_cryptographic_safe_string () in
    CE_registered_closure (crypto, Eliom_lib.to_poly cf)

  let event_handler cf =
    Caml (caml_event_handler cf)

  let biggest_event_handler_attrib name cf =
    internal_event_handler_attrib name (event_handler cf)
  let event_handler_attrib name (cf : event_handler) =
    biggest_event_handler_attrib name cf
  let mouse_event_handler_attrib name (cf : mouse_event_handler) =
    biggest_event_handler_attrib name cf
  let keyboard_event_handler_attrib name (cf : keyboard_event_handler) =
    biggest_event_handler_attrib name cf

  let client_attrib ?init (x : attrib Eliom_client_value.t) =
    let crypto = make_cryptographic_safe_string () in
    let empty_name = "" in
    empty_name,RAClient (crypto,init,Eliom_lib.to_poly x)

  let closing_cdata = Pcre.(regexp (quote "]]>"))

  let cdata s = (* GK *)
    (* For security reasons, we do not allow "]]>" inside CDATA
       (as this string is to be considered as the end of the cdata)
     *)
    let s' = "\n<![CDATA[\n"^
      (Eliom_lib.Netstring_pcre.global_replace closing_cdata "" s)
      ^"\n]]>\n" in
    encodedpcdata s'

  let cdata_script s = (* GK *)
    (* For security reasons, we do not allow "]]>" inside CDATA
       (as this string is to be considered as the end of the cdata)
     *)
    let s' = "\n//<![CDATA[\n"^
      (Eliom_lib.Netstring_pcre.global_replace closing_cdata "" s)
      ^"\n//]]>\n" in
    encodedpcdata s'

  let cdata_style s = (* GK *)
    (* For security reasons, we do not allow "]]>" inside CDATA
       (as this string is to be considered as the end of the cdata)
     *)
    let s' = "\n/* <![CDATA[ */\n"^
      (Eliom_lib.Netstring_pcre.global_replace closing_cdata "" s)
      ^"\n/* ]]> */\n" in
    encodedpcdata s'

  let make_node_name ~global () =
    (* !!! The "global_" prefix is checked in eliom_client.client.ml !!! *)
    (if global then "global_" else "")
    (* FIX: put a prefix as a debugging option? *)
       ^ (* "server_" ^ *) make_cryptographic_safe_string ()

  let make_process_node ?(id = make_node_name ~global:true ()) elt' =
    { elt' with elt = { elt'.elt with node_id = ProcessId id } }

  let make_request_node ?(reset = false) elt' =
    let f () =
      let id = RequestId (make_node_name ~global:false ()) in
      { elt' with elt = { elt'.elt with node_id = id } }
    in
    if reset then
      f ()
    else
      match elt'.elt.node_id with
      | Eliom_runtime.RawXML.NoId ->
        f ()
      | _ ->
        elt'

  (** Ref tree *)

  let rec fold_attrib f acc elt =
    match content elt with
    | Empty | EncodedPCDATA _ | PCDATA _
    | Entity _ | Comment _  -> acc
    | Leaf (_, attribs) -> f acc attribs
    | Node (_, attribs, elts) ->
      let acc = f acc attribs in
      List.fold_left (fold_attrib f) acc elts

  let make_event_handler_table elt =
    let f acc attribs =
      List.fold_right (fun att acc ->
          match racontent att with
          | RACamlEventHandler (CE_registered_closure (closure_id, cv)) ->
            ClosureMap.add closure_id cv acc
          | _ -> acc) attribs acc
    in
    fold_attrib f ClosureMap.empty elt

  let make_client_attrib_table elt : client_attrib_table =
    let f acc attribs =
      List.fold_right (fun att acc ->
          match racontent att with
          | RAClient (id,_,cv) ->
            ClosureMap.add id cv acc
          | _ -> acc) attribs acc
    in
    fold_attrib f ClosureMap.empty elt

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

module Svg = struct

  module D = struct
    module Xml' = struct

      include Xml

      let make elt = make_request_node (make elt)

      let empty () = make Empty

      let comment c = make (Comment c)
      let pcdata d = make (PCDATA d)
      let encodedpcdata d = make (EncodedPCDATA d)
      let entity e = make (Entity e)

      let leaf ?(a = []) name =  make (Leaf (name, a))
      let node ?(a = []) name children = make (Node (name, a, children))

    end
    module Raw = Svg_f.Make(Xml')
    let client_attrib ?init (x : 'a Raw.attrib Eliom_client_value.t) =
      Xml.client_attrib ?init x

    include Raw

   end

  module F = struct

    module Raw = Svg_f.Make(Xml)

      include Raw

    end

  module Make
      (Xml : Xml_sigs.T with type elt = Xml.elt
                         and type attrib = Xml.attrib)
      (C : Svg_sigs.Wrapped_functions with module Xml = Xml) =
    Svg_f.Make_with_wrapped_functions(Xml)(C)

  type +'a elt = 'a F.elt
  type 'a wrap = 'a
  type 'a list_wrap = 'a list
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
    let create_request_elt ?reset elt =
      D.tot (Xml.make_request_node ?reset (D.toelt elt))
  end

  module Printer = Xml_print.Make_typed_fmt(Xml)(F)

end

module Html = struct

  module D = struct

    (* This is [Eliom_content.Xml] adapted such that request nodes are produced *)
    module Xml' = struct
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


    end

    module Raw = Html_f.Make(Xml')(Svg.D.Raw)
    let client_attrib ?init (x : 'a Raw.attrib Eliom_client_value.t) =
      Xml.client_attrib ?init x

    include Raw

    type ('a, 'b, 'c) lazy_star =
      ?a: (('a attrib) list) -> ('b elt) list Eliom_lazy.request -> 'c elt

    let lazy_form ?(a = []) elts =
      tot (Xml'.lazy_node ~a:(to_xmlattribs a) "form"
             (Eliom_lazy.from_fun
                (fun () -> toeltl (Eliom_lazy.force elts))))
  end

  module F = struct

    module Xml' = Xml
    module Raw = Html_f.Make(Xml')(Svg.F.Raw)
    include Raw

    type ('a, 'b, 'c) lazy_star =
      ?a: (('a attrib) list) -> ('b elt) list Eliom_lazy.request -> 'c elt

    let lazy_form ?(a = []) elts =
      tot (Xml'.lazy_node ~a:(to_xmlattribs a) "form"
             (Eliom_lazy.from_fun
                (fun () -> toeltl (Eliom_lazy.force elts))))

  end

  module Make
      (Xml : Xml_sigs.T
       with type elt = Xml.elt
        and type attrib = Xml.attrib)
      (C : Html_sigs.Wrapped_functions with module Xml = Xml)
      (Svg : Svg_sigs.T with module Xml := Xml) =
    Html_f.Make_with_wrapped_functions(Xml)(C)(Svg)

  type +'a elt = 'a F.elt
  type 'a wrap = 'a
  type 'a list_wrap = 'a list
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
    let create_request_elt ?reset elt =
      D.tot (Xml.make_request_node ?reset (D.toelt elt))
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

  module Printer = Xml_print.Make_typed_fmt(Xml)(F)

end
