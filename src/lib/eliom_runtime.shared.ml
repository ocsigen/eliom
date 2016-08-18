(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) CNRS Univ Paris Diderot
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


(**/**)

module Client_value_server_repr = struct

  type u = {
    mutable loc : Eliom_lib_base.pos option;
    instance_id: int;
    unwrapper: Eliom_wrap.unwrapper
  }
  type 'a t = u

  let create ?loc ~instance_id ~unwrapper = { instance_id; loc; unwrapper }
  let instance_id cv = cv.instance_id
  let loc cv = cv.loc
  let clear_loc cv = cv.loc <- None
  let to_poly v = v
end

type escaped_value = Ocsigen_lib.poly

module RawXML = struct

  type separator = Space | Comma

  let separator_to_string = function
    | Space -> " "
    | Comma -> ", "

  type cookie_info = (bool * string list) deriving (Json)

  type caml_event_handler =
    | CE_registered_closure of
        string * Ocsigen_lib.poly (* 'a Js.t -> unit) client_value *)
    | CE_client_closure of
        (Dom_html.event Js.t -> unit) (* Client side-only *)
    | CE_client_closure_mouse of
        (Dom_html.mouseEvent Js.t -> unit) (* Client side-only *)
    | CE_client_closure_keyboard of
        (Dom_html.keyboardEvent Js.t -> unit) (* Client side-only *)
    | CE_call_service of
        ( [ `A | `Form_get | `Form_post] *
          (cookie_info option) *
          string option *
          Ocsigen_lib.poly (* (event -> bool) client_value *)
        ) option Eliom_lazy.request

  type internal_event_handler =
    | Raw of string
    | Caml of caml_event_handler

  type uri = string Eliom_lazy.request
  let string_of_uri = Eliom_lazy.force
  let uri_of_string = Eliom_lazy.from_val
  let uri_of_fun = Eliom_lazy.from_fun

  let internal_event_handler_of_string s = Raw s
  let string_of_internal_event_handler = function
    | Raw s -> s
    | Caml _ -> "/* Invalid Caml value */"
  let internal_event_handler_of_service info = Caml (CE_call_service info)

  let ce_registered_closure_class = "caml_c" (*"caml_closure"*)
  let ce_registered_attr_class = "caml_attr"
  let ce_call_service_class = "caml_link"
  let process_node_class = "caml_p" (*"caml_process_node"*)
  let request_node_class = "caml_r" (*"caml_request_node"*)

  let ce_call_service_attrib = "data-eliom-cookies-info"
  let ce_template_attrib = "data-eliom-template"
  let node_id_attrib = "data-eliom-id" (*"data-eliom-node-id"*)

  let closure_attr_prefix = "" (*"caml_closure_id"*)
  let closure_name_prefix = "data-eliom-c-"
      (*!!! This prefix has to be different from any other prefix *)

  let client_attr_prefix = "eliom_attrib"
  let client_name_prefix = "data-eliom-"
  type aname = string
  type acontent =
    | AFloat of float
    | AInt of int
    | AStr of string
    | AStrL of separator * string list

  type racontent =
    | RA of acontent
    | RAReact of acontent option React.signal
    | RACamlEventHandler of caml_event_handler
    | RALazyStr of string Eliom_lazy.request
    | RALazyStrL of separator * string Eliom_lazy.request list
    | RAClient of string * attrib option * Ocsigen_lib.poly (*attrib client_value *)
  and attrib = aname * racontent

  let aname = function
    | name, RACamlEventHandler (CE_registered_closure (crypto, _)) ->
      closure_name_prefix^name
    | _, RAClient (s, Some (name,_), c)
    | name, RAClient (s, None, c) -> client_name_prefix^name
    | name, _ -> name
  let acontent = function
    | _ ,RAReact s -> (match React.S.value s with None -> AStr "" | Some x -> x)
    | _, RA a -> a
    | _, RACamlEventHandler (CE_registered_closure (crypto, _)) ->
      AStr (closure_attr_prefix^crypto)
    | _, RACamlEventHandler _ -> AStr ("")
    | _, RALazyStr str -> AStr (Eliom_lazy.force str)
    | _, RALazyStrL (sep, str) -> AStrL (sep, List.map Eliom_lazy.force str)
    | _, RAClient (crypto,_,_) -> AStr (client_attr_prefix^crypto)
  let racontent (_, a) = a

  let react_float_attrib name s = name, RAReact (React.S.map (fun f -> Some (AFloat f)) s)
  let react_int_attrib name s = name, RAReact (React.S.map (fun f -> Some (AInt f)) s)
  let react_string_attrib name s = name, RAReact (React.S.map (fun f -> Some (AStr f)) s)
  let react_space_sep_attrib name s = name, RAReact (React.S.map (fun f -> Some(AStrL (Space,f))) s)
  let react_comma_sep_attrib name s = name, RAReact (React.S.map (fun f -> Some (AStrL (Comma,f))) s)

  let react_poly_attrib name v s = name, RAReact (React.S.map (function false -> None | true -> Some (AStr v)) s)

  let float_attrib name value = name, RA (AFloat value)
  let int_attrib name value = name, RA (AInt value)
  let string_attrib name value = name, RA (AStr value)
  let space_sep_attrib name values = name, RA (AStrL (Space, values))
  let comma_sep_attrib name values = name, RA (AStrL (Comma, values))
  let internal_event_handler_attrib name value = match value with
    | Raw value -> name, RA (AStr value)
    | Caml v -> name, RACamlEventHandler v
  let uri_attrib name value = name, RALazyStr value
  let uris_attrib name value = name, RALazyStrL (Space, value)

  type ename = string
  type node_id =
    | NoId
    | ProcessId of string
    | RequestId of string

  module ClosureMap = Map.Make(struct type t = string let compare = compare end)

  type event_handler_table =
    Ocsigen_lib.poly (* (biggest_event Js.t -> unit) client_value *) ClosureMap.t

  type client_attrib_table = Ocsigen_lib.poly (* attrib client_value *) ClosureMap.t

  let filter_class_value acc = function
    | AStr v ->
      v :: acc
    | AStrL (space, v) ->
      v @ acc
    | _ ->
      failwith "attribute class is not a string"

  let filter_class (freepos,acc_class,acc_attr) = function
    | "class", RA value ->
      freepos, filter_class_value acc_class value, acc_attr
    | _, RACamlEventHandler (CE_registered_closure _) as attr ->
      (freepos,ce_registered_closure_class :: acc_class, attr :: acc_attr)
    | _, RACamlEventHandler (CE_call_service link_info) ->
      begin
        match Eliom_lazy.force link_info with
          | None -> freepos, acc_class, acc_attr
          | Some (kind, cookie_info, tmpl, _) ->
            let acc_class = ce_call_service_class::acc_class in
            let acc_attr =
              match cookie_info with
              | None -> acc_attr
              | Some v ->
                (ce_call_service_attrib, RA (AStr (Json.to_string<cookie_info> v)))
                :: acc_attr
            in
            let acc_attr =
              match tmpl with
              | None -> acc_attr
              | Some tmpl -> (ce_template_attrib, RA (AStr tmpl)) :: acc_attr
            in
            freepos, acc_class, acc_attr
      end
    | "" , RAClient (crypt, Some ("class", RA v), cv) ->
      let acc_class = filter_class_value acc_class v in
      let acc_class = ce_registered_attr_class :: acc_class
      and acc_attr = ("class", RAClient (crypt, None, cv)) :: acc_attr in
      freepos, acc_class, acc_attr
    | "" , RAClient (crypt,init,cv)->
      let freepos,acc_attr = match init with
        | Some ((an,_) as a) -> freepos, (an,RAClient(crypt,None,cv))::a::acc_attr
        | None ->
          let name = (Printf.sprintf "anonym%d" freepos) in
          let freepos = succ freepos in
          freepos, (name, RAClient(crypt,None,cv)) :: acc_attr in
      (freepos, ce_registered_attr_class :: acc_class, acc_attr)
    | _ , RAClient _ -> assert false
    | attr -> (freepos, acc_class,attr::acc_attr)

  let filter_class_attribs node_id attribs =
    let nid_classes,nid_attribs = match node_id with
      | NoId -> [],[]
      | ProcessId i -> [process_node_class], [node_id_attrib,RA (AStr i)]
      | RequestId i -> [request_node_class], [node_id_attrib,RA (AStr i)]
    in
    let (_,classes,attribs) =
      List.fold_left filter_class (0,nid_classes,nid_attribs) attribs in
    match classes with
      | [] -> attribs
      | _ -> ("class",RA (AStrL(Space,classes)))::attribs

end

let tyxml_unwrap_id_int = 1
let client_value_unwrap_id_int = 7

type client_value_datum = {
  closure_id : string;
  args : Ocsigen_lib.poly;
  value : Ocsigen_lib.poly Client_value_server_repr.t
}

type injection_datum = {
  injection_dbg : (Eliom_lib_base.pos * string option) option;
  injection_id : int;
  injection_value : Ocsigen_lib.poly;
}

type compilation_unit_global_data = {
  server_sections_data : client_value_datum array array;
  client_sections_data : injection_datum array array;
}

type global_data = compilation_unit_global_data Eliom_lib.String_map.t

type request_data = client_value_datum array

let global_data_unwrap_id_int = 8

type 'a eliom_caml_service_data = {
  ecs_request_data: request_data;
  ecs_data: 'a;
}

(* the data sent on channels *)
type 'a eliom_comet_data_type = 'a Eliom_wrap.wrapped_value
