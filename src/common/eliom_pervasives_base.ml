exception Eliom_Internal_Error of string

external id : 'a -> 'a = "%identity"

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)
let (!!) = Lazy.force

let comp f g x = f (g x)
let uncurry2 f (x, y) = f x y

type yesnomaybe = Yes | No | Maybe
type ('a, 'b) leftright = Left of 'a | Right of 'b

let map_option f = function
  | None -> None
  | Some v -> Some (f v)

let iter_option f = function
  | None -> ()
  | Some v -> (f v)

let fst3 (a, _, _) = a
let snd3 (_, a, _) = a
let thd3 (_, _, a) = a

type poly
external to_poly : 'a -> poly = "%identity"
external from_poly : poly -> 'a = "%identity"

type 'a client_expr = int64 * poly

module List_base = struct
  let map_filter f l =
    let rec aux acc = function
      | [] -> acc
      | t::q ->
	match f t with
	  | None -> aux acc q
	  | Some r -> aux (r::acc) q
    in
    List.rev (aux [] l)
end

let tyxml_unwrap_id_int = 1

module RawXML = struct

  type separator = Space | Comma

  let separator_to_string = function
    | Space -> " "
    | Comma -> ", "

  type cookie_info = (bool * string list) deriving (Json)

  type -'a caml_event_handler =
    | CE_registered_closure of string * ((#Dom_html.event as 'a) Js.t -> unit) client_expr
    | CE_client_closure of ('a Js.t -> unit) (* Client side-only *)
    | CE_call_service of
	([ `A | `Form_get | `Form_post] * (cookie_info option) * string option) option Eliom_lazy.request

  type event_handler =
    | Raw of string
    | Caml of Dom_html.event caml_event_handler

  type uri = string Eliom_lazy.request
  let string_of_uri = Eliom_lazy.force
  let uri_of_string = Eliom_lazy.from_val
  let uri_of_fun = Eliom_lazy.from_fun

  let event_handler_of_string s = Raw s
  let string_of_event_handler = function
    | Raw s -> s
    | Caml _ -> "/* Invalid Caml value */"
  let event_handler_of_service info = Caml (CE_call_service info)

  (* Deprecated alias. *)
  let event_of_service = event_handler_of_service
  let event_of_string = event_handler_of_string
  let string_of_handler = string_of_event_handler

  let ce_registered_closure_class = "caml_closure"
  let ce_call_service_class = "caml_link"
  let process_node_class = "caml_process_node"
  let request_node_class = "caml_request_node"

  let ce_call_service_attrib = "data-eliom-cookies-info"
  let ce_template_attrib = "data-eliom-template"
  let node_id_attrib = "data-eliom-node-id"

  let closure_attr_prefix = "caml_closure_id"
  let closure_attr_prefix_len = String.length closure_attr_prefix

  type aname = string
  type acontent =
    | AFloat of float
    | AInt of int
    | AStr of string
    | AStrL of separator * string list
  type racontent =
    | RA of acontent
    | RACamlEventHandler of Dom_html.event caml_event_handler
    | RALazyStr of string Eliom_lazy.request
    | RALazyStrL of separator * string Eliom_lazy.request list
  type attrib = aname * racontent
  let aname (name, _) = name
  let acontent = function
    | _, RA a -> a
    | _, RACamlEventHandler (CE_registered_closure (closure_id,_)) ->
      AStr (closure_attr_prefix^closure_id)
    | _, RACamlEventHandler _ -> AStr ("")
    | _, RALazyStr str -> AStr (Eliom_lazy.force str)
    | _, RALazyStrL (sep, str) -> AStrL (sep, List.map Eliom_lazy.force str)
  let racontent (_, a) = a

  let float_attrib name value = name, RA (AFloat value)
  let int_attrib name value = name, RA (AInt value)
  let string_attrib name value = name, RA (AStr value)
  let space_sep_attrib name values = name, RA (AStrL (Space, values))
  let comma_sep_attrib name values = name, RA (AStrL (Comma, values))
  let event_handler_attrib name value = match value with
    | Raw value -> name, RA (AStr value)
    | Caml v -> name, RACamlEventHandler v
  let uri_attrib name value = name, RALazyStr value
  let uris_attrib name value = name, RALazyStrL (Space, value)

  (* Deprecated alias. *)
  let event_attrib = event_handler_attrib

  type ename = string
  type node_id =
    | NoId
    | ProcessId of string
    | RequestId of string

  module ClosureMap = Map.Make(struct type t = string let compare = compare end)

  type event_handler_table = ((unit -> unit) client_expr) ClosureMap.t

end
