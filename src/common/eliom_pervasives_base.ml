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

module RawXML = struct

  type separator = Space | Comma

  let separator_to_string = function
    | Space -> " "
    | Comma -> ", "

  type cookie_info = (bool * string list) deriving (Json)

  type caml_event =
    | CE_registered_closure of int * (unit -> unit) client_expr
    | CE_client_closure of (unit -> unit)
    | CE_call_service of
	([ `A | `Form_get | `Form_post] * (cookie_info option)) option Eliom_lazy.request

  type event =
    | Raw of string
    | Caml of caml_event

  type uri = string Eliom_lazy.request
  let string_of_uri = Eliom_lazy.force
  let uri_of_string = Eliom_lazy.from_val
  let uri_of_fun = Eliom_lazy.from_fun

  let event_of_string s = Raw s
  let string_of_event = function
    | Raw s -> s
    | Caml _ -> "/* Invalid Caml value */"
  let event_of_js id args =
    let closure_id = Random.bits () in
    Caml (CE_registered_closure (closure_id, (id, args)))

  let event_of_service info = Caml (CE_call_service info)

  let ce_registered_closure_class = "caml_closure"
  let ce_call_service_class = "caml_link"
  let unique_class = "caml_unique"

  let ce_call_service_attrib = "data-eliom-cookies-info"
  let unique_attrib = "data-eliom-unique-id"

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
    | RACamlEvent of caml_event
    | RALazyStr of string Eliom_lazy.request
    | RALazyStrL of separator * string Eliom_lazy.request list
  type attrib = aname * racontent
  let aname (name, _) = name
  let acontent = function
    | _, RA a -> a
    | _, RACamlEvent (CE_registered_closure (id,_)) ->
      AStr (closure_attr_prefix^(string_of_int id))
    | _, RACamlEvent _ -> AStr ("")
    | _, RALazyStr str -> AStr (Eliom_lazy.force str)
    | _, RALazyStrL (sep, str) -> AStrL (sep, List.map Eliom_lazy.force str)
  let racontent (_, a) = a

  let float_attrib name value = name, RA (AFloat value)
  let int_attrib name value = name, RA (AInt value)
  let string_attrib name value = name, RA (AStr value)
  let space_sep_attrib name values = name, RA (AStrL (Space, values))
  let comma_sep_attrib name values = name, RA (AStrL (Comma, values))
  let event_attrib name value = match value with
    | Raw value -> name, RA (AStr value)
    | Caml v -> name, RACamlEvent v
  let uri_attrib name value = name, RALazyStr value
  let uris_attrib name value = name, RALazyStrL (Space, value)

  type ename = string
  type node_id = string
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
  and elt = {
    elt : recontent;
    unique_id : node_id option;
  }

  let content e = match e.elt with
    | RE e -> e
    | RELazy e -> Eliom_lazy.force e

  let rcontent e = e.elt

  let is_unique elt = elt.unique_id <> None
  let get_unique_id elt = elt.unique_id

  let make elt =
    { elt = RE elt;
      unique_id = None; }

  let make_lazy elt =
    { elt = RELazy elt;
      unique_id = None; }

  let empty () = make Empty

  let comment c = make (Comment c)
  let pcdata d = make (PCDATA d)
  let encodedpcdata d = make (EncodedPCDATA d)
  let entity e = make (Entity e)

  let leaf ?(a = []) name =  make (Leaf (name, a))
  let node ?(a = []) name children = make (Node (name, a, children))
  let lazy_node ?(a = []) name children =
    make_lazy (Eliom_lazy.from_fun (fun () -> (Node (name, a, Eliom_lazy.force children))))

  let rec flatmap f = function
    | [] -> []
    | x :: rest -> f x @ flatmap f rest

  let translate root_leaf root_node sub_leaf sub_node update_state state n =
    let rec translate' state  n =
      match content n with
      | (Empty | Comment _ | PCDATA _ | Entity _) -> [n]
      | Leaf (name, attribs) ->
          sub_leaf state name attribs
      | Node (name, attribs, elts) ->
          sub_node state name attribs
            (flatmap (translate' (update_state name attribs state)) elts)
      | _ -> failwith "not implemented for Ocsigen syntax extension"
    in
    match content n with
    | (Empty | Comment _ | PCDATA _ | Entity _) -> n
    | Leaf (name, attribs) ->
	root_leaf name attribs
    | Node (name, attribs, elts) ->
	root_node name attribs (flatmap (translate' state) elts)
    | _ -> failwith "not implemented for Ocsigen syntax extension"

  module ClosureMap = Map.Make(struct type t = int let compare = compare end)

  type id_event_table =
      { event_table : ((unit -> unit) client_expr) ClosureMap.t }

end
