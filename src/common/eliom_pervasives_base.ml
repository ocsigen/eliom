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
type 'a wrapped_value = poly * 'a

module RawXML = struct

  type separator = Space | Comma

  let separator_to_string = function
    | Space -> " "
    | Comma -> ", "

  type caml_event =
    | CE_registered_closure of (unit -> unit) client_expr
    | CE_client_closure of (unit -> unit)
    | CE_call_service of [ `A | `Form_get | `Form_post] * (bool * string list) option

  type event =
    | Raw of string
    | Caml of caml_event

  let event_of_string s = Raw s
  let string_of_event = function
    | Raw s -> s
    | Caml _ -> "/* Invalid Caml value */"
  let event_of_js id args = Caml (CE_registered_closure (id, args))

  let event_of_service kind cookies =
    Caml (CE_call_service (kind, cookies))

  type aname = string
  type acontent =
    | AFloat of aname * float
    | AInt of aname * int
    | AStr of aname * string
    | AStrL of separator * aname * string list
  type racontent =
    | RA of acontent
    | RACamlEvent of (aname * caml_event)

  type attrib = racontent
  let aname = function
    | RA (AFloat (name, _) | AInt (name, _)
      | AStr (name, _) | AStrL (_, name, _))
    | RACamlEvent (name, _) -> name
  let acontent = function
    | RA a -> a
    | RACamlEvent (n, _) -> AStr (n, "/* To be patched... */")
  let racontent = id

  let float_attrib name value = RA (AFloat (name, value))
  let int_attrib name value = RA (AInt (name, value))
  let string_attrib name value = RA (AStr (name, value))
  let space_sep_attrib name values = RA (AStrL (Space, name, values))
  let comma_sep_attrib name values = RA (AStrL (Comma, name, values))
  let event_attrib name value = match value with
    | Raw value -> RA (AStr (name, value))
    | Caml v -> RACamlEvent (name, v)

  let attrib_hash = Hashtbl.hash

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
  and elt = {
    elt : econtent ;
    hash : int ;
    unique_id : node_id option;
  }

  let content e = e.elt
  let hash e = e.hash
  let compare e1 e2 =
    let r = compare e1.hash e2.hash in
    if r = 0 then compare e1 e2 else r

  let is_unique elt = elt.unique_id <> None
  let get_unique_id elt = elt.unique_id

  let (>+>) x f = f x

  let combine x y = (x + 65599 * y) mod 32
  let make_hash elt = match elt with
    | Empty -> 0
    | Comment s -> 1 + Hashtbl.hash s
    | EncodedPCDATA s -> 2 + Hashtbl.hash s
    | PCDATA s -> 3 + Hashtbl.hash s
    | Entity s -> 4 + Hashtbl.hash s
    | Leaf (tag, attribs) ->
	5 + Hashtbl.hash tag
	>+> List.fold_right (fun a -> combine (attrib_hash a)) attribs
    | Node (name, attribs, elts) ->
	6 + Hashtbl.hash name
	>+> List.fold_right (fun a -> combine (attrib_hash a)) attribs
	>+> List.fold_right (fun e -> combine e.hash) elts

  let make elt =
    { elt = elt;
      hash = (make_hash elt) mod 32 ;
      unique_id = None; }

  let empty () = make Empty

  let comment c = make (Comment c)
  let pcdata d = make (PCDATA d)
  let encodedpcdata d = make (EncodedPCDATA d)
  let entity e = make (Entity e)

  let leaf ?(a = []) name =  make (Leaf (name, a))
  let node ?(a = []) name children = make (Node (name, a, children))

  let rec flatmap f = function
    | [] -> []
    | x :: rest -> f x @ flatmap f rest

  let translate root_leaf root_node sub_leaf sub_node update_state state n =
    let rec translate' state  n =
      match n.elt with
      | (Empty | Comment _ | PCDATA _ | Entity _) -> [n]
      | Leaf (name, attribs) ->
          sub_leaf state name attribs
      | Node (name, attribs, elts) ->
          sub_node state name attribs
            (flatmap (translate' (update_state name attribs state)) elts)
      | _ -> failwith "not implemented for Ocsigen syntax extension"
    in
    match n.elt with
    | (Empty | Comment _ | PCDATA _ | Entity _) -> n
    | Leaf (name, attribs) ->
	root_leaf name attribs
    | Node (name, attribs, elts) ->
	root_node name attribs (flatmap (translate' state) elts)
    | _ -> failwith "not implemented for Ocsigen syntax extension"


  module Hashtbl =
    Hashtbl.Make(struct
		   type t = elt
		   let equal = (==)
		   let hash e = e.hash
		 end)

  type ref_tree =
    | Ref_node of (node_id option * (string * caml_event) list * ref_tree list)
    | Ref_empty of int

end
