(*external get_closure_arg : unit -> 'a = "caml_get_closure_arg"*)
val register_closure : int -> ('a -> 'b) -> unit
(*val nodes : (int, Js.Node.t) Hashtbl.t
val set_node_id : Js.Node.t -> int -> unit*)
val retrieve_node : int -> Js.Node.t
type ref_tree
val unwrap : 'a Eliom_client_types.data_key -> 'a
