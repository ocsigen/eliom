(*external get_closure_arg : unit -> 'a = "caml_get_closure_arg"*)
val register_closure : int -> ('a -> 'b) -> unit
(*val nodes : (int, Js.Node.t) Hashtbl.t
val set_node_id : Js.Node.t -> int -> unit*)
type ref_tree

val unwrap : 'a Eliom_client_types.data_key -> 'a

val unwrap_sp : Eliom_client_types.server_params Eliom_client_types.data_key ->
  Eliom_client_types.server_params

val unwrap_node : Eliom_client_types.server_params Eliom_client_types.data_key -> Js.Node.t


(**/**)
val relink_dom_list : float -> Js.Node.t list -> (int * ref_tree) list -> unit
val fill_global_data_table : (float * int) * unit list -> unit
