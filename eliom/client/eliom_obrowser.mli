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
val relink_dom_list : int -> Js.Node.t list -> (int * ref_tree) list -> unit
val fill_global_data_table : (int * int) * unit list -> unit

val make_a_with_onclick :
  (?a:'a -> onclick:string -> 'b -> 'c) ->
  ?absolute:'d ->
  ?absolute_path:'e ->
  ?https:'f ->
  ?a:'a ->
  service:'g ->
  sp:Eliom_sessions.server_params ->
  ?hostname:'h ->
  ?port:'i ->
  ?fragment:'j ->
  ?keep_nl_params:'k -> ?nl_params:'l -> 'b -> 'm -> 'c
