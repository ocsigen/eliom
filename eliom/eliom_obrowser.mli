
(* Fresh name generator *)
val fresh_id : unit -> string


(**/**)
val get_global_eliom_appl_data_ : 
  sp:Eliom_sessions.server_params -> (int * int) * unit list

val wrap : sp:Eliom_sessions.server_params -> 'a -> 
  'a Eliom_client_types.data_key

val wrap_sp : sp:Eliom_sessions.server_params ->
  Eliom_client_types.server_params Eliom_client_types.data_key

val wrap_node : sp:Eliom_sessions.server_params ->
  'a XHTML.M.elt -> 'node Eliom_client_types.data_key

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
