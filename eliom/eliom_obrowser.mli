(* Marshal an OCaml value into a string. All characters are escaped *)
val jsmarshal: 'a -> string

(* Fresh name generator *)
val fresh_id : unit -> string


(**/**)
val get_global_eliom_appl_data_ : 
  sp:Eliom_sessions.server_params -> (float * int) * unit list

val wrap : sp:Eliom_sessions.server_params -> 'a -> 
  'a Eliom_client_types.data_key

val wrap_sp : sp:Eliom_sessions.server_params ->
  Eliom_client_types.server_params Eliom_client_types.data_key

val wrap_node : sp:Eliom_sessions.server_params ->
  'a XHTML.M.elt -> 'node Eliom_client_types.data_key
