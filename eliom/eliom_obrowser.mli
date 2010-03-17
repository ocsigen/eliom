(* Marshal an OCaml value into a string. All characters are escaped *)
val jsmarshal: 'a -> string

(* Fresh name generator *)
val fresh_id : unit -> string

(* Translates sp into client side sp *)
val client_sp : Eliom_sessions.server_params -> Eliom_client_types.server_params

