(* Marshal an OCaml value into a string. All characters are escaped *)
val jsmarshal: 'a -> string

(* Fresh name generator *)
val fresh_id : unit -> string
