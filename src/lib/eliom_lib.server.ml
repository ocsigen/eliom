
include Ocsigen_lib
include (Eliom_lib_base : module type of Eliom_lib_base
                          with type 'a Int64_map.t = 'a Eliom_lib_base.Int64_map.t
                          with type 'a String_map.t = 'a Eliom_lib_base.String_map.t
                          with type 'a Int_map.t = 'a Eliom_lib_base.Int_map.t
                          with type escaped_value = Eliom_lib_base.escaped_value
                          with type +'a Client_value_server_repr.t = 'a Eliom_lib_base.Client_value_server_repr.t
                          with type client_value_datum = Eliom_lib_base.client_value_datum
                          with type 'a injection_datum = 'a Eliom_lib_base.injection_datum
                          with type 'a compilation_unit_global_data = 'a Eliom_lib_base.compilation_unit_global_data
                          with type 'a global_data := 'a Eliom_lib_base.global_data
                          with type request_data = Eliom_lib_base.request_data)

let escaped_value_escaped_value = fst

let debug f = Printf.ksprintf (fun s -> Printf.eprintf "%s\n%!" s) f

let to_json ?typ v =
  match typ with
    | Some typ -> Deriving_Json.to_string typ v
    | None -> assert false (* implemented only client side *)

let of_json ?typ s =
  match typ with
    | Some typ -> Deriving_Json.from_string typ s
    | None -> assert false (* implemented only client side *)


type file_info = Ocsigen_extensions.file_info

let string_escape s =
  let l = String.length s in
  let b = Buffer.create (4 * l) in
  let conv = "0123456789abcdef" in
  for i = 0 to l - 1 do
    let c = s.[i] in
    match c with
      '\000' when i = l - 1 || s.[i + 1] < '0' || s.[i + 1] > '9' ->
        Buffer.add_string b "\\0"
    | '\b' ->
        Buffer.add_string b "\\b"
    | '\t' ->
        Buffer.add_string b "\\t"
    | '\n' ->
        Buffer.add_string b "\\n"
    (*| '\011' -> (* IE<9 doesn't like vertical tab \v *)
        Buffer.add_string b "\\v"*)
    | '\012' ->
        Buffer.add_string b "\\f"
    | '\r' ->
        Buffer.add_string b "\\r"
    | '\'' ->
        Buffer.add_string b "\\'"
    | '\\' ->
        Buffer.add_string b "\\\\"
    | '\000' .. '\031' | '\127' .. '\255' | '&' | '<' | '>' ->
        let c = Char.code c in
        Buffer.add_string b "\\x";
        Buffer.add_char b conv.[c lsr 4];
        Buffer.add_char b conv.[c land 0xf]
    | _ ->
        Buffer.add_char b c
  done;
  Buffer.contents b

let jsmarshal v = string_escape (Marshal.to_string v [])

let wrap_and_marshall_poly : poly -> string =
  fun poly ->
    string_escape (Marshal.to_string (Eliom_wrap.wrap poly) [])

type +'a client_value =
    'a Client_value_server_repr.t * Eliom_wrap.unwrapper

let create_client_value cv =
  cv, Eliom_wrap.create_unwrapper
        (Eliom_wrap.id_of_int
           Eliom_lib_base.client_value_unwrap_id_int)

let client_value_server_repr = fst

exception Client_value_creation_invalid_context of int64

let escaped_value value : escaped_value (* * Eliom_wrap.unwrapper *) =
  to_poly value

type global_data = poly Eliom_lib_base.global_data * Eliom_wrap.unwrapper

let global_data_unwrapper =
  Eliom_wrap.create_unwrapper
    (Eliom_wrap.id_of_int global_data_unwrap_id_int)
