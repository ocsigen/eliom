
include Ocsigen_lib
include Eliom_lib_base

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
    | '\011' ->
        Buffer.add_string b "\\v"
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

type 'a client_value =
    'a Eliom_server.Client_value.t * Eliom_wrap.unwrapper

let create_client_value cv =
  cv, Eliom_wrap.create_unwrapper
        (Eliom_wrap.id_of_int
           Eliom_lib_base.client_value_unwrap_id_int)

let client_value_client_value = fst

let escaped_value value : Eliom_server.escaped_value (* * Eliom_wrap.unwrapper *) =
  to_poly value

let merge_aux err_msg =
  curry
    (function
       | Some value, None
       | None, Some value -> Some value
       | None, None
       | Some _, Some _ -> failwith err_msg)

module Client_value_data = struct

  include Client_value_data_base
  type t = ((int64 * int * poly) * Eliom_wrap.unwrapper) list

  let unwrapper =
    Eliom_wrap.create_unwrapper
      (Eliom_wrap.id_of_int unwrap_id_int)

  let with_unwrapper client_value_data =
    List.map
      (fun datum -> datum, unwrapper)
      client_value_data

end

module Injection_data = struct

  include Injection_data_base
  type t = ((string * poly) * Eliom_wrap.unwrapper) list

  let unwrapper =
    Eliom_wrap.create_unwrapper
      (Eliom_wrap.id_of_int unwrap_id_int)

  let with_unwrapper injection_data =
    List.map
      (fun (str, f) -> (str, f ()), unwrapper)
      injection_data

end
