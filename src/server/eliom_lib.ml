
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

let escaped_value value : Eliom_server.escaped_value * Eliom_wrap.unwrapper =
  wrap_and_marshall_poly (to_poly value),
  Eliom_wrap.create_unwrapper
    (Eliom_wrap.id_of_int Eliom_lib_base.escaped_value_unwrap_id_int)

let merge_aux err_msg =
  curry
    (function
       | Some value, None
       | None, Some value -> Some value
       | None, None
       | Some _, Some _ -> failwith err_msg)

module Client_value_data = struct

  type t = poly Int_map.t Int64_map.t

  type client = string Int_map.t Int64_map.t

  let empty =
    Int64_map.empty

  let map f table =
    Int64_map.map (Int_map.map f) table

  let add closure_id instance_id poly table =
    let instances =
      try Int64_map.find closure_id table
      with Not_found -> Int_map.empty
    in
    let instances' = Int_map.add instance_id poly instances in
    Int64_map.add closure_id instances' table

  let union table_1 table_2 =
    Int64_map.merge
      (fun closure_id opt_instances_1 opt_instances_2 ->
         let instances_1 = Option.get (fun () -> Int_map.empty) opt_instances_1 in
         let instances_2 = Option.get (fun () -> Int_map.empty) opt_instances_2 in
         Some (Int_map.merge (fun _ -> merge_aux "Client_value_data.union")
                 instances_1 instances_2))
      table_1 table_2

  let to_client : t -> client =
    fun table ->
      map wrap_and_marshall_poly table
end

module Injection_data = struct

  type 'a t = 'a String_map.t
  type client = string String_map.t

  let empty = String_map.empty
  let add = String_map.add
  let union table_1 table_2 =
    String_map.merge (fun _ -> merge_aux "Injection_data.union")
      table_1 table_2

  let to_client : poly t -> client =
    fun table ->
      String_map.map wrap_and_marshall_poly table
end
