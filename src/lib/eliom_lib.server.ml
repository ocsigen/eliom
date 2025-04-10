(* Please put in this file ONLY extensions of the standard OCaml library.
   And remove all the Eliom/Ocsigen specific stuff. *)

include Ocsigen_lib

include (
  Eliom_lib_base :
    module type of Eliom_lib_base
    with type 'a Int64_map.t = 'a Eliom_lib_base.Int64_map.t
    with type 'a String_map.t = 'a Eliom_lib_base.String_map.t
    with type 'a Int_map.t = 'a Eliom_lib_base.Int_map.t)

let debug f = Printf.ksprintf (fun s -> Printf.eprintf "%s\n%!" s) f

let to_json ?typ v =
  match typ with
  | Some typ -> Deriving_Json.to_string typ v
  | None -> assert false
(* implemented only client side *)

let of_json ?typ s =
  match typ with
  | Some typ -> Deriving_Json.from_string typ s
  | None -> assert false
(* implemented only client side *)

let eliom_logs_src = Lwt_log.Section.make "eliom"

type file_info = Ocsigen_extensions.file_info

let b = Buffer.create (16 * 1024)

let string_escape s =
  let l = String.length s in
  Buffer.clear b;
  let conv = "0123456789abcdef" in
  for i = 0 to l - 1 do
    let c = s.[i] in
    match c with
    | '\000' when i = l - 1 || s.[i + 1] < '0' || s.[i + 1] > '9' ->
        Buffer.add_string b "\\0"
    | '\b' -> Buffer.add_string b "\\b"
    | '\t' -> Buffer.add_string b "\\t"
    | '\n' -> Buffer.add_string b "\\n"
    (*| '\011' -> (* IE<9 doesn't like vertical tab \v *)
        Buffer.add_string b "\\v"*)
    | '\012' -> Buffer.add_string b "\\f"
    | '\r' -> Buffer.add_string b "\\r"
    | '\'' -> Buffer.add_string b "\\'"
    | '\\' -> Buffer.add_string b "\\\\"
    | '\000' .. '\031' | '\127' .. '\255' | '&' | '<' | '>' ->
        let c = Char.code c in
        Buffer.add_string b "\\x";
        Buffer.add_char b conv.[c lsr 4];
        Buffer.add_char b conv.[c land 0xf]
    | _ -> Buffer.add_char b c
  done;
  Buffer.contents b

let jsmarshal v = string_escape (Marshal.to_string v [])

let make_cryptographic_safe_string ?len () =
  match len with
  | None -> Ocsigen_lib.make_cryptographic_safe_string ()
  | Some l -> String.sub (Ocsigen_lib.make_cryptographic_safe_string ()) 0 l
