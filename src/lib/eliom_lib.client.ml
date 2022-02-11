(* Ocsigen
 * Copyright (C) 2005-2008 Vincent Balat, Stéphane Glondu
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

open Js_of_ocaml
include Ocsigen_lib_base

include (
  Eliom_lib_base :
    module type of Eliom_lib_base
      with type 'a Int64_map.t = 'a Eliom_lib_base.Int64_map.t
      with type 'a String_map.t = 'a Eliom_lib_base.String_map.t
      with type 'a Int_map.t = 'a Eliom_lib_base.Int_map.t)

(*****************************************************************************)

module Url = struct
  include Url
  include Url_base

  let decode = Url.urldecode
  let encode ?plus s = Url.urlencode ?with_plus:plus s
  let make_encoded_parameters = Url.encode_arguments
  let split_path = Url.path_of_path_string
  let ssl_re = Regexp.regexp "^(https?):\\/\\/"

  let get_ssl s =
    Option.map
      (fun r -> Regexp.matched_group r 1 = Some "https")
      (Regexp.string_match ssl_re s 0)

  let resolve s =
    let a = Dom_html.createA Dom_html.document in
    a##.href := Js.string s;
    Js.to_string a##.href

  let has_get_args url =
    try
      ignore (String.index url '?');
      true
    with Not_found -> false

  let add_get_args url get_args =
    if get_args = []
    then url
    else
      url ^ (if has_get_args url then "&" else "?") ^ encode_arguments get_args

  let string_of_url_path ~encode l =
    if encode
    then print_endline "Warning: Eliom_lib.string_of_url_path ignores ~encode";
    String.concat "/" l

  let path_of_url = function
    | Url.Http {Url.hu_path = path}
    | Url.Https {Url.hu_path = path}
    | Url.File {Url.fu_path = path} ->
        path

  let path_of_url_string s =
    match Url.url_of_string s with
    | Some path -> path_of_url path
    | _ -> (
        (* assuming relative URL and improvising *)
        split_path
        @@ try String.(sub s 0 (index s '?')) with Not_found -> s)
end

module Lwt_log = struct
  include Lwt_log_js

  let raise_error ?inspect ?exn ?section ?location ?logger msg =
    Lwt.ignore_result
      (log ?inspect ?exn ?section ?location ?logger ~level:Error msg);
    match exn with Some exn -> raise exn | None -> failwith msg

  let raise_error_f ?inspect ?exn ?section ?location ?logger fmt =
    Printf.ksprintf (raise_error ?inspect ?exn ?section ?location ?logger) fmt

  let eliom = Section.make "eliom"
end

let _ =
  Lwt_log.default := Lwt_log.console;
  Lwt.async_exception_hook :=
    fun exn ->
      Firebug.console##error_3 (Js.string "Lwt.async:")
        (Js.string (Printexc.to_string exn))
        exn

(* Deprecated ON *)
let debug_exn fmt exn = Lwt_log.ign_info_f ~exn fmt
let debug fmt = Lwt_log.ign_info_f fmt
let error fmt = Lwt_log.raise_error_f fmt
let error_any any fmt = Lwt_log.raise_error_f ~inspect:any fmt
let jsdebug a = Lwt_log.ign_info ~inspect:a "Jsdebug"
(* Deprecated OFF *)

let trace fmt =
  if Eliom_config.get_tracing ()
  then Lwt_log.ign_info_f (">> " ^^ fmt)
  else Printf.ksprintf ignore fmt

let lwt_ignore ?(message = "") t =
  Lwt.on_failure t (fun exn -> Lwt_log.ign_info_f ~exn "%s" message)

(* Debbuging *)
let jsalert a = Dom_html.window ## (alert a)
let alert fmt = Printf.ksprintf (fun s -> jsalert (Js.string s)) fmt

let confirm =
  let f s =
    let s = Js.string s in
    Dom_html.window ## (confirm s) |> Js.to_bool
  in
  fun fmt -> Printf.ksprintf f fmt

let debug_var s v = Js.Unsafe.set Dom_html.window (Js.string s) v

module String = struct
  include String_base

  let eol_re = Regexp.regexp "[\r\n]"
  let remove_eols s = Regexp.global_replace eol_re s ""
end

(*****************************************************************************)

(* let () =
  (Js.Unsafe.coerce Dom_html.window)##set_tracing <-
    Js.wrap_callback (fun v -> set_tracing (Js.to_bool v)) *)

(* We do not use the deriving (un)marshaling even if typ is available
   because direct jsn (un)marshaling is very fast client side
*)
let to_json ?typ:_ s = Js.to_string (Json.output s)
let of_json ?typ:_ v = Json.unsafe_input (Js.string v)

(* to marshal data and put it in a form *)
let encode_form_value x = to_json x

(* Url.urlencode ~with_plus:true (Marshal.to_string x [])
    (* I encode the data because it seems that multipart does not
       like \0 character ... *)
*)
let encode_header_value x =
  (* We remove end of lines *)
  String.remove_eols (to_json x)

let unmarshal_js var = Marshal.from_string (Js.to_bytestring var) 0

type file_info = File.file Js.t

let make_cryptographic_safe_string ?len:_ () =
  failwith "make_cryptographic_safe_string not implemented client-side"
