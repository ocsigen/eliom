let () = print_endline "[DEBUG ELIOM] eliom_lib.client: module start"

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
    | Url.Http {Url.hu_path = path; _}
    | Url.Https {Url.hu_path = path; _}
    | Url.File {Url.fu_path = path; _} ->
        path

  let path_of_url_string s =
    match Url.url_of_string s with
    | Some path -> path_of_url path
    | _ ->
        (* assuming relative URL and improvising *)
        split_path (try String.(sub s 0 (index s '?')) with Not_found -> s)
end

let raise_error ?exn ?section fmt =
  let k msg =
    Logs.msg ?src:section Logs.Error (fun fmt -> fmt "%s" msg);
    match exn with Some exn -> raise exn | None -> failwith msg
  in
  Printf.ksprintf k fmt

let log_inspect obj = Console.console##log (Obj.repr obj)
let eliom_logs_src = Logs.Src.create "eliom"
let _ = Logs.set_reporter (Logs_browser.console_reporter ())

let trace fmts =
  if Eliom_config.get_tracing ()
  then Printf.ksprintf (fun msg -> Logs.info (fun fmt -> fmt ">> %s" msg)) fmts
  else Printf.ksprintf ignore fmts

(* Debbuging *)
let jsalert a = Dom_html.window##(alert a)
let alert fmt = Printf.ksprintf (fun s -> jsalert (Js.string s)) fmt

let confirm =
  let f s =
    let s = Js.string s in
    Dom_html.window##(confirm s) |> Js.to_bool
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
let to_json ?typ s =
  match Sys.backend_type with
  | Other "js_of_ocaml" -> Js.to_string (Json.output s)
  | _ -> (
    match typ with
    | Some typ -> Deriving_Json.to_string typ s
    | None -> Js.to_string (Json.output s))

let of_json ?typ v =
  match Sys.backend_type with
  | Other "js_of_ocaml" -> Json.unsafe_input (Js.string v)
  | _ -> (
    match typ with
    | Some typ -> Deriving_Json.from_string typ v
    | None -> assert false)

(* Url.urlencode ~with_plus:true (Marshal.to_string x [])
    (* I encode the data because it seems that multipart does not
       like \0 character ... *)
*)
let encode_header_value ~typ x =
  (* We remove end of lines *)
  String.remove_eols (to_json ~typ x)

let unmarshal_js var = Marshal.from_string (Js.to_bytestring var) 0

type file_info = File.file Js.t

let make_cryptographic_safe_string ?len:_ () =
  failwith "make_cryptographic_safe_string not implemented client-side"

module Dom_reference = struct
  class type ['a, 'b] map = object
    method set : 'a -> 'b -> unit Js.meth
    method get : 'a -> 'b Js.Optdef.t Js.meth
    method delete : 'a -> unit Js.meth
  end

  let create_map () : (_, _) map Js.t =
    let map = Js.Unsafe.global##._Map in
    new%js map

  let create_weak_map () : (_, _) map Js.t =
    let weakMap = Js.Unsafe.global##._WeakMap in
    new%js weakMap

  type key = unit ref

  let retain_map : (Obj.t, (key, Obj.t) map Js.t) map Js.t = create_weak_map ()
  let new_key () = ref ()

  let retain ?(key = new_key ()) node ~keep =
    let node = Obj.repr node in
    let m =
      Js.Optdef.get
        (retain_map##get node)
        (fun () ->
           let m = create_map () in
           retain_map##set node m;
           m)
    in
    m##set key (Obj.repr keep)

  let retain_generic = retain

  let release ~key node =
    let node = Obj.repr node in
    Js.Optdef.iter (retain_map##get node) (fun m -> m##delete key)

  let transfer ~key ~src ~dst =
    let src = Obj.repr src in
    Js.Optdef.iter
      (retain_map##get src)
      (fun m ->
         Js.Optdef.iter (m##get key) (fun keep -> retain dst ~key ~keep);
         m##delete key)
end

let fork = Js_of_ocaml_eio.Eio_js.start

(* fork_promise launches f in a new Eio fiber and returns a promise for its result.
   The promise is created before entering the Eio context so it can be returned
   immediately. Eio.Promise.create() does NOT use Get_context effect - it only
   uses Trace.mint_id and Broadcast.create which are pure operations. *)
let fork_promise f =
  let p, u = Eio.Promise.create () in
  Js_of_ocaml_eio.Eio_js.start (fun () ->
    try
      let v = f () in
      ignore (Eio.Promise.try_resolve u (Ok v))
    with e -> ignore (Eio.Promise.try_resolve u (Error e)));
  p
