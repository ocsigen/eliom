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

include Ocsigen_lib_base
include Eliom_lib_base

exception False

type 'a client_value = 'a

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

end

module String = struct
  include String_base
  let remove_eols s =
    let eol_re = Regexp.regexp "[\r\n]" in
    Regexp.global_replace eol_re s ""
end

let js_array_to_list arr =
  let li = ref [] in
  for i = 0 to pred arr##length do
    let x =
      Js.Optdef.get
        (Js.array_get arr i)
        (fun () -> failwith "js_array_to_list")
    in
    li := x :: !li
  done;
  List.rev !li

(*****************************************************************************)

let debug_exn f e =
  Printf.ksprintf (fun s -> Firebug.console##log (Js.string (s^(Printexc.to_string e)))) f
let debug f = Printf.ksprintf (fun s -> Firebug.console##log (Js.string s)) f
let error f = Printf.ksprintf (fun s -> Firebug.console##error (Js.string s); failwith s) f
let jsdebug a = Firebug.console##log (a)
let alert f = Printf.ksprintf (fun s -> Dom_html.window##alert (Js.string s)) f
let jsalert a = Dom_html.window##alert (a)

let debug_var s v = Js.Unsafe.set Dom_html.window (Js.string s) v

let lwt_ignore ?(message="") t = Lwt.on_failure t (fun e -> debug_exn "%s" e message)

(* We do not use the deriving (un)marshaling even if typ is available
   because direct jsn (un)marshaling is very fast client side
*)
let to_json ?typ s = Js.to_string (Json.output s)
let of_json ?typ v = Json.unsafe_input (Js.string v)

(* to marshal data and put it in a form *)
let encode_form_value x = to_json x
(* Url.urlencode ~with_plus:true (Marshal.to_string x [])
    (* I encode the data because it seems that multipart does not
       like \0 character ... *)
*)
let encode_header_value x =
  (* We remove end of lines *)
  String.remove_eols (to_json x)

let unmarshal_js_var s =
  Marshal.from_string (Js.to_bytestring (Js.Unsafe.variable s)) 0

let unwrap_bytestring_unescape str =
  Eliom_unwrap.unwrap
    (Js.to_bytestring
       (Js.unescape
          (Js.Unsafe.eval_string
             ("\""^escape_quotes str^"\""))))
    0

(** Empty type (not used on client side, see eliom_parameter_base.ml) *)
type file_info

module Client_value_data = struct

  type t = string Int_map.t Int64_map.t

  let closure_ids table =
    List.map fst
      (Int64_map.bindings table)

  let instance_ids closure_id table =
    List.map fst
      (Int_map.bindings
         (Int64_map.find closure_id table))

  let find closure_id instance_id table =
    let instances = Int64_map.find closure_id table in
    unwrap_bytestring_unescape
      (Int_map.find instance_id instances)
end

module Injection_data = struct

  type t = string String_map.t

  let names table =
    List.map fst (String_map.bindings table)

  let find name table =
    unwrap_bytestring_unescape
      (String_map.find name table)
end


