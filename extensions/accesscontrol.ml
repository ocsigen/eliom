(* Ocsigen
 * http://www.ocsigen.org
 * Module accesscontrol.ml
 * Copyright (C) 2007 Vincent Balat, Stéphane Glondu
 * Laboratoire PPS - CNRS Université Paris Diderot
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

(** Filtering requests in the configuration file *)

(*

Then load it dynamically from Ocsigen's config file:
   <extension module=".../accesscontrol.cmo"/>

*)

open Printf
open Lwt
open Ocsigen_extensions
open Simplexmlparser
open Ocsigen_http_frame


(*****************************************************************************)
let rec parse_global_config = function
  | [] -> ()
  | _ -> badconfig "Unexpected content inside accesscontrol config"

let _ = parse_global_config (Ocsigen_extensions.get_config ())


(*****************************************************************************)
(* Parsing a condition *)

let rec parse_condition = function

    | Element ("ip", ["value", s], []) ->
        let ip_with_mask =
          try
            Ocsigen_lib.parse_ip s
          with Failure _ ->
            badconfig "Bad ip/netmask [%s] in <ip> condition" s
        in
        (fun ri ->
           let r = Ocsigen_lib.match_ip ip_with_mask (Lazy.force ri.ri_remote_ip_parsed) in
           if r then
             Ocsigen_messages.debug2 (sprintf "--Access control (ip): %s matches %s" ri.ri_remote_ip s)
           else
             Ocsigen_messages.debug2 (sprintf "--Access control (ip): %s does not match %s" ri.ri_remote_ip s);
           r)

    | Element ("header", ["name", name; "regexp", reg], []) ->
        let regexp =
          try
            Netstring_pcre.regexp ("^"^reg^"$")
          with Failure _ ->
            badconfig "Bad regular expression [%s] in <header> condition" reg
        in
        (fun ri ->
           let r =
             List.exists
               (fun a ->
                  let r = Netstring_pcre.string_match regexp a 0 <> None in
                  if r then Ocsigen_messages.debug2 (sprintf "--Access control (header): header %s matches \"%s\"" name reg);
                  r)
               (Http_headers.find_all
                  (Http_headers.name name)
                  ri.ri_http_frame.Ocsigen_http_frame.header.Ocsigen_http_frame.Http_header.headers)
           in
           if not r then Ocsigen_messages.debug2 (sprintf "--Access control (header): header %s does not match \"%s\"" name reg);
           r)

    | Element ("method", ["value", s], []) ->
        let meth =
          try
            Framepp.method_of_string s
          with Failure _ ->
            badconfig "Bad method [%s] in <method> condition" s
        in
        (fun ri ->
           let r = meth = ri.ri_method in
           if r then Ocsigen_messages.debug
             (fun () -> sprintf "--Access control (method): %s matches %s" (Framepp.string_of_method ri.ri_method) s)
           else Ocsigen_messages.debug
             (fun () -> sprintf "--Access control (method): %s does not match %s" (Framepp.string_of_method ri.ri_method) s);
           r)

    | Element ("protocol", ["value", s], []) ->
        let pr =
          try
            Framepp.proto_of_string s
          with Failure _ ->
            badconfig "Bad protocol [%s] in <protocol> condition" s
        in
        (fun ri ->
           let r = pr = ri.ri_protocol in
           if r then Ocsigen_messages.debug
             (fun () -> sprintf "--Access control (protocol): %s matches %s" (Framepp.string_of_proto ri.ri_protocol) s)
           else Ocsigen_messages.debug
             (fun () -> sprintf "--Access control (protocol): %s does not match %s" (Framepp.string_of_proto ri.ri_protocol) s);
           r)

    | Element ("path", ["regexp", s], []) ->
        let regexp =
          try
            Netstring_pcre.regexp ("^"^s^"$")
          with Failure _ ->
            badconfig "Bad regular expression [%s] in <path> condition" s
        in
        (fun ri ->
           let r =
             Netstring_pcre.string_match
               regexp ri.ri_sub_path_string 0 <> None
           in
           if r then Ocsigen_messages.debug
             (fun () -> sprintf "--Access control (path): \"%s\" matches \"%s\"" ri.ri_sub_path_string s)
           else Ocsigen_messages.debug
               (fun () -> sprintf "--Access control (path): \"%s\" does not match \"%s\"" ri.ri_sub_path_string s);
           r)

    | Element ("and", [], sub) ->
        let sub = List.map parse_condition sub in
        (fun ri -> List.for_all (fun cond -> cond ri) sub)

    | Element ("or", [], sub) ->
        let sub = List.map parse_condition sub in
        (fun ri -> List.exists (fun cond -> cond ri) sub)

    | Element ("not", [], [sub]) ->
        let sub = parse_condition sub in
        (fun ri -> not (sub ri))

    | Element (("and"|"or"|"not") as t, _, _) ->
        badconfig "Bad syntax for <%s> condition" t

    | _ ->
        badconfig "Bad syntax for condition"


(*****************************************************************************)
(* Parsing filters *)

let parse_config path charset _ parse_fun = function

  | Element ("if", [], sub) ->
      let (condition, sub) = match sub with
        | cond::q -> (parse_condition cond, q)
        | _ -> badconfig "Bad condition in <if>"
      in
      let (ithen, sub) = match sub with
          | Element("then", [], ithen)::q -> (parse_fun ithen, q)
          | _ -> badconfig "Bad <then> branch in <if>"
      in
      let (ielse, sub) = match sub with
          | Element ("else", [], ielse)::([] as q) -> (parse_fun ielse, q)
          | [] -> (parse_fun [], [])
          | _ -> badconfig "Bad <else> branch in <if>"
      in
      (function
        | Ocsigen_extensions.Req_found (ri, _)
        | Ocsigen_extensions.Req_not_found (_, ri) ->
            Lwt.return
              (if condition ri then begin
                 Ocsigen_messages.debug2 "--Access control: => going into <then> branch";
                 Ocsigen_extensions.Ext_sub_result ithen
               end
               else begin
                 Ocsigen_messages.debug2 "--Access control: => going into <else> branch, if any";
                 Ocsigen_extensions.Ext_sub_result ielse
               end))

  | Element ("notfound", [], []) ->
      (fun rs ->
         Ocsigen_messages.debug2 "--Access control: taking in charge 404";
         fail (Ocsigen_http_error (Ocsigen_http_frame.Cookies.empty, 404)))

  | Element ("forbidden", [], []) ->
      (fun rs ->
         Ocsigen_messages.debug2 "--Access control: taking in charge 403";
         fail (Ocsigen_http_error (Ocsigen_http_frame.Cookies.empty, 403)))

  | Element ("iffound", [], sub) ->
      let ext = parse_fun sub in
      (function
         | Ocsigen_extensions.Req_found (_, _) ->
             Lwt.return (Ext_sub_result ext)
         | Ocsigen_extensions.Req_not_found (err, ri) ->
             Lwt.return (Ocsigen_extensions.Ext_next err))

  | Element ("ifnotfound", [], sub) ->
      let ext = parse_fun sub in
      (function
         | Ocsigen_extensions.Req_found (_, r) ->
             Lwt.return (Ocsigen_extensions.Ext_found r)
         | Ocsigen_extensions.Req_not_found (err, ri) ->
             Lwt.return (Ext_sub_result ext))

  | Element ("ifnotfound", [("code", s)], sub) ->
      let ext = parse_fun sub in
      let r = Netstring_pcre.regexp ("^"^s^"$") in
      (function
         | Ocsigen_extensions.Req_found (_, r) ->
             Lwt.return (Ocsigen_extensions.Ext_found r)
         | Ocsigen_extensions.Req_not_found (err, ri) ->
             if Netstring_pcre.string_match r (string_of_int err) 0 <> None then
               Lwt.return (Ext_sub_result ext)
             else
               Lwt.return (Ocsigen_extensions.Ext_next err))

  | Element (t, _, _) -> raise (Bad_config_tag_for_extension t)
  | _ -> badconfig "(accesscontrol extension) Bad data"



(*****************************************************************************)
(** Function to be called at the beginning of the initialisation phase
    of the server (actually each time the config file is reloaded) *)
let start_init () =
  ()

(** Function to be called at the end of the initialisation phase *)
let end_init () =
  ()




(*****************************************************************************)
(** Registration of the extension *)
let _ = register_extension
  (fun hostpattern -> parse_config)
  (fun hostpattern -> parse_config)
  start_init
  end_init
  raise

