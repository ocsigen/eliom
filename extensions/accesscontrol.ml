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

open Lwt
open Extensions
open Simplexmlparser
open Http_frame


(*****************************************************************************)

type password = [ `Plain of string ]

type filter =
  | Filter_Ip of (int32 * int32)
  | Filter_Path of Netstring_pcre.regexp
  | Filter_Method of Http_frame.Http_header.http_method
  | Filter_Header of string * Netstring_pcre.regexp
  | Filter_Protocol of Http_frame.Http_header.proto
  | Filter_Auth of (string -> password -> bool)
  | Filter_Disj of filter list
  | Filter_Conj of filter list
  | Filter_Neg of filter
  | Filter_Error of exn


(*****************************************************************************)
let rec parse_global_config = function
  | [] -> ()
  | _ -> raise (Error_in_config_file
                  ("Unexpected content inside accesscontrol config"))

let _ = parse_global_config (Extensions.get_config ())



(*****************************************************************************)
(* Management of authentication methods *)

let register_authentication_method,
  get_authentication_method =
  let fun_auth = ref
    (fun config ->
       raise (Bad_config_tag_for_extension "<unknown authentication method>"))
  in

  (********* register_authentication_method *********)
  (fun new_fun_auth ->
     let old_fun_auth = !fun_auth in
     fun_auth :=
       (fun config ->
          try
            old_fun_auth config
          with
            | Bad_config_tag_for_extension c -> new_fun_auth config)),

  (********* get_authentication_method *********)
  (fun config ->
     !fun_auth config)


(** for tests **)
let _ = register_authentication_method
  (function
     | Element ("htpasswd", _, _) -> (fun login passwd -> assert false)
     | _ -> raise (Bad_config_tag_for_extension "not for htpasswd"))

(*****************************************************************************)
(* Finding access pattern *)

let find_access ri =
  let rec one_access = function
    | Filter_Ip (ip32, mask) ->
        let r = Int32.logand (Lazy.force ri.ri_ip32) mask = ip32 in
        if r then
          Messages.debug2 "--Access control: IP matches mask"
        else
          Messages.debug2 "--Access control: IP does not match mask";
        r
    | Filter_Path regexp ->
        let r =
          Netstring_pcre.string_match
            regexp ri.ri_sub_path_string 0 <> None
        in
        if r then
          Messages.debug
            (fun () -> "--Access control: Path "^
              ri.ri_sub_path_string^" matches regexp")
        else
          Messages.debug
            (fun () -> "--Access control: Path "^
              ri.ri_sub_path_string^" does not match regexp");
        r
    | Filter_Method meth ->
        let r = meth = ri.ri_method in
        if r then
          Messages.debug2 "--Access control: Method matches"
        else
          Messages.debug2 "--Access control: Method does not match";
        r
    | Filter_Protocol pr ->
        let r = pr = ri.ri_protocol in
        if r then
          Messages.debug2 "--Access control: Protocol matches"
        else
          Messages.debug2 "--Access control: Protocol does not match";
        r
    | Filter_Auth auth ->
        Messages.debug2 "--Access control: Authentication failed";
        raise (Http_error.Http_exception (401, None, None))
    | Filter_Header (name, regexp) ->
        let r =
          List.exists
            (fun a -> Netstring_pcre.string_match regexp a 0 <> None)
            (Http_headers.find_all
               (Http_headers.name name)
               ri.ri_http_frame.Http_frame.header.Http_frame.Http_header.headers)
        in
        if r then
          Messages.debug2 "--Access control: Header matches regexp"
        else
          Messages.debug2 "--Access control: Header does not match regexp";
        r
    | Filter_Conj filters -> List.for_all one_access filters
    | Filter_Disj filters -> List.exists one_access filters
    | Filter_Neg f -> not (one_access f)
    | Filter_Error e -> raise e
  in
  one_access



let gen test charset = function
  | Extensions.Req_found (_, r) ->
      Lwt.return (Extensions.Ext_found r)
  | Extensions.Req_not_found (err, ri) ->
      try
        if find_access ri test then begin
          Messages.debug2 "--Access control: => Access granted!";
          Lwt.return (Ext_next err)
        end
        else begin
          Messages.debug2 "--Access control: => Access denied!";
          Lwt.return (Ext_stop_site (Http_frame.Cookies.empty, 403))
        end
      with
        | e ->
            Messages.debug2 "--Access control: taking in charge an error";
            fail e (* for example Ocsigen_http_error 404 or 403 *)
              (* server.ml has a default handler for HTTP errors *)




(*****************************************************************************)
(** Configuration for each site.
    These tags are inside <site ...>...</site> in the config file.

   For example:
   <site dir="">
     <accesscontrol regexp="" dest="" />
   </site>

 *)


let parse_config path charset _ parse_fun =
  let parse_filter = function
    | ("ip", ["value", s], []) ->
        (try
          Filter_Ip (Ocsimisc.parse_ip_netmask s)
        with Failure _ ->
          raise (Error_in_config_file "Bad ip/netmask value in ip filter"))
    | ("header", ["name", s; "regexp", r], []) ->
        (try
          Filter_Header (s, Netstring_pcre.regexp r)
        with Failure _ ->
          raise (Error_in_config_file
                   "Bad regular expression in header filter"))
    | ("method", ["value", s], []) ->
        (try
          Filter_Method (Framepp.method_of_string s)
        with Failure _ ->
          raise (Error_in_config_file "Bad method value in method filter"))
    | ("protocol", ["value", s], []) ->
        (try
          Filter_Protocol (Framepp.proto_of_string s)
        with Failure _ ->
          raise (Error_in_config_file "Bad protocol value in protocol filter"))
    | ("path", ["regexp", s], []) ->
        (try
          Filter_Path (Netstring_pcre.regexp s)
        with Failure _ ->
          raise (Error_in_config_file
                   "Bad regular expression in <path/>"))
    | ("auth", ["authtype", "basic"; "realm", r], [sub]) ->
        (try
           Filter_Auth (get_authentication_method sub)
         with Bad_config_tag_for_extension _ ->
           raise (Error_in_config_file "Unable to find proper authentication method"))
    | (t, _, _) -> raise (Error_in_config_file ("(accesscontrol extension) Problem with "^t^" filter in configuration file."))
  in
  let rec parse_sub = function
    | Element ("allow", ["type", "and"], sub) -> Filter_Conj (List.map parse_sub sub)
    | Element ("allow", ["type", "or"], sub) -> Filter_Disj (List.map parse_sub sub)
    | Element ("allow", ("type", t)::attrs, sub) -> parse_filter (t, attrs, sub)
    | Element ("deny", attrs, sub) -> Filter_Neg (parse_sub (Element ("allow", attrs, sub)))
    | Element (t, _, _) ->
        raise (Error_in_config_file ("(accesscontrol extension) Problem with tag <"^t^"> in configuration file."))
    | _ -> raise (Error_in_config_file "(accesscontrol extension) Bad data")
  in
  function
    | Element (("allow" | "deny"), attrs, sub) as e -> gen (parse_sub e) charset
    | Element ("forbidden", [], []) -> 
        gen (Filter_Error
               (Ocsigen_http_error (Http_frame.Cookies.empty, 403))) charset
    | Element ("notfound", [], []) -> 
        gen (Filter_Error 
               (Ocsigen_http_error (Http_frame.Cookies.empty, 404))) charset
    | Element ("iffound", [], sub) ->
        let ext = parse_fun sub in
        (*VVV DANGER: parse_fun MUST be called BEFORE the function! *)
        (function
           | Extensions.Req_found (_, _) ->
               Lwt.return (Ext_sub_result ext)
           | Extensions.Req_not_found (err, ri) ->
               Lwt.return (Extensions.Ext_next err))
    | Element ("ifnotfound", [], sub) ->
        let ext = parse_fun sub in
        (function
           | Extensions.Req_found (_, r) ->
               Lwt.return (Extensions.Ext_found r)
           | Extensions.Req_not_found (err, ri) ->
               Lwt.return (Ext_sub_result ext))
    | Element ("ifnotfound", [("code", s)], sub) ->
        let ext = parse_fun sub in
        let r = Netstring_pcre.regexp s in
        (function
           | Extensions.Req_found (_, r) ->
               Lwt.return (Extensions.Ext_found r)
           | Extensions.Req_not_found (err, ri) ->
               if Netstring_pcre.string_match r (string_of_int err) 0 <> None then
                 Lwt.return (Ext_sub_result ext)
               else
                 Lwt.return (Extensions.Ext_next err))
    | Element (("forbidden"|"notfound"|"iffound"|"ifnotfound") as t, _, _) ->
        raise (Error_in_config_file ("(accesscontrol extension) Problem with tag <"^t^"> in configuration file."))
    | Element (t, _, _) -> raise (Bad_config_tag_for_extension t)
    | _ -> raise (Error_in_config_file "(accesscontrol extension) Bad (toplevel) data")



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

