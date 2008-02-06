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
open Extensions
open Simplexmlparser
open Http_frame


(*****************************************************************************)
let rec parse_global_config = function
  | [] -> ()
  | _ -> raise (Error_in_config_file
                  ("Unexpected content inside accesscontrol config"))

let _ = parse_global_config (Extensions.get_config ())



(*****************************************************************************)
(* Management of basic authentication methods *)

exception Bad_config_tag_for_auth of string

let register_basic_authentication_method,
  get_basic_authentication_method =

  let fun_auth = ref
    (fun config ->
       raise (Bad_config_tag_for_auth "<unknown basic authentication method>"))
  in

  (********* register_basic_authentication_method *********)
  (fun new_fun_auth ->
     let old_fun_auth = !fun_auth in
     fun_auth :=
       (fun config ->
          try
            old_fun_auth config
          with
            | Bad_config_tag_for_auth c -> new_fun_auth config)),

  (********* get_basic_authentication_method *********)
  (fun config ->
     !fun_auth config)


(*****************************************************************************)
(* Basic authentication with a predefined login/password (example) *)

let _ = register_basic_authentication_method
  (function
     | Element ("plain", ["login", login; "password", password], _) ->
	 (fun l p -> Lwt.return (login = l && password = p))
     | _ -> raise (Bad_config_tag_for_extension "not for htpasswd"))


(*****************************************************************************)

exception Bad_config_tag_for_filter

let rec parse_filter = function

    | Element ("ip", ["value", s], []) ->
        let ip_with_mask =
          try
            Ocsimisc.parse_ip s
          with Failure _ ->
            raise (Error_in_config_file (sprintf "Bad ip/netmask [%s] in <ip> condition" s))
        in
        (fun ri ->
           let r = Ocsimisc.match_ip ip_with_mask (Lazy.force ri.ri_ip_parsed) in
           if r then
             Messages.debug2 (sprintf "--Access control (ip): %s matches %s" ri.ri_ip s)
           else
             Messages.debug2 (sprintf "--Access control (ip): %s does not match %s" ri.ri_ip s);
           r)

    | Element ("header", ["name", name; "regexp", reg], []) ->
        let regexp =
          try
            Netstring_pcre.regexp ("^"^reg^"$")
          with Failure _ ->
            raise (Error_in_config_file (sprintf "Bad regular expression [%s] in <header> condition" reg))
        in
        (fun ri ->
           let r =
             List.exists
               (fun a ->
                  let r = Netstring_pcre.string_match regexp a 0 <> None in
                  if r then Messages.debug2 (sprintf "--Access control (header): header %s matches \"%s\"" name reg);
                  r)
               (Http_headers.find_all
                  (Http_headers.name name)
                  ri.ri_http_frame.Http_frame.header.Http_frame.Http_header.headers)
           in
           if not r then Messages.debug2 (sprintf "--Access control (header): header %s does not match \"%s\"" name reg);
           r)

    | Element ("method", ["value", s], []) ->
        let meth =
          try
            Framepp.method_of_string s
          with Failure _ ->
            raise (Error_in_config_file (sprintf "Bad method [%s] in <method> condition" s))
        in
        (fun ri ->
           let r = meth = ri.ri_method in
           if r then Messages.debug
             (fun () -> sprintf "--Access control (method): %s matches %s" (Framepp.string_of_method ri.ri_method) s)
           else Messages.debug
             (fun () -> sprintf "--Access control (method): %s does not match %s" (Framepp.string_of_method ri.ri_method) s);
           r)

    | Element ("protocol", ["value", s], []) ->
        let pr =
          try
            Framepp.proto_of_string s
          with Failure _ ->
            raise (Error_in_config_file (sprintf "Bad protocol [%s] in <protocol> condition" s))
        in
        (fun ri ->
           let r = pr = ri.ri_protocol in
           if r then Messages.debug
             (fun () -> sprintf "--Access control (protocol): %s matches %s" (Framepp.string_of_proto ri.ri_protocol) s)
           else Messages.debug
             (fun () -> sprintf "--Access control (protocol): %s does not match %s" (Framepp.string_of_proto ri.ri_protocol) s);
           r)

    | Element ("path", ["regexp", s], []) ->
        let regexp =
          try
            Netstring_pcre.regexp ("^"^s^"$")
          with Failure _ ->
            raise (Error_in_config_file (sprintf "Bad regular expression [%s] in <path> condition" s))
        in
        (fun ri ->
           let r =
             Netstring_pcre.string_match
               regexp ri.ri_sub_path_string 0 <> None
           in
           if r then Messages.debug
             (fun () -> sprintf "--Access control (path): \"%s\" matches \"%s\"" ri.ri_sub_path_string s)
           else Messages.debug
               (fun () -> sprintf "--Access control (path): \"%s\" does not match \"%s\"" ri.ri_sub_path_string s);
           r)

    | Element (("nand"|"and") as t, [], sub) ->
        begin try
          let rec aux = function
            | [] -> (fun ri -> true)
            | f::sub ->
                let f = parse_filter f and sub = aux sub in
                fun ri -> f ri && sub ri
          in
          let sub = aux sub in
          if t = "and" then sub else (fun ri -> not (sub ri))
        with
          | Bad_config_tag_for_filter ->
              raise (Error_in_config_file (sprintf "Error in <%s> condition" t))
        end

    | Element (("nor"|"or") as t, [], sub) ->
        begin try
          let rec aux = function
            | [] -> (fun ri -> false)
            | f::sub ->
                let f = parse_filter f and sub = aux sub in
                fun ri -> f ri || sub ri
          in
          let sub = aux sub in
          if t = "or" then sub else (fun ri -> not (sub ri))
        with
          | Bad_config_tag_for_filter ->
              raise (Error_in_config_file (sprintf "Error in <%s> condition" t))
        end

    | _ -> raise Bad_config_tag_for_filter


let parse_config path charset _ parse_fun = function

  | Element ("filter", attrs, sub) ->
      let (typ, if_then_else) =
        (* determine the type of filter *)
        let rec aux (typ, els) = function
          | [] -> (typ, els)
          | ("type", (("and"|"or"|"nand"|"nor") as t))::attrs when typ = None ->
              aux (Some t, els) attrs
          | ("else", "present")::attrs -> aux (typ, true) attrs
          | (t, _)::_ ->
              raise (Error_in_config_file ("Do not know what to do with attribute "^t^" in <filter>"))
        in aux (None, false) attrs
      in
      let typ = match typ with
        | None -> raise (Error_in_config_file "Attribute type missing in <filter>")
        | Some t -> t
      in
      let (conditions, actions) =
        (* split sub into conditions and actions *)
        let rec aux = function
          | [] -> ([], [])
          | f::sub as x ->
              try
                let f = parse_filter f in
                let (c, a) = aux sub in
                (f::c, a)
              with
                | Bad_config_tag_for_filter -> ([], x)
        in aux sub
      in
      let test =
        (* computing the resulting test function *)
        match typ with
          | "and" -> (fun ri -> List.for_all (fun f -> f ri) conditions)
          | "nand" -> (fun ri -> not (List.for_all (fun f -> f ri) conditions))
          | "or" -> (fun ri -> List.exists (fun f -> f ri) conditions)
          | "nor" -> (fun ri -> not (List.exists (fun f -> f ri) conditions))
          | _ -> assert false (* should not happen *)
      in
      if if_then_else then begin (* if-then-else *)
        match actions with
          | [Element (ta, _, _) as a; Element (tb, _, _) as b] ->
              let fa = parse_fun [a] and fb = parse_fun [b] in
              begin function
                | Extensions.Req_found (ri, _)
                | Extensions.Req_not_found (_, ri) ->
                    Lwt.return
                      (if test ri then begin
                         Messages.debug2 ("--Access control: => Going into "^ta);
                         Extensions.Ext_sub_result fa
                       end
                       else begin
                         Messages.debug2 ("--Access control: => Going into"^tb);
                         Extensions.Ext_sub_result fb
                       end)
              end
          | _ ->
              raise (Error_in_config_file "A filter with else=\"present\" must have exactly two children")
      end
      else begin
        let ext = parse_fun actions in
        function
          | Extensions.Req_found (ri, r) ->
            Lwt.return
              (if test ri then begin
                 Messages.debug2 "--Access control: => Passthrough granted!";
                 Extensions.Ext_sub_result ext
               end
               else begin
                 Messages.debug2 "--Access control: => Passthrough denied!";
                 Extensions.Ext_found r
               end)
        | Extensions.Req_not_found (_, ri) ->
            Lwt.return
              (if test ri then begin
                 Messages.debug2 "--Access control: => Access granted!";
                 Extensions.Ext_sub_result ext
               end
               else begin
                 Messages.debug2 "--Access control: => Access denied!";
                 Extensions.Ext_stop_site (Http_frame.Cookies.empty, 403)
               end)
      end

    | Element ("auth", ["authtype", "basic"; "realm", realm], auth::sub) ->
        (* http://www.ietf.org/rfc/rfc2617.txt *)
        (* TODO: check that realm is correct *)
        let auth =
          try
            get_basic_authentication_method auth
          with Bad_config_tag_for_extension _ ->
            raise (Error_in_config_file "Unable to find proper authentication method")
        in
        (fun rs ->
           match rs with
             | Extensions.Req_not_found (err, ri) ->
                 let reject () =
                   let h = Http_headers.add
                     (Http_headers.name "WWW-Authenticate")
                     (sprintf "Basic realm=\"%s\"" realm)
                     Http_headers.empty
                   in
                   Messages.debug2 "--Access control (auth): invalid credentials!";
                   fail (Http_error.Http_exception (401, None, Some h))
                 in
                 begin try
                   let (login, password) =
                     let credentials =
                       Http_headers.find
                         (Http_headers.name "Authorization")
                         ri.ri_http_frame.Http_frame.header.Http_frame.Http_header.headers
                     in
                     let encoded =
                       let n = String.length credentials in
                       if n > 6 && String.sub credentials 0 6 = "Basic " then
                         String.sub credentials 6 (n-6)
                       else
                         failwith "credentials"
                     in
                     let decoded = Netencoding.Base64.decode encoded in
                     let i = String.index decoded ':' in
                     (String.sub decoded 0 i,
                      String.sub decoded (i+1) (String.length decoded - (i+1)))
                   in
                   auth login password >>=
                     (fun r ->
                        if r then begin
                          Messages.debug2 "--Access control (auth): valid credentials!";
                          Lwt.return (Extensions.Ext_next err)
                        end
                        else reject ())
                 with
                   | Not_found -> reject ()
                   | e ->
                       Messages.debug
                         (fun () -> sprintf
                            "--Access control (auth): Invalid Authorization header (%s)"
                            (Printexc.to_string e));
                       fail (Ocsigen_http_error (Http_frame.Cookies.empty, 400))
                 end
             | Extensions.Req_found (ri, r) ->
                 Lwt.return (Extensions.Ext_found r))

    | Element ("notfound", [], []) ->
        (fun rs ->
           Messages.debug2 "--Access control: taking in charge 404";
           fail (Ocsigen_http_error (Http_frame.Cookies.empty, 404)))

    | Element ("forbidden", [], []) ->
        (fun rs ->
           Messages.debug2 "--Access control: taking in charge 403";
           fail (Ocsigen_http_error (Http_frame.Cookies.empty, 403)))

    | Element ("iffound", [], sub) ->
        let ext = parse_fun sub in
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
        let r = Netstring_pcre.regexp ("^"^s^"$") in
        (function
           | Extensions.Req_found (_, r) ->
               Lwt.return (Extensions.Ext_found r)
           | Extensions.Req_not_found (err, ri) ->
               if Netstring_pcre.string_match r (string_of_int err) 0 <> None then
                 Lwt.return (Ext_sub_result ext)
               else
                 Lwt.return (Extensions.Ext_next err))

    | Element (t, _, _) -> raise (Bad_config_tag_for_extension t)
    | _ -> raise (Error_in_config_file "(accesscontrol extension) Bad data")



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

