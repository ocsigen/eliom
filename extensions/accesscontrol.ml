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


(*****************************************************************************)
let rec parse_global_config = function
  | [] -> ()
  | _ -> raise (Error_in_config_file
                  ("Unexpected content inside accesscontrol config"))

let _ = parse_global_config (Extensions.get_config ())



(*****************************************************************************)
(* Management of authentication methods *)

exception Bad_config_tag_for_auth of string

let register_authentication_method,
  get_authentication_method =

  let fun_auth = ref
    (fun config ->
       raise (Bad_config_tag_for_auth "<unknown authentication method>"))
  in

  (********* register_authentication_method *********)
  (fun new_fun_auth ->
     let old_fun_auth = !fun_auth in
     fun_auth :=
       (fun config ->
          try
            old_fun_auth config
          with
            | Bad_config_tag_for_auth c -> new_fun_auth config)),

  (********* get_authentication_method *********)
  (fun config ->
     !fun_auth config)


(** for tests **)
let _ = register_authentication_method
  (function
     | Element ("htpasswd", _, _) -> (fun login passwd -> assert false)
     | _ -> raise (Bad_config_tag_for_extension "not for htpasswd"))

(*****************************************************************************)

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
            regexp (Lazy.force ri.ri_sub_path_string) 0 <> None
        in
        if r then
          Messages.debug
            (fun () -> "--Access control: Path "^
              (Lazy.force ri.ri_sub_path_string)^" matches regexp")
        else
          Messages.debug
            (fun () -> "--Access control: Path "^
              (Lazy.force ri.ri_sub_path_string)^" does not match regexp");
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
          Lwt.return (Ext_stop_site (ri, Http_frame.Cookies.empty, 403))
        end
      with
        | e ->
            Messages.debug2 "--Access control: taking in charge an error";
            fail e (* for example Ocsigen_http_error 404 or 403 *)
              (* server.ml has a default handler for HTTP errors *)



let rec parse_filter = function

    | Element ("ip", ["value", s], []) ->
        let (ip32, mask) =
          try
            Ocsimisc.parse_ip_netmask s
          with Failure _ ->
            raise (Error_in_config_file "Bad ip/netmask value in ip filter")
        in
        (fun ri ->
           let r = Int32.logand (Lazy.force ri.ri_ip32) mask = ip32 in
           if r then
             Messages.debug2 "--Access control: IP matches mask"
           else
             Messages.debug2 "--Access control: IP does not match mask";
           r)

    | Element ("header", ["name", name; "regexp", r], []) ->
        let regexp =
          try
            Netstring_pcre.regexp r
          with Failure _ ->
            raise (Error_in_config_file
                     "Bad regular expression in header filter")
        in
        (fun ri ->
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
           r)

    | Element ("method", ["value", s], []) ->
        let meth =
          try
            Framepp.method_of_string s
          with Failure _ ->
            raise (Error_in_config_file "Bad method value in method filter")
        in
        (fun ri ->
           let r = meth = ri.ri_method in
           if r then
             Messages.debug2 "--Access control: Method matches"
           else
             Messages.debug2 "--Access control: Method does not match";
           r)

    | Element ("protocol", ["value", s], []) ->
        let pr =
          try
            Framepp.proto_of_string s
          with Failure _ ->
            raise (Error_in_config_file "Bad protocol value in protocol filter")
        in
        (fun ri ->
           let r = pr = ri.ri_protocol in
           if r then
             Messages.debug2 "--Access control: Protocol matches"
           else
             Messages.debug2 "--Access control: Protocol does not match";
           r)

    | Element ("path", ["regexp", s], []) ->
        let regexp =
          try
            Netstring_pcre.regexp s
          with Failure _ ->
            raise (Error_in_config_file
                     "Bad regular expression in <path/>")
        in
        (fun ri ->
           let r =
             Netstring_pcre.string_match
               regexp (Lazy.force ri.ri_sub_path_string) 0 <> None
           in
           if r then
             Messages.debug
               (fun () -> "--Access control: Path "^
                  (Lazy.force ri.ri_sub_path_string)^" matches regexp")
           else
             Messages.debug
               (fun () -> "--Access control: Path "^
                  (Lazy.force ri.ri_sub_path_string)^" does not match regexp");
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
              raise (Error_in_config_file ("Cannot parse children of <"^t^"> as conditions"))
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
              raise (Error_in_config_file ("Cannot parse children of <"^t^"> as conditions"))
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
                 Extensions.Ext_stop_site (ri, Http_frame.Cookies.empty, 403)
               end)
      end

    | Element ("auth", ["authtype", "basic"; "realm", r], auth::sub) ->
        let _ =
          try
            get_authentication_method auth
          with Bad_config_tag_for_extension _ ->
            raise (Error_in_config_file "Unable to find proper authentication method")
        in
        (fun rs ->
           Messages.debug2 "--Access control: Authentication to be implemented";
           fail (Http_error.Http_exception (401, None, None)))

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
        let r = Netstring_pcre.regexp s in
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

