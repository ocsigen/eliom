(* Ocsigen
 * http://www.ocsigen.org
 * Module revproxy.ml
 * Copyright (C) 2007 Vincent Balat
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

(** Reverse proxy for Ocsigen *)

(*
   The reverse proxy is still experimental because it relies on the
   experimental Ocsigen_http_client module.

   TODO
   - Change the policy for « trusted servers » for pipelining?
   (see ocsigen_http_client.ml)
   - add the ability to rewrite some headers from the config file
   (for ex after a redirection, the new URL is wrong)
   probably in another (filter) extension
   - enhance pipelining
   - HTTP/1.0
   - ...


   - Make possible to return for example (Ext_next 404) to allow
   other extensions to take the request?
   There is a problem if the body contains data (POST request) ...
   this data has been sent and is lost ...
*)


(* To compile it:
ocamlfind ocamlc  -thread -package netstring,ocsigen -c revproxy.ml

Then load it dynamically from Ocsigen's config file:
   <extension module=".../revproxy.cmo"/>

*)

open Lwt
open Ocsigen_extensions
open Simplexmlparser



(*****************************************************************************)
(* The table of redirections for each virtual server                         *)
type redir =
    { regexp: Netstring_pcre.regexp;
      full_url: Ocsigen_lib.yesnomaybe;
      dest: string;
      pipeline: bool}



(*****************************************************************************)
let rec parse_global_config = function
  | [] -> ()
  | _ -> raise (Error_in_config_file
                  ("Unexpected content inside revproxy config"))

let _ = parse_global_config (Ocsigen_extensions.get_config ())




(*****************************************************************************)
(* Finding redirections *)




(*****************************************************************************)
(** The function that will generate the pages from the request. *)
exception Bad_answer_from_http_server

let gen dir = function
| Ocsigen_extensions.Req_found _ -> 
    Lwt.return Ocsigen_extensions.Ext_do_nothing
| Ocsigen_extensions.Req_not_found (err, ri) ->
  catch
    (* Is it a redirection? *)
    (fun () ->
       Ocsigen_messages.debug2 "--Revproxy: Is it a redirection?";
       let dest =
         let ri = ri.request_info in
         let fi full =
           Ocsigen_extensions.find_redirection
             dir.regexp
             full
             dir.dest
             ri.ri_ssl
             ri.ri_host
             ri.ri_server_port
             ri.ri_get_params_string
             ri.ri_sub_path_string
             ri.ri_full_path_string
         in 
         match dir.full_url with
           | Ocsigen_lib.Yes -> fi true
           | Ocsigen_lib.No -> fi false
           | Ocsigen_lib.Maybe -> 
               try fi false 
               with Ocsigen_extensions.Not_concerned -> fi true
       in
       let (https, host, port, uri) = 
         try
           match Ocsigen_lib.parse_url dest with
             | (Some https, Some host, port, uri, _, _, _, _) -> 
                 let port = match port with
                   | None -> if https then 443 else 80
                   | Some p -> p
                 in
                 (https, host, port, uri)
             | _ -> raise (Ocsigen_extensions.Error_in_config_file
                             ("Revproxy : error in destination URL "^dest))
(*VVV catch only Neturl exceptions! *)
         with e -> raise (Ocsigen_extensions.Error_in_config_file
                            ("Revproxy : error in destination URL "^dest^" - "^
                               Printexc.to_string e))
       in
       Ocsigen_messages.debug
         (fun () ->
            "--Revproxy: YES! Redirection to "^
              (if https then "https://" else "http://")^host^":"^
              (string_of_int port)^uri);

       Ocsigen_lib.get_inet_addr host >>= fun inet_addr ->

       (* It is now safe to start next request.
          We are sure that the request won't be taken in disorder.
          => We return.
       *)

       let do_request =
         let ri = ri.request_info in
         if dir.pipeline then
           Ocsigen_http_client.raw_request
             ~headers:ri.ri_http_frame.Ocsigen_http_frame.header.Ocsigen_http_frame.Http_header.headers
             ~https
             ~port
             ~client:ri.ri_client
             ~keep_alive:true
             ~content:ri.ri_http_frame.Ocsigen_http_frame.content
             ?content_length:ri.ri_content_length
             ~http_method:ri.ri_method
             ~host
             ~inet_addr
             ~uri ()
           else
             fun () ->
               Ocsigen_http_client.basic_raw_request
                 ~headers:ri.ri_http_frame.Ocsigen_http_frame.header.Ocsigen_http_frame.Http_header.headers
                 ~https
                 ~port
                 ~content:ri.ri_http_frame.Ocsigen_http_frame.content
                 ?content_length:ri.ri_content_length
                 ~http_method:ri.ri_method
                 ~host
                 ~inet_addr
                 ~uri ()
       in
       Lwt.return
         (Ext_found
            (fun () ->
               do_request ()

               >>= fun http_frame ->
               let headers =
                 http_frame.Ocsigen_http_frame.header.Ocsigen_http_frame.Http_header.headers
               in
               let code =
                 match
                   http_frame.Ocsigen_http_frame.header.Ocsigen_http_frame.Http_header.mode
                 with
                   | Ocsigen_http_frame.Http_header.Answer code -> code
                   | _ -> raise Bad_answer_from_http_server
               in
               match http_frame.Ocsigen_http_frame.content with
                 | None ->
                     let empty_result = Ocsigen_http_frame.empty_result () in
                     let length =
                       Ocsigen_headers.get_content_length http_frame
                     in
                     Lwt.return
                       {empty_result with
                        Ocsigen_http_frame.res_content_length = length;
                        Ocsigen_http_frame.res_headers= headers;
                        Ocsigen_http_frame.res_stop_stream =
                        http_frame.Ocsigen_http_frame.abort;
                        Ocsigen_http_frame.res_code= code;
                       }
                 | Some stream ->
                     let default_result =
                       Ocsigen_http_frame.default_result ()
                     in
                     let length =
                       Ocsigen_headers.get_content_length http_frame
                     in
                     Lwt.return
                       {default_result with
                        Ocsigen_http_frame.res_content_length = length;
                        Ocsigen_http_frame.res_stream = (stream, None);
                        Ocsigen_http_frame.res_stop_stream =
                        http_frame.Ocsigen_http_frame.abort;
                        Ocsigen_http_frame.res_headers= headers;
                        Ocsigen_http_frame.res_code= code;
                       }
            )
         )
    )
    (function
       | Not_concerned -> return (Ext_next err)
       | e -> fail e)




(*****************************************************************************)
(** Configuration for each site.
    These tags are inside <site ...>...</site> in the config file.

   For example:
   <site dir="">
     <revproxy regexp="" ... />
   </extension>

 *)

let parse_config path _ parse_site = function
  | Element ("revproxy", atts, []) ->
      let rec parse_attrs ((r, f, d, pipeline) as res) = function
        | [] -> res
        | ("regexp", regexp)::l when r = None -> (* deprecated *)
            parse_attrs
              (Some (Netstring_pcre.regexp ("^"^regexp^"$")), Ocsigen_lib.Maybe,
               d, pipeline)
              l
        | ("fullurl", regexp)::l when r = None ->
            parse_attrs
              (Some (Netstring_pcre.regexp ("^"^regexp^"$")), Ocsigen_lib.Yes,
               d, pipeline)
              l
        | ("suburl", regexp)::l when r = None ->
            parse_attrs
              (Some (Netstring_pcre.regexp ("^"^regexp^"$")), Ocsigen_lib.No,
               d, pipeline)
              l
        | ("dest", dest)::l when d = None ->
            parse_attrs
              (r, f, Some dest, pipeline)
              l
        | ("nopipeline", "nopipeline")::l ->
            parse_attrs
              (r, f, d, false)
              l
        | _ -> raise (Error_in_config_file "Wrong attribute for <revproxy>")
        in
        let dir =
          match parse_attrs (None, Ocsigen_lib.Yes, None, true) atts with
          | (None, _, _, _) -> raise (Error_in_config_file "Missing attribute regexp for <revproxy>")
          | (_, _, None, _) -> raise (Error_in_config_file "Missing attribute dest for <revproxy>")
          | (Some r, full, Some d, pipeline) ->
              {
                regexp=r;
                full_url=full;
                dest=d;
                pipeline=pipeline;
             }
        in
        gen dir
  | Element (t, _, _) -> raise (Bad_config_tag_for_extension t)
  | _ -> raise (Error_in_config_file "(revproxy extension) Bad data")





(*****************************************************************************)
(** Registration of the extension *)
let _ = register_extension
  ~fun_site:(fun _ -> parse_config)
  ~user_fun_site:(fun _ _ -> parse_config)
  ~respect_pipeline:true (* We ask ocsigen to respect pipeline order
                            when sending to extensions! *)
  ()
