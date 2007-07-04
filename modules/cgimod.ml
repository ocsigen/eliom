(* Ocsigen
 * http://www.ocsigen.org
 * Module cgimod.ml
 * Copyright (C) 2007 Jérôme Velleine
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

(** Module CGI for Ocsigen *)

(* To compile it:

ocamlfind ocamlc  -thread -package netstring -I ~/install_ocsigen/usr/local/lib/ocaml/3.09.2/ocsigen/ -c cgimod.ml

*)

open Lwt
open Extensions
open Simplexmlparser
open Http_frame
open Http_com
open Predefined_senders



exception Ocsigen_No_CGI


(*****************************************************************************)
(** table of cgi dir*)

let cgi_dir_table = ref []

let find k = Module_static_cgi.find k !cgi_dir_table

let add k a = Module_static_cgi.add k a cgi_dir_table



(*****************************************************************************)

(**permet de recuperer le fichier correspondant a l url*)
let find_cgi_page cgidirref path =
  let find_file = function
    | None ->raise Ocsigen_404
    | Some filename ->
        (* See also module Files in eliom.ml *)
        Messages.debug ("--Cgimod: Testing \""^filename^"\".");
        let stat= Unix.LargeFile.stat filename in
        let (filename, stat) = 
          if (stat.Unix.LargeFile.st_kind = Unix.S_DIR)
          then 
            raise Ocsigen_Is_a_directory
          else (filename, stat)
        in
        Messages.debug ("--Cgimod: Looking for \""^filename^"\".");

        if (stat.Unix.LargeFile.st_kind 
              = Unix.S_REG)
        then begin
	  try
            Unix.access filename [Unix.X_OK];
            (filename, stat)
	  with 
	    |Unix.Unix_error (Unix.EACCES,"access",filename) -> 
	       raise Ocsigen_No_CGI
	    |e->raise e
        end
        else raise Ocsigen_404 (* ??? *)
  in
  find_file (Module_static_cgi.find_page None !cgidirref path)



(*****************************************************************************)


(** permet de creer le tableau des variables d environnement *)
let array_environment pages_tree filename ri=
  let opt=function
    |None->""
    |Some(a)->a
  and opt_int=function
    |None->0
    |Some(a)->Int64.to_int a
  in let meth = 
    match Http_header.get_firstline ri.ri_http_frame.Stream_http_frame.header
    with
      | Http_header.Query (meth, _) -> meth
      | _ -> failwith "Bad request"
     and doc_root=function
       | Module_static_cgi.Page_dir(None,_, _)->""
       | Module_static_cgi.Page_dir(Some(a),_, _)->a
  in let get_ri_value var info=
    try
      let st=String.lowercase 
(Http_header.get_headers_value ri.ri_http_frame.Stream_http_frame.header info)
      in [var^Printf.sprintf "=%s" st]
    with
	_->[]
  in

  Array.append (
    Array.of_list ([
      Printf.sprintf "CONTENT_LENGTH=%d" (opt_int ri.ri_content_length);
      Printf.sprintf "CONTENT_TYPE=%s"  (opt ri.ri_content_type);
      Printf.sprintf "DOCUMENT_ROOT=%s" (doc_root !pages_tree);
      Printf.sprintf "HTTP_COOKIE=%s" (opt (Lazy.force ri.ri_cookies_string));
      Printf.sprintf "HTTP_HOST=%s" (opt ri.ri_host);
      Printf.sprintf "HTTP_REFERER=%s" (opt (Lazy.force ri.ri_referer));
      Printf.sprintf "HTTP_USER_AGENT=%s" ri.ri_user_agent;
      Printf.sprintf "PATH_INFO=%s" ri.ri_path_string;
      Printf.sprintf "PATH_TRANSLATED=%s" filename;
      Printf.sprintf "QUERY_STRING=%s" (opt ri.ri_get_params_string);
      Printf.sprintf "REQUEST_METHOD=%s" (Framepp.string_of_method meth);
      Printf.sprintf "REMOTE_PORT=%d" ri.ri_remote_port;
      Printf.sprintf "REMOTE_ADDR=%s" ri.ri_ip;
      Printf.sprintf "SCRIPT_NAME=%s" ri.ri_path_string;
      Printf.sprintf "SCRIPT_FILE_NAME=%s" filename;
      Printf.sprintf "SERVER_NAME=%s" (Ocsiconfig.server_name);
      Printf.sprintf "SERVER_PORT=%s" (string_of_int ri.ri_port)]@
      (get_ri_value "HTTP_ACCEPT" "Accept")@
      (get_ri_value "HTTP_ACCEPT_CHARSET" "Accept-Charset")@
      (get_ri_value "HTTP_ACCEPT_ENCODING" "Accept-Encoding")@
      (get_ri_value "HTTP_ACCEPT_LANGUAGE" "Accept-Language")@
      (get_ri_value "HTTP_CONNECT" "Connection")))
      (Unix.environment ())
(*  
GATEWAY_INTERFACE

REMOTE_HOST

SERVER_ADDR
SERVER_ADMIN
SERVER_PROTOCOL 
SERVER_SIGNATURE
SERVER_SOFTWARE
*)

(*****************************************************************************)
(** Cette fonction permet de lancer le prog CGI, depuis un nouveau processus
puis de lire par stream (petit a petit) le reultat de ce dernier  -- le 
content de l ancienne requete est ecrite en entree du processus*)

let recupere_cgi pages_tree filename ri=
  let opt=function
    |None->failwith "CAS IMPOSSIBLE"
    |Some(c)->c
  in let (post_out,post_in) = Unix.pipe () in
  let (cgi_out, cgi_in) = Unix.pipe () in
  Unix.set_nonblock post_in;
  Unix.set_nonblock cgi_out;
  (if ri.ri_http_frame.Stream_http_frame.content = None
   then (return ())
   else 
     (let content_post=opt ri.ri_http_frame.Stream_http_frame.content ()
     and fct ()=() in
     Stream_sender.really_write (Lwt_unix.Plain post_in) fct content_post))
  >>= function () ->
  let receiver= Http_com.create_receiver 
    ~mode:Http_com.Nofirstline (Lwt_unix.Plain cgi_out) in
  let pid = Unix.create_process_env filename [|filename|]
      (array_environment pages_tree filename ri) post_out cgi_in Unix.stderr in
  Stream_receiver.get_http_frame (return ()) receiver 
    ~doing_keep_alive:false () >>= fun http_frame ->
  ignore (
    Unix.close cgi_in;
    Unix.close post_in;
    Unix.close post_out;
    return ());
  ignore (http_frame.Stream_http_frame.waiter_thread >>= fun () ->
    Unix.close cgi_out;
    return ());
  return http_frame
            
(** retourne le header correspondant au frame *)

let get_header str =
  let a=str.Stream_http_frame.header 
  in return a


(** retourne le content correspondant au frame *)

let get_content str =
  match str.Stream_http_frame.content with
    |None->return (Ocsistream.empty_stream None)
    |Some(k)-> let stream = k () in return stream


(*****************************************************************************)

(** Parsing of config file *)

let parse_config page_tree path = function 
  | Element ("cgi", atts, []) -> 
        let dir = match atts with
          | [] -> 
              raise (Error_in_config_file
                       "dir attribute expected for <cgi>")
          | [("dir", s)] -> Module_static_cgi.Dir s
          | [("regexp", s);("dest",t)] -> 
	      Module_static_cgi.Regexp ((Netstring_pcre.regexp s), t)
          | _ -> raise (Error_in_config_file "Wrong attribute for <cgi>")
        in
        Module_static_cgi.set_dir page_tree dir path
  | Element (t, _, _) -> raise (Bad_config_tag_for_extension t)
  | _ -> 
      raise (Error_in_config_file "Unexpected data in config file")
	




(*****************************************************************************)
(** A function that will create an error message from the exceptions
    that may be raised during the initialisation phase, and raise again 
    all other exceptions. That function has type exn -> string. Use the 
   raise function if you don't need any. *)
let exn_handler = raise



(*****************************************************************************)
let gen pages_tree charset ri =
  catch
    (* Is it a cgi page? *)
    (fun () ->
       if ri.ri_path_string <> ""
         (* cgi pages have parameters *)
       then begin
         Messages.debug ("--Cgimod: Is it a cgi file?");
        let (filename, stat) =
          find_cgi_page pages_tree ri.ri_path
        in
	recupere_cgi pages_tree filename ri >>= fun frame ->
	  get_content frame >>= fun content -> 
	    get_header frame >>= fun header -> 
	  return
	    (Ext_found
               {res_cookies= [];
		res_send_page= 
		   Predefined_senders.send_stream_page 
		     ~contenttype: "text/html" ~content:(fun () -> content);
		res_headers=header.Http_header.headers;
		res_code= None; (* 200 by default *)
		res_lastmodified= None;
		res_etag= None;
		res_charset= None})
       end
       else return Ext_not_found)
    (function
        Unix.Unix_error (Unix.EACCES,_,_)
      | Ocsigen_Is_a_directory
      | Ocsigen_malformed_url 
      | Ocsigen_No_CGI 
      | Connection_reset_by_peer as e ->fail e
      | Ocsigen_404 ->return Ext_not_found 
      | Unix.Unix_error (Unix.ENOENT,_,_) -> return Ext_not_found 
      | e -> fail e)
          

(*****************************************************************************)
(** Function to be called at the beginning of the initialisation phase 
    of the server  *)
let start_init () =
  ()

(** Function to be called at the end of the initialisation phase *)
let end_init () =
  match Ocsiconfig.get_default_cgi_dir () with
  | None -> ()
  | Some path -> 
      let page_tree = Module_static_cgi.new_pages_tree () in
      Module_static_cgi.set_dir page_tree (Module_static_cgi.Dir path) [];
      add_virthost ([([Wildcard], None)], 
                    fun ri -> 
                      gen page_tree (Ocsiconfig.get_default_charset ()) ri >>=
                      (fun r -> return (r,[])))
(*	for default cgi dir *)   



(*****************************************************************************)
(** Registration of the extension *)
let _ = R.register_extension (* takes a quadruple *)
  ((fun hostpattern -> 
      let page_tree = 
        try 
          find hostpattern
        with Not_found -> 
          let n = Module_static_cgi.new_pages_tree () in
          add hostpattern n;
          n
      in
      (gen page_tree, 
       parse_config page_tree)),
   start_init,
   end_init,
   exn_handler)

