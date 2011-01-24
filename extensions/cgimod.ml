(* Ocsigen
 * http://www.ocsigen.org
 * Module cgimod.ml
 * Copyright (C) 2007 Jérôme Velleine - Gabriel Kerneis
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

(* TODO
   - nph- scripts
*)

open Lwt
open Ocsigen_extensions
open Simplexmlparser
open Ocsigen_http_frame
open Ocsigen_http_com
open Ocsigen_senders


module Regexp = Netstring_pcre

exception Failed_403
exception Failed_404
exception Not_concerned
exception CGI_Timeout
exception CGI_Error of exn

let cgitimeout = ref 30


(*****************************************************************************)
(* The table of cgi pages for each virtual server                            *)

type reg = {
  regexp:Regexp.regexp; (** regexp of the script url *)

  doc_root: Ocsigen_extensions.ud_string;
                                (** physical directory of the script (regexp) *)
  script: Ocsigen_extensions.ud_string; (** physical name of the script (regexp) *)

  path: string; (** path of the script *)
  path_info: string; (** path_info environment variable *)

  exec:string option; (** binary to execute the script with (optionnal) *)
  env:(string * string) list (** environment variables *) }

(*****************************************************************************)


let environment= ["CONTENT_LENGTH=%d";
                  "CONTENT_TYPE";
                  "DOCUMENT_ROOT";
                  "GATEWAY_INTERFACE";
                  "HTTP_COOKIE";
                  "HTTP_HOST";
                  "HTTP_REFERER";
                  "HTTP_USER_AGENT";
                  "PATH_INFO";
                  "PATH_TRANSLATED";
                  "QUERY_STRING";
                  "REMOTE_PORT";
                  "REMOTE_ADDR";
                  "REQUEST_METHOD%s";
                  "SCRIPT_NAME";
                  "SCRIPT_FILENAME";
                  "SERVER_NAME";
                  "SERVER_PORT";
                  "SERVER_PROTOCOL";
                  "SERVER_SOFTWARE"]

(*****************************************************************************)

let string_conform s = match String.length s with
  | 0 -> "/"
  | n -> match  s.[0], s.[n - 1] with
        | '/' ,'/' -> s
        | _, '/' -> "/"^s
        | '/', _ -> s^"/"
        | _, _ -> "/"^s^"/"

let string_conform0 s =
  try
    match  s.[0] with
      | '/' -> s
      | _ -> "/"^s
  with Invalid_argument _ -> "/"

let string_conform1 s =
  try
    match  s.[String.length s - 1] with
      | '/' -> s
      | _ -> s^"/"
  with Invalid_argument _ -> "/"

let string_conform2 s =
  match String.length s with
  | 0 | 1 when s = "/" -> ""
  | n -> match s.[0], s.[n - 1] with
        | '/', '/' -> String.sub s 1 (n-1)
        | _, '/' -> s
        | '/', _ -> (String.sub s 1 (n-1))^"/"
        | _, _ -> s^"/"

(* split a string in two parts, according to a regexp *)
let split_regexp r s =
  match Regexp.string_match r s 0 with
  | None -> None (* the begining of the string doesn't match the regexp *)
  | Some result ->
    let (split,l) = Regexp.match_end result, String.length s in
    let s' = Regexp.first_chars s split in
    let s'' = Regexp.last_chars s (l - split) in
    Some (s',s'')


(** permet de recuperer le fichier correspondant a l url *)
let find_cgi_page request reg sub_path =
  let find_file (filename, re, doc_root) =
    (* See also module Files in eliom.ml *)
    Ocsigen_messages.debug (fun () -> "--Cgimod: Testing \""^filename^"\".");
    try
      let stat = Unix.LargeFile.stat filename in
      let filename =
        if (stat.Unix.LargeFile.st_kind = Unix.S_DIR)
        then
          (Ocsigen_messages.debug2 "--Cgimod: this is a directory.";
          raise (Ocsigen_Is_a_directory request))
        else filename
      in
      Ocsigen_messages.debug (fun () -> "--Cgimod: Looking for \""^filename^"\".");

      if (stat.Unix.LargeFile.st_kind = Unix.S_REG)
      then begin
        match re.exec with
          | None ->
              Unix.access filename [Unix.X_OK];
              (filename, re, doc_root)
          | Some exec ->
              Unix.access filename [Unix.R_OK];
              (filename, re, doc_root)
      end
      else raise Failed_403
    with
    | Unix.Unix_error (Unix.ENOENT, _, _) -> raise Failed_404
  in

  let sub_path = (Ocsigen_lib.string_of_url_path ~encode:true sub_path) in

  match split_regexp reg.regexp sub_path with
  | None -> raise Failed_404
  | Some (path', path_info) ->
    let path'' = reg.path^path' in
    try
      let dr = Ocsigen_extensions.replace_user_dir reg.regexp reg.doc_root path' in
      let sc = Ocsigen_extensions.replace_user_dir reg.regexp reg.script path' in
      let reg =
        {reg with
         path = path'';
         path_info= string_conform0 path_info}
      in
      find_file (dr^sc, reg, dr)
    with Ocsigen_extensions.NoSuchUser -> raise Failed_404



(*****************************************************************************)
(** permet de creer le tableau des variables d environnement *)

(*XXX Is this documented anywhere?*)
let suitable_header = Regexp.regexp "[a-zA-Z-]+"
let hyphen = Regexp.regexp_string "-"

(* Headers processed separately when setting CGI's variable *)
let exclude_headers = Http_headers.NameHtbl.create 10
let _ =
  List.iter
    (fun x ->
       Http_headers.NameHtbl.add exclude_headers (Http_headers.name x) ())
    ["Content-type"; "Authorization"; "Content-length";
     (*"Referer"; "Host"; "Cookie"*) ]

let array_environment filename re doc_root ri hostname =
  let header = ri.ri_http_frame.Ocsigen_http_frame.frame_header in
  let opt = function
    | None -> ""
    | Some a -> a
  and opt_int = function
    | None -> "0"
    | Some a -> Int64.to_string a
  in
  let meth =
    match Http_header.get_firstline header with
    | Http_header.Query (meth, _) -> Framepp.string_of_method meth
    | _ -> raise Ocsigen_lib.Ocsigen_Bad_Request
  in

   (* Rule  : the header lines  received from the client,  if any, are
   placed into the * environment with the prefix HTTP_ followed by the
   header name. Any - characters * in the header name are changed to _
   characters.  The server  may exclude  any  * headers  which it  has
   already  processed,  such  as  Authorization, Content-type,  and  *
   Content-length. If necessary, the  server may choose to exclude any
   or all of * these headers if including them would exceed any system
   environment limits. *)

  let additionnal_headers =
    let headers =
      List.filter
        (fun (h,_) ->
           Regexp.string_match
             suitable_header (Http_headers.name_to_string h) 0 <> None &&
           not (Http_headers.NameHtbl.mem exclude_headers h))
        (Http_headers.fold (fun n vl rem -> (n, String.concat "," vl) :: rem)
           (Http_header.get_headers header) []) in
    let transform (h,v) =
      let h' =
        Regexp.global_replace hyphen "_" (Http_headers.name_to_string h) in
      Printf.sprintf "HTTP_%s=%s" (String.uppercase h') v  in
    List.map transform headers
 in
 List.concat
 [ (* Let's follow CGI spec : http://hoohoo.ncsa.uiuc.edu/cgi/env.html *)

   (* Not request-specific variables *)
  [Printf.sprintf "SERVER_NAME=%s" hostname;
   Printf.sprintf "SERVER_SOFTWARE=%s" Ocsigen_config.full_server_name ;
   "GATEWAY_INTERFACE=CGI/1.1"] ;

   (* Request-specific variables *)
  ["SERVER_PROTOCOL=HTTP/1.1";
   Printf.sprintf "SERVER_PORT=%s" (string_of_int ri.ri_server_port);
   Printf.sprintf "REQUEST_METHOD=%s" meth;
   Printf.sprintf "PATH_INFO=%s" re.path_info;
   Printf.sprintf "PATH_TRANSLATED=" ; (* PATH_INFO virtual -> physical; unclear, so don't set *)
   Printf.sprintf "SCRIPT_NAME=%s" re.path;
   Printf.sprintf "QUERY_STRING=%s" (opt ri.ri_get_params_string);
   Printf.sprintf "REMOTE_ADDR=%s" ri.ri_remote_ip;
   (* no REMOTE_HOST: implies reverse DNS resolution *)
   (* neither AUTH_TYPE, REMOTE_USER nor REMOTE_IDENT: implies authentication *)
   Printf.sprintf "CONTENT_LENGTH=%s" (opt_int ri.ri_content_length);
   Printf.sprintf "CONTENT_TYPE=%s"  (opt ri.ri_content_type_string)] ;

   (* Additional headers, coming from the client *)
 [(* Document_root is defined by Apache but not in the CGI's spec *)
   Printf.sprintf "DOCUMENT_ROOT=%s" doc_root;

   (* Should be retrieved from additionnal_headers
   Printf.sprintf "HTTP_COOKIE=%s" (opt (Lazy.force ri.ri_cookies_string));
   Printf.sprintf "HTTP_HOST=%s" (opt ri.ri_host);
   Printf.sprintf "HTTP_REFERER=%s" (opt (Lazy.force ri.ri_referer)); *)

   (* Neither in the CGI's spec nor in the HTTP headers but used, e.g., by PHP *)
   Printf.sprintf "REMOTE_PORT=%d" ri.ri_remote_port;
   Printf.sprintf "REQUEST_URI=%s" ri.ri_url_string ; (* FIXME: URI instead of URL ? *)
   Printf.sprintf "SCRIPT_FILENAME=%s" filename ] ;
   additionnal_headers
 ]

(*****************************************************************************)

let rec set_env_list=function
  | [] -> []
  | (vr, vl) :: l -> (vr^"="^vl) :: set_env_list l


(** launch the process *)

let create_process_cgi filename ri post_out cgi_in err_in re doc_root hostname =
  let envir = Array.of_list (
    (array_environment filename re doc_root ri hostname)@(set_env_list re.env)) in
  match re.exec with
  | None ->
      Unix.create_process_env
        "/bin/sh"
        [|"/bin/sh"; "-c"; filename|]
        envir
        post_out
        cgi_in
        err_in
  | Some r ->
      Unix.create_process_env
        "/bin/sh"
        [|"/bin/sh"; "-c"; (r^" "^filename)|]
        envir
        post_out
        cgi_in
        err_in



(** This function makes it possible to launch a cgi script *)

let recupere_cgi head re doc_root filename ri hostname =
  try
    (* Create the three pipes to communicate with the CGI script: *)
    let (post_out, post_in) = Lwt_unix.pipe_out () in
    let (cgi_out, cgi_in) = Lwt_unix.pipe_in () in
    let (err_out, err_in) = Lwt_unix.pipe_in () in

    (* I don't want to give them to the script: *)
    Lwt_unix.set_close_on_exec cgi_out;
    Lwt_unix.set_close_on_exec post_in;
    Lwt_unix.set_close_on_exec err_out;

    (* Launch the CGI script *)
    let pid = create_process_cgi
        filename
        ri
        post_out
        cgi_in
        err_in
        re
        doc_root
        hostname
    in

    Unix.close cgi_in;
    Unix.close post_out;
    Unix.close err_in;

    let is_running = ref true in

    (* A timeout for CGI scripts *)
    (* For now a timeout for the whole process.
       We may want to reset the timeout each time the CGI writes something.
     *)
    let timeout =
      Lwt_timeout.create
        !cgitimeout
        (fun () ->
           try
             Lwt_unix.abort cgi_out CGI_Timeout;
             Lwt_unix.abort post_in CGI_Timeout;
             Lwt_unix.abort err_out CGI_Timeout;
             if !is_running
             then begin
               Unix.kill Sys.sigterm pid;
               ignore
                 (Lwt_unix.sleep 1. >>= fun () ->
                    if !is_running
                    then Unix.kill Sys.sigkill pid;
                    return ())
             end
           with Unix.Unix_error (Unix.ESRCH, _, _) -> ()
          )
    in
    Lwt_timeout.start timeout;

    (* A thread giving POST data to the CGI script: *)
    let post_in_ch = Lwt_chan.out_channel_of_descr post_in in
    ignore
      (catch
         (fun () ->
           (match ri.ri_http_frame.Ocsigen_http_frame.frame_content with
           | None -> Lwt_unix.close post_in
           | Some content_post ->
               Ocsigen_http_com.write_stream post_in_ch content_post >>= fun () ->
               Lwt_chan.flush post_in_ch >>= fun () ->
               Lwt_unix.close post_in
           ))
(*XXX Check possible errors! *)
         (function
           | Unix.Unix_error (Unix.EPIPE, _, _) ->
               Lwt_unix.close post_in
           | e ->
               Ocsigen_messages.unexpected_exception e "Cgimod.recupere_cgi (1)";
               Lwt_unix.close post_in
         ));


    (* A thread listening the error output of the CGI script
       and writing them in warnings.log *)
    let err_channel = Lwt_chan.in_channel_of_descr err_out in
    let rec get_errors () =
      Lwt_chan.input_line err_channel >>= fun err ->
      Ocsigen_messages.warning ("CGI says: "^err);
      get_errors ()
    in ignore
      (catch
         get_errors
         (function
           | End_of_file -> Lwt_unix.close err_out
           | e ->
               Ocsigen_messages.unexpected_exception e "Cgimod.recupere_cgi (2)";
               Lwt_unix.close err_out));
    (* This threads terminates, as you can see by doing:
    in ignore (catch get_errors (fun _ -> print_endline "the end";
                                          Lwt_unix.close err_out; return ()));
     *)


    (* A thread waiting the end of the process.
       if the process terminates with an error, we raise CGI_Error
     *)
    ignore
      (Lwt_unix.waitpid [] pid >>= fun (_, status) ->
      is_running := false;
      Lwt_timeout.stop timeout;
      (* All "read" will return 0, and "write" will raise "Broken Pipe" *)
      (match status with
      | Unix.WEXITED 0 -> ()
      | Unix.WEXITED i ->
          Ocsigen_messages.warning ("CGI exited with code "^(string_of_int i))
      | Unix.WSIGNALED i ->
          Ocsigen_messages.warning ("CGI killed by signal "^(string_of_int i))
      | Unix.WSTOPPED i ->
          (* Cannot occur without Unix.WUNTRACED wait_flag *)
          assert false
      );
      Lwt.return ());

    (* A thread getting the result of the CGI script *)
    let receiver =
      Ocsigen_http_com.create_receiver
        (Ocsigen_config.get_server_timeout ())
        Ocsigen_http_com.Nofirstline (Lwt_ssl.plain cgi_out)
    in
    catch
      (fun () ->
        Ocsigen_http_com.get_http_frame ~head receiver
          >>= fun http_frame ->
        return (http_frame, fun _ -> Lwt_unix.close cgi_out))
      (fun e -> Lwt_unix.close cgi_out >>= fun () -> fail e);

  with e -> fail e

(** return the content of the frame *)

let get_content str =
  match str.Ocsigen_http_frame.frame_content with
  | None   -> Ocsigen_stream.make (fun () -> Ocsigen_stream.empty None)
  | Some c -> c


(*****************************************************************************)
let rec parse_global_config = function
  | [] -> ()
  | (Element ("cgitimeout", [("value", s)], []))::[] ->
      cgitimeout := int_of_string s
  | _ -> raise (Error_in_config_file
                  ("Unexpected content inside cgimod config"))


(*****************************************************************************)

let gen reg = function
  | Ocsigen_extensions.Req_found _ -> 
      Lwt.return Ocsigen_extensions.Ext_do_nothing
  | Ocsigen_extensions.Req_not_found (err, ri) ->
  catch
    (* Is it a cgi page? *)
    (fun () ->
         Ocsigen_messages.debug2 "--Cgimod: Is it a cgi file?";
         let (filename, re, doc_root) =
           find_cgi_page ri reg ri.request_info.ri_sub_path
         in
         recupere_cgi
           (ri.request_info.ri_method = Http_header.HEAD)
           re doc_root filename ri.request_info
           (Ocsigen_extensions.get_hostname ri)
         >>= fun (frame, finalizer) ->
         let header = frame.Ocsigen_http_frame.frame_header in
         let content = get_content frame in
         Ocsigen_stream.add_finalizer content finalizer;
         Lwt.catch
           (fun () ->
              let code =
                try
                  let status =
                    Ocsigen_http_frame.Http_header.get_headers_value
                      header Http_headers.status in
                  if String.length status < 3 then
                    raise (Failure "Cgimod.gen");
                  Some (int_of_string (String.sub status 0 3))
                with
                | Not_found -> None
                | Failure _ ->
                    raise (CGI_Error (Failure "Bad Status line in header"))
              in
              let loc =
                try
                  Some (Ocsigen_http_frame.Http_header.get_headers_value
                          header Http_headers.location)
                with Not_found ->
                  None
              in
              match code, loc with
              | None, Some loc ->
                  Ocsigen_stream.finalize content `Success >>= fun () ->
                  if loc <> "" && loc.[0] = '/' then
                    Lwt.return
                      (Ext_retry_with ({ ri with request_info =
                                           ri_of_url loc ri.request_info },
                                       Ocsigen_cookies.Cookies.empty))
                  else
                    let default_result = Ocsigen_http_frame.default_result () in
                    Lwt.return
                      (Ext_found
                         (fun () ->
                            Lwt.return
                              { default_result with
                                  res_code= 301; (* Moved permanently *)
                                  res_location= Some loc}))
              | _, _ ->
                  let code = match code with
                  | None -> 200
                  | Some c -> c
                  in
                  let default_result = Ocsigen_http_frame.default_result () in
(*VVV Warning: this is really late to make the return Ext_found ... *)
(*VVV But the extension may also answer Ext_retry_with ... *)
(*VVV and the other extensions may receive requests in wrong order ... *)

                  Lwt.return
                    (Ext_found
                       (fun () ->
(*VVV NO! If sending is interrupted, we probably must do something else! *)
                          Ocsigen_stream.add_finalizer content
                            (fun outcome ->
                               match outcome with
                                 `Failure ->
                                   frame.Ocsigen_http_frame.frame_abort ()
                               | `Success ->
                                   Lwt.return ());
                          Lwt.return
                            {default_result with
                               res_content_length = None;
                               res_stream = (content, None);
                               res_location= loc;
                               res_headers =
                                Http_headers.replace_opt
                                  Http_headers.status None
                                  header.Http_header.headers;
                               res_code = code})))
           (fun e ->
              Ocsigen_stream.finalize content `Failure >>= fun () ->
              Lwt.fail e))
    (function
      | Unix.Unix_error (Unix.EACCES,_,_)
      | Lost_connection _ as e -> fail e
      | Unix.Unix_error (Unix.ENOENT,_,_) -> return (Ext_next 404)
      | Failed_403 -> return (Ext_next 403)
      | Failed_404 -> return (Ext_next 404)
      | Not_concerned -> return (Ext_next err)
      | e -> fail e)




(*****************************************************************************)
(** Parsing of config file *)

let rec set_env = function
  | [] -> []
  | (Element("setenv", [("var",vr);("val",vl)], []))::l ->
     if List.mem vr environment
     then (Ocsigen_messages.debug (fun () -> "--Cgimod: variable no set "^vr);
           set_env l)
     else (vr,vl)::set_env l
  | _ :: l -> raise (Error_in_config_file "Bad config tag for <cgi>")

let parse_config _ path _ _ = function
  | Element ("cgi", atts, l) ->
      let good_root r =
        Regexp.quote (string_conform2 r) 
      in
      let dir = match atts with
      | [] ->
          raise (Error_in_config_file
                   "attributes expected for <cgi>")
      | [("root",r);("dir", s)] ->
      {
           regexp= Regexp.regexp ("^"^(good_root r)^"([^/]*)");

           doc_root= Ocsigen_extensions.parse_user_dir (string_conform1 s);
           script= Ocsigen_extensions.parse_user_dir "$1";

           path= string_conform 
          (Ocsigen_lib.string_of_url_path ~encode:true path);
           path_info="";

           exec=None;
           env=set_env l}
      | ("regexp", s)::("dir",d)::("script",t)::q ->
          {
           regexp=Regexp.regexp ("^"^s);

           doc_root= Ocsigen_extensions.parse_user_dir (string_conform1 d);
           script= Ocsigen_extensions.parse_user_dir t;

           path= string_conform 
              (Ocsigen_lib.string_of_url_path ~encode:true path);
           path_info=""; (* unknown for the moment *)

           exec= (match q with
                 |[] -> None
                 |[("exec",x)] -> Some(x)
                 |_ -> raise (Error_in_config_file
                                "Wrong attributes for <cgi>")) ;
           env=set_env l}
      | _ -> raise (Error_in_config_file "Wrong attributes for <cgi>")
      in
      gen dir
  | Element (t, _, _) -> raise (Bad_config_tag_for_extension t)
  | _ ->
      raise (Error_in_config_file "Unexpected data in config file")




(*****************************************************************************)
(** Registration of the extension *)
let () = register_extension
  ~name:"cgimod"
  ~fun_site:(fun _ -> parse_config)
  ~init_fun:parse_global_config
  ()
