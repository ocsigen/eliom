(* Ocsigen
 * http://www.ocsigen.org
 * Module cgimod.ml
 * Copyright (C) 2007 J‚r“me Velleine - Gabriel Kerneis
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

(** Module CGI for Ocsigen *)


open Lwt
open Extensions
open Simplexmlparser
open Http_frame
open Http_com
open Predefined_senders

module Regexp = Netstring_pcre

exception CGI_Timeout
exception CGI_Error of exn

let cgitimeout = ref 30


(*****************************************************************************)
(* The table of cgi pages for each virtual server                            *)

type reg = {
  regexp:Regexp.regexp; (** regexp of the script url *)
  
  doc_root:string; (** physical directory of the script (regexp) *)
  script: string; (** physical name of the script (regexp) *)
  
  path: string; (** path of the script *)
  path_info: string; (** path_info environment variable *)
  
  exec:string option; (** binary to execute the script with (optionnal) *)
  env:(string * string) list (** environment variables *) }

(* cgi or static pages *)
type page_dir = 
    Page_dir of reg list * (string * page_dir) list

(* cgi or static pages *)
type pages_tree = 
    page_dir ref

let new_pages_tree () =
  (ref (Page_dir ([],[])))

(*****************************************************************************)
(** table of cgi dir*)

let cgi_dir_table = ref []

let find k = List.assoc k !cgi_dir_table

let add k a = cgi_dir_table:= (k,a)::!cgi_dir_table

let user_dir_regexp = Regexp.regexp "(.*)\\$u\\(([^\\)]*)\\)(.*)"

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

(* split a string in two parts, according to a regexp *)
let split_regexp r s = 
  match Regexp.string_match r s 0 with
  | None -> None (* the begining of the string doesn't match the regexp *)
  | Some result -> 
    let (split,l) = Regexp.match_end result, String.length s in
    let s' = Regexp.first_chars s split in
    let s'' = Regexp.last_chars s (l - split) in
    Some (s',s'')
	
let set_dir dirref assoc path =
  let rec assoc_and_remove a = function
    | [] -> raise Not_found
    | (b,v)::l when a = b -> (v,l)
    | e::l -> let v,ll = assoc_and_remove a l
          in v,(e::ll)
  in
  let rec add_path = function
    | [] -> Page_dir ([assoc],[])
    | a::l -> Page_dir ([], [(a, add_path l)])
  in
  let rec aux (Page_dir (rl, l1)) = function
    | [] -> Page_dir (rl@[assoc], l1)
    | a::l -> 
        try
          let sd1,l2 = assoc_and_remove a l1 in
          let sd = aux sd1 l in
          Page_dir (rl, (a, sd)::l2)
        with Not_found -> Page_dir (rl, (a,(add_path l))::l1)
  in 
  dirref := aux !dirref path

(**permet de recuperer le fichier correspondant a l url*)
let find_cgi_page cgidirref path =
  let find_file (filename, re) handler =
    (* See also module Files in eliom.ml *)
    Messages.debug ("--Cgimod: Testing \""^filename^"\".");
    try
      let stat = Unix.LargeFile.stat filename in
      let filename = 
        if (stat.Unix.LargeFile.st_kind = Unix.S_DIR)
        then 
          raise Ocsigen_Is_a_directory
        else filename
      in
      Messages.debug ("--Cgimod: Looking for \""^filename^"\".");
      
      if (stat.Unix.LargeFile.st_kind 
            = Unix.S_REG)
      then begin
	match re.exec with
	  | None ->
	      Unix.access filename [Unix.X_OK];
	      (filename, re)
	  | Some exec ->
	      Unix.access filename [Unix.R_OK];
	      (filename, re)
      end
      else raise Ocsigen_403
    with
    | Unix.Unix_error (Unix.ENOENT, _, _) -> handler ()
  in

  let rec find_in_dir dirtotry path handler =
    match dirtotry with
    | [] -> handler ()
    | re::l ->
        match split_regexp re.regexp path with
        | None -> find_in_dir l path handler
        | Some (path',path_info) -> 
	    let re = 
	    {re with
	     doc_root=Regexp.global_replace re.regexp re.doc_root path';
	     script=Regexp.global_replace re.regexp re.script path';
	     path = path';
	     path_info=path_info}
            in
            let s = re.doc_root^re.script in
            (* hack to get user dirs *)
            match Regexp.string_match user_dir_regexp s 0 with
            | None -> 
                find_file (s, re) (fun () -> find_in_dir l path handler)
            | Some result ->
	        let user = Regexp.matched_group result 2 s in
                let userdir = (Unix.getpwnam user).Unix.pw_dir in
                find_file
                  ((Regexp.matched_group result 1 s)^
                   userdir^
                   (Regexp.matched_group result 3 s),
                   re)
                  (fun () -> find_in_dir l path handler)
  in                

  let rec find_page 
      dirtotry pathtotry (Page_dir (regexps, subdir_list)) path handler = 
    match path with
    | [] -> 
        find_in_dir regexps ""
          (fun () -> 
            find_in_dir dirtotry 
              (Ocsimisc.string_of_url_path pathtotry) handler)
    | [""] ->
        find_in_dir regexps "/"
          (fun () -> 
            find_in_dir dirtotry 
              (Ocsimisc.string_of_url_path
                 (pathtotry@[""])) handler)
    | ""::l
    | ".."::l -> raise Ocsigen_malformed_url
          (* For security reasons, .. is not allowed in paths *)
          (* Actually it has already been removed by server.ml *)
    | a::l -> 
        try 
          let e = List.assoc a subdir_list in
          match regexps with
          | [] ->
              find_page dirtotry (pathtotry@[a]) e l handler
          | _ ->
	      find_page regexps [""; a] e l
                (fun () -> 
                  find_in_dir dirtotry 
                    (Ocsimisc.string_of_url_path (pathtotry@[a])) handler)
        with 
	| Not_found ->
            let p2 = Ocsimisc.string_of_url_path path in
            match regexps with
            | [] ->
                find_in_dir dirtotry 
                  (Ocsimisc.string_of_url_path (pathtotry@[p2]))
                  handler
            | _ ->
                find_in_dir regexps ("/"^p2)
                  (fun () -> 
                    find_in_dir dirtotry 
                      (Ocsimisc.string_of_url_path (pathtotry@[p2])) handler)

  in
  find_page [] [] !cgidirref path (fun () -> raise Ocsigen_404)

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

let array_environment pages_tree filename re ri =
  let header = ri.ri_http_frame.Stream_http_frame.header in
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
    | _ -> raise Ocsimisc.Ocsigen_Bad_Request
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
  [Printf.sprintf "SERVER_NAME=%s" Ocsiconfig.server_name;
   Printf.sprintf "SERVER_SOFTWARE=%s" Ocsiconfig.full_server_name ;
   "GATEWAY_INTERFACE=CGI/1.1"] ;
   
   (* Request-specific variables *)
  ["SERVER_PROTOCOL=HTTP/1.1";
   Printf.sprintf "SERVER_PORT=%s" (string_of_int ri.ri_port);
   Printf.sprintf "REQUEST_METHOD=%s" meth;
   Printf.sprintf "PATH_INFO=%s" re.path_info;
   Printf.sprintf "PATH_TRANSLATED=" ; (* PATH_INFO virtual -> physical; unclear, so don't set *)
   Printf.sprintf "SCRIPT_NAME=%s" re.path;
   Printf.sprintf "QUERY_STRING=%s" (opt ri.ri_get_params_string);
   Printf.sprintf "REMOTE_ADDR=%s" ri.ri_ip; 
   (* no REMOTE_HOST: implies reverse DNS resolution *)
   (* neither AUTH_TYPE, REMOTE_USER nor REMOTE_IDENT: implies authentication *)
   Printf.sprintf "CONTENT_LENGTH=%s" (opt_int ri.ri_content_length);
   Printf.sprintf "CONTENT_TYPE=%s"  (opt ri.ri_content_type)] ;
   
   (* Additional headers, coming from the client *)
 [(* Document_root is defined by Apache but not in the CGI's spec *)
   Printf.sprintf "DOCUMENT_ROOT=%s" re.doc_root;
   
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

let create_process_cgi pages_tree filename ri post_out cgi_in err_in re =
  let envir = Array.of_list (
    (array_environment pages_tree filename re ri)@(set_env_list re.env)) in
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

let recupere_cgi head pages_tree re filename ri =
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
        pages_tree 
        filename 
        ri
        post_out 
        cgi_in 
        err_in
        re
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
          )
    in

    (* A thread giving POST data to the CGI script: *)
    let post_in_ch = Lwt_unix.out_channel_of_descr post_in in
    ignore
      (catch
         (fun () ->
           (match ri.ri_http_frame.Stream_http_frame.content with
           | None -> Lwt_unix.close post_in; return ()
           | Some content_post -> 
               Stream_sender.really_write post_in_ch
                 (fun () -> return ())
                 (content_post ()) >>= fun () ->
                 Lwt_chan.flush post_in_ch >>= fun () ->
                 Lwt_unix.close post_in;
                 return ()
           ))
         (function
           | Unix.Unix_error (Unix.EPIPE, _, _) -> 
               Lwt_unix.close post_in; 
               return ()
           | e -> Messages.unexpected_exception e "Cgimod.recupere_cgi (1)";
               Lwt_unix.close post_in; 
               return ()
         ));
    
    (* A thread listening the error output of the CGI script 
       and writing them in warnings.log *)
    let err_channel = Lwt_unix.in_channel_of_descr err_out in
    let rec get_errors () =
      Lwt_chan.input_line err_channel >>= fun err ->
      Messages.warning ("CGI says: "^err);
      get_errors ()
    in ignore 
      (catch
         get_errors 
         (function 
           | End_of_file -> Lwt_unix.close err_out; return ()
           | e -> Messages.unexpected_exception e "Cgimod.recupere_cgi (2)";
               Lwt_unix.close err_out; 
               return ()));
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
      Lwt_timeout.remove timeout; 
      (* All "read" will return 0, and "write" will raise "Broken Pipe" *)
      (match status with
      | Unix.WEXITED 0 -> ()
      | Unix.WEXITED i -> 
          Messages.warning ("CGI exited with code "^(string_of_int i))
      | Unix.WSIGNALED i -> 
          Messages.warning ("CGI killed by signal "^(string_of_int i))
      | Unix.WSTOPPED i -> 
          (* Cannot occur without Unix.WUNTRACED wait_flag *)
          assert false
      );
      return ());

    (* A thread getting the result of the CGI script *)
    let receiver = Http_com.create_receiver 
        ~mode:Http_com.Nofirstline (Lwt_ssl.plain cgi_out)
    in
    catch 
      (fun () ->
	Stream_receiver.get_http_frame ((* now *) return ()) receiver ~head 
          ~doing_keep_alive:false () >>= fun http_frame ->
        ignore 
	  (http_frame.Stream_http_frame.waiter_thread >>= fun () ->
          Lwt_unix.close cgi_out;
	  return ());
	return http_frame)
      (fun e -> Lwt_unix.close cgi_out; fail e);

  with e -> fail e
            
(** return the header of the frame *)

let get_header str =
  let a = str.Stream_http_frame.header 
  in return a


(** return the content of the frame *)

let get_content str =
  match str.Stream_http_frame.content with
  | None -> return (fun () -> Ocsistream.empty_stream None)
  | Some c -> return c


(*****************************************************************************)
let rec parse_global_config = function
  | [] -> ()
  | (Element ("cgitimeout", [("value", s)], []))::ll ->
      cgitimeout := int_of_string s
  | _ -> raise (Error_in_config_file 
                  ("Unexpected content inside cgimod config"))

let _ = parse_global_config (Extensions.get_config ())

(*****************************************************************************)
(** Parsing of config file *)

let rec set_env=function
  | [] -> []
  | (Element("setenv", [("var",vr);("val",vl)], []))::l ->
     if List.mem vr environment
     then (Messages.debug ("--Cgimod: no set variable "^vr); set_env l)
     else (vr,vl)::set_env l
  | _ :: l -> raise (Error_in_config_file "Bad config tag for <cgi>")

let string_conform s = match String.length s with
  |0 -> "/"
  |n -> match  s.[0], s.[n - 1] with
        | '/' ,'/' -> s
        | _, '/' -> "/"^s
        | '/', _ -> s^"/"
        | _, _ -> "/"^s^"/"

let parse_config page_tree path = function 
  | Element ("cgi", atts, l) -> 
      let good_root r = Regexp.quote (string_conform
      (Ocsimisc.string_of_url_path (path@(Regexp.split (Regexp.regexp "/") r)))) in
      let dir = match atts with
      | [] -> 
          raise (Error_in_config_file
                   "attributes expected for <cgi>")
      | [("root",r);("dir", s)] ->
      {
	   regexp= Regexp.regexp ((good_root r)^"([^/]*)");
	   
	   doc_root= string_conform s;
	   script="$1";
	   
	   path=""; 
           path_info="";
	   
	   exec=None; 
           env=set_env l}
      | ("regexp", s)::("dir",d)::("script",t)::q -> 
	  {
	   regexp=Regexp.regexp ((good_root "")^s);
	   
	   doc_root= string_conform d;
	   script=t;
	   
	   path="";
           path_info=""; (* unknown for the moment *)
	   
	   exec= (match q with 
	         |[] -> None 
		 |[("exec",x)] -> Some(x)
		 |_ ->  raise (Error_in_config_file "Wrong attributes for <cgi>")) ;
	   env=set_env l}
(*      | [("root",r);("regexp", s);("dir",d);("dest",t);("path",p);("exec",x)] -> 
	  let stat = Unix.LargeFile.stat x in
	  if (stat.Unix.LargeFile.st_kind 
            <> Unix.S_REG)
	  then 
	    raise (Error_in_config_file "<cgi> Exec does not exist")
	  else

	    let conform = string_conform r in
	    {
	      root=string_conform((Ocsimisc.string_of_url_path path)^conform);
	      regexp=Regexp.regexp (conform^s);
	      doc_root= string_conform d;
	      dest=t;
	      path=p;
	      exec=Some(x);
	      env=set_env l}
*)
      | _ -> raise (Error_in_config_file "Wrong attributes for <cgi>")
      in 
      set_dir page_tree dir path
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
         let (filename, re) =
           find_cgi_page pages_tree ri.ri_path
         in 
	 recupere_cgi 
           (ri.ri_method = Http_header.HEAD) 
           pages_tree re filename ri >>= fun frame ->
         get_header frame >>= fun header -> 
         get_content frame >>= fun content -> 
         (try 
           return
             (Some
                (int_of_string
                   (String.sub 
                      (Http_frame.Http_header.get_headers_value 
                         header Http_headers.status)
                      0 3)))
         with 
         | Not_found -> return None
         | _ -> fail (CGI_Error 
                        (Failure "Bad Status line in header"))
         ) >>= fun code ->
         try
           if code <> None
           then raise Not_found
           else 
             let loc =
               Http_frame.Http_header.get_headers_value
                 header Http_headers.location
             in
             (try
               ignore (Neturl.extract_url_scheme loc);
               return
                 (Ext_found
                    {res_cookies= [];
                     res_lastmodified= None;
                     res_etag= None;
                     res_code= Some 301; (* Moved permanently *)
                     res_send_page= 
                     (fun ?filter ?cookies waiter ~clientproto ?code
                         ?etag ~keep_alive ?last_modified ?location
                         ~head ?headers ?charset s ->
                           Predefined_senders.send_empty
                             ~content:() 
                             ?filter
                             ?cookies
                             waiter 
                             ~clientproto
                             ?code
                             ?etag ~keep_alive
                             ?last_modified 
                             ~location:loc
                             ~head ?headers ?charset s);
                     res_headers= Http_headers.empty;
                     res_charset= None;
                     res_filter=None
                   })
             with 
             | Neturl.Malformed_URL -> 
                 return (Ext_retry_with ((ri_of_url loc ri), []))
             )
         with
         | Not_found ->
             return
	       (Ext_found
                  {res_cookies= [];
		   res_send_page= 
		   Predefined_senders.send_stream_page 
		     ?contenttype:None
                     ~content;
		   res_headers=
                     Http_headers.replace_opt
                       Http_headers.status None header.Http_header.headers;
		   res_code= code;
		   res_lastmodified= None;
		   res_etag= None;
		   res_charset= None;
                   res_filter=None})
       end
       else return (Ext_not_found Ocsigen_404))
    (function
      | Unix.Unix_error (Unix.EACCES,_,_)
      | Ocsigen_Is_a_directory
      | Ocsigen_malformed_url 
      | Lost_connection as e -> fail e
      | Ocsigen_404 ->return (Ext_not_found Ocsigen_404)
      | Unix.Unix_error (Unix.ENOENT,_,_) -> return (Ext_not_found Ocsigen_404)
      | e -> fail e)

(*****************************************************************************)
(** Function to be called at the beginning of the initialisation phase 
    of the server  *)
let start_init () =
  ()

(** Function to be called at the end of the initialisation phase *)
let end_init () = ()
(*	for default cgi dir *)   

(*****************************************************************************)
(** Registration of the extension *)
let _ = R.register_extension (* takes a quadruple *)
  ((fun hostpattern -> 
      let page_tree = 
        try 
          find hostpattern
        with Not_found -> 
          let n = new_pages_tree () in
          add hostpattern n;
          n
      in
      (gen page_tree, 
       parse_config page_tree)),
   start_init,
   end_init,
   exn_handler)
