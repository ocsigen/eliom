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


open Lwt
open Extensions
open Simplexmlparser
open Http_frame
open Http_com
open Predefined_senders


(** les droite du fichier ne sont pas compatibles pour le CGI*)
exception Ocsigen_No_CGI

(** il n'existe pas d'executable pour ce fichier dans le .conf*)
exception NoExecCGI 


(*****************************************************************************)
(* The table of cgi pages for each virtual server                            *)

type reg={
  root:string;
  regexp:Netstring_pcre.regexp;
  doc_root:string;
  dest: string;
  path:string;
  exec:string option;
  env:(string * string) list}

type assockind = 
    Regexp of reg

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

let user_dir_regexp = Netstring_pcre.regexp "(.*)\\$u\\(([^\\)]*)\\)(.*)"

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

let set_dir dirref assoc path =
  let rec assoc_and_remove a = function
    | [] ->raise Not_found
    | (b,v)::l when a = b -> (v,l)
    | e::l -> let v,ll = assoc_and_remove a l
          in v,(e::ll)
  in
  let rec add_path = function
    | [] -> 
        (match assoc with
           | Regexp r -> Page_dir ([r],[]))
    | a::l -> Page_dir ([], [(a, add_path l)])
  in
  let rec aux (Page_dir (rl, l1)) = function
    | [] ->
        (match assoc with
        | Regexp r -> Page_dir (rl@[r], l1))
    | a::l -> 
        try
          let sd1,l2 = assoc_and_remove a l1 in
          let sd = aux sd1 l in
          Page_dir (rl, (a, sd)::l2)
        with Not_found -> Page_dir (rl, (a,(add_path l))::l1)
  in 
  dirref := aux !dirref path




let rec replace_first_match stringpath = function
  | [] -> None
  | re::l ->
      match Netstring_pcre.string_match re.regexp stringpath 0 with
	| None -> replace_first_match stringpath l
	| Some _ -> 
	    Some (
	      {
		root=re.root;
		regexp=re.regexp;
		doc_root=re.doc_root;
		dest=Netstring_pcre.global_replace re.regexp re.dest stringpath;
		path=Netstring_pcre.global_replace re.regexp re.path stringpath;
		exec=re.exec;
		env=re.env;})



 
let rec find_page (Page_dir (regexps, subdir_list)) path = 
  (* First we try the regexps *)
  match 
    (match regexps with
       | [] ->None
       | _ -> 
           let stringpath = Ocsimisc.string_of_url_path path in
           replace_first_match stringpath regexps)
  with
    | Some re -> (* Matching regexp found! *)
	let s=re.doc_root^re.dest in
        Some (
          ((* hack to get user dirs *)
            match Netstring_pcre.string_match user_dir_regexp s 0 with
              | None -> s
              | Some result -> 
		  let user = Netstring_pcre.matched_group result 2 s in
		  try
                    let userdir = (Unix.getpwnam user).Unix.pw_dir in
                    (Netstring_pcre.matched_group result 1 s)^
                      userdir^
                      (Netstring_pcre.matched_group result 3 s)
		  with _ -> raise Not_found
          ) ,re)
    | None -> 
(* Then we continue *)
        match path with
          | [] -> None
          | [""] -> None
        | ""::l
        | ".."::l -> raise Ocsigen_malformed_url
            (* For security reasons, .. is not allowed in paths *)
            (* Actually it has already been removed by server.ml *)
        | a::l -> 
            try 
              let e = List.assoc a subdir_list in
              find_page e l
            with 
		Not_found -> None







(**permet de recuperer le fichier correspondant a l url*)
let find_cgi_page cgidirref path =
  let find_file = function
    | None ->raise Ocsigen_404
    | Some (filename, re) ->
        (* See also module Files in eliom.ml *)
        Messages.debug ("--Cgimod: Testing \""^filename^"\".");
        let stat= Unix.LargeFile.stat filename in
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
	  try
	    match re.exec with
	      | None ->
		  Unix.access filename [Unix.X_OK];
		  (filename, re)
	      | Some exec ->
		  Unix.access filename [Unix.R_OK];
		  (filename, re)
	  with 
	    |Unix.Unix_error (Unix.EACCES,"access",filename) -> 
	       raise Ocsigen_No_CGI
	    |e->raise e
        end
        else raise Ocsigen_404 (* ??? *)
  in
  find_file (find_page !cgidirref path)




(*****************************************************************************)



(** permet de creer le tableau des variables d environnement *)
let array_environment pages_tree filename re ri=
  let opt=function
    |None->""
    |Some(a)->a
  and opt_int=function
    |None->0
    |Some(a)->Int64.to_int a
  in 
  let meth= 
    match Http_header.get_firstline ri.ri_http_frame.Stream_http_frame.header
    with
      | Http_header.Query (meth, _) -> Framepp.string_of_method meth
      | _ -> failwith "Bad request"
  in let get_ri_value var info=
    try
      let st=String.lowercase 
(Http_header.get_headers_value ri.ri_http_frame.Stream_http_frame.header info)
      in [var^Printf.sprintf "=%s" st]
    with
	_->[]
  in

  [Printf.sprintf "CONTENT_LENGTH=%d" (opt_int ri.ri_content_length);
   Printf.sprintf "CONTENT_TYPE=%s"  (opt ri.ri_content_type);
   Printf.sprintf "DOCUMENT_ROOT=%s" re.doc_root;
   "GATEWAY_INTERFACE=CGI/1.1";
   Printf.sprintf "HTTP_COOKIE=%s" (opt (Lazy.force ri.ri_cookies_string));
   Printf.sprintf "HTTP_HOST=%s" (opt ri.ri_host);
   Printf.sprintf "HTTP_REFERER=%s" (opt (Lazy.force ri.ri_referer));
   Printf.sprintf "HTTP_USER_AGENT=%s" ri.ri_user_agent;
   Printf.sprintf "PATH_INFO=%s" re.path;
   Printf.sprintf "PATH_TRANSLATED=%s" (re.doc_root^re.path);
   Printf.sprintf "QUERY_STRING=%s" (opt ri.ri_get_params_string);
   Printf.sprintf "REMOTE_PORT=%d" ri.ri_remote_port;
   Printf.sprintf "REMOTE_ADDR=%s" ri.ri_ip;
   Printf.sprintf "REQUEST_METHOD=%s" meth;
   Printf.sprintf "SCRIPT_NAME=%s" (re.root^re.dest);
   Printf.sprintf "SCRIPT_FILENAME=%s" filename;
   Printf.sprintf "SERVER_NAME=%s" Ocsiconfig.server_name;
   Printf.sprintf "SERVER_PORT=%s" (string_of_int ri.ri_port);
   "SERVER_PROTOCOL=HTTP/1.1";
   Printf.sprintf "SERVER_SOFTWARE=%s" (Ocsiconfig.server_name^
					  "/"^
					  Ocsiconfig.version_number)]@
    (get_ri_value "HTTP_ACCEPT" "Accept")@
    (get_ri_value "HTTP_ACCEPT_CHARSET" "Accept-Charset")@
    (get_ri_value "HTTP_ACCEPT_ENCODING" "Accept-Encoding")@
    (get_ri_value "HTTP_ACCEPT_LANGUAGE" "Accept-Language")@
    (get_ri_value "HTTP_CONNECT" "Connection")


(*****************************************************************************)


let rec set_env_list=function
  |[]->[]
  |(vr, vl) :: l -> (vr^"="^vl) :: set_env_list l


(** launch the process *)

let create_process_cgi pages_tree filename ri post_out cgi_in re=
  let opt=function
    |None -> failwith "CAS IMPOSSIBLE"
    |Some a -> a
  and envir=Array.of_list (
    (array_environment pages_tree filename re ri)@(set_env_list re.env)) in
  if re.exec = None then
    Unix.create_process_env 
      "/bin/sh" 
      [|"/bin/sh";"-c";filename|]
      envir
      post_out 
      cgi_in 
      Unix.stderr
  else 
    Unix.create_process_env 
      "/bin/sh" 
      [|"/bin/sh";"-c";((opt re.exec)^" "^filename)|]
      envir
      post_out 
      cgi_in 
      Unix.stderr


    
(** This function makes it possible to launch prog cgi, since a new process 
    with environment variables appropriated in reading by stream the result 
    of this last*)

let recupere_cgi pages_tree re filename ri=
  let opt=function
    |None->failwith "CAS IMPOSSIBLE"
    |Some(c)->c
  in let (post_out,post_in) = Unix.pipe () in
  let (cgi_out, cgi_in) = Unix.pipe () in
  let (err_out, err_in) = Unix.pipe () in
  Unix.set_nonblock post_in;
  Unix.set_nonblock cgi_out;
  Unix.set_nonblock err_out;
  (if ri.ri_http_frame.Stream_http_frame.content = None
   then (return ())
   else 
     (let content_post=opt ri.ri_http_frame.Stream_http_frame.content ()
     and fct ()=() in
     Stream_sender.really_write (Lwt_unix.Plain post_in) fct content_post))
  >>= function () ->
  let receiver= Http_com.create_receiver 
    ~mode:Http_com.Nofirstline (Lwt_unix.Plain cgi_out) in
  let pid = create_process_cgi 
    pages_tree 
    filename 
    ri
    post_out 
    cgi_in 
    re

  in Stream_receiver.get_http_frame (return ()) receiver 
    ~doing_keep_alive:false () >>= fun http_frame ->
  ignore ( 
    Lwt_unix.waitpid [] pid >>= fun _ ->
    Unix.close cgi_in;
    Unix.close post_in;
    Unix.close post_out;
    return ());
  ignore (http_frame.Stream_http_frame.waiter_thread >>= fun () ->
    Unix.close cgi_out;
    return ());
  return http_frame
            
(** return the header of the frame *)

let get_header str =
  let a=str.Stream_http_frame.header 
  in return a


(** return the content of the frame *)

let get_content str =
  match str.Stream_http_frame.content with
    |None->return (Ocsistream.empty_stream None)
    |Some(k)-> let stream = k () in return stream


(*****************************************************************************)

(** Parsing of config file *)

let rec set_env=function
  | [] -> []
  | (Element("setenv", [("var",vr);("val",vl)], []))::l ->
     if List.mem vr environment
     then (Messages.debug ("--Cgimod: no set variable "^vr); set_env l)
     else (vr,vl)::set_env l
  | _ :: l -> raise (Error_in_config_file "Bad config tag for <cgi>")

let string_conform file=
  try
    match file.[(String.length file) - 1] , file.[0] with
      | '/' ,'/' ->String.sub file 1 ((String.length file) - 1)
      | '/' ,_  -> file
      | _, '/'-> String.sub file 1 ((String.length file) - 1)^"/"
      | _, _ -> file^"/"
  with _ -> file


let parse_config page_tree path = function 
  | Element ("cgi", atts, l) -> 
        let dir = match atts with
          | [] -> 
              raise (Error_in_config_file
                       "dir attribute expected for <cgi>")
          | [("root",r);("dir", s)] ->
	      let conform=(string_conform r) in
	      {
		root="/"^(Ocsimisc.string_of_url_path path)^"/"^conform;
		regexp=Netstring_pcre.regexp (conform^"([^/]*)(.*)");
		doc_root=s;
		dest="$1";
		path="$2";
		exec=None;
		env=set_env l}
	  | [("root",r);("regexp", s);("dir",d);("dest",t);("path",p)] -> 
	      let conform=(string_conform r) in
	      {
		root="/"^(Ocsimisc.string_of_url_path path)^"/"^conform;
		regexp=Netstring_pcre.regexp (conform^s);
		doc_root=d;
		dest=t;
		path=p;
		exec=None;
		env=set_env l}
	  | [("root",r);("regexp", s);("dir",d);("dest",t);("path",p);("exec",x)] -> 
	      let conform=(string_conform r) in
	      {
		root="/"^(Ocsimisc.string_of_url_path path)^"/"^conform;
		regexp=Netstring_pcre.regexp (conform^s);
		doc_root=d;
		dest=t;
		path=p;
		exec=Some(x);
		env=set_env l}
          | _ -> raise (Error_in_config_file "Wrong attribute for <cgi>")
        in 
        set_dir page_tree (Regexp dir) path
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
	recupere_cgi pages_tree re filename ri >>= fun frame ->
	get_header frame >>= fun header -> 
	get_content frame >>= fun content -> 
	  return
	    (Ext_found
               {res_cookies= [];
		res_send_page= 
		   Predefined_senders.send_stream_page 
		     ?contenttype:None ~content:(fun () -> content);
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

