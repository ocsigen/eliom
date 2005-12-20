(* Ocsigen
 * sender_helpers.ml Copyright (C) 2005 Denis Berthod
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)
(** this module provides helpers fonctions to create senders*)

open Http_frame
open Http_com
open Lwt

(** this module instantiate the HTTP_CONTENT signature for an Xhtml content*)
module Xhtml_content =
  struct
    type t = [ `Html ] XHTML.M.elt
    let string_of_content c = 
      let s = XHTML.M.ocsigen_print c in
      (* debug *)Messages.debug s;
      s
    (*il n'y a pas encore de parser pour ce type*)
    let content_of_string s = assert false
  end

module Empty_content =
  struct
    type t = unit
    let string_of_content c = ""
    let content_of_string s = ()
  end

let read_file ?(buffer_size=512) fd =
  let rec read_aux (res:string) =
    function
      |0 ->  return res
      |_ ->
          let buf = String.create buffer_size in
          Lwt_unix.read fd buf 0 buffer_size >>=
            (fun nb_lu -> 
              let str_lu = String.sub buf 0 nb_lu in
              read_aux (res^str_lu) nb_lu
            )
  in let buf = String.create buffer_size in
  Lwt_unix.read fd buf 0 buffer_size >>=
    (fun nb_lu ->
      let str_lu = String.sub buf 0 nb_lu in
      read_aux str_lu nb_lu 
     
    )

(** this module instanciate the HTTP_CONTENT signature for the files*)
module File_content =
  struct
    type t = string (*nom du fichier*)
    let string_of_content c  =
      (*ouverture du fichier*)
      let fd = Unix.openfile c [Unix.O_RDONLY;Unix.O_NONBLOCK] 0o666 in
      Lwt_unix.run (read_file fd )

    let content_of_string s = assert false
      
  end

(** this module is a Http_frame with empty content*)
module Empty_http_frame = FHttp_frame (Empty_content)

(** this module is a sender that send Http_frame with empty content*)
module Empty_sender = FHttp_sender(Empty_content)

(** this module is a Http_frame with Xhtml content*)
module Xhtml_http_frame = FHttp_frame (Xhtml_content)

(** this module is a sender that send Http_frame with Xhtml content*)
module Xhtml_sender = FHttp_sender(Xhtml_content)

(** this module is a Http_frame with file content*) 
module File_http_frame = FHttp_frame (File_content)

(** this module is a sender that send Http_frame with file content*)
module File_sender = FHttp_sender(File_content)

(** fonction that create a sender with xhtml content
server_name is the name of the server send in the HTTP header
proto is the protocol, default is HTTP/1.1
fd is the Unix file descriptor *)
let create_xhtml_sender ?server_name ?proto fd =
  let hd =
    match server_name with
    |None -> []
    |Some s -> [("Server",s)]
  in
  let hd2 =
    [
      ("Accept-Ranges","bytes");
      ("Cache-Control","no-cache");
      ("Content-Type","text/html")
    ]@hd
  in
  match proto with
  |None ->
      Xhtml_sender.create ~headers:hd2 fd
  |Some p -> 
      Xhtml_sender.create ~headers:hd2 ~proto:p fd

(** fonction that create a sender with empty content
server_name is the name of the server send in the HTTP header
proto is the protocol, default is HTTP/1.1
fd is the Unix file descriptor *)
let create_empty_sender ?server_name ?proto fd =
  let hd =
    match server_name with
    |None -> []
    |Some s -> [("Server",s)]
  in
  let hd2 =
    [
      ("Accept-Ranges","bytes");
      ("Cache-Control","no-cache")
    ]@hd
  in
  match proto with
  |None ->
      Empty_sender.create ~headers:hd2 fd
  |Some p -> 
      Empty_sender.create ~headers:hd2 ~proto:p fd

(** fonction that sends something
* code is the code of the http answer
* keep_alive is a boolean value that set the field Connection
* cookie is a string value that give a value to the session cookie
* page is the page to send
* xhtml_sender is the used sender*)
let send_generic 
    ?code ?keep_alive ?cookie ?last_modified
    ?path ?location ?(header=[]) page sender 
    (send : ?mode:Xhtml_sender.H.http_mode ->
      ?proto:string ->
      ?headers:(string * string) list ->
      ?meth:'c ->
      ?url:string ->
      ?code:int -> 
      ?content:'a -> 'b -> unit Lwt.t) =
  (*ajout des option spécifique à la page*)
  let date = Netdate.mk_mail_date ~zone:0 (Unix.time ()) in
  (*il faut récupérer la date de dernière modification *)
  let last_mod =
    match last_modified with
    |None -> date
    |Some l  -> Netdate.mk_mail_date ~zone:0 l
  in
  let hds =
      ("Date",date)::
      ("Last-Modified",last_mod)::header
  in
  let hds2 =
    match cookie with
    |None -> hds
    |Some c -> ("Set-Cookie",("session="^c^
			      (match path with 
				Some s -> ("; path="^s) 
			      | None -> "")))::hds
  in
  let hds3 =
    match keep_alive with
    |None ->  hds2
    |Some true  -> ("Connection","Keep-Alive")::hds2
    |Some false -> ("Connection","Close")::hds2
  in
  let hds4 =
    match location with
    |None ->  hds3
    |Some l  -> ("Location",l)::hds3
  in
  match code with
    |None -> send ~code:200 ~content:page ~headers:hds4 sender
    |Some c -> send ~code:c ~content:page ~headers:hds4 sender
  
(** fonction that sends a xhtml page
* code is the code of the http answer
* keep_alive is a boolean value that set the field Connection
* cookie is a string value that give a value to the session cookie
* path is the path associated to the cookie
* page is the page to send
* xhtml_sender is the used sender*)
let send_page ?code ?keep_alive ?cookie ?path 
    ?last_modified ?location page xhtml_sender =
  send_generic 
    ?code ?keep_alive ?cookie ?path ?location ?last_modified
    page xhtml_sender Xhtml_sender.send
  
(** fonction that sends an empty answer
* code is the code of the http answer
* keep_alive is a boolean value that set the field Connection
* cookie is a string value that give a value to the session cookie
* page is the page to send
* empty_sender is the used sender *)
let send_empty ?code ?keep_alive ?cookie 
    ?path ?location ?last_modified empty_sender =
  send_generic  ?last_modified
    ?code ?keep_alive ?cookie ?path ?location () empty_sender Empty_sender.send

  

(** sends an error page that fit the error number *)
let send_error ?(http_exception) ?(error_num=500) xhtml_sender =
  let (error_code,error_msg) =
    (
      match http_exception with
      |Some (Http_error.Http_exception (code,msgs) )->
          (
            let error_num =
              match code with
              |Some c -> c
              |None -> 500
            in
            let msg =
              Http_error.string_of_http_exception
              (Http_error.Http_exception(code,msgs))
            in (error_num,msg)
          )
          
        |_ ->
           let  error_mes = Http_error.expl_of_code error_num in
           (error_num,error_mes)
     ) in
  let str_code = string_of_int error_code in
        let err_page =
          <<
          <html>
	  <body>
          <h1> Error $str:str_code$ </h1> 
          <p>$str:error_msg$</p>
	  </body>
          </html>
          >>
  in
  send_page ~code:error_code err_page xhtml_sender
(*Xhtml_sender.send ~code:error_code (*~content:err_page*) xhtml_sender*)

(** this fonction create a sender that send http_frame with fiel content*)
let create_file_sender ?server_name ?proto fd =
  let hd =
    match server_name with
    |None -> []
    |Some s -> [("Server",s)]
  in
  let hd2 =
    [
      ("Accept-Ranges","bytes");
      ("Cache-Control","no-cache")
    ]@hd
  in
  match proto with
  |None -> 
      File_sender.create ~headers:hd2 fd
  |Some p ->
      File_sender.create ~headers:hd2 ~proto:p fd

(* send a file in an HTTP frame*)
let content_type_from_file_name filename =
  (* Il faudrait un parseur du fichier mime.types d'Apache *)
  let extens = 
    try 
      let pos = (String.rindex filename '.') in 
      String.sub filename 
	(pos+1)
	((String.length filename) - pos - 1)
    with _ -> filename 
  in
    match extens with
	"css" -> "text/css"
      | "html" -> "text/html"
      | "xhtml" -> "application/xhtml+xml"
      | "jpg" | "jpeg" -> "image/jpeg"
      | "js" -> "application/x-javascript"
      | "tar" -> "application/x-tar"
      | _ -> "unknown"

let send_file ?code ?keep_alive ?cookie ?path
    ?last_modified ?location file file_sender =
  send_generic 
    ?code ?keep_alive ?cookie ?path ?location ?last_modified
    ~header:[("Content-Type",content_type_from_file_name file)]
    file file_sender File_sender.send

  
