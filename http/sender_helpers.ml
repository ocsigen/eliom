(* Ocsigen
 * http://www.ocsigen.org
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

let cookiename = "ocsigensession"

let add_css (a : 'a) : 'a = 
    let css = 
      XHTML.M.toelt 
	(XHTML.M.style ~contenttype:"text/css"
	   [XHTML.M.pcdata "\n.inline {display: inline}\n.nodisplay {display: none}\n"])
    in
    let rec aux = function
    | (XML.Element ("head",al,el))::l -> (XML.Element ("head",al,css::el))::l
    | (XML.BlockElement ("head",al,el))::l -> 
	(XML.BlockElement ("head",al,css::el))::l
    | (XML.SemiBlockElement ("head",al,el))::l -> 
	(XML.SemiBlockElement ("head",al,css::el))::l
    | (XML.Node ("head",al,el))::l -> (XML.Node ("head",al,css::el))::l
    | e::l -> e::(aux l)
    | [] -> []
    in
    XHTML.M.tot
      (match XHTML.M.toelt a with
      | XML.Element ("html",al,el) -> XML.Element ("html",al,aux el) 
      | XML.BlockElement ("html",al,el) -> XML.BlockElement ("html",al,aux el) 
      | XML.SemiBlockElement ("html",al,el) -> 
	  XML.SemiBlockElement ("html",al,aux el)
      | XML.Node ("html",al,el) -> XML.Node ("html",al,aux el)
      | e -> e)

(** this module instantiate the HTTP_CONTENT signature for an Xhtml content*)
module Xhtml_content =
  struct
    type t = [ `Html ] XHTML.M.elt
    let stream_of_content c = 
      let x = (XHTML.M.ocsigen_print (add_css c)) in
      let md5 = Digest.to_hex (Digest.string x) in
      	 Lwt.return (String.length x, md5, (Cont (x, (fun () -> Finished))))
    (*il n'y a pas encore de parser pour ce type*)
    let content_of_string s = assert false
  end

module Text_content =
  struct
    type t = string
    let stream_of_content c =
    let md5 = Digest.to_hex (Digest.string c) in
      Lwt.return (String.length c, md5, Cont (c, (fun () -> Finished)))
    let content_of_string s = Lwt.return s
  end

module Empty_content =
  struct
    type t = unit
    let stream_of_content c = Lwt.return (0, "same", Cont("",(fun () -> Finished)))
    let content_of_string s = Lwt.return ()
  end

(** this module instanciate the HTTP_CONTENT signature for the files*)
module File_content =
  struct
    type t = string (*nom du fichier*)
    let read_file ?(buffer_size=512) fd =
	Messages.debug ("start reading ");
  	let buf = String.create buffer_size in
        let rec read_aux () =
		let lu = Unix.read fd buf 0 buffer_size in
         	   if lu = 0 then  begin
	             Unix.close fd;
		     Messages.debug ("Finished ");
		     Finished
		   end
		   else begin 
		   	if lu = buffer_size
		        then Cont (buf, (fun () -> read_aux ()))
			else Cont ((String.sub buf 0 lu), (fun () -> read_aux ()))
			end
	in read_aux ()			 
						      
    let stream_of_content c  =
      (*ouverture du fichier*)
      let fd = Unix.openfile c [Unix.O_RDONLY;Unix.O_NONBLOCK] 0o666 in
      let st = Unix.stat c in 
      let etag = Printf.sprintf "%x-%x-%f" st.Unix.st_size
                        st.Unix.st_ino st.Unix.st_mtime in    	
      	Lwt.return (st.Unix.st_size, etag, read_file fd)	
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

(** this module is a Http_frame with text content*)
module Text_http_frame = FHttp_frame (Text_content)

(** this module is a sender that send Http_frame with text content*)
module Text_sender = FHttp_sender(Text_content)

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
      ("Accept-Ranges","none");
      ("Cache-Control","no-cache");
      ("Expires", "0");
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
      ("Accept-Ranges","none");
      ("Cache-Control","no-cache")
    ]@hd
  in
  match proto with
  |None ->
      Empty_sender.create ~headers:hd2 fd
  |Some p -> 
      Empty_sender.create ~headers:hd2 ~proto:p fd

let gmtdate d =  
	let x = Netdate.mk_mail_date ~zone:0 d in try
	let ind_plus =  String.index x '+' in  
	String.set x ind_plus 'G';
	String.set x (ind_plus + 1) 'M';
	String.set x (ind_plus + 2) 'T';
	String.sub x 0 (ind_plus + 3)
	with _ -> Messages.debug "no +"; x
(** fonction that sends something
* code is the code of the http answer
* keep_alive is a boolean value that set the field Connection
* cookie is a string value that give a value to the session cookie
* page is the page to send
* xhtml_sender is the used sender*)
let send_generic 
    ?code ~keep_alive ?cookie ?last_modified
    ?path ?location ?(header=[]) ?head ~content sender 
    (send : ?mode:Xhtml_sender.H.http_mode ->
      ?proto:string ->
      ?headers:(string * string) list ->
      ?meth:'c ->
      ?url:string ->
      ?code:int -> 
      ?content:'a -> ?head:bool -> 'b -> unit Lwt.t) =
  (*ajout des option spécifique à la page*)
  let date = gmtdate (Unix.time ()) in
  (*il faut récupérer la date de dernière modification *)
  let last_mod =
    match last_modified with
    |None -> date
    |Some l  -> gmtdate l
  in
  let hds =
      ("Date",date)::
      ("Last-Modified",last_mod)::header
  in
  let hds2 =
    match cookie with
    |None -> hds
    |Some c -> ("Set-Cookie",(cookiename^"="^c^
			      (match path with 
				Some s -> ("; path="^s) 
			      | None -> "")))::hds
  in
  let hds3 =
    if keep_alive
    then ("Connection","Keep-Alive")::hds2 (* obsolete? *)
    else ("Connection","Close")::hds2
  in
  let hds4 =
    match location with
    |None ->  hds3
    |Some l -> ("Location",l)::hds3
  in
  match code with
    |None -> send ~code:200 ~content ~headers:hds4 ?head sender
    |Some c -> send ~code:c ~content ~headers:hds4 ?head sender


type create_sender_type = ?server_name:string ->
    ?proto:string -> Lwt_unix.descr -> Http_com.sender_type

type send_page_type =
    ?code:int ->
      keep_alive:bool ->
	?cookie:string ->
	  ?path:string ->
	    ?last_modified:float ->
	      ?location:string -> 
	        ?head:bool ->Http_com.sender_type -> unit Lwt.t
  
(** fonction that sends a xhtml page
 * code is the code of the http answer
 * keep_alive is a boolean value that set the field Connection
 * cookie is a string value that give a value to the session cookie
 * path is the path associated to the cookie
 * page is the page to send
 * xhtml_sender is the sender to be used *)
let send_xhtml_page ~content ?code ~keep_alive ?cookie ?path 
    ?last_modified ?location ?head xhtml_sender =
  send_generic 
    ?code ~keep_alive ?cookie ?path ?location ?last_modified
    ~content ?head xhtml_sender Xhtml_sender.send
  
(** fonction that sends an empty answer
 * code is the code of the http answer
 * keep_alive is a boolean value that set the field Connection
 * cookie is a string value that give a value to the session cookie
 * page is the page to send
 * empty_sender is the used sender *)
let send_empty ?code ~keep_alive ?cookie 
    ?path ?location ?last_modified ?head empty_sender =
  send_generic  ?last_modified
    ?code ~keep_alive ?cookie ?path ?location ~content:() 
    ?head empty_sender Empty_sender.send

let send_text_page ~content ?code ~keep_alive ?cookie ?path 
    ?last_modified ?location ?head xhtml_sender =
  send_generic 
    ?code ~keep_alive ?cookie ?path ?location ?last_modified
    ~content ?head xhtml_sender Text_sender.send
  
  

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
           let error_mes = Http_error.expl_of_code error_num in
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
  send_xhtml_page ~code:error_code ~content:err_page xhtml_sender

(** this fonction create a sender that send http_frame with fiel content*)
let create_file_sender ?server_name ?proto fd =
  let hd =
    match server_name with
    |None -> []
    |Some s -> [("Server",s)]
  in
  let hd2 =
    [
      ("Accept-Ranges","none");
      ("Cache-Control","no-cache")
    ]@hd
  in
  match proto with
  |None -> 
      File_sender.create ~headers:hd2 fd
  |Some p ->
      File_sender.create ~headers:hd2 ~proto:p fd

let mimeht = Hashtbl.create 600

let parse_mime_types filename =
  let rec read_and_split in_ch = try
    let line = input_line in_ch in
    let line_upto = try 
    	let upto = String.index line '#' in 
	String.sub line 0 upto 
    with Not_found -> line in
    let strlist = Netstring_pcre.split (Netstring_pcre.regexp "\\s+") line_upto in
    match  List.length strlist with
    0 | 1 -> read_and_split in_ch
    | _ -> let make_pair = (fun h -> Hashtbl.add mimeht h (List.hd strlist)) in
    	   List.iter make_pair (List.tl strlist);
    	   read_and_split in_ch
    with End_of_file -> ()
  in
  try
    let in_ch =  open_in filename in
    read_and_split in_ch;
    close_in in_ch
  with _ -> ()

let parsed = ref false 

let rec affiche_mime () =
    Hashtbl.iter (fun f s -> Messages.debug (f^" "^s)) mimeht
    
(* send a file in an HTTP frame*)
let content_type_from_file_name filename =
  if not !parsed then parse_mime_types (Ocsiconfig.get_mimefile ());
  try 
    let pos = (String.rindex filename '.') in 
    let extens = 
      String.sub filename 
	(pos+1)
	((String.length filename) - pos - 1)
    in Hashtbl.find mimeht extens
  with _ -> "unknown" 

let send_file ?code ~keep_alive ?cookie ?path
    ?last_modified ?location ?head file file_sender =
  send_generic 
    ?code ~keep_alive ?cookie ?path ?location ?last_modified
    ~header:[("Content-Type",content_type_from_file_name file)]
    ~content:file ?head file_sender File_sender.send

  
