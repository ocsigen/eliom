(* Ocsigen
 * http://www.ocsigen.org
 * sender_helpers.ml Copyright (C) 2005 Denis Berthod
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
(** This module provides predefined "senders" for usual types of pages to be
  sent by the server: xhtml, files, ... *)

open Http_frame
open Http_com
open Lwt
open Ocsistream
open XHTML.M


type mycookieslist = 
  (string list option * float option * (string * string) list) list
(** The cookies I want to set *)

type full_stream =
    (int64 option * Http_frame.etag * Ocsistream.stream * (unit -> unit Lwt.t))
(** The type of streams to be send by the server.
   The [int64 option] is the content-length. 
   [None] means Transfer-encoding: chunked
   The last function is the termination function
   (for ex closing a file if needed), 
   that will be called after the stream has been fully read. 
   Your new termination function should probably call the former one. *)

type stream_filter_type =
    string option (* content-type *) -> full_stream -> full_stream Lwt.t
(** A function to transform a stream into another one. *)



let id x = x

let add_css (a : 'a) : 'a = 
    let css = 
      XHTML.M.toelt 
        (XHTML.M.style ~contenttype:"text/css"
           [XHTML.M.pcdata "\n.eliom_inline {display: inline}\n.eliom_nodisplay {display: none}\n"])
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

    let get_etag_aux x =
      Digest.to_hex (Digest.string x)

    let get_etag c =
      let x = (XHTML.M.ocsigen_print (add_css c)) in
      get_etag_aux x

    let stream_of_content c = 
      let x = (XHTML.M.ocsigen_print (add_css c)) in
      let md5 = get_etag_aux x in
      Lwt.return (Some (Int64.of_int (String.length x)), 
                  md5, 
                  (new_stream x 
                     (fun () -> Lwt.return (empty_stream None))),
                  return
                 )

    (*il n'y a pas encore de parser pour ce type*)
    let content_of_stream s = assert false
  end

module Text_content =
  struct
    type t = string

    let get_etag x =
      Digest.to_hex (Digest.string x)

    let stream_of_content c =
      let md5 = get_etag c in
      Lwt.return (Some (Int64.of_int (String.length c)), 
                  md5, 
                  new_stream c (fun () -> Lwt.return (empty_stream None)),
                  return)

    let content_of_stream = string_of_stream
  end

exception Stream_already_read

module Stream_content =
  (* Use to receive any type of data, before knowing the content-type,
     or to send data from a stream
   *)
  struct
    type t = unit -> stream

    let get_etag c = ""

    let stream_of_content c = 
      Lwt.return (None, get_etag c, c (), return)

    let content_of_stream s = 
      Lwt.return
        (let already_read = ref false in
        fun () -> 
          if !already_read
          then raise Stream_already_read
          else begin
            already_read := true;
            s
          end)
  end


module Empty_content =
  struct
    type t = unit

    let get_etag c = "empty"

    let stream_of_content c = 
      Lwt.return (Some (Int64.of_int 0), (get_etag ()), empty_stream None, 
                  return)

    let content_of_stream s = Lwt.return ()
  end

(** this module instanciate the HTTP_CONTENT signature for the files*)
module File_content =
  struct
    type t = string (* nom du fichier *)

    let read_file ?buffer_size fd =
      let buffer_size = match buffer_size with
      | None -> Ocsiconfig.get_filebuffersize ()
      | Some s -> s
      in
      Messages.debug ("start reading file (file opened)");
      let buf = String.create buffer_size in
      let rec read_aux () =
        Lwt_unix.yield () >>=
        (fun () ->
          let lu = Unix.read fd buf 0 buffer_size in (
            if lu = 0 then  
              Lwt.return (empty_stream None)
            else begin 
              if lu = buffer_size
              then Lwt.return (new_stream buf read_aux)
              else Lwt.return (new_stream (String.sub buf 0 lu) read_aux)
            end))
      in read_aux ()                         

    let get_etag_aux st =
      Printf.sprintf "%Lx-%x-%f" st.Unix.LargeFile.st_size
        st.Unix.LargeFile.st_ino st.Unix.LargeFile.st_mtime
        
    let get_etag f =
      let st = Unix.LargeFile.stat f in 
      get_etag_aux st

    let stream_of_content c  =
      (* open the file *)
      let fd = Unix.openfile c [Unix.O_RDONLY;Unix.O_NONBLOCK] 0o666 in
      let st = Unix.LargeFile.stat c in 
      let etag = get_etag_aux st in
      read_file fd >>=
      (fun r ->
        Lwt.return (
          Some st.Unix.LargeFile.st_size, 
          etag, r, 
          fun () ->     
            Messages.debug ("closing file"); 
            Unix.close fd;
            return ()))
        
    let content_of_stream s = assert false
      
  end

(** this module is a sender that send Http_frame with empty content *)
module Empty_sender = FHttp_sender(Empty_content)

(** this module is a receiver that receives Http_frame with empty content *)
module Empty_receiver = FHttp_receiver(Empty_content)

(** this module is a sender that send Http_frame with Xhtml content *)
module Xhtml_sender = FHttp_sender(Xhtml_content)

(** this module is a sender that send Http_frame with text content *)
module Text_sender = FHttp_sender(Text_content)

(** this module is a receiver that receives Http_frame with text content *)
module Text_receiver = FHttp_receiver (Text_content)

(** this module is a Http_frame with stream content *)
module Stream_http_frame = FHttp_frame

(** this module is a receiver that receives Http_frame with stream content
   (any text stream, when we don't know the type) *)
module Stream_receiver = FHttp_receiver (Stream_content)

(** creates a sender for any stream *)
module Stream_sender = FHttp_sender(Stream_content)

(** this module is a sender that send Http_frame with file content *)
module File_sender = FHttp_sender(File_content)


let (<<) h (n, v) = Http_headers.replace n v h
let (<<?) h (n, v) = Http_headers.replace_opt n v h

(** Headers for dynamic pages *)
let dyn_headers =
  Http_headers.empty
  << (Http_headers.cache_control,"no-cache")
  << (Http_headers.expires, "0")

let gmtdate d =  
        let x = Netdate.mk_mail_date ~zone:0 d in try
(*XXX !!!*)
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
 * xhtml_sender is the used sender *)
let send_generic
    (send : 
       ?filter:stream_filter_type ->
         unit Lwt.t ->
           clientproto:Http_frame.Http_header.proto ->
             ?etag:etag ->
               mode:Http_frame.Http_header.http_mode ->
                 ?proto:Http_frame.Http_header.proto ->
                   ?headers:Http_headers.t ->
                     ?contenttype: string ->
                       ?content:'a ->
                         head:bool -> 
                           Http_com.sender_type -> 
                             res Lwt.t)
    ?contenttype
    ~content
    ?filter
    ?(cookies=[])
    waiter
    ~clientproto
    ?code
    ?etag
    ~keep_alive
    ?last_modified 
    ?location
    ~head
    ?(headers = Http_headers.empty)
    ?charset
    sender
    =

(*XXX Maybe we can compute this only at most once a second*)
  (* ajout des options spécifiques à la page *)
  let date = gmtdate (Unix.time ()) in

  let headers =
    headers
    <<?
    (* il faut récupérer la date de dernière modification *)
    (Http_headers.last_modified,
     match last_modified with
       None    -> None (* We do not put last modified for dynamically
                          generated pages, otherwise it is not possible
                          to cache them.  Without Last-Modified, ETag is
                          taken into account by proxies/browsers *)
     | Some l  -> Some (gmtdate l))
    <<
    (Http_headers.date, date)
  in
  let mkcook path exp (name, c) =
    (Http_headers.set_cookie,
     Format.sprintf "%s=%s%s%s" name c
       (match path with
        | Some s -> "; path=/" ^ Ocsimisc.string_of_url_path s
        | None   -> "")
       (match exp with
        | Some s -> "; expires=" ^
                    Netdate.format
                      "%a, %d-%b-%Y %H:%M:%S GMT"
                      (Netdate.create s)
        | None   -> ""))
  in
  let mkcookl hds (path, exp, cl) =
    List.fold_left (fun h c -> h << mkcook path exp c) hds cl
  in
  let headers =
    List.fold_left mkcookl headers cookies
    <<
    (Http_headers.connection,
     if keep_alive then "keep-alive" (* obsolete? *) else "close")
    <<?
    (Http_headers.location, location)
    <<?
    (Http_headers.etag,
     match etag with
    | None   ->  None
    | Some l ->  Some (Format.sprintf "\"%s\"" l))
                 (*XXX Is it the right place to perform quoting?*)
    <<?
    (Http_headers.content_type,
     match contenttype with
     | None   -> None
     | Some s ->
         match String.sub s 0 4, charset with
         | "text", Some c -> Some (Format.sprintf "%s; charset=%s" s c)
         | _              -> contenttype)
  in
  let mode =
    Http_header.Answer
      (match code with
      | None -> 200
      | Some c -> c)
  in
(*XXX Shouldn't this function now about the keep_alive parameter? *)
  send ?filter
    waiter ~clientproto ?etag
    ~mode
    ?contenttype ~content
    ~headers ~head sender
    

type send_page_type =
    (* no content
       no content-type *)
    ?filter:stream_filter_type ->
    ?cookies:mycookieslist ->
    unit Lwt.t ->
    clientproto:Http_frame.Http_header.proto ->
    ?code:int ->
    ?etag:Http_frame.etag ->
    keep_alive:bool ->
    ?last_modified:float ->
    ?location:string ->
    head:bool ->
    ?headers:Http_headers.t ->
    ?charset:string ->
    Http_com.sender_type ->
    Http_com.res Lwt.t

(** fonction that sends a xhtml page
 * code is the code of the http answer
 * keep_alive is a boolean value that set the field Connection
 * cookies is a list of pairs: 
 * string value (the path) and list of string pairs (name, value)   
 * page is the page to send
 * xhtml_sender is the sender to be used *)
let send_xhtml_page
(*    ~content
    ?cookies
    waiter
    ~clientproto
    ?code
    ?etag
    ~keep_alive
    ?last_modified
    ?location
    head
    ?headers
    ?charset
    sender *)
    =
  send_generic
    Xhtml_sender.send
    ~contenttype:"text/html"
(*    ~content 
    ?cookies
    waiter
    ~clientproto
    ?code
    ?etag
    ~keep_alive
    ?last_modified
    ?location
    head
    ?headers
    ?charset
    sender *)
  

(** fonction that sends an empty answer *)
let send_empty =
  send_generic Empty_sender.send ?contenttype:None

(** fonction that sends a text answer *)
let send_text_page =
  send_generic Text_sender.send
  
(** fonction that uses a stream to send the answer step by step *)
let send_stream_page =
  send_generic Stream_sender.send  
  
  

(** sends an error page that fit the error number *)
let send_error
    ?http_exception
    ?filter
    ?cookies
    waiter
    ~clientproto
    ?(code=500)
    ?etag
    ~keep_alive
    ?last_modified 
    ?location
    ~head
    ?(headers = Http_headers.empty)
    ?charset
    sender
    =
  let (error_code,error_msg) =
    (
      match http_exception with
      | Some (Http_error.Http_exception (errcode,msgs) )->
          (
           let error_num =
             match errcode with
             | Some c -> c
             | None -> 500
           in
           let msg =
             Http_error.string_of_http_exception
               (Http_error.Http_exception(errcode, msgs))
           in (error_num,msg)
          )
            
      | _ ->
          let error_mes = Http_error.expl_of_code code in
          (code, error_mes)
    ) in
  let str_code = string_of_int error_code in
  let err_page =
    html
      (XHTML.M.head (title (pcdata "Error")) [])
      (body [h1 [pcdata str_code];
             p [pcdata error_msg]])
  in
  send_xhtml_page
    ~content:err_page
    ?filter
    waiter
    ~clientproto
    ~code:error_code
    ~headers:dyn_headers
    ?etag
    ~keep_alive
    ?last_modified 
    ?location
    ~head
    ?charset
    sender



let mimeht = Hashtbl.create 600

let parse_mime_types filename =
  let rec read_and_split in_ch = try
    let line = input_line in_ch in
    let line_upto = try 
            let upto = String.index line '#' in 
        String.sub line 0 upto 
    with Not_found -> line in
    let strlist = 
      Netstring_pcre.split (Netstring_pcre.regexp "\\s+") line_upto in
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


let rec affiche_mime () =
    Hashtbl.iter (fun f s -> Messages.debug (f^" "^s)) mimeht
    
(* send a file in an HTTP frame*)
let content_type_from_file_name =
  let parsed = ref false in
  fun filename ->
    if not !parsed
    then (parse_mime_types (Ocsiconfig.get_mimefile ());
          parsed := true);
    try 
      let pos = (String.rindex filename '.') in 
      let extens = 
        String.sub filename 
          (pos+1)
          ((String.length filename) - pos - 1)
      in Hashtbl.find mimeht extens
    with _ -> "unknown" 

let send_file ~content:file ?filter ?cookies waiter ~clientproto
    ?code ?etag ~keep_alive
    ?last_modified ?location ~head ?headers ?charset file_sender =
  Lwt_unix.yield () >>=
  (fun () ->
    send_generic File_sender.send
      ~contenttype:(content_type_from_file_name file)
      ~content:file
      ?filter
      ?cookies waiter
      ~clientproto
      ?code ?etag ~keep_alive
      ?last_modified ?location ~head ?headers ?charset file_sender)

  
