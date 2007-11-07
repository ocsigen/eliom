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


(*****************************************************************************)
(** this module instantiate the HTTP_CONTENT signature for an Xhtml content*)
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


module Xhtml_content =
  struct
    type t = [ `Html ] XHTML.M.elt

    let get_etag_aux x =
      Some (Digest.to_hex (Digest.string x))

    let get_etag c =
      let x = XHTML.M.ocsigen_print (add_css c) in
      get_etag_aux x

    let result_of_content c = 
      let x = XHTML.M.ocsigen_print (add_css c) in
      let md5 = get_etag_aux x in
      Lwt.return 
        {default_result with
         res_content_length = Some (Int64.of_int (String.length x));
         res_content_type = Some "text/html";
         res_etag = md5;
         res_headers= Http_headers.dyn_headers;
         res_stream = 
         Ocsistream.make 
           (fun () -> Ocsistream.cont x
               (fun () -> Ocsistream.empty None))
       }
           
  end

(*****************************************************************************)
module Text_content =
  struct
    type t = string (* content *) * string (* content-type *)

    let get_etag (x, _) =
      Some (Digest.to_hex (Digest.string x))

    let result_of_content ((c, ct) as content) =
      let md5 = get_etag content in
      Lwt.return
        {default_result with
         res_content_length = Some (Int64.of_int (String.length c));
         res_etag = md5;
         res_content_type = Some ct;
         res_headers= Http_headers.dyn_headers;
         res_stream = 
         Ocsistream.make
           (fun () -> Ocsistream.cont c (fun () -> Ocsistream.empty None))

       }

  end

(*****************************************************************************)
module Stream_content =
  (* Used to send data from a stream
   *)
  struct
    type t = string Ocsistream.t

    let get_etag c = None

    let result_of_content c =
      Lwt.return
        {default_result with
         res_content_length = None;
         res_headers= Http_headers.dyn_headers;
         res_stream = c}

  end

(*****************************************************************************)
module Streamlist_content =
  (* Used to send data from streams *)
  struct
    type t = (unit -> string Ocsistream.t Lwt.t) list
          * string (* content-type *)

    let get_etag c = None

    let result_of_content (c, ct) =
      let finalizer = ref (fun () -> Lwt.return ()) in
      let finalize () =
        let f = !finalizer in
        finalizer := (fun () -> Lwt.return ());
        f ()
      in
      let rec next stream l =
        Lwt.try_bind (fun () -> Ocsistream.next stream)
          (fun s ->
             match s with
               Ocsistream.Finished None ->
                 finalize () >>= fun () ->
                 next_stream l
             | Ocsistream.Finished (Some stream) ->
                 next stream l
             | Ocsistream.Cont (v, stream) ->
                 Ocsistream.cont v (fun () -> next stream l))
          (function Interrupted e | e ->
(*XXX string_of_exn should know how to print "Interrupted _" exceptions*)
             exnhandler e l)
      and next_stream l =
        match l with
          [] -> Ocsistream.empty None
        | f :: l ->
            Lwt.try_bind f
              (fun stream ->
                 finalizer := (fun () -> Ocsistream.finalize stream);
                 next (Ocsistream.get stream) l)
              (fun e -> exnhandler e l)
      and exnhandler e l =
        Messages.warning
          ("Error while reading stream list: " ^ Ocsimisc.string_of_exn e);
        finalize () >>= fun () ->
        next_stream l
      in
      Lwt.return
        {default_result with
         res_content_length = None;
         res_etag = get_etag c;
         res_stream = Ocsistream.make ~finalize (fun () -> next_stream c);
         res_headers= Http_headers.dyn_headers;
         res_content_type = Some ct}

  end


(*****************************************************************************)
module Empty_content =
  struct
    type t = unit

    let get_etag c = None

    let result_of_content c = Lwt.return empty_result

  end

(*****************************************************************************)
(* Files *)
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
    | 0 | 1 -> read_and_split in_ch
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
  Hashtbl.iter (fun f s -> Messages.debug (fun () -> f^" "^s)) mimeht
    
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

(** this module instanciate the HTTP_CONTENT signature for files *)
module File_content =
  struct
    type t = string (* nom du fichier *)

    let read_file ?buffer_size fd =
      let buffer_size = match buffer_size with
      | None -> Ocsiconfig.get_filebuffersize ()
      | Some s -> s
      in
      Messages.debug2 "start reading file (file opened)";
      let buf = String.create buffer_size in
      let rec read_aux () =
          let lu = Unix.read fd buf 0 buffer_size in (
            if lu = 0 then  
              Ocsistream.empty None
            else begin 
              if lu = buffer_size
              then Ocsistream.cont buf read_aux
              else Ocsistream.cont (String.sub buf 0 lu) read_aux
            end)
      in read_aux

    let get_etag_aux st =
      Some (Printf.sprintf "%Lx-%x-%f" st.Unix.LargeFile.st_size
              st.Unix.LargeFile.st_ino st.Unix.LargeFile.st_mtime)
        
    let get_etag f =
      let st = Unix.LargeFile.stat f in 
      get_etag_aux st

    let result_of_content c  =
      (* open the file *)
      let fd = Unix.openfile c [Unix.O_RDONLY;Unix.O_NONBLOCK] 0o666 in
      let st = Unix.LargeFile.stat c in 
      let etag = get_etag_aux st in
      let stream = read_file fd in
      Lwt.return
        {default_result with
         res_content_length = Some st.Unix.LargeFile.st_size;
         res_content_type = Some (content_type_from_file_name c);
         res_lastmodified = Some st.Unix.LargeFile.st_mtime;
         res_etag = etag;
         res_stream = 
         Ocsistream.make
           ~finalize:
           (fun () ->
             Messages.debug2 "closing file";
             Unix.close fd;
             return ())
           stream
       }
        

  end

(*****************************************************************************)
(* directory listing - by Gabriel Kerneis *)

(** this module instanciate the HTTP_CONTENT signature for directories *)
module Directory_content =
  struct
    type t = string (* dir name *) * string list (* corresponding URL path *)

    let get_etag_aux st =
      Some (Printf.sprintf "%Lx-%x-%f" st.Unix.LargeFile.st_size
              st.Unix.LargeFile.st_ino st.Unix.LargeFile.st_mtime)
        
    let get_etag (f, _) =
      let st = Unix.LargeFile.stat f in 
      get_etag_aux st

    let date fl = 
      let t = Unix.gmtime fl in
      Printf.sprintf 
        "%02d-%02d-%04d %02d:%02d:%02d" 
        t.Unix.tm_mday 
        (t.Unix.tm_mon + 1)
        (1900 + t.Unix.tm_year)
        t.Unix.tm_hour
        t.Unix.tm_min
        t.Unix.tm_sec 


    let image_found fich =
      if fich="README" || fich="README.Debian"
      then "/ocsigenstuff/readme.png"
      else
        let reg=Netstring_pcre.regexp "([^//.]*)(.*)"
        in match Netstring_pcre.global_replace reg "$2" fich with
        | ".jpeg" | ".jpg" | ".gif" | ".tif"
        | ".png" -> "/ocsigenstuff/image.png"
        | ".ps" -> "/ocsigenstuff/postscript.png"
        | ".pdf" -> "/ocsigenstuff/pdf.png"
        | ".html" | ".htm"
        | ".php" -> "/ocsigenstuff/html.png"
        | ".mp3"
        | ".wma" -> "/ocsigenstuff/sound.png"
        | ".c" -> "/ocsigenstuff/source_c.png"
        | ".java" -> "/ocsigenstuff/source_java.png"
        | ".pl" -> "/ocsigenstuff/source_pl.png"
        | ".py" -> "/ocsigenstuff/source_py.png"
        | ".iso" | ".mds" | ".mdf" | ".cue" | ".nrg"
        | ".cdd" -> "/ocsigenstuff/cdimage.png"
        | ".deb" -> "/ocsigenstuff/deb.png"
        | ".dvi" -> "/ocsigenstuff/dvi.png"
        | ".rpm" -> "/ocsigenstuff/rpm.png"
        | ".tar" | ".rar" -> "/ocsigenstuff/tar.png"
        | ".gz" | ".tar.gz" | ".tgz" | ".zip"
        | ".jar"  -> "/ocsigenstuff/tgz.png"
        | ".tex" -> "/ocsigenstuff/tex.png"
        | ".avi" | ".mov" -> "/ocsigenstuff/video.png"
        | ".txt" -> "/ocsigenstuff/txt.png"
        | _ -> "/ocsigenstuff/unknown.png"



    let directory filename =
      let dir = Unix.opendir filename in
      let rec aux d =
        try
          let f = Unix.readdir dir in
          let stat = Unix.LargeFile.stat (filename^f) in
          if (stat.Unix.LargeFile.st_kind = Unix.S_DIR && f <> "." && f <> "..")
          then 
	    (
	     `Dir, f, (
	     "<tr>\n"^
	     "<td class=\"img\"><img src=\"/ocsigenstuff/folder_open.png\" alt=\"\" /></td>\n"^
	     "<td><a href=\""^f^"\">"^f^"</a></td>\n"^
	     "<td>"^(Int64.to_string stat.Unix.LargeFile.st_size)^"</td>\n"^
	     "<td>"^(date stat.Unix.LargeFile.st_mtime)^"</td>\n"^
	     "</tr>\n")
            )::aux d
          else
	    if (stat.Unix.LargeFile.st_kind 
                  = Unix.S_REG)
	    then
	      (
	       if f.[(String.length f) - 1] = '~'
	       then aux d
	       else 
	         (
	          `Reg, f,
	          "<tr>\n"^
	          "<td class=\"img\"><img src=\""^image_found f^"\" alt=\"\" /></td>\n"^
	          "<td><a href=\""^f^"\">"^f^"</a></td>\n"^
	          "<td>"^(Int64.to_string stat.Unix.LargeFile.st_size)^"</td>\n"^
	          "<td>"^(date stat.Unix.LargeFile.st_mtime)^"</td>\n"^
	          "</tr>\n"
	         )::aux d
	      )
	    else aux d
        with
	  End_of_file -> Unix.closedir d;[]

      in 
      let trie li =
        List.sort (fun (a1, b1, _) (a2, b2, _) -> match a1, a2 with
	| `Dir, `Dir -> 
	    if b1<b2
	    then 0
	    else 1
	| `Dir, _ -> 0
	| _, `Dir -> 1
	| _, _->
	    if b1<b2
	    then 0
	    else 1) li

      in let rec aux2 = function 
        | [] -> ""
        | (_, _, i)::l -> i^(aux2 l)
      in aux2 (trie (aux dir))



    let result_of_content (filename, path) =
      let stat = Unix.LargeFile.stat filename in 
      let rec back = function
        | [] -> assert false
        | [a] -> "/"
        | [a;""] -> "/"
        | i::j -> "/"^i^(back j)
      in 
      let parent =
        if (path= []) || (path = [""])
        then "/"
        else back path
      in
      let before =
        let st = (Ocsimisc.string_of_url_path path) in
        "<html>\n\
         <head><meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" />\n\
         <link rel=\"stylesheet\" type=\"text/css\" href=\"/ocsigenstuff/style.css\" media=\"screen\" />\n\
         <title>Listing Directory: "^st^"</title>\n</head>\n\
         <body><h1>"^st^"</h1>\n\
         <table summary=\"Contenu du dossier "^st^"\">\n\
         <tr id=\"headers\"><th></th><th>Name</th><th>Size</th>\
         <th>Last modified</th></tr>\
         <tr>\n\
         <td class=\"img\"><img src=\"/ocsigenstuff/back.png\" alt=\"\" /></td>\n\
         <td><a href=\""^parent^"\">Parent Directory</a></td>\n\
         <td>"^(Int64.to_string stat.Unix.LargeFile.st_size)^"</td>\n\
         <td>"^(date stat.Unix.LargeFile.st_mtime)^"</td>\n\
         </tr>\n"
          
      and after=
        "</table>\
         <p id=\"footer\">Ocsigen Webserver</p>\
         </body></html>"
      in
      let c = before^(directory filename)^after in
      let etag = get_etag_aux stat in
      Text_content.result_of_content (c, "text/html") >>= fun r ->
      Lwt.return
        {r with
         res_lastmodified = Some stat.Unix.LargeFile.st_mtime;
         res_etag = etag;
         res_charset= Some "utf-8"
       }
        

  end



(*****************************************************************************)
module Error_content =
(** sends an error page that fit the error number *)
  struct
    type t = int option * exn option

    let get_etag c = None

    let error_page s c =
      XHTML.M.html
        (XHTML.M.head (XHTML.M.title (XHTML.M.pcdata s)) [])
        (XHTML.M.body
           ((XHTML.M.h1 [XHTML.M.pcdata "Error"])::c)
        )

    let result_of_content (code, exn) =
      let code = match code with
      | None -> 500
      | Some c -> c
      in
      let (error_code, error_msg) =
        match exn with
        | Some (Http_error.Http_exception (errcode, msgs) )->
            let msg =
              Http_error.string_of_http_exception
                (Http_error.Http_exception(errcode, msgs))
            in (errcode, msg)
        | _ ->
            let error_mes = Http_error.expl_of_code code in
            (code, error_mes)
      in
      let str_code = string_of_int error_code in
      let err_page = 
        match exn with
        | Some exn when Ocsiconfig.get_debugmode () ->
            error_page
              ("error "^str_code)
              [XHTML.M.p
                 [XHTML.M.pcdata (Ocsimisc.string_of_exn exn);
                  XHTML.M.br ();
                  XHTML.M.em
                    [XHTML.M.pcdata "(Ocsigen running in debug mode)"]
                ]]
        | _ ->
          error_page
            ("error "^str_code)
            []
      in
      Xhtml_content.result_of_content err_page >>= fun r ->
      Lwt.return
          {r with
           res_code = error_code;
           res_charset= Some "utf-8";
           res_headers= Http_headers.dyn_headers;
         }


  end


let send_error 
    ?code
    ?exn
    slot
    ~clientproto
    ?mode
    ?proto
    ~keep_alive
    ~head
    ~sender
    ()
    = 
  Error_content.result_of_content (code, exn) >>= fun r ->
  send 
    slot
    ~clientproto
    ?mode
    ?proto
    ~keep_alive
    ~head
    ~sender
    r
