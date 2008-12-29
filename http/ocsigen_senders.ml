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

open Ocsigen_http_frame
open Ocsigen_http_com
open Lwt
open Ocsigen_stream
open XHTML.M


(*****************************************************************************)
(** this module instantiate the HTTP_CONTENT signature for an Xhtml content*)

module Old_Xhtml_content =
  struct
    type t = [ `Html ] XHTML.M.elt

    type options = unit

    let get_etag_aux x =
      Some (Digest.to_hex (Digest.string x))

    let get_etag c =
      let x = Xhtmlpretty.xhtml_print c in
      get_etag_aux x

    let result_of_content ?(options = ()) c =
      let x = Xhtmlpretty.xhtml_print c in
      let md5 = get_etag_aux x in
      let default_result = default_result () in
      Lwt.return
        {default_result with
         res_content_length = Some (Int64.of_int (String.length x));
         res_content_type = Some "text/html";
         res_etag = md5;
         res_headers= Http_headers.dyn_headers;
         res_stream =
            (Ocsigen_stream.make
               (fun () -> Ocsigen_stream.cont x
                  (fun () -> Ocsigen_stream.empty None)),
             None)
       }

  end

module Xhtml_content_(Xhtmlprinter : sig
                        val xhtml_stream :
                          ?version:[< `HTML_v03_02 | `HTML_v04_01
                          | `XHTML_01_00 | `XHTML_01_01 | `Doctype of string
                              > `XHTML_01_01 ] ->
                          ?width:int ->
                          ?encode:(string -> string) ->
                          ?html_compat:bool ->
                          [ `Html ] XHTML.M.elt -> string Ocsigen_stream.t
                      end) =
  struct
    type t = [ `Html ] XHTML.M.elt

    type options = [ `HTML_v03_02 | `HTML_v04_01 | `XHTML_01_00 | `XHTML_01_01 | `Doctype of string ]

    let get_etag_aux x = None

    let get_etag c = None

    let result_of_content ?(options = `XHTML_01_01) c =
      let x = Xhtmlprinter.xhtml_stream ~version:options c in
      let default_result = default_result () in
      Lwt.return
        {default_result with
         res_content_length = None;
         res_content_type = Some "text/html";
         res_etag = get_etag c;
         res_headers= Http_headers.dyn_headers;
         res_stream = (x, None)
       }

  end

module Xhtml_content = Xhtml_content_(Xhtmlpretty_streams)
module Xhtmlcompact_content = Xhtml_content_(Xhtmlcompact_streams)

(*****************************************************************************)
module Text_content =
  struct
    type t = string (* content *) * string (* content-type *)

    type options = unit

    let get_etag (x, _) =
      Some (Digest.to_hex (Digest.string x))

    let result_of_content ?(options = ()) ((c, ct) as content) =
      let md5 = get_etag content in
      let default_result = default_result () in
      Lwt.return
        {default_result with
         res_content_length = Some (Int64.of_int (String.length c));
         res_etag = md5;
         res_content_type = Some ct;
         res_headers= Http_headers.dyn_headers;
         res_stream =
            (Ocsigen_stream.make
               (fun () -> 
                  Ocsigen_stream.cont c (fun () -> Ocsigen_stream.empty None)),
             None)
               
            }

  end

(*****************************************************************************)
module Stream_content =
  (* Used to send data from a stream *)
  struct
    type t = string Ocsigen_stream.t

    type options = unit

    let get_etag c = None

    let result_of_content ?(options = ()) c =
      let default_result = default_result () in
      Lwt.return
        {default_result with
         res_content_length = None;
         res_headers= Http_headers.dyn_headers;
         res_stream = (c, None)}

  end

(*****************************************************************************)
module Streamlist_content =
  (* Used to send data from streams *)
  struct
    type t = (unit -> string Ocsigen_stream.t Lwt.t) list
          * string (* content-type *)

    type options = unit

    let get_etag c = None

    let result_of_content ?(options = ()) (c, ct) =
      let finalizer = ref (fun () -> Lwt.return ()) in
      let finalize () =
        let f = !finalizer in
        finalizer := (fun () -> Lwt.return ());
        f ()
      in
      let rec next stream l =
        Lwt.try_bind (fun () -> Ocsigen_stream.next stream)
          (fun s ->
             match s with
               Ocsigen_stream.Finished None ->
                 finalize () >>= fun () ->
                 next_stream l
             | Ocsigen_stream.Finished (Some stream) ->
                 next stream l
             | Ocsigen_stream.Cont (v, stream) ->
                 Ocsigen_stream.cont v (fun () -> next stream l))
          (function Interrupted e | e ->
(*XXX string_of_exn should know how to print "Interrupted _" exceptions*)
             exnhandler e l)
      and next_stream l =
        match l with
          [] -> Ocsigen_stream.empty None
        | f :: l ->
            Lwt.try_bind f
              (fun stream ->
                 finalizer := (fun () -> Ocsigen_stream.finalize stream);
                 next (Ocsigen_stream.get stream) l)
              (fun e -> exnhandler e l)
      and exnhandler e l =
        Ocsigen_messages.warning
          ("Error while reading stream list: " ^ Ocsigen_lib.string_of_exn e);
        finalize () >>= fun () ->
        next_stream l
      in
      let default_result = default_result () in
      Lwt.return
        {default_result with
         res_content_length = None;
         res_etag = get_etag c;
         res_stream = 
            (Ocsigen_stream.make ~finalize (fun () -> next_stream c), None);
         res_headers= Http_headers.dyn_headers;
         res_content_type = Some ct}

  end


(*****************************************************************************)
module Empty_content =
  struct
    type t = unit

    type options = unit

    let get_etag c = None

    let result_of_content ?(options = ()) c = Lwt.return (empty_result ())

  end

(*****************************************************************************)
(* Files *)

(** this module instanciate the HTTP_CONTENT signature for files *)
module File_content =
  struct
    type t =
        string (* nom du fichier *) *
        Ocsigen_charset_mime.charset_assoc *
        Ocsigen_charset_mime.mime_assoc

    type options = unit

    let read_file ?buffer_size fd =
      let buffer_size = match buffer_size with
      | None -> Ocsigen_config.get_filebuffersize ()
      | Some s -> s
      in
      Ocsigen_messages.debug2 "start reading file (file opened)";
      let buf = String.create buffer_size in
      let rec read_aux () =
          Lwt_unix.read fd buf 0 buffer_size >>= fun lu ->
          if lu = 0 then
            Ocsigen_stream.empty None
          else begin
            if lu = buffer_size
            then Ocsigen_stream.cont buf read_aux
            else Ocsigen_stream.cont (String.sub buf 0 lu) read_aux
          end
      in read_aux

    let get_etag_aux st =
      Some (Printf.sprintf "%Lx-%x-%f" st.Unix.LargeFile.st_size
              st.Unix.LargeFile.st_ino st.Unix.LargeFile.st_mtime)

    let get_etag (f, _, _) =
      let st = Unix.LargeFile.stat f in
      get_etag_aux st

    let skip fd stream k =
      try
        ignore 
          (Unix.LargeFile.lseek (Lwt_unix.unix_file_descr fd) k Unix.SEEK_CUR);
        Ocsigen_stream.next (Ocsigen_stream.get stream)
      with e -> Lwt.fail e

    let result_of_content ?options (c, charset_assoc, mime_assoc) =
      (* open the file *)
      try
        let fdu = Unix.openfile c [Unix.O_RDONLY;Unix.O_NONBLOCK] 0o666 in
        let fd = Lwt_unix.of_unix_file_descr fdu in
        let st = Unix.LargeFile.fstat fdu in
        let etag = get_etag_aux st in
        let stream = read_file fd in
        let default_result = default_result () in
        Lwt.return
          {default_result with
           res_content_length = Some st.Unix.LargeFile.st_size;
           res_content_type =
              Some (Ocsigen_charset_mime.find_mime_type_file
                      ~mime_assoc ~filename:c);
           res_charset =
              Some (Ocsigen_charset_mime.find_charset_file
                      ~charset_assoc ~filename:c);
           res_lastmodified = Some st.Unix.LargeFile.st_mtime;
           res_etag = etag;
           res_stream =
              (Ocsigen_stream.make
                 ~finalize:
                 (fun () ->
                    Ocsigen_messages.debug2 "closing file";
                    Lwt_unix.close fd;
                    return ())
                 stream,
               Some (skip fd))
         }
      with e -> Ocsigen_messages.debug2 (Printexc.to_string e);  fail e

  end

(*****************************************************************************)
(* directory listing - by Gabriel Kerneis *)

(** this module instanciate the HTTP_CONTENT signature for directories *)
module Directory_content =
  struct
    type t = string (* dir name *) * string list (* corresponding URL path *)

    type options = unit

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


    (* An html row for a file in the directory listing *)
    let file_row name icon stat = Printf.sprintf "
<tr>
  <td class=\"img\"><img src=\"%s\" alt=\"\" /></td>
  <td><a href=\"%s\">%s</a></td>
  <td>%Ld</td>
  <td>%s</td>
</tr>"
      icon (Netencoding.Url.encode ~plus:false name) name
      stat.Unix.LargeFile.st_size (date stat.Unix.LargeFile.st_mtime)


    let directory filename =
      let dir = Unix.opendir filename in
      let rec aux d =
        try
          let f = Unix.readdir dir in
          try
            let stat = Unix.LargeFile.stat (filename^f) in
            if stat.Unix.LargeFile.st_kind = Unix.S_DIR && f <> "." && f <> ".."
            then
              (`Dir, f,
               file_row f "/ocsigenstuff/folder_open.png" stat) :: aux d
            else
              if stat.Unix.LargeFile.st_kind = Unix.S_REG &&
                f.[(String.length f) - 1] <> '~'
              then
                (`Reg, f, file_row f (image_found f) stat) :: aux d
              else aux d
          with _ (* Unix.stat can fail for a lot of reasons *) -> aux d
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



    let result_of_content ?(options = ()) (filename, path) =
      let stat = Unix.LargeFile.stat filename in
      let rec back = function
        | [] | [""] -> assert false
        | [_] | [_ ; ""] -> []
        | i::j -> i :: (back j)
      in
      let parent =
        if path = [] || path = [""] then
          None
        else
          Some ("/"^Ocsigen_lib.string_of_url_path ~encode:true (back path))
      in
      let before =
        let st = (Ocsigen_lib.string_of_url_path ~encode:true path) in
        "<html>\n\
         <head><meta http-equiv=\"Content-Type\" content=\"text/html;\" />\n\
         <link rel=\"stylesheet\" type=\"text/css\" href=\"/ocsigenstuff/style.css\" media=\"screen\" />\n\
         <title>Listing Directory: "^st^"</title>\n</head>\n\
         <body><h1>"^st^"</h1>\n\
         <table summary=\"Contenu du dossier "^st^"\">\n\
         <tr id=\"headers\"><th></th><th>Name</th><th>Size</th>\
         <th>Last modified</th></tr>\n"

      and back = match parent with
        | None -> ""
        | Some parent ->
         "<tr>\n\
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
      let c = before^back^(directory filename)^after in
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
    type t = int option * exn option * Ocsigen_http_frame.cookieset

    type options = unit

    let get_etag c = None

    let error_page s msg c =
      XHTML.M.html
        (XHTML.M.head (XHTML.M.title (XHTML.M.pcdata s)) [])
        (XHTML.M.body
           (XHTML.M.h1 [XHTML.M.pcdata msg]::
            p [pcdata s]::
            c)
        )

    let result_of_content ?(options = ()) (code, exn, cookies_to_set) =
      let code = match code with
      | None -> 500
      | Some c -> c
      in
      let (error_code, error_msg, headers) =
        match exn with
        | Some (Http_error.Http_exception (errcode, msgs, h) as e) ->
            let msg = Http_error.string_of_http_exception e in
            let headers = match h with
              | Some h -> h
              | None -> Http_headers.dyn_headers
            in (errcode, msg, headers)
        | _ ->
            let error_mes = Http_error.expl_of_code code in
            (code, error_mes, Http_headers.empty)
      in
      let headers =
        (* puts dynamic headers *)
        let (<<) h (n, v) = Http_headers.replace n v h in
        headers
        << (Http_headers.cache_control, "no-cache")
        << (Http_headers.expires, "0")
      in
      let str_code = string_of_int error_code in
      let err_page =
        match exn with
        | Some exn when Ocsigen_config.get_debugmode () ->
            error_page
              ("Error "^str_code)
              error_msg
              [XHTML.M.p
                 [XHTML.M.pcdata (Ocsigen_lib.string_of_exn exn);
                  XHTML.M.br ();
                  XHTML.M.em
                    [XHTML.M.pcdata "(Ocsigen running in debug mode)"]
                ]]
        | _ ->
          error_page
              ("Error "^str_code)
              error_msg
              []
      in
      Xhtml_content.result_of_content err_page >>= fun r ->
      Lwt.return
          {r with
           res_cookies = cookies_to_set;
           res_code = error_code;
           res_charset = Some "utf-8";
           res_headers = headers;
         }


  end


let send_error
    ?code
    ?exn
    slot
    ~clientproto
    ?mode
    ?proto
    ?(cookies = Ocsigen_http_frame.Cookies.empty)
    ~head
    ~sender
    ()
    =
  Error_content.result_of_content (code, exn, cookies) >>= fun r ->
  send
    slot
    ~clientproto
    ?mode
    ?proto
    ~head
    ~sender
    r
