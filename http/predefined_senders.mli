(* Ocsigen
 * http://www.ocsigen.org
 * sender_helpers.ml Copyright (C) 2005 Denis Berthod
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
(** this module provides functions to create senders *)

type mycookieslist = 
  (string list option * float option * (string * string) list) list

type send_page_type =
    ?cookies:mycookieslist ->
    unit Lwt.t ->
    clientproto:Http_frame.Http_header.proto ->
    ?code:int ->
    ?etag:Http_frame.etag ->
    keep_alive:bool ->
    ?last_modified:float ->
    ?location:string -> 
    ?head:bool -> 
    ?headers:(string * string) list ->
    ?charset:string ->
      Http_com.sender_type -> unit Lwt.t

(** Sending xhtml *)
val send_xhtml_page : content: [ `Html ] XHTML.M.elt -> send_page_type

(** Sending a file *)
val send_file : content: string -> send_page_type

(** Sending an empty page (no content) *)
val send_empty : content: unit -> send_page_type

(** Sending a text page *)
val send_text_page : ?contenttype: string -> content:string -> send_page_type

(** fonction that uses a stream to send a (text) answer step by step *)
val send_stream_page : ?contenttype: string -> 
  content:(unit -> Ocsistream.stream) -> send_page_type

(** Headers for a non cachable request *)
val dyn_headers : (string * string) list


(**/**)
exception Stream_already_read

(** Sending an error page *)
val send_error :
    ?http_exception:exn ->
      send_page_type

module Xhtml_content :
  sig
    type t = [ `Html ] XHTML.M.elt
    val get_etag_aux : string -> string
    val get_etag : [ `Html ] XHTML.M.elt -> string
    val stream_of_content :
      [ `Html ] XHTML.M.elt ->
      (int64 option * string * Ocsistream.stream * ('a -> 'a)) Lwt.t
    val content_of_stream : 'a -> 'b
  end
module Text_content :
  sig
    type t = string
    val get_etag : string -> string
    val stream_of_content :
      string -> (int64 option * string * Ocsistream.stream * ('a -> 'a)) Lwt.t
    val content_of_stream : Ocsistream.stream -> t Lwt.t
  end
module Stream_content :
  sig
    type t = unit -> Ocsistream.stream
    val get_etag : t -> string
    val stream_of_content : 
        t -> (int64 option * string * Ocsistream.stream * ('a -> 'a)) Lwt.t
    val content_of_stream : Ocsistream.stream -> t Lwt.t
  end
module Empty_content :
  sig
    type t = unit
    val get_etag : 'a -> string
    val stream_of_content :
      'a -> (int64 option * string * Ocsistream.stream * ('b -> 'b)) Lwt.t
    val content_of_stream : Ocsistream.stream -> unit Lwt.t
  end
module File_content :
  sig
    type t = string
    val read_file :
      ?buffer_size:int -> Unix.file_descr -> Ocsistream.stream Lwt.t
    val get_etag_aux : Unix.LargeFile.stats -> string
    val get_etag : string -> string
    val stream_of_content :
      string -> (int64 option * string * 
                   Ocsistream.stream * (unit -> unit)) Lwt.t
    val content_of_stream : 'a -> 'b
  end
module Empty_sender :
  sig
    module H :
      sig
        type http_mode = Http_frame.Http_header.http_mode
        type http_method =
          Http_frame.Http_header.http_method =
            GET
          | POST
          | HEAD
          | PUT
          | DELETE
          | TRACE
          | OPTIONS
          | CONNECT
          | LINK
          | UNLINK
          | PATCH
        type http_header =
          Http_frame.Http_header.http_header
        val get_headers_value : http_header -> string -> string
        val get_proto : http_header -> Http_frame.Http_header.proto
        val get_firstline : http_header -> Http_frame.Http_header.http_mode
        val add_headers : http_header -> string -> string -> http_header
      end
    module Http :
      sig
        type frame_content = Empty_content.t option
        type http_frame =
            Http_frame.FHttp_frame(Empty_content).http_frame = {
            header: Http_frame.Http_header.http_header;
            content: frame_content;
            waiter_thread: unit Lwt.t;
          }
      end
    module PP :
      sig
        module Http :
          sig
            type frame_content = Empty_content.t option
            type http_frame =
                Http_frame.FHttp_frame(Empty_content).http_frame = {
                header : Http_frame.Http_header.http_header;
                content : frame_content;
                waiter_thread: unit Lwt.t;
            }
          end
        val string_of_http_frame : Http.http_frame -> string option -> string
      end
    val really_write :
        ?chunked:bool ->
      Lwt_unix.lwt_out_channel -> (unit -> unit) -> Ocsistream.stream -> unit Lwt.t
    val non_case_equality : string -> string -> bool
    val non_case_compare : string -> string -> int
    val pair_order : string * string -> string * string -> bool
    val add_header : Http_com.sender_type -> string -> string -> unit
    val rem_header : Http_com.sender_type -> string -> unit
    val get_protocol : Http_com.sender_type -> Http_frame.Http_header.proto
    val get_mode : Http_com.sender_type -> Http_com.s_http_mode
    val get_headers : Http_com.sender_type -> (string * string) list
    val get_header_value : Http_com.sender_type -> string -> string
    val hds_fusion :
      bool ->
      int64 option ->
      (string * string) list ->
      (string * string) list -> (string * string) list
    val send :
      unit Lwt.t ->
      clientproto:Http_frame.Http_header.proto ->
      ?etag:Http_frame.etag ->
      mode:H.http_mode ->
      ?proto:Http_frame.Http_header.proto ->
      ?headers:(string * string) list ->
      ?content:Empty_content.t ->
      ?head:bool -> Http_com.sender_type -> unit Lwt.t
  end
module Xhtml_sender :
  sig
    module H :
      sig
        type http_mode = Http_frame.Http_header.http_mode
        type http_method =
          Http_frame.Http_header.http_method =
            GET
          | POST
          | HEAD
          | PUT
          | DELETE
          | TRACE
          | OPTIONS
          | CONNECT
          | LINK
          | UNLINK
          | PATCH
        type http_header =
          Http_frame.Http_header.http_header
        val get_headers_value : http_header -> string -> string
        val get_proto : http_header -> Http_frame.Http_header.proto
        val get_firstline : http_header -> Http_frame.Http_header.http_mode
        val add_headers : http_header -> string -> string -> http_header
      end
    module Http :
      sig
        type frame_content = Xhtml_content.t option
        type http_frame =
            Http_frame.FHttp_frame(Xhtml_content).http_frame = {
            header : Http_frame.Http_header.http_header;
            content : frame_content;
            waiter_thread: unit Lwt.t;
        }
      end
    module PP :
      sig
        module Http :
          sig
            type frame_content = Xhtml_content.t option
            type http_frame =
                Http_frame.FHttp_frame(Xhtml_content).http_frame = {
                header : Http_frame.Http_header.http_header;
                content : frame_content;
                waiter_thread: unit Lwt.t;
              }
          end
        val string_of_http_frame : Http.http_frame -> string option -> string
      end
    val really_write :
        ?chunked:bool ->
      Lwt_unix.lwt_out_channel -> (unit -> unit) -> Ocsistream.stream -> unit Lwt.t
    val non_case_equality : string -> string -> bool
    val non_case_compare : string -> string -> int
    val pair_order : string * string -> string * string -> bool
    val add_header : Http_com.sender_type -> string -> string -> unit
    val rem_header : Http_com.sender_type -> string -> unit
    val get_protocol : Http_com.sender_type -> Http_frame.Http_header.proto
    val get_mode : Http_com.sender_type -> Http_com.s_http_mode
    val get_headers : Http_com.sender_type -> (string * string) list
    val get_header_value : Http_com.sender_type -> string -> string
    val hds_fusion :
      bool ->
      int64 option ->
      (string * string) list ->
      (string * string) list -> (string * string) list
    val send :
      unit Lwt.t ->
      clientproto:Http_frame.Http_header.proto ->
      ?etag:Http_frame.etag ->
      mode:H.http_mode ->
      ?proto:Http_frame.Http_header.proto ->
      ?headers:(string * string) list ->
      ?content:Xhtml_content.t ->
      ?head:bool -> Http_com.sender_type -> unit Lwt.t
  end
module Text_http_frame :
  sig
    type frame_content = Text_content.t option
    type http_frame =
        Http_frame.FHttp_frame(Text_content).http_frame = {
        header : Http_frame.Http_header.http_header;
        content : frame_content;
        waiter_thread: unit Lwt.t;
    }
  end
module Text_sender :
  sig
    module H :
      sig
        type http_mode = Http_frame.Http_header.http_mode
        type http_method =
          Http_frame.Http_header.http_method =
            GET
          | POST
          | HEAD
          | PUT
          | DELETE
          | TRACE
          | OPTIONS
          | CONNECT
          | LINK
          | UNLINK
          | PATCH
        type http_header =
          Http_frame.Http_header.http_header
        val get_headers_value : http_header -> string -> string
        val get_proto : http_header -> Http_frame.Http_header.proto
        val get_firstline : http_header -> Http_frame.Http_header.http_mode
        val add_headers : http_header -> string -> string -> http_header
      end
    module Http :
      sig
        type frame_content = Text_content.t option
        type http_frame =
            Http_frame.FHttp_frame(Text_content).http_frame = {
            header : Http_frame.Http_header.http_header;
            content : frame_content;
            waiter_thread: unit Lwt.t;
        }
      end
    module PP :
      sig
        module Http :
          sig
            type frame_content = Text_content.t option
            type http_frame =
                Http_frame.FHttp_frame(Text_content).http_frame = {
                header : Http_frame.Http_header.http_header;
                content : frame_content;
                waiter_thread: unit Lwt.t;
            }
          end
        val string_of_http_frame : Http.http_frame -> string option -> string
      end
    val really_write :
        ?chunked:bool ->
      Lwt_unix.lwt_out_channel -> (unit -> unit) -> Ocsistream.stream -> unit Lwt.t
    val non_case_equality : string -> string -> bool
    val non_case_compare : string -> string -> int
    val pair_order : string * string -> string * string -> bool
    val add_header : Http_com.sender_type -> string -> string -> unit
    val rem_header : Http_com.sender_type -> string -> unit
    val get_protocol : Http_com.sender_type -> Http_frame.Http_header.proto
    val get_mode : Http_com.sender_type -> Http_com.s_http_mode
    val get_headers : Http_com.sender_type -> (string * string) list
    val get_header_value : Http_com.sender_type -> string -> string
    val hds_fusion :
      bool ->
      int64 option ->
      (string * string) list ->
      (string * string) list -> (string * string) list
    val send :
      unit Lwt.t ->
      clientproto:Http_frame.Http_header.proto ->
      ?etag:Http_frame.etag ->
      mode:H.http_mode ->
      ?proto:Http_frame.Http_header.proto ->
      ?headers:(string * string) list ->
      ?content:Text_content.t ->
      ?head:bool -> Http_com.sender_type -> unit Lwt.t
  end
module Text_receiver :
  sig
    module Http :
      sig
        type frame_content = Text_content.t option
        type http_frame =
            Http_frame.FHttp_frame(Text_content).http_frame = {
            header : Http_frame.Http_header.http_header;
            content : frame_content;
            waiter_thread: unit Lwt.t;
        }
      end
    val http_header_of_stream :
      ?withoutfirstline:bool ->
      Ocsistream.stream -> Http_frame.Http_header.http_header Lwt.t
    val get_http_frame :
      unit Lwt.t -> Http_com.receiver_type -> 
        doing_keep_alive:bool -> unit -> Http.http_frame Lwt.t
  end
module Stream_http_frame :
  sig
    type frame_content = Stream_content.t option
    type http_frame =
      Http_frame.FHttp_frame(Stream_content).http_frame = {
      header : Http_frame.Http_header.http_header;
      content : frame_content;
      waiter_thread : unit Lwt.t;
    }
  end
module Stream_receiver :
  sig
    module Http :
      sig
        type frame_content = Stream_content.t option
        type http_frame =
            Http_frame.FHttp_frame(Stream_content).http_frame = {
            header : Http_frame.Http_header.http_header;
            content : frame_content;
            waiter_thread: unit Lwt.t;
        }
      end
    val http_header_of_stream :
      ?withoutfirstline:bool ->
      Ocsistream.stream -> Http_frame.Http_header.http_header Lwt.t
    val get_http_frame :
      unit Lwt.t -> Http_com.receiver_type -> 
        doing_keep_alive:bool -> unit -> Http.http_frame Lwt.t
  end
module File_sender :
  sig
    module H :
      sig
        type http_mode = Http_frame.Http_header.http_mode
        type http_method =
          Http_frame.Http_header.http_method =
            GET
          | POST
          | HEAD
          | PUT
          | DELETE
          | TRACE
          | OPTIONS
          | CONNECT
          | LINK
          | UNLINK
          | PATCH
        type http_header =
          Http_frame.Http_header.http_header
        val get_headers_value : http_header -> string -> string
        val get_proto : http_header -> Http_frame.Http_header.proto
        val get_firstline : http_header -> Http_frame.Http_header.http_mode
        val add_headers : http_header -> string -> string -> http_header
      end
    module Http :
      sig
        type frame_content = File_content.t option
        type http_frame =
            Http_frame.FHttp_frame(File_content).http_frame = {
            header : Http_frame.Http_header.http_header;
            content : frame_content;
            waiter_thread: unit Lwt.t;
        }
      end
    module PP :
      sig
        module Http :
          sig
            type frame_content = File_content.t option
            type http_frame =
                Http_frame.FHttp_frame(File_content).http_frame = {
                header : Http_frame.Http_header.http_header;
                content : frame_content;
                waiter_thread: unit Lwt.t;
              }
          end
        val string_of_http_frame : Http.http_frame -> string option -> string
      end
    val really_write :
        ?chunked:bool ->
      Lwt_unix.lwt_out_channel -> (unit -> unit) -> Ocsistream.stream -> unit Lwt.t
    val non_case_equality : string -> string -> bool
    val non_case_compare : string -> string -> int
    val pair_order : string * string -> string * string -> bool
    val add_header : Http_com.sender_type -> string -> string -> unit
    val rem_header : Http_com.sender_type -> string -> unit
    val get_protocol : Http_com.sender_type -> Http_frame.Http_header.proto
    val get_mode : Http_com.sender_type -> Http_com.s_http_mode
    val get_headers : Http_com.sender_type -> (string * string) list
    val get_header_value : Http_com.sender_type -> string -> string
    val hds_fusion :
      bool ->
      int64 option ->
      (string * string) list ->
      (string * string) list -> (string * string) list
    val send :
      unit Lwt.t ->
      clientproto:Http_frame.Http_header.proto ->
      ?etag:Http_frame.etag ->
      mode:H.http_mode ->
      ?proto:Http_frame.Http_header.proto ->
      ?headers:(string * string) list ->
      ?content:File_content.t ->
      ?head:bool -> Http_com.sender_type -> unit Lwt.t
  end
module Stream_sender :
  sig
    module H :
      sig
        type http_mode = Http_frame.Http_header.http_mode
        type http_method =
          Http_frame.Http_header.http_method =
            GET
          | POST
          | HEAD
          | PUT
          | DELETE
          | TRACE
          | OPTIONS
          | CONNECT
          | LINK
          | UNLINK
          | PATCH
        type http_header =
          Http_frame.Http_header.http_header
        val get_headers_value : http_header -> string -> string
        val get_proto : http_header -> Http_frame.Http_header.proto
        val get_firstline : http_header -> Http_frame.Http_header.http_mode
        val add_headers : http_header -> string -> string -> http_header
      end
    module Http :
      sig
        type frame_content = Stream_content.t option
        type http_frame =
          Http_frame.FHttp_frame(Stream_content).http_frame = {
          header : Http_frame.Http_header.http_header;
          content : frame_content;
          waiter_thread : unit Lwt.t;
        }
      end
    module PP :
      sig
        module Http :
          sig
            type frame_content = Stream_content.t option
            type http_frame =
              Http_frame.FHttp_frame(Stream_content).http_frame = {
              header : Http_frame.Http_header.http_header;
              content : frame_content;
              waiter_thread : unit Lwt.t;
            }
          end
        val string_of_http_frame : Http.http_frame -> string option -> string
      end
    val really_write :
        ?chunked:bool ->
      Lwt_unix.lwt_out_channel -> (unit -> unit) -> Ocsistream.stream -> unit Lwt.t
    val non_case_equality : string -> string -> bool
    val non_case_compare : string -> string -> int
    val pair_order : string * string -> string * string -> bool
    val add_header : Http_com.sender_type -> string -> string -> unit
    val rem_header : Http_com.sender_type -> string -> unit
    val get_protocol : Http_com.sender_type -> Http_frame.Http_header.proto
    val get_mode : Http_com.sender_type -> Http_com.s_http_mode
    val get_headers : Http_com.sender_type -> (string * string) list
    val get_header_value : Http_com.sender_type -> string -> string
    val hds_fusion :
      bool ->
      int64 option ->
      (string * string) list ->
      (string * string) list -> (string * string) list
    val send :
      unit Lwt.t ->
      clientproto:Http_frame.Http_header.proto ->
      ?etag:Http_frame.etag ->
      mode:H.http_mode ->
      ?proto:Http_frame.Http_header.proto ->
      ?headers:(string * string) list ->
      ?content:Stream_content.t ->
      ?head:bool -> Http_com.sender_type -> unit Lwt.t
  end


val gmtdate : float -> string
val send_generic :
    (unit Lwt.t ->
      clientproto:Http_frame.Http_header.proto ->
      ?etag:Http_frame.etag ->
        mode:Xhtml_sender.H.http_mode ->
          ?proto:Http_frame.Http_header.proto ->
            ?headers:(string * string) list ->
              ?content:'a ->
                ?head:bool -> 
                  Http_com.sender_type -> 
                    unit Lwt.t) ->
  ?contenttype:string ->
  content:'a ->
  ?cookies:mycookieslist ->
  unit Lwt.t ->
  clientproto:Http_frame.Http_header.proto ->
  ?code:int ->
  ?etag:Http_frame.etag ->
  keep_alive:bool ->
  ?last_modified:float ->
  ?location:string ->
  ?head:bool ->
  ?headers:(string * string) list ->
  ?charset:string ->
  Http_com.sender_type ->
  unit Lwt.t

val mimeht : (string, string) Hashtbl.t
val parse_mime_types : string -> unit
val affiche_mime : unit -> unit
val content_type_from_file_name : File_content.t -> string

