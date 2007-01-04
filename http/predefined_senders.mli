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

type create_sender_type =
    ?server_name:string ->
    ?proto:string -> Lwt_unix.descr -> Http_com.sender_type
type send_page_type =
    unit Lwt.t ->
    ?code:int ->
    ?etag:Http_frame.etag ->
    keep_alive:bool ->
    ?cookies:(string * string) list ->
    ?path:string ->
    ?last_modified:float ->
    ?location:string -> ?head:bool -> Http_com.sender_type -> unit Lwt.t

(** Sending xhtml *)
val send_xhtml_page : content: [ `Html ] XHTML.M.elt -> send_page_type

(** Sending a file *)
val send_file : string -> send_page_type

(** Sending an empty page (no content) *)
val send_empty : send_page_type

(** Sending a text page *)
val send_text_page :
  content:string -> send_page_type

(** Sending an error page *)
val send_error :
  unit Lwt.t ->
  ?http_exception:exn ->
  ?error_num:int -> Http_com.sender_type -> keep_alive:bool -> unit Lwt.t

(** Creating an xhtml (or text) sender *)
val create_xhtml_sender : create_sender_type

(** Creating a sender for empty content *)
val create_empty_sender : create_sender_type

(** Creating a sender for a file *)
val create_file_sender : create_sender_type

(**/**)
module Xhtml_content :
  sig
    type t = [ `Html ] XHTML.M.elt
    val get_etag_aux : string -> string
    val get_etag : [ `Html ] XHTML.M.elt -> string
    val stream_of_content :
      [ `Html ] XHTML.M.elt ->
      (int64 * string * Ocsistream.stream * ('a -> 'a)) Lwt.t
    val content_of_stream : 'a -> 'b
  end
module Text_content :
  sig
    type t = string
    val get_etag : string -> string
    val stream_of_content :
      string -> (int64 * string * Ocsistream.stream * ('a -> 'a)) Lwt.t
    val content_of_stream : Ocsistream.stream -> string Lwt.t
  end
module Stream_content :
  sig
    type t = Ocsistream.stream
    val get_etag : 'a -> 'b
    val stream_of_content : 'a -> 'b
    val content_of_stream : 'a -> 'a Lwt.t
  end
module Empty_content :
  sig
    type t = unit
    val get_etag : 'a -> string
    val stream_of_content :
      'a -> (int64 * string * Ocsistream.stream * ('b -> 'b)) Lwt.t
    val content_of_stream : 'a -> unit Lwt.t
  end
module File_content :
  sig
    type t = string
    val read_file :
      ?buffer_size:int -> Unix.file_descr -> Ocsistream.stream Lwt.t
    val get_etag_aux : Unix.LargeFile.stats -> string
    val get_etag : string -> string
    val stream_of_content :
      string -> (int64 * string * Ocsistream.stream * (unit -> unit)) Lwt.t
    val content_of_stream : 'a -> 'b
  end
module Empty_sender :
  sig
    module H :
      sig
        type http_mode = Http_frame.Http_header.http_mode = Query | Answer
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
          Http_frame.Http_header.http_header = {
          mode : http_mode;
          meth : http_method option;
          url : string option;
          code : int option;
          proto : string;
          headers : (string * string) list;
        }
        val get_url : http_header -> string
        val get_headers_value : http_header -> string -> string
        val get_proto : http_header -> string
        val get_method : http_header -> http_method option
        val add_headers : http_header -> string -> string -> http_header
      end
    module Http :
      sig
        type frame_content = Empty_content.t option
        type http_frame =
          Http_frame.FHttp_frame(Empty_content).http_frame = {
          header : Http_frame.Http_header.http_header;
          content : frame_content;
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
            }
          end
        val string_of_http_frame : Http.http_frame -> string option -> string
      end
    type t = Http_com.sender_type
    val really_write :
      Lwt_unix.descr -> (unit -> unit) -> Ocsistream.stream -> unit Lwt.t
    val create :
      ?mode:Http_frame.Http_header.http_mode ->
      ?headers:(string * string) list ->
      ?proto:string -> Lwt_unix.descr -> Http_com.sender_type
    val change_protocol : string -> Http_com.sender_type -> unit
    val change_headers :
      (string * string) list -> Http_com.sender_type -> unit
    val change_mode :
      Http_frame.Http_header.http_mode -> Http_com.sender_type -> unit
    val non_case_equality : string -> string -> bool
    val non_case_compare : string -> string -> int
    val pair_order : string * string -> string * string -> bool
    val add_header : Http_com.sender_type -> string -> string -> unit
    val rem_header : Http_com.sender_type -> string -> unit
    val get_protocol : Http_com.sender_type -> string
    val get_mode : Http_com.sender_type -> Http_frame.Http_header.http_mode
    val get_headers : Http_com.sender_type -> (string * string) list
    val get_header_value : Http_com.sender_type -> string -> string
    val hds_fusion :
      int64 option ->
      (string * string) list ->
      (string * string) list -> (string * string) list
    val send :
      unit Lwt.t ->
      ?etag:Http_frame.etag ->
      ?mode:H.http_mode ->
      ?proto:string ->
      ?headers:(string * string) list ->
      ?meth:H.http_method ->
      ?url:string ->
      ?code:int ->
      ?content:Empty_content.t ->
      ?head:bool -> Http_com.sender_type -> unit Lwt.t
  end
module Xhtml_sender :
  sig
    module H :
      sig
        type http_mode = Http_frame.Http_header.http_mode = Query | Answer
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
          Http_frame.Http_header.http_header = {
          mode : http_mode;
          meth : http_method option;
          url : string option;
          code : int option;
          proto : string;
          headers : (string * string) list;
        }
        val get_url : http_header -> string
        val get_headers_value : http_header -> string -> string
        val get_proto : http_header -> string
        val get_method : http_header -> http_method option
        val add_headers : http_header -> string -> string -> http_header
      end
    module Http :
      sig
        type frame_content = Xhtml_content.t option
        type http_frame =
          Http_frame.FHttp_frame(Xhtml_content).http_frame = {
          header : Http_frame.Http_header.http_header;
          content : frame_content;
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
            }
          end
        val string_of_http_frame : Http.http_frame -> string option -> string
      end
    type t = Http_com.sender_type
    val really_write :
      Lwt_unix.descr -> (unit -> unit) -> Ocsistream.stream -> unit Lwt.t
    val create :
      ?mode:Http_frame.Http_header.http_mode ->
      ?headers:(string * string) list ->
      ?proto:string -> Lwt_unix.descr -> Http_com.sender_type
    val change_protocol : string -> Http_com.sender_type -> unit
    val change_headers :
      (string * string) list -> Http_com.sender_type -> unit
    val change_mode :
      Http_frame.Http_header.http_mode -> Http_com.sender_type -> unit
    val non_case_equality : string -> string -> bool
    val non_case_compare : string -> string -> int
    val pair_order : string * string -> string * string -> bool
    val add_header : Http_com.sender_type -> string -> string -> unit
    val rem_header : Http_com.sender_type -> string -> unit
    val get_protocol : Http_com.sender_type -> string
    val get_mode : Http_com.sender_type -> Http_frame.Http_header.http_mode
    val get_headers : Http_com.sender_type -> (string * string) list
    val get_header_value : Http_com.sender_type -> string -> string
    val hds_fusion :
      int64 option ->
      (string * string) list ->
      (string * string) list -> (string * string) list
    val send :
      unit Lwt.t ->
      ?etag:Http_frame.etag ->
      ?mode:H.http_mode ->
      ?proto:string ->
      ?headers:(string * string) list ->
      ?meth:H.http_method ->
      ?url:string ->
      ?code:int ->
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
    }
  end
module Text_sender :
  sig
    module H :
      sig
        type http_mode = Http_frame.Http_header.http_mode = Query | Answer
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
          Http_frame.Http_header.http_header = {
          mode : http_mode;
          meth : http_method option;
          url : string option;
          code : int option;
          proto : string;
          headers : (string * string) list;
        }
        val get_url : http_header -> string
        val get_headers_value : http_header -> string -> string
        val get_proto : http_header -> string
        val get_method : http_header -> http_method option
        val add_headers : http_header -> string -> string -> http_header
      end
    module Http :
      sig
        type frame_content = Text_content.t option
        type http_frame =
          Http_frame.FHttp_frame(Text_content).http_frame = {
          header : Http_frame.Http_header.http_header;
          content : frame_content;
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
            }
          end
        val string_of_http_frame : Http.http_frame -> string option -> string
      end
    type t = Http_com.sender_type
    val really_write :
      Lwt_unix.descr -> (unit -> unit) -> Ocsistream.stream -> unit Lwt.t
    val create :
      ?mode:Http_frame.Http_header.http_mode ->
      ?headers:(string * string) list ->
      ?proto:string -> Lwt_unix.descr -> Http_com.sender_type
    val change_protocol : string -> Http_com.sender_type -> unit
    val change_headers :
      (string * string) list -> Http_com.sender_type -> unit
    val change_mode :
      Http_frame.Http_header.http_mode -> Http_com.sender_type -> unit
    val non_case_equality : string -> string -> bool
    val non_case_compare : string -> string -> int
    val pair_order : string * string -> string * string -> bool
    val add_header : Http_com.sender_type -> string -> string -> unit
    val rem_header : Http_com.sender_type -> string -> unit
    val get_protocol : Http_com.sender_type -> string
    val get_mode : Http_com.sender_type -> Http_frame.Http_header.http_mode
    val get_headers : Http_com.sender_type -> (string * string) list
    val get_header_value : Http_com.sender_type -> string -> string
    val hds_fusion :
      int64 option ->
      (string * string) list ->
      (string * string) list -> (string * string) list
    val send :
      unit Lwt.t ->
      ?etag:Http_frame.etag ->
      ?mode:H.http_mode ->
      ?proto:string ->
      ?headers:(string * string) list ->
      ?meth:H.http_method ->
      ?url:string ->
      ?code:int ->
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
        }
      end
    type t =
      Http_com.FHttp_receiver(Text_content).t = {
      buffer : Http_com.Com_buffer.t;
      fd : Lwt_unix.descr;
    }
    val create : Lwt_unix.descr -> t
    val http_header_of_stream :
      Ocsistream.stream -> Http_frame.Http_header.http_header Lwt.t
    val get_http_frame :
      unit Lwt.t ->
      t -> doing_keep_alive:bool -> unit -> Http.http_frame Lwt.t
  end
module Stream_http_frame :
  sig
    type frame_content = Stream_content.t option
    type http_frame =
      Http_frame.FHttp_frame(Stream_content).http_frame = {
      header : Http_frame.Http_header.http_header;
      content : frame_content;
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
        }
      end
    type t =
      Http_com.FHttp_receiver(Stream_content).t = {
      buffer : Http_com.Com_buffer.t;
      fd : Lwt_unix.descr;
    }
    val create : Lwt_unix.descr -> t
    val http_header_of_stream :
      Ocsistream.stream -> Http_frame.Http_header.http_header Lwt.t
    val get_http_frame :
      unit Lwt.t ->
      t -> doing_keep_alive:bool -> unit -> Http.http_frame Lwt.t
  end
module File_sender :
  sig
    module H :
      sig
        type http_mode = Http_frame.Http_header.http_mode = Query | Answer
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
          Http_frame.Http_header.http_header = {
          mode : http_mode;
          meth : http_method option;
          url : string option;
          code : int option;
          proto : string;
          headers : (string * string) list;
        }
        val get_url : http_header -> string
        val get_headers_value : http_header -> string -> string
        val get_proto : http_header -> string
        val get_method : http_header -> http_method option
        val add_headers : http_header -> string -> string -> http_header
      end
    module Http :
      sig
        type frame_content = File_content.t option
        type http_frame =
          Http_frame.FHttp_frame(File_content).http_frame = {
          header : Http_frame.Http_header.http_header;
          content : frame_content;
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
            }
          end
        val string_of_http_frame : Http.http_frame -> string option -> string
      end
    type t = Http_com.sender_type
    val really_write :
      Lwt_unix.descr -> (unit -> unit) -> Ocsistream.stream -> unit Lwt.t
    val create :
      ?mode:Http_frame.Http_header.http_mode ->
      ?headers:(string * string) list ->
      ?proto:string -> Lwt_unix.descr -> Http_com.sender_type
    val change_protocol : string -> Http_com.sender_type -> unit
    val change_headers :
      (string * string) list -> Http_com.sender_type -> unit
    val change_mode :
      Http_frame.Http_header.http_mode -> Http_com.sender_type -> unit
    val non_case_equality : string -> string -> bool
    val non_case_compare : string -> string -> int
    val pair_order : string * string -> string * string -> bool
    val add_header : Http_com.sender_type -> string -> string -> unit
    val rem_header : Http_com.sender_type -> string -> unit
    val get_protocol : Http_com.sender_type -> string
    val get_mode : Http_com.sender_type -> Http_frame.Http_header.http_mode
    val get_headers : Http_com.sender_type -> (string * string) list
    val get_header_value : Http_com.sender_type -> string -> string
    val hds_fusion :
      int64 option ->
      (string * string) list ->
      (string * string) list -> (string * string) list
    val send :
      unit Lwt.t ->
      ?etag:Http_frame.etag ->
      ?mode:H.http_mode ->
      ?proto:string ->
      ?headers:(string * string) list ->
      ?meth:H.http_method ->
      ?url:string ->
      ?code:int ->
      ?content:File_content.t ->
      ?head:bool -> Http_com.sender_type -> unit Lwt.t
  end
val gmtdate : float -> string
val send_generic :
  unit Lwt.t ->
  ?code:int ->
  ?etag:Http_frame.etag ->
  keep_alive:bool ->
  ?cookies:(string * string) list ->
  ?last_modified:float ->
  ?path:string ->
  ?location:string ->
  ?header:(string * string) list ->
  ?head:bool ->
  content:'a ->
  'b ->
  (unit Lwt.t ->
   ?etag:Http_frame.etag ->
   ?mode:Xhtml_sender.H.http_mode ->
   ?proto:string ->
   ?headers:(string * string) list ->
   ?meth:'c ->
   ?url:string -> ?code:int -> ?content:'a -> ?head:bool -> 'b -> unit Lwt.t) ->
  unit Lwt.t

val mimeht : (string, string) Hashtbl.t
val parse_mime_types : string -> unit
val affiche_mime : unit -> unit
val content_type_from_file_name : File_content.t -> string

