(* Ocsigen
 * http://www.ocsigen.org
 * Module deflatemod.ml
 * Copyright (C) 2007 Gabriel Kerneis
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
(*****************************************************************************)
(*****************************************************************************)
(* This module allows to compress output sent by the server                  *)
(*****************************************************************************)
(*****************************************************************************)

open Lwt
open Ocsigen_extensions
open Simplexmlparser
open Ocsigen_headers


(* Content-type *)
type filter = Type of string option * string option | Extension of string
type compress_choice = All_but of filter list |
                       Compress_only of filter list

let should_compress (t, t') url choice_list =
 let check = function
 |Type (None, None) -> true
 |Type (None, Some x') -> x' = t'
 |Type (Some x, None) -> x = t
 |Type (Some x, Some x') -> x = t && x' = t'
 |Extension suff -> Filename.check_suffix url suff
 in
 match choice_list with
 |Compress_only l -> List.exists check l
 |All_but l -> List.for_all (fun c -> not (check c)) l

(* Pas de filtre global pour l'instant
let choice_list = ref (All_but [])
*)

(** Compression *)

let buffer_size = ref 8192

(*  0 = no compression ; 1 = best speed ; 9 = best compression *)
let compress_level = ref 6

(* Minimal header, by X. Leroy *)
let gzip_header_length = 10
let gzip_header = String.make gzip_header_length (Char.chr 0)
let () =
  gzip_header.[0] <- Char.chr 0x1F;
  gzip_header.[1] <- Char.chr 0x8B;
  gzip_header.[2] <- Char.chr 8;
  gzip_header.[9] <- Char.chr 0xFF


(* inspired by an auxiliary function from camlzip, by Xavier Leroy *)
type output_buffer =
  {
  stream: Zlib.stream;
  buf: string;
  mutable pos: int;
  mutable avail: int;
  }

(* puts in oz the content of buf, from pos to pos + len ;
 * f is the continuation of the current stream *)
let rec output oz f buf pos len  =
   if pos < 0 || len < 0 || pos + len > String.length buf then
           assert false ;
  if len = 0 then next_cont oz f else
  if oz.avail = 0 then
    (let cont () = output oz f buf pos len in
    Ocsigen_messages.debug2 "--Deflatemod: Flushing because output buffer is full";
    flush oz cont)
  else (
  (catch
      (fun () ->
        (try return(Zlib.deflate oz.stream buf pos len
                                 oz.buf oz.pos oz.avail
                                 Zlib.Z_SYNC_FLUSH)
            with e -> fail e))
      (function
         |Zlib.Error(s, s') ->
                fail (Ocsigen_stream.Stream_error("Error during compression: "^s^" "^s'))
         | e -> fail e)) >>=
  (fun  (_, used_in, used_out) ->
  oz.pos <- oz.pos + used_out;
  oz.avail <- oz.avail - used_out;
  (* If we didn't deflate the whole input or if the buffer is full, continue *)
  if (used_in < len) || (oz.avail = 0)
  then output oz f buf (pos + used_in) (len - used_in)
  (* otherwise, go to the next part of the stream *)
  else
    next_cont oz f
  ))

(* Flush oz, ie. produces a new_stream with the content of oz, cleans it
 * and returns the continuation of the stream *)
and flush oz cont =
        let len = oz.pos in
        let s = String.sub oz.buf 0 len in
        Ocsigen_messages.debug2 "--Deflatemod: Flushing!";
        oz.pos <- 0 ;
        oz.avail <- String.length oz.buf ;
        if len > 0 then Ocsigen_stream.cont s cont else cont ()

and next_cont oz stream =
  Ocsigen_stream.next stream >>= fun e ->
  match e with
  | Ocsigen_stream.Finished None ->
      Ocsigen_messages.debug2 "--Deflatemod: End of stream: big cleaning for zlib" ;

      (* loop until there is nothing left to compress and flush *)
      let rec after_flushing () =
        (* buffer full *)
        if oz.avail = 0 then flush oz after_flushing
        else (
        (* no more input, deflates only what were left because output buffer
         * was full *)
        let (finished, _, used_out) =
         Zlib.deflate oz.stream oz.buf 0 0 oz.buf oz.pos oz.avail Zlib.Z_FINISH
        in
        oz.pos <- oz.pos + used_out;
        oz.avail <- oz.avail - used_out;
        if not finished then
            after_flushing ()
        else
            (Ocsigen_messages.debug2 "--Deflatemod: Zlib.deflate finished, last flush" ;
            flush oz (fun () -> Ocsigen_stream.empty None))) in

      flush oz after_flushing
  | Ocsigen_stream.Finished (Some s) -> next_cont oz s
  | Ocsigen_stream.Cont(s,f) ->
      output oz f s 0 (String.length s)

(* deflate param : true = deflate ; false = gzip (no header in this case) *)
let compress deflate stream =
  let zstream = Zlib.deflate_init !compress_level deflate in
  let finalize status =
    Ocsigen_stream.finalize stream status >>= fun e ->
    (try
      Zlib.deflate_end zstream
    with
      (* ignore errors, deflate_end cleans everything anyway *)
      Zlib.Error _ -> ());
    return (Ocsigen_messages.debug2 "--Deflatemod: Zlib stream closed") in
  let oz =
    { stream = zstream ;
      buf = String.create !buffer_size;
      pos = 0;
      avail = !buffer_size
    } in
  let new_stream () = next_cont oz (Ocsigen_stream.get stream) in
  Ocsigen_messages.debug2 "--Deflatemod: Zlib stream initialized" ;
  if deflate then
    Ocsigen_stream.make ~finalize new_stream
  else
    Ocsigen_stream.make
      ~finalize (fun () -> Ocsigen_stream.cont gzip_header new_stream)


(*****************************************************************************)
(** The filter function *)
(* We implement Content-Encoding, not Transfer-Encoding *)

type encoding = Deflate | Gzip | Id | Star | Not_acceptable

let qvalue = function Some x -> x |None -> 1.0

let enc_compare e e' = match e,e' with
|(Star,_),(_,_) -> -1 (* star should be at the very end *)
|(_,_),(Star,_) -> 1
|(_,v),(_,v') when v<v' -> 1 (* then, sort by qvalue *)
|(_,v),(_,v') when v>v' -> -1
|(x,_),(x',_) when x=x' -> 0
|(Deflate,_),(_,_) -> 1 (* and subsort by encoding *)
|(_,_),(Deflate,_) -> -1
|(Gzip,_),(_,_) -> 1
|(_,_),(Gzip,_) -> -1
|(Id,_),(_,_) -> 1
|(_,_),(Id,_) -> -1
|_ -> assert false

let rec filtermap f = function
  |[] -> []
  |t::q -> match f t with
    |Some s -> s::(filtermap f q)
    |None -> filtermap f q

let convert = function
|(Some "deflate",v) -> Some (Deflate, qvalue v)
|(Some "gzip",v)|(Some "x-gzip",v) -> Some (Gzip, qvalue v)
|(Some "identity",v) -> Some (Id, qvalue v)
|(None,v) -> Some (Star, qvalue v)
|_ -> None

(* Follow http's RFC to select the transfert encoding *)
let select_encoding accept_header =
  let h = List.sort enc_compare (filtermap convert accept_header) in
  let (exclude,accept) =
    let (e,a) = List.partition (fun x -> snd x = 0.) h in
    (List.map fst e, List.map fst a) in
  let rec aux = function
  |[] ->
    if ((List.mem Star exclude) || (List.mem Id exclude))
    then Not_acceptable else Id
  |t::q ->
    if (List.mem t exclude)
    then aux q else t
  in
  aux accept

exception No_compress

(* deflate = true -> mode deflate
 * deflate = false -> mode gzip *)
let stream_filter contentencoding url deflate choice res =
 return (Ext_found (fun () ->
 try (
   match res.Ocsigen_http_frame.res_content_type with
   | None -> raise No_compress (* il faudrait défaut ? *)
   | Some contenttype ->
       match Ocsigen_headers.parse_mime_type contenttype with
       | None, _ | _, None -> raise No_compress (* should never happen? *)
       | (Some a, Some b)
            when should_compress (a, b) url choice ->
          return { res with
               Ocsigen_http_frame.res_content_length = None;
               Ocsigen_http_frame.res_etag =
               (match res.Ocsigen_http_frame.res_etag with
               | Some e ->
                   Some ((if deflate then "Ddeflatemod" else "Gdeflatemod")^e)
               | None -> None);
               Ocsigen_http_frame.res_stream =
               (compress deflate (fst res.Ocsigen_http_frame.res_stream), None);
               Ocsigen_http_frame.res_headers =
               Http_headers.replace
                 Http_headers.content_encoding
                 contentencoding res.Ocsigen_http_frame.res_headers;
             }
       | _ -> raise No_compress)
 with Not_found | No_compress -> return res))

let filter choice_list = function
  | Req_not_found (code,_) -> return (Ext_next code)
  | Req_found ({ request_info = ri }, res) ->
      match select_encoding (Lazy.force(ri.ri_accept_encoding)) with
        | Deflate ->
            stream_filter "deflate" ri.ri_sub_path_string true choice_list res
        | Gzip ->
            stream_filter "gzip" ri.ri_sub_path_string false choice_list res
        | Id | Star -> return (Ext_found (fun () -> return res))
        | Not_acceptable ->
            return (Ext_stop_all (res.Ocsigen_http_frame.res_cookies,406))


(*****************************************************************************)

let rec parse_filter = function
 |[] -> []
 |(Element ("type",[],[PCData t]))::q ->
   let (a,b) = (Ocsigen_headers.parse_mime_type t)
   in Type (a,b) :: parse_filter q
 |(Element ("extension",[],[PCData t]))::q ->
  (Extension t) :: parse_filter q
 |_ -> raise (Error_in_config_file
                  "Unexpected element inside contenttype (should be <type> or
                  <extension>)")

let rec parse_global_config = function
  | [] -> ()
  | (Element ("compress", [("level", l)], []))::ll ->
     let l = try int_of_string l
     with Failure _ -> raise (Error_in_config_file
         "Compress level should be an integer between 0 and 9") in
     compress_level := if (l <= 9 && l >= 0) then l else 6 ;
     parse_global_config ll
  | (Element ("buffer", [("size", s)], []))::ll ->
     let s = (try int_of_string s
     with Failure _ -> raise (Error_in_config_file
         "Buffer size should be a positive integer")) in
     buffer_size := if s > 0 then s else 8192 ;
     parse_global_config ll
(* TODO: Pas de filtre global pour l'instant
 * le nom de balise contenttype est mauvais, au passage
  | (Element ("contenttype", [("compress", b)], choices))::ll ->
     let l = (try parse_filter choices
             with Not_found -> raise (Error_in_config_file
                  "Can't parse mime-type content")) in
     (match b with
     |"only" -> choice_list := Compress_only l
     |"allbut" -> choice_list := All_but l
     | _ ->  raise (Error_in_config_file
     "Attribute \"compress\" should be \"allbut\" or \"only\""));
     parse_global_config ll
*)
  | _ -> raise (Error_in_config_file
                  "Unexpected content inside deflatemod config")




(*****************************************************************************)

let parse_config = function
  | Element ("deflate", [("compress",b)], choices) ->
      let l = (try parse_filter choices
               with Not_found -> raise (Error_in_config_file
                                          "Can't parse filter content")) in
      (match b with
         |"only" -> filter (Compress_only l)
         |"allbut" -> filter (All_but l)
         | _ ->  raise (Error_in_config_file
                     "Attribute \"compress\" should be \"allbut\" or \"only\""))
  | Element ("deflate" as s, _, _) -> badconfig "Bad syntax for tag %s" s

  | Element (t, _, _) -> raise (Bad_config_tag_for_extension t)
  | _ ->
      raise (Error_in_config_file "Unexpected data in config file")





(*****************************************************************************)
(** Registration of the extension *)
let () = Ocsigen_extensions.register_extension
  ~name:"deflatemod"
  ~fun_site:(fun _ _ _ _ _ -> parse_config)
  ~init_fun:parse_global_config
  ()
