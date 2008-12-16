(* Ocsigen
 * http://www.ocsigen.org
 * ocsigen_range.ml Copyright (C) 2008
 * Vincent Balat
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

(* - We send the range only if we know the content length
   (the header of partial answers must contain the length)
   - We compute range after content-encoding (deflation)
   - We do not support multipart ranges. We send only an interval.
   - The following works with any stream.
   For files, it should be optimized with seek!!!!!
*)

let (>>=) = Lwt.bind

exception Range_416

(* We do not support multipart ranges. We send only an interval.
   The following function checks if we support the range requested.
*)
let rec change_range = function
  | Some ([], Some b, ifmatch) -> Some (b, None, ifmatch)
  | Some ([ (b, e) ], None, ifmatch) -> Some (b, Some e, ifmatch)
  | _ -> None

let select_range length beg endopt stream =
  let rec aux step num () =
    (match step with
      | Ocsigen_stream.Finished _  -> 
          Lwt.fail Ocsigen_stream.Stream_too_small
      | Ocsigen_stream.Cont (c, f) -> Lwt.return (c, f))
    >>= fun (buf, nextstream) ->
    let buflen = String.length buf in
    let buflen64 = Int64.of_int buflen in
    if num = 0L
    then Ocsigen_stream.empty None
    else if (Int64.compare buflen64 num) <= 0
    then 
      Ocsigen_stream.next nextstream >>= fun next ->
      Ocsigen_stream.cont buf (aux next (Int64.sub num buflen64))
    else
      Ocsigen_stream.cont (String.sub buf 0 (Int64.to_int num))
        (fun () -> Ocsigen_stream.empty None)
  in
  Ocsigen_stream.next (Ocsigen_stream.get stream) >>= fun s ->
  Lwt.catch
    (fun () ->
       Ocsigen_stream.skip s beg >>= fun new_s ->
       Lwt.return
         (match endopt with
           | None -> Ocsigen_stream.make (fun () -> Lwt.return new_s)
           | Some endc -> Ocsigen_stream.make (aux new_s length))
    )
    (function
       | Ocsigen_stream.Stream_too_small -> Lwt.fail Range_416
(* RFC 2616 A server SHOULD return a response with this status code if a request included a Range request-header field, and none of the range-specifier values in this field overlap the current extent of the selected resource, and the request did not include an If-Range request-header field. (For byte-ranges, this means that the first- byte-pos of all of the byte-range-spec values were greater than the current length of the selected resource.) *)
       | e -> Lwt.fail e)


let compute_range ri res =
  match res.Ocsigen_http_frame.res_content_length with
      (* We support Ranges only if we know the content length, because
         Content-Range always contains the length ... *)
    | None -> Lwt.return res
    | Some cl ->
        (* Send range only if the code is 200!! *)
        if res.Ocsigen_http_frame.res_code <> 200
        then Lwt.return res
        else begin
          match change_range (Lazy.force ri.Ocsigen_extensions.ri_range) with
            | None -> Lwt.return res
            | Some (_, _, Ocsigen_extensions.IR_ifmatch etag) 
                when (match res.Ocsigen_http_frame.res_etag with
                        | None -> true
                        | Some resetag -> String.compare etag resetag <> 0) ->
                Lwt.return res
            | Some (_, _, Ocsigen_extensions.IR_Ifunmodsince date)
                when (match res.Ocsigen_http_frame.res_lastmodified with
                        | None -> true
                        | Some l -> l > date)
                  ->
                Lwt.return res
            | Some (beg, endopt, _) ->

                let endc = match endopt with
                  | None -> Int64.sub cl 1L
                  | Some e -> e
                in
                let length = Int64.add (Int64.sub endc beg) 1L in

                Lwt.catch
                  (fun () ->
                     (* stream transform *)
                     select_range 
                       length beg endopt res.Ocsigen_http_frame.res_stream
                     >>= fun new_s ->
                     Lwt.return 
                       {res with
                          Ocsigen_http_frame.res_stream = new_s;
                          Ocsigen_http_frame.res_code = 206;
                          Ocsigen_http_frame.res_headers =
                           Http_headers.replace 
                             Http_headers.content_range
                             ("bytes "^Int64.to_string beg^"-"^
                                Int64.to_string endc^"/"^
                                Int64.to_string cl)
                             res.Ocsigen_http_frame.res_headers;
                          Ocsigen_http_frame.res_content_length = Some length
                       }
                  )
                  (function
                     | Range_416 ->
(* RFC 2616 When this status code is returned for a byte-range request, the response SHOULD include a Content-Range entity-header field specifying the current length of the selected resource *)
                         let dr = Ocsigen_http_frame.default_result () in
                         Lwt.return
                           {dr with
                              Ocsigen_http_frame.res_code = 416;
                              Ocsigen_http_frame.res_headers =
                               Http_headers.replace 
                                 Http_headers.content_range
                                 ("bytes */"^Int64.to_string cl)
                                 res.Ocsigen_http_frame.res_headers;
                           }
                     | e -> Lwt.fail e)

  end
