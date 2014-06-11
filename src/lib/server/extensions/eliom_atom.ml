(* Ocsigen
 * Copyright (C) 2010 Archibald Pontier
 *
 * This source file is part of Ocsigen < http://ocsigen.org/ >
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

open Eliom_lib

module F = Ocsigen_http_frame
module H = Ocsigen_http_frame.Http_header

let get_etag c = Some (Digest.to_hex (Digest.string c))

module Atom_info = struct
  let content_type = "application/atom+xml"
  let version = "Atom 1.0"
  let standard = "http://www.w3.org/2005/Atom"
  let doctype = ""
  let emptytags = []
end

module Format = Xml_print.Make_simple(Xml)(Atom_info)

let result_of_content feed headers =
   let b = Buffer.create 10 in
   Format.print_list ~output:(Buffer.add_string b) [Atom_feed.xml_of_feed feed];
   let c = Buffer.contents b in
   let md5 = get_etag c in
   let dr = Ocsigen_http_frame.Result.default () in
   (Ocsigen_http_frame.Result.update dr
      ~content_length:(Some (Int64.of_int (String.length c)))
      ~content_type:(Some "application/atom+xml")
      ~etag:md5
      ~headers:(match headers with
            | None -> Ocsigen_http_frame.Result.headers dr
            | Some headers ->
            Http_headers.with_defaults headers (Ocsigen_http_frame.Result.headers dr)
            )
      ~stream:
         (Ocsigen_stream.make
          (fun () ->
           Ocsigen_stream.cont c
           (fun () -> Ocsigen_stream.empty None)), None)
      ())

module Reg_base = struct
   type page = Atom_feed.feed
   type options = unit
   type return = Eliom_registration.http_service
   type result = Eliom_registration.browser_content Eliom_registration.kind
   let result_of_http_result = Eliom_registration.cast_http_result
   let send_appl_content = Eliom_service.XNever
   let pre_service ?options () = Lwt.return ()
   let send ?options ?charset ?code ?content_type ?headers
     feed = Lwt.return (result_of_content feed headers )
end

module Reg =  Eliom_mkreg.MakeRegister(Reg_base)

let (>>=) = Lwt.bind

type feed = { notify_updates : unit -> unit }

let retry_after = Http_headers.name "Retry-After"

open CalendarLib

let log_error e = Ocsigen_messages.warning
      ("Eliom_atom: error while contacting hub: " ^ Printexc.to_string e)

let parse_503 header = let r_int = Str.regexp "^[0-9]+$" in
      let r_date = Str.regexp
            "[a-zA-Z]+,.[0-9]+ [a-zA-Z]+ [0-9]+ [0-9]+:[0-9]+:[0-9]+ GMT" in
   if Str.string_match r_int header 0 then Lwt_unix.sleep
            (float_of_string header)
   else if Str.string_match r_date header 0 then let d = Time_Zone.on
            CalendarLib.Calendar.to_unixfloat Time_Zone.UTC
            (CalendarLib.Printer.Calendar.from_fstring
             "%a, %d %b %Y %H:%M:%S GMT" header) in
      let d2 = Unix.gettimeofday () in
      let d3 = d -. d2 in
      if d3 < 0. then failwith "bad retry-after header"
      else if d3 > 7200. then Lwt_unix.sleep 7200.
      else Lwt_unix.sleep d3
   else failwith "bad retry-after header"

let rec ping_hub u address t =
   Lwt.try_bind
     (fun () -> let path = Neturl.join_path (Neturl.url_path u) in
      Ocsigen_http_client.post_urlencoded ~port:(try Neturl.url_port u with
         Not_found -> 80) ~host:(Neturl.url_host u)
                  ~uri:(if path = "" then "/" else path)
                  ~content:[("hub.mode","publish"); ("hub.url",address)] ())
     (fun frame -> match frame.F.frame_header.H.mode with
      | H.Answer 204    -> Lwt.return ()
      | H.Answer 503    -> Lwt.try_bind (fun () -> parse_503 (Http_headers.find
         retry_after frame.F.frame_header.H.headers)) (fun () ->
         ping_hub u address 1.) (fun e -> log_error e ; retry_ping u address t)
      | _               -> retry_ping u address t)
     (fun e -> log_error e ; retry_ping u address t)
   and retry_ping u address t = Lwt_unix.sleep (Random.float t) >>=
         (fun () -> ping_hub u address (t*.2.))

let rec nfu_s hubs address = match hubs with
   | []     -> ()
   | s :: r -> let u = Neturl.parse_url (Xml.string_of_uri s) in ignore (ping_hub u address 1.) ;
      nfu_s r address

let notify_feed_updates address hubs s =
   nfu_s hubs address; ()

let register_feed ~path ~hubs address f =
   let s = Eliom_service.Http.service ~path ~get_params:Eliom_parameter.unit () in
   Reg.register ~service:s
     (fun () () -> f () >>= fun feed -> Lwt.return
       (Atom_feed.insert_hub_links hubs feed));
   notify_feed_updates address hubs s;
   {notify_updates = fun () -> notify_feed_updates address hubs s}
