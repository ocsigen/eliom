(* Ocsigen
 * Copyright (C) 2005 Vincent Balat
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

(* A page providing infos about the server (number of sessions, uptime...) *)

open Pagesearch
open Ocsigen
open Ocsigen.Xhtml
open Unix

let launchtime = Unix.time ()

let _ = 
  register_new_service
    ~url:[]
    ~get_params:unit
    (fun _ () () ->
      let n = string_of_int (number_of_sessions ()) in
      let tm = Unix.gmtime ((Unix.time ()) -. launchtime) in
      let year = if tm.tm_year>0 then (string_of_int (tm.tm_year - 70))^" years, "
      else "" in
      let days = (string_of_int (tm.tm_yday))^" days, " in
      let hour = (string_of_int (tm.tm_hour))^" hours, " in
      let min = (string_of_int (tm.tm_min))^" min, " in
      let sec = (string_of_int (tm.tm_sec))^" seconds" in
      let uptime = year^days^hour^min^sec in

      let stat = Gc.quick_stat () in
      let size = string_of_int (stat.Gc.heap_words) in
      let maxsize = string_of_int (stat.Gc.top_heap_words) in
      let compactions = string_of_int stat.Gc.compactions in
      let mincol = string_of_int stat.Gc.minor_collections in
      let majcol = string_of_int stat.Gc.major_collections in
      let top_heap_words = string_of_int stat.Gc.top_heap_words in
      let pid = string_of_int (Unix.getpid ()) in
      let fd = 
        try
          let dir = Unix.opendir 
              ("/proc/"^pid^"/fd") in
          let rec aux v =
            try ignore ((* print_endline *) (readdir dir)); aux (v+1) 
            with End_of_file -> v
          in
          let r = 
            try string_of_int ((aux 0) - 2) 
            with e -> ("(Error: "^(Printexc.to_string e)^")") in
          Unix.closedir dir;
          Some r
        with _ -> None
      in
Lwt.return
<<
 <html>
   <head>
   </head>
   <body>
     <h1>Ocsigen server monitoring</h1>
     <p>Uptime: $str:uptime$.</p>
     <p>There are currently $str:n$ sessions.</p>
     <p>Number of clients connected: 
         $str:(string_of_int (get_number_of_connected ()))$.</p>
     <p>PID : $str:pid$</p>
     <p>$str:match fd with
       None -> "Information on file descriptors not accessible in /proc."
     | Some fd -> fd^" file descriptors opened."$</p>
     <h2>GC</h2>
     <p>Size of major heap: $str:size$ words (max: $str:maxsize$).</p>
     <p>Since the beginning:</p>
       <ul><li>$str:mincol$ minor garbage collections,</li>
           <li> $str:majcol$ major collections,</li>
           <li>$str:compactions$ compactions of the heap.</li>
           <li>Maximum size reached by the major heap: $str:top_heap_words$ words.</li>
       </ul>
     <h2>Lwt threads</h2>
     <p>
       $str:(string_of_int (Lwt_unix.inputs_length ()))$ 
             lwt threads waiting for inputs<br/>
       $str:(string_of_int (Lwt_unix.outputs_length ()))$ 
               lwt threads waiting for outputs<br/>
       $str:(string_of_int (Lwt_unix.wait_children_length ()))$
               lwt threads waiting for children<br/>
       $str:(string_of_int (Lwt_unix.sleep_queue_size ()))$
               sleeping lwt threads<br/>
       $str:(string_of_int (Lwt_unix.get_new_sleeps ()))$ new sleeps.<br/>
     </p>
     <h2>Preemptive threads</h2>
     <p>There are currently $str:(string_of_int (Preemptive.nbthreads ()))$ 
             detached threads running
             (min $str:(string_of_int (Ocsiconfig.get_minthreads ()))$,
             max $str:(string_of_int (Ocsiconfig.get_maxthreads ()))$),
      from which $str:(string_of_int (Preemptive.nbthreadsbusy ()))$ are busy.
        $str:(string_of_int (Preemptive.nbthreadsqueued ()))$ computations 
           queued (max 
        $str:(string_of_int (Ocsiconfig.get_max_number_of_threads_queued ()))$).</p>
   </body>
 </html>
>>)

