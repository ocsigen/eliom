(* Ocsigen
 * http://www.ocsigen.org
 * Module preemptive.ml
 * Copyright (C) 2005 Nataliya Guts, Vincent Balat, Jérôme Vouillon
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

open Lwt;;

exception Task_failed

let maxthreads : int ref = ref 0

let mypipe () = 
  let (in_fd, out_fd) as p = Unix.pipe() in
  if not (Sys.os_type <> "Unix") then begin
     Unix.set_nonblock in_fd
  end;
  p

let finishedpipe = mypipe ()   
let worker_chan n : (unit -> unit) Event.channel= Event.new_channel () 
let clients : unit Lwt.t array ref = ref [||]
let taskchannels : (unit -> unit) Event.channel array ref = ref [||]
let busy : bool array ref = ref [||]

let busylock = Mutex.create () 
let setbusy n b = 
  Mutex.lock busylock; 
  !busy.(n) <- b;
  Mutex.unlock busylock

let rec worker (n : int) : unit =
	let me = Thread.id (Thread.self ()) in
	let g = Event.sync (Event.receive !taskchannels.(n)) in
	g ();
	let buf = string_of_int n in
	let r = Unix.write (snd finishedpipe) buf 0 (String.length buf) in
	setbusy n false;
	worker n	
let workers : Thread.t array ref = ref [||]


exception All_preemptive_threads_are_busy
let rec free () : int Lwt.t =
    let rec free1 i : int = 
	if i >= !maxthreads 
	then raise All_preemptive_threads_are_busy 
	else if not !busy.(i) then i else free1 (i+1) 
    in
    try
      let libre = 
	Mutex.lock busylock; 
	let f = free1 0 in
	Mutex.unlock busylock; f in
      setbusy libre true; Lwt.return libre
    with e -> Mutex.unlock busylock; fail e

let detach (f : 'a -> 'b) (args : 'a) : 'b Lwt.t = 
  let res : 'b option ref = ref None in
  let exc : exn option ref = ref None in
  let g () = try res := Some (f args) with e -> exc := Some e
  in
  free () >>= (fun whatthread ->
    Event.sync (Event.send !taskchannels.(whatthread) g);
    !clients.(whatthread) <- Lwt.wait ();
    !clients.(whatthread) >>= 
    (fun () -> match !res with 
      None -> 
	(match !exc with
	  None -> assert false; fail Task_failed
	| Some e -> fail e)
    | Some r -> Lwt.return r))

let try_type x = 
    let tag = Obj.tag (Obj.repr x) in
    begin
    if tag = Obj.int_tag then print_string "int"
    else 
      if tag = Obj.string_tag then print_string "string"
      else print_string "other"
      end;
    print_newline ()

let sizebuf = 20
let buf = String.create sizebuf
let dispatch maxthreads = 
  let rec aux () =
    (catch
       (fun () ->
	 (Lwt_unix.read
	    (Lwt_unix.Plain (fst finishedpipe)) buf 0 sizebuf) >>=
	 (fun n ->
	   return
	     (wakeup !clients.(int_of_string (String.sub buf 0 n)) ())))
       (fun e -> (* error message? *) return ())
    ) >>= (fun () -> aux ())
  in aux ()


let init max = 
	clients := Array.create max (Lwt.return ());
	taskchannels := Array.init max worker_chan;
	busy := Array.make max false;
	workers := Array.init max (Thread.create worker);
	maxthreads := max;
	dispatch max
	

let nbthreads () = 
  Array.length !clients

let nbbusy () =
  Mutex.lock busylock; 
  let r = Array.fold_left (fun nb elt -> if elt then nb+1 else nb) 0 !busy in
  Mutex.unlock busylock;
  r
