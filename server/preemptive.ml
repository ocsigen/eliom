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

(* exception Task_failed *)

let minthreads : int ref = ref 0
let maxthreads : int ref = ref 0

let mypipe () = 
  let (in_fd, out_fd) as p = Unix.pipe() in
  if not (Sys.os_type <> "Unix") then begin
     Unix.set_nonblock in_fd
  end;
  p

let finishedpipe = mypipe ()   

let worker_chan n : (unit -> unit) Event.channel= Event.new_channel () 
type th = {mutable client: unit Lwt.t;
	   mutable busy: bool;
	   taskchannel: (unit -> unit) Event.channel;
	   mutable worker: Thread.t option}
let pool : th array ref = ref [||]

let busylock = Mutex.create () 
let setbusy n b = 
  Mutex.lock busylock; 
  !pool.(n).busy <- b;
  Mutex.unlock busylock

let rec worker (n : int) : unit =
  let g = Event.sync (Event.receive !pool.(n).taskchannel) in
  g ();
  let buf = string_of_int n in
  ignore (Unix.write (snd finishedpipe) buf 0 (String.length buf));
  setbusy n false;
  worker n	


exception All_preemptive_threads_are_busy
let free,nbthreadsqueued =
  let nb_threads_queued = ref 0 in
  let max_thread_waiting_queue = 
    Ocsiconfig.get_max_number_of_threads_queued () in
  let rec free1 i : int = 
    if i >= !maxthreads
    then raise All_preemptive_threads_are_busy 
    else if not !pool.(i).busy then i else free1 (i+1) 
  in
  let launch_threads first = 
    let rec aux last n =
      !pool.(n).worker <- Some (Thread.create worker n);
      if n<last then aux last (n+1)
    in
    match !pool.(first).worker with
      None -> 
	let last = (min (first + (max !minthreads 10)) !maxthreads) - 1 in
	Messages.debug 
	  ("Creating "^(string_of_int (last - first + 1))^" threads.");
	aux last first
    | _ -> ()
  in
  let rec aux () =
    try
      let libre = 
	Mutex.lock busylock; 
	let f = free1 0 in
	Mutex.unlock busylock; f in
      setbusy libre true;
      launch_threads libre;
      Lwt.return libre
    with
      All_preemptive_threads_are_busy -> 
	Mutex.unlock busylock;
	if (!maxthreads = 0) || 
	(!nb_threads_queued >= max_thread_waiting_queue)
	then fail All_preemptive_threads_are_busy
	else (nb_threads_queued := !nb_threads_queued + 1;
	      Lwt_unix.sleep 1.0 >>= (fun () -> 
		nb_threads_queued := !nb_threads_queued -1 ; 
		aux ()))
    | e -> Mutex.unlock busylock; fail e
  in
  (aux,(fun () -> !nb_threads_queued))

let detach (f : 'a -> 'b) (args : 'a) : 'b Lwt.t = 
  let res : 'b option ref = ref None in
  let exc : exn option ref = ref None in
  let g () = try res := Some (f args) with e -> exc := Some e
  in
  free () >>= (fun whatthread ->
    Event.sync (Event.send !pool.(whatthread).taskchannel g);
    !pool.(whatthread).client <- Lwt.wait ();
    !pool.(whatthread).client >>= 
    (fun () -> match !res with 
      None -> 
	(match !exc with
	  None -> assert false (*; fail Task_failed *)
	| Some e -> fail e)
    | Some r -> Lwt.return r))


let sizebuf = 20
let buf = String.create sizebuf
let dispatch () = 
  let rec aux () =
    (catch
       (fun () ->
	 (Lwt_unix.read
	    (Lwt_unix.Plain (fst finishedpipe)) buf 0 sizebuf) >>=
	 (fun n ->
	   return
	     (wakeup !pool.(int_of_string (String.sub buf 0 n)).client ())))
       (fun e -> Messages.errlog ("Internal error in preemptive.ml (read failed on the pipe) "^(Printexc.to_string e)^" - Please report the bug"); return ())
    ) >>= (fun () -> aux ())
  in aux ()


let initthread =
  let def = Lwt.return () in
  fun n ->
    {client = def;
     busy = false;
     taskchannel = worker_chan n;
     worker = None}

let rec start_initial_threads min n =
  if n<min
  then begin
    !pool.(n).worker <- Some (Thread.create worker n);
    start_initial_threads min (n+1);
  end

let init min max =
  pool := Array.init max initthread;
  minthreads := min;
  maxthreads := max;
  start_initial_threads min 0;
  dispatch ()


let nbthreads () = 
  Array.fold_left (fun nb elt -> 
    match elt.worker with None -> nb | _ -> nb+1) 0 !pool

let nbthreadsbusy () =
  Mutex.lock busylock; 
  let r = 
    Array.fold_left (fun nb elt -> if elt.busy then nb+1 else nb) 0 !pool in
  Mutex.unlock busylock;
  r
