open Lwt;;

exception Task_failed

let maxthreads = 2 

let mypipe () = 
  let (in_fd, out_fd) as p = Unix.pipe() in
  if not (Sys.os_type <> "Unix") then begin
     Unix.set_nonblock in_fd
  end;
  p

let finishedpipe = mypipe ()   
let clients : unit Lwt.t array = Array.create maxthreads (Lwt.return ())
let worker_chan n : (unit -> unit) Event.channel= Event.new_channel () 
let taskchannels : (unit -> unit) Event.channel array = 
	Array.init maxthreads worker_chan
let busy : bool array = Array.make maxthreads false

let busylock = Mutex.create () 

let setbusy n b = Mutex.lock busylock; busy.(n) <- b;
		Mutex.unlock busylock

let rec worker (n : int) : unit =
	let me = Thread.id (Thread.self ()) in
	let g = Event.sync (Event.receive taskchannels.(n)) in
	g ();
	let buf = string_of_int n in
	let r = Unix.write (snd finishedpipe) buf 0 (String.length buf) in
	setbusy n false;
	worker n
;;
let workers : Thread.t array= Array.init maxthreads (Thread.create worker);;

let rec free () : int Lwt.t =
    let rec free1 i : int = 
	if i >= maxthreads then -1 else
	if not busy.(i) then i else free1 (i+1) in
    let libre = Mutex.lock busylock; 
    		let f = free1 0 in Mutex.unlock busylock; f in
    if libre = -1 then Lwt_unix.sleep 1.0 >>= (fun () -> free ())
    else begin setbusy libre true; Lwt.return libre end
;;

let detach (f : 'a -> 'b) (args : 'a) : 'b Lwt.t = 
	let res: 'b option ref = ref None in
	let ch = Event.new_channel () in 

	let g () = res := Some (f args) in

	free () >>= (fun whatthread ->
	Event.sync (Event.send taskchannels.(whatthread) g);
	clients.(whatthread) <- Lwt.wait ();
	clients.(whatthread) >>= 
	(fun _ -> match !res with None -> Lwt.fail Task_failed | Some r -> Lwt.return r))
;;

let try_type x = 
    let tag = Obj.tag (Obj.repr x) in
    begin
    if tag = Obj.int_tag then print_string "int"
    else 
      if tag = Obj.string_tag then print_string "string"
      else print_string "other"
      end;
    print_newline ();
;;

let buf = String.create maxthreads ;;
let rec dispatch () = 
  (catch 
  (fun () -> Lwt_unix.read (Lwt_unix.Plain (fst finishedpipe)) buf 0 maxthreads)
  (function e -> Lwt.fail e)) >>= 
  (fun n ->
  let who = try 
    if n = maxthreads then int_of_string buf
    else int_of_string (String.sub buf 0 n)
    with e -> -1 in
  if who >= 0 then Lwt.wakeup clients.(who) ();
  dispatch ())
;;

(*  
let rec launch n = 
   if n <= 0 then Lwt.return ()
   else begin
     detach f2 (string_of_int n, 2) >>= (fun x1 -> print_string x1; try_type x1;Lwt.return ());
     detach f1 (n, 3) >>= (fun x2 -> print_int x2; try_type x2; Lwt.return ());
     launch ( n - 1)
     end
;;

try Lwt_unix.run (
  dispatch ();
  (*launch 1*)
  detach f2 ("c",5) >>= (fun x1 -> print_string x1;
  try_type x1; Lwt.return ());
  (detach f1 (2,2) >>= (fun x2 -> print_int x2;
  try_type x2; Lwt.return ()))
(* (detach f2 ("2",2) >>= (fun x3 -> print_string x3;
  try_type x3; Lwt.return ()))
  *)
  )
with  e -> print_endline (Printexc.to_string e)
*)
