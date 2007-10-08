(*
Non-blocking I/O and select does not (fully) work under Windows.
The libray therefore does not use them under Windows, and will
therefore have the following limitations:
- No read will be performed while there are some threads ready to run
  or waiting to write;
- When a read is pending, everything else will be blocked: [sleep]
  will not terminate and other reads will not be performed before
  this read terminates;
- A write on a socket or a pipe can block the execution of the program
  if the data are never consumed at the other end of the connection.
  In particular, if both ends use this library and write at the same
  time, this could result in a dead-lock.
- [connect] is blocking
*)
let windows_hack = Sys.os_type <> "Unix"

module SleepQueue =
  Pqueue.Make (struct
    type t = float * unit Lwt.t
    let compare (t, _) (t', _) = compare t t'
  end)
let sleep_queue = ref SleepQueue.empty
let new_sleeps = ref []

let sleep d =
  let res = Lwt.wait () in
  let t = if d <= 0. then 0. else Unix.gettimeofday () +. d in
  new_sleeps := (t, res) :: !new_sleeps;
  res

let yield () = sleep 0.

let get_time t =
  if !t = -1. then t := Unix.gettimeofday ();
  !t

let in_the_past now t =
  t = 0. || t <= get_time now

let rec restart_threads now =
  match
    try Some (SleepQueue.find_min !sleep_queue) with Not_found -> None
  with
    Some (time, thr) when in_the_past now time ->
      sleep_queue := SleepQueue.remove_min !sleep_queue;
      Lwt.wakeup thr ();
      restart_threads now
  | _ ->
      ()

(****)

module FdMap =
  Map.Make (struct type t = Unix.file_descr let compare = compare end)

type watchers = (unit -> unit) list ref FdMap.t ref

let inputs = ref FdMap.empty
let outputs = ref FdMap.empty

exception Retry
exception Retry_write
exception Retry_read

let find_actions set fd =
  try
    FdMap.find fd !set
  with Not_found ->
    let res = ref [] in
    set := FdMap.add fd res !set;
    res

type 'a outcome =
    Success of 'a
  | Exn of exn
  | Requeued

let rec wrap_syscall set fd cont action =
  let res =
    try
      Success (action ())
    with
      Retry
    | Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK | Unix.EINTR), _, _)
    | Sys_blocked_io ->
        (* EINTR because we are catching SIG_CHLD hence the system call
           might be interrupted to handle the signal; this lets us restart
           the system call eventually. *)
        add_action set fd cont action;
        Requeued
    | Retry_read ->
        add_action inputs fd cont action;
        Requeued
    | Retry_write ->
        add_action outputs fd cont action;
        Requeued
    | e ->
        Exn e
  in
  match res with
    Success v ->
      Lwt.wakeup cont v
  | Exn e ->
      Lwt.wakeup_exn cont e
  | Requeued ->
      ()

and add_action set fd cont action =
  let actions = find_actions set fd in
  actions := (fun () -> wrap_syscall set fd cont action) :: !actions

let register_action set fd action =
  let cont = Lwt.wait () in
  add_action set fd cont action;
  cont

let perform_actions set fd =
  let actions = find_actions set fd in
  set := FdMap.remove fd !set;
  List.iter (fun f -> f ()) !actions

let active_descriptors set = FdMap.fold (fun key _ l -> key :: l) !set []

let blocked_thread_count set =
  FdMap.fold (fun key l c -> List.length !l + c) !set 0

(****)

let wait_children = ref []

let child_exited = ref false
let _ =
  if not windows_hack then
    ignore (Sys.signal Sys.sigchld
              (Sys.Signal_handle (fun _ -> child_exited := true)))

let bad_fd fd =
  try ignore (Unix.LargeFile.fstat fd); false with
    Unix.Unix_error (_, _, _) ->
      true

let rec run thread =
  match Lwt.poll thread with
    Some v ->
      v
  | None ->
      sleep_queue :=
        List.fold_left
          (fun q e -> SleepQueue.add e q) !sleep_queue !new_sleeps;
      new_sleeps := [];
      let next_event =
        try
          let (time, _) = SleepQueue.find_min !sleep_queue in Some time
        with Not_found ->
          None
      in
      let now = ref (-1.) in
      let delay =
        match next_event with
          None      -> -1.
        | Some 0.   -> 0.
        | Some time -> max 0. (time -. get_time now)
      in
      let infds = active_descriptors inputs in
      let outfds = active_descriptors outputs in
      let (readers, writers, _) =
        if windows_hack then
          let writers = outfds in
          let readers =
            if delay = 0. || writers <> [] then [] else infds in
          (readers, writers, [])
        else if infds = [] && outfds = [] && delay = 0. then
          ([], [], [])
        else
          try
            let (readers, writers, _) as res =
              Unix.select infds outfds [] delay in
            if delay > 0. && !now <> -1. && readers = [] && writers = [] then
              now := !now +. delay;
            res
          with
            Unix.Unix_error (Unix.EINTR, _, _) ->
              ([], [], [])
          | Unix.Unix_error (Unix.EBADF, _, _) ->
              (List.filter bad_fd infds, List.filter bad_fd outfds, [])
      in
      restart_threads now;
      List.iter (fun fd -> perform_actions inputs fd) readers;
      List.iter (fun fd -> perform_actions outputs fd) writers;
      if !child_exited then begin
        child_exited := false;
        let l = !wait_children in
        wait_children := [];
        List.iter
          (fun ((cont, flags, pid) as e) ->
             try
               let (pid', _) as v = Unix.waitpid flags pid in
               if pid' = 0 then
                 wait_children := e :: !wait_children
               else
                 Lwt.wakeup cont v
             with e ->
               Lwt.wakeup_exn cont e)
          l
      end;
      run thread

(****)

type file_descr = Unix.file_descr

let unix_file_descr fd = fd
let of_unix_file_descr fd =
  if not windows_hack then Unix.set_nonblock fd;
  fd

let wait_read ch = register_action inputs ch (fun () -> ())

let wait_write ch = register_action outputs ch (fun () -> ())

let read ch buf pos len =
  try
    if windows_hack then raise (Unix.Unix_error (Unix.EAGAIN, "", ""));
    Lwt.return (Unix.read ch buf pos len)
  with
    Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) ->
      register_action inputs ch (fun () -> Unix.read ch buf pos len)
  | e ->
      Lwt.fail e

let write ch buf pos len =
  try
    Lwt.return (Unix.write ch buf pos len)
  with
    Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) ->
      register_action outputs ch (fun () -> Unix.write ch buf pos len)
  | e ->
      Lwt.fail e

let pipe () =
  let (out_fd, in_fd) as fd_pair = Unix.pipe() in
  if not windows_hack then begin
    Unix.set_nonblock in_fd;
    Unix.set_nonblock out_fd
  end;
  fd_pair

let pipe_in () =
  let (out_fd, in_fd) as fd_pair = Unix.pipe() in
  if not windows_hack then Unix.set_nonblock out_fd;
  fd_pair

let pipe_out () =
  let (out_fd, in_fd) as fd_pair = Unix.pipe() in
  if not windows_hack then Unix.set_nonblock in_fd;
  fd_pair

let socket dom typ proto =
  let s = Unix.socket dom typ proto in
  if not windows_hack then Unix.set_nonblock s;
  Lwt.return s

let shutdown ch shutdown_command = Unix.shutdown ch shutdown_command

let socketpair dom typ proto =
  let (s1, s2) as spair = Unix.socketpair dom typ proto in
  if not windows_hack then begin
    Unix.set_nonblock s1; Unix.set_nonblock s2
  end;
  Lwt.return spair

let accept ch =
  register_action inputs ch
    (fun () ->
       let (s, _) as v = Unix.accept ch in
       if not windows_hack then Unix.set_nonblock s;
       v)

let check_socket ch =
  register_action outputs ch
    (fun () ->
       try ignore (Unix.getpeername ch) with
         Unix.Unix_error (Unix.ENOTCONN, _, _) ->
           (* Get the socket error *)
           ignore (Unix.read ch " " 0 1))

let connect s addr =
  try
    Unix.connect s addr;
    Lwt.return ()
  with
    Unix.Unix_error
      ((Unix.EINPROGRESS | Unix.EWOULDBLOCK | Unix.EAGAIN), _, _) ->
        check_socket s
  | e ->
      Lwt.fail e

let _waitpid flags pid =
  try
    Lwt.return (Unix.waitpid flags pid)
  with e ->
    Lwt.fail e

let waitpid flags pid =
  if List.mem Unix.WNOHANG flags || windows_hack then
    _waitpid flags pid
  else
    let flags = Unix.WNOHANG :: flags in
    Lwt.bind (_waitpid flags pid) (fun ((pid', _) as res) ->
    if pid' <> 0 then
      Lwt.return res
    else
      let res = Lwt.wait () in
      wait_children := (res, flags, pid) :: !wait_children;
      res)

let wait () = waitpid [] (-1)

let system cmd =
  match Unix.fork () with
     0 -> begin try
            Unix.execv "/bin/sh" [| "/bin/sh"; "-c"; cmd |]
          with _ ->
            exit 127
          end
  | id -> Lwt.bind (waitpid [] id) (fun (pid, status) -> Lwt.return status)

let close = Unix.close
let setsockopt = Unix.setsockopt
let bind = Unix.bind
let listen = Unix.listen
let set_close_on_exec = Unix.set_close_on_exec

(****)

let out_channel_of_descr fd =
  Lwt_chan.make_out_channel (fun buf pos len -> write fd buf pos len)
let in_channel_of_descr fd =
  Lwt_chan.make_in_channel (fun buf pos len -> read fd buf pos len)

(*
type popen_process =
    Process of in_channel * out_channel
  | Process_in of in_channel
  | Process_out of out_channel
  | Process_full of in_channel * out_channel * in_channel

let popen_processes = (Hashtbl.create 7 : (popen_process, int) Hashtbl.t)

let open_proc cmd proc input output toclose =
  match Unix.fork () with
     0 -> if input <> Unix.stdin then begin
            Unix.dup2 input Unix.stdin;
            Unix.close input
          end;
          if output <> Unix.stdout then begin
            Unix.dup2 output Unix.stdout;
            Unix.close output
          end;
          List.iter Unix.close toclose;
          Unix.execv "/bin/sh" [| "/bin/sh"; "-c"; cmd |]
  | id -> Hashtbl.add popen_processes proc id

let open_process_in cmd =
  let (in_read, in_write) = pipe_in () in
  let inchan = in_channel_of_descr in_read in
  open_proc cmd (Process_in inchan) Unix.stdin in_write [in_read];
  Unix.close in_write;
  Lwt.return inchan

let open_process_out cmd =
  let (out_read, out_write) = pipe_out () in
  let outchan = out_channel_of_descr out_write in
  open_proc cmd (Process_out outchan) out_read Unix.stdout [out_write];
  Unix.close out_read;
  Lwt.return outchan

let open_process cmd =
  let (in_read, in_write) = pipe_in () in
  let (out_read, out_write) = pipe_out () in
  let inchan = in_channel_of_descr in_read in
  let outchan = out_channel_of_descr out_write in
  open_proc cmd (Process(inchan, outchan)) out_read in_write
                                           [in_read; out_write];
  Unix.close out_read;
  Unix.close in_write;
  Lwt.return (inchan, outchan)

let open_proc_full cmd env proc input output error toclose =
  match Unix.fork () with
     0 -> Unix.dup2 input Unix.stdin; Unix.close input;
          Unix.dup2 output Unix.stdout; Unix.close output;
          Unix.dup2 error Unix.stderr; Unix.close error;
          List.iter Unix.close toclose;
          Unix.execve "/bin/sh" [| "/bin/sh"; "-c"; cmd |] env
  | id -> Hashtbl.add popen_processes proc id

let open_process_full cmd env =
  let (in_read, in_write) = pipe_in () in
  let (out_read, out_write) = pipe_out () in
  let (err_read, err_write) = pipe_in () in
  let inchan = in_channel_of_descr in_read in
  let outchan = out_channel_of_descr out_write in
  let errchan = in_channel_of_descr err_read in
  open_proc_full cmd env (Process_full(inchan, outchan, errchan))
                 out_read in_write err_write [in_read; out_write; err_read];
  Unix.close out_read;
  Unix.close in_write;
  Unix.close err_write;
  Lwt.return (inchan, outchan, errchan)

let find_proc_id fun_name proc =
  try
    let pid = Hashtbl.find popen_processes proc in
    Hashtbl.remove popen_processes proc;
    pid
  with Not_found ->
    raise (Unix.Unix_error (Unix.EBADF, fun_name, ""))

let close_process_in inchan =
  let pid = find_proc_id "close_process_in" (Process_in inchan) in
  close_in inchan;
  Lwt.bind (waitpid [] pid) (fun (_, status) -> Lwt.return status)

let close_process_out outchan =
  let pid = find_proc_id "close_process_out" (Process_out outchan) in
  close_out outchan;
  Lwt.bind (waitpid [] pid) (fun (_, status) -> Lwt.return status)

let close_process (inchan, outchan) =
  let pid = find_proc_id "close_process" (Process(inchan, outchan)) in
  close_in inchan; close_out outchan;
  Lwt.bind (waitpid [] pid) (fun (_, status) -> Lwt.return status)

let close_process_full (inchan, outchan, errchan) =
  let pid =
    find_proc_id "close_process_full"
                 (Process_full(inchan, outchan, errchan)) in
  close_in inchan; close_out outchan; close_in errchan;
  Lwt.bind (waitpid [] pid) (fun (_, status) -> Lwt.return status)
*)

(****)

(* Monitoring functions *)
let inputs_length () = blocked_thread_count inputs
let outputs_length () = blocked_thread_count outputs
let wait_children_length () = List.length !wait_children
let get_new_sleeps () = List.length !new_sleeps
let sleep_queue_size () = SleepQueue.size !sleep_queue
