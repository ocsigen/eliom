type t = {dir : string}

let socket_name = "local.sock"
let command_pipe = "local.cmd"
let log_name = "server.log"
let socket {dir} = Filename.concat dir socket_name

let rec remove path =
  match Sys.is_directory path with
  | true ->
      Array.iter (fun f -> remove (Filename.concat path f)) (Sys.readdir path);
      Sys.rmdir path
  | false -> Sys.remove path
  | exception Sys_error _ -> ()

(* The path of a Unix-domain socket is limited to about 100 bytes, less than
   the usual temporary directory of macOS. *)
let temp_base =
  let base = Filename.get_temp_dir_name () in
  if String.length base > 50 && Sys.file_exists "/tmp" then "/tmp" else base

let rec temp_dir n =
  let dir =
    Filename.concat temp_base
      (Printf.sprintf "eliom-test-%d-%d" (Unix.getpid ()) n)
  in
  match Sys.mkdir dir 0o700 with
  | () -> dir
  | exception Sys_error _ when n < 1000 -> temp_dir (n + 1)

let read_file file =
  match In_channel.with_open_bin file In_channel.input_all with
  | s -> s
  | exception Sys_error e -> e

let print_log dir =
  Printf.eprintf "Server log (%s):\n%s\n%!" dir
    (read_file (Filename.concat dir log_name))

let exited pid =
  match Unix.waitpid [Unix.WNOHANG] pid with
  | 0, _ -> false
  | _ -> true
  | exception Unix.Unix_error (Unix.ECHILD, _, _) -> true

(* [wait_for p] waits until [p ()] holds, for 20 s at most. *)
let wait_for p =
  let rec loop n =
    if p ()
    then true
    else if n = 0
    then false
    else (
      Unix.sleepf 0.01;
      loop (n - 1))
  in
  loop 2000

let kill pid =
  Unix.kill pid Sys.sigkill;
  ignore (Unix.waitpid [] pid)

let stop pid dir =
  if not (exited pid)
  then
    (* With O_NONBLOCK, opening the pipe fails rather than blocks if the
       server does not read it. *)
    match
      let fd =
        Unix.openfile
          (Filename.concat dir command_pipe)
          [Unix.O_WRONLY; Unix.O_NONBLOCK]
          0
      in
      Fun.protect
        ~finally:(fun () -> Unix.close fd)
        (fun () -> ignore (Unix.write_substring fd "shutdown\n" 0 9))
    with
    | () ->
        if not (wait_for (fun () -> exited pid))
        then (
          prerr_endline "The test server did not stop: killing it.";
          kill pid)
    | exception Unix.Unix_error (e, _, _) ->
        Printf.eprintf
          "Cannot send the shutdown command to the test server (%s): killing it.\n%!"
          (Unix.error_message e);
        kill pid

(* The server is ready when it accepts connections. Its socket file exists a
   little earlier, as soon as the socket is bound; the command pipe is opened
   before. *)
let accepts_connections server =
  let fd = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Fun.protect
    ~finally:(fun () -> Unix.close fd)
    (fun () ->
       match Unix.connect fd (Unix.ADDR_UNIX (socket server)) with
       | () -> true
       | exception Unix.Unix_error ((Unix.ENOENT | Unix.ECONNREFUSED), _, _) ->
           false)

let with_server exe f =
  let dir = temp_dir 0 in
  let log =
    Unix.openfile
      (Filename.concat dir log_name)
      [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC]
      0o600
  in
  let pid = Unix.create_process exe [|exe; dir|] Unix.stdin log log in
  Unix.close log;
  let server = {dir} in
  if
    not
      (wait_for (fun () -> accepts_connections server || exited pid)
      && not (exited pid))
  then (
    print_log dir;
    stop pid dir;
    failwith ("the test server " ^ exe ^ " did not start"));
  match f server with
  | r -> stop pid dir; remove dir; r
  | exception e ->
      let bt = Printexc.get_raw_backtrace () in
      stop pid dir;
      print_log dir;
      Printexc.raise_with_backtrace e bt

let start instructions =
  let dir = Sys.argv.(1) in
  Sys.chdir dir;
  Sys.mkdir "log" 0o700;
  Sys.mkdir "data" 0o700;
  Ocsigen.Server.start
    ~ports:[`Unix socket_name, 0]
    ~command_pipe ~logdir:"log" ~datadir:"data" ~debugmode:true
    [Ocsigen.Server.host instructions]
