(* Ocsigen
 * Copyright (C) 2005 Vincent Balat
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

(** Writing messages in the logs *)

let (>>=) = Lwt.bind

let access_file = "access.log"
let warning_file = "warnings.log"
let error_file = "errors.log"

let full_path f = Filename.concat (Ocsigen_config.get_logdir ()) f

let error_log_path () = full_path error_file

let stderr = Lwt_log.channel `Keep Lwt_io.stderr ()

let loggers = ref []

let open_files () =
  (* CHECK: we are closing asynchronously!  That should be ok, though. *)
  List.iter (fun l -> ignore (Lwt_log.close l : unit Lwt.t)) !loggers;
  Lwt_log.file (full_path access_file) () >>= fun acc ->
  Lwt_log.file (full_path warning_file) () >>= fun war ->
  Lwt_log.file (full_path error_file) () >>= fun err ->
  loggers := [acc; war; err];
  Lwt_log.default :=
    Lwt_log.broadcast
      [Lwt_log.dispatch
         (fun sect lev ->
            if Lwt_log.Section.name sect = "access" then acc else
            match lev with
              Lwt_log.Error | Lwt_log.Fatal -> err
            | _                             -> war);
       Lwt_log.dispatch
         (fun sect lev ->
            let show =
              match lev with
                Lwt_log.Error | Lwt_log.Fatal ->
                  not (Ocsigen_config.get_silent ())
              | _ ->
                  Ocsigen_config.get_verbose ()
            in
            if show then stderr else Lwt_log.null)];
  Lwt.return ()
                
(****)

let access_sect = Lwt_log.Section.make "access"

let accesslog s =
  ignore (Lwt_log.notice ~section:access_sect s : unit Lwt.t)

let errlog s = ignore (Lwt_log.error s : unit Lwt.t)

let warning s = ignore (Lwt_log.warning s : unit Lwt.t)

let unexpected_exception e s =
  warning ("Unexpected exception in "^s^": "^Ocsigen_lib.string_of_exn e)

(****)

let debug_noel =
  if Ocsigen_config.get_veryverbose () then
    (fun s -> Pervasives.prerr_string (s ()))
  else
    (fun s -> ())

let debug_noel2 =
  if Ocsigen_config.get_veryverbose () then
    Pervasives.prerr_string
  else
    (fun s -> ())

let debug =
  if Ocsigen_config.get_veryverbose () then
    (fun s -> Pervasives.prerr_endline (s ()))
  else
    (fun s -> ())

let debug2 =
  if Ocsigen_config.get_veryverbose () then
    Pervasives.prerr_endline
  else
    (fun s -> ())

let bip =
  if Ocsigen_config.get_veryverbose () then
    (fun i -> Pervasives.prerr_endline ("bip"^(string_of_int i)))
  else
    (fun i -> ())

let console =
  if (not (Ocsigen_config.get_silent ())) then
    (fun s -> print_endline (s ()))
  else
    (fun s -> ())

let console2 =
  if (not (Ocsigen_config.get_silent ())) then
    print_endline
  else
    (fun s -> ())

(*

Re: [Caml-list] log function without evaluate arguments
From: tmp123 <tmp123@menta.net>
To: caml-list@inria.fr
Date: Nov 7 2007, 10:37 am

Hello,

Thanks a lot to everybody for your help.

I've been testing the different proposals. I must recognize I've not yet
reviewed the proposed library, it is next step.

The four methods tested are: lazy, fun, ifprint, and fun moving the "if"
to the caller (see full listing and results at the end of the post). Two
test has been done for each one: when parameter is an integer constant
and when parameter is the result of a funcion call who mades an addition.

The conclusion seems: defining that "lazy" method needs 1 unit of time,
proposal using "fun" instead of lazy needs 0.8, and the version
"ifprintf" needs 16. Proposal moving the "if" needs 0.7.

Thus, if no error has been done, fun is the fastest option, lazy is near.

Another point is the posibility of, using a camlp4 syntax extension, to
introduce a few of sugar. Something like expand:

from: log "some=%d\n" 14;
to: logint ( fun () -> Printf.printf "some=%d\n" 14);
or to: if log_active.val then logint ( fun() -> Printf.printf
"some=%d\n" 14) else ();

Thanks again to everybody.

Full listing and results:

value log_active = ref False;

value log1 exp =
  if log_active.val
  then
    Lazy.force exp
  else ();

value log2 exp =
  if log_active.val
  then
    exp()
  else ();

value log3 fmt =
  if log_active.val
  then
    Printf.printf fmt
  else
    Printf.ifprintf stderr fmt;

value log4 exp = exp ();




value suma a b =
(
  a+b;
);

value some = ref 14;

value test1 () =
  log1 (lazy (Printf.printf "%d" (suma some.val 3)));

value test2 () =
  log2 ( fun () -> Printf.printf "%d" (suma some.val 3));

value test3 () =
  log3 "%d" (suma some.val 3);

value test4 () =
  if log_active.val then log4 ( fun () -> Printf.printf "%d" (suma
some.val 3))
                    else ();

value testb1 () =
  log1 (lazy (Printf.printf "%d" 3));

value testb2 () =
  log2 ( fun () -> Printf.printf "%d" 3);

value testb3 () =
  log3 "%d" 3;

value testb4 () =
  if log_active.val then log4 ( fun () -> Printf.printf "%d" 3)
                    else ();




value loop f =
(
    let t=Unix.times() in
    Printf.printf "%f %f %f\n" (Unix.gettimeofday())
                             t.Unix.tms_utime t.Unix.tms_stime;

    for i = 0 to 1000 do
    for j = 0 to 1000000 do
      f ();
    done;
    done;

    let t=Unix.times() in
    Printf.printf "%f %f %f\n" (Unix.gettimeofday())
                             t.Unix.tms_utime t.Unix.tms_stime;
);

value main () =
(
  Printf.printf "test1\n";
  loop test1;

  Printf.printf "test2\n";
  loop test2;

  Printf.printf "test3\n";
  loop test3;

  Printf.printf "test4\n";
  loop test4;

  Printf.printf "\n";

  Printf.printf "testb1\n";
  loop testb1;

  Printf.printf "testb2\n";
  loop testb2;

  Printf.printf "testb3\n";
  loop testb3;

  Printf.printf "testb4\n";
  loop testb4;

);

main();


Results:

test1
1194426404.657406 0.015000 0.000000
1194426414.136406 9.453000 0.000000
test2
1194426414.137406 9.468000 0.000000
1194426422.147406 17.453000 0.000000
test3
1194426422.147406 17.453000 0.000000
1194426593.308406 188.515000 0.000000
test4
1194426593.308406 188.515000 0.000000
1194426599.964406 195.156000 0.000000

testb1
1194426599.964406 195.156000 0.000000
1194426609.408406 204.609000 0.000000
testb2
1194426609.408406 204.609000 0.000000
1194426617.378406 212.578000 0.000000
testb3
1194426617.378406 212.578000 0.000000
1194426790.412406 385.484000 0.000000
testb4
1194426790.412406 385.484000 0.000000
1194426797.060406 392.125000 0.000000


-------------

_______________________________________________
Caml-list mailing list.

*)

