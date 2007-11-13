(* Ocsigen
 * Copyright (C) 2005 Vincent Balat
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


(** Writing messages in the logs *)

let access = "access.log", ref stdout, ref Unix.stdout
let warningfile = "warnings.log", ref stderr, ref Unix.stderr
let error = "errors.log", ref stderr, ref Unix.stderr


(* Several processes will access the same files, but if I am right,
   it is not a problem when opening with O_APPEND
 *)
let open_files =
  let opened = ref false in
  let openlog f =
    Unix.openfile
      ((Ocsiconfig.get_logdir ())^"/"^f)
      [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_APPEND] 0o640
  in
  fun () ->
    if !opened
    then begin
      Unix.close !(Ocsimisc.thd3 access);
      Unix.close !(Ocsimisc.thd3 warningfile);
      Unix.close !(Ocsimisc.thd3 error)
    end;
    opened := true;
    let acc = openlog (Ocsimisc.fst3 access) in
    let war = openlog (Ocsimisc.fst3 warningfile) in
    let err = openlog (Ocsimisc.fst3 error) in
    Ocsimisc.snd3 access := Unix.out_channel_of_descr acc;
    Ocsimisc.snd3 warningfile := Unix.out_channel_of_descr war;
    Ocsimisc.snd3 error := Unix.out_channel_of_descr err;
    Ocsimisc.thd3 access := acc;
    Ocsimisc.thd3 warningfile := war;
    Ocsimisc.thd3 error := err;
    Unix.set_close_on_exec acc;
    Unix.set_close_on_exec war;
    Unix.set_close_on_exec err
        
let log_aux file console_print s =
  let date = 
    let t = Unix.localtime (Unix.time ()) in
    Printf.sprintf 
      "%02d-%02d-%04d %02d:%02d:%02d" 
      t.Unix.tm_mday 
      (t.Unix.tm_mon + 1)
      (1900 + t.Unix.tm_year)
      t.Unix.tm_hour
      t.Unix.tm_min
      t.Unix.tm_sec 
  in
  let s = date^" - "^s^"\n" in
  if console_print then prerr_endline ("["^(Ocsimisc.fst3 file)^"] "^s);
  output_string !(Ocsimisc.snd3 file) s;
  output_string !(Ocsimisc.snd3 file) "\n";
  flush !(Ocsimisc.snd3 file)

      
let accesslog s =
  log_aux access (Ocsiconfig.get_verbose ()) s

let errlog s =
  log_aux error (not (Ocsiconfig.get_silent ())) s

let warning s =
  log_aux warningfile (Ocsiconfig.get_verbose ()) s

(*
let lwtlog = 
  fun s ->
    let s = s^"\n" in
    let syslog = Syslog.openlog ~facility:`LOG_DAEMON ~logpath:????
           ~flags:[ `LOG_CONS ] "ocsigen" in
    Syslog.syslog syslog `LOG_NOTICE s;
    Syslog.closelog syslog
*)


let debug_noel =
  if Ocsiconfig.get_veryverbose () then
    (fun s -> Pervasives.prerr_string (s ()))
  else
    (fun s -> ())

let debug_noel2 =
  if Ocsiconfig.get_veryverbose () then
    Pervasives.prerr_string
  else
    (fun s -> ())

let debug =
  if Ocsiconfig.get_veryverbose () then
    (fun s -> Pervasives.prerr_endline (s ()))
  else 
    (fun s -> ())

let debug2 =
  if Ocsiconfig.get_veryverbose () then
    Pervasives.prerr_endline
  else 
    (fun s -> ())

let bip = 
  if Ocsiconfig.get_veryverbose () then
    (fun i -> Pervasives.prerr_endline ("bip"^(string_of_int i)))
  else
    (fun i -> ())

let console =
  if (not (Ocsiconfig.get_silent ())) then
    (fun s -> print_endline (s ()))
  else 
    (fun s -> ())

let console2 =
  if (not (Ocsiconfig.get_silent ())) then
    print_endline
  else 
    (fun s -> ())

let unexpected_exception e s =
  warning ("Unexpected exception in "^s^": "^Ocsimisc.string_of_exn e)



(*

Re: [Caml-list] log function without evaluate arguments
De : 
tmp123 <tmp123@menta.net>
  À : 
caml-list@inria.fr
  Date : 
Aujourd'hui 11:31:04
   
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

