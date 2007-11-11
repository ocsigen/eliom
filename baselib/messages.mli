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


(** Write a message in access.log *)
val accesslog : string -> unit Lwt.t

(** Write a message in errors.log *)
val errlog : string -> unit Lwt.t

(** Write a message in warnings.log *)
val warning : string -> unit Lwt.t

(** Write a message only in debugging mode (-V option) - Non cooperative *)
val debug : (unit -> string) -> unit

(** Write a message only in debugging mode (-V option) - Non cooperative *)
val debug2 : string -> unit

(** Same as [debug] without new line at the end - Non cooperative *)
val debug_noel : (unit -> string) -> unit

(** Same as [debug] without new line at the end - Non cooperative *)
val debug_noel2 : string -> unit

(** Write a message in the console (if not called in silent mode) *)
val console : (unit -> string) -> unit Lwt.t

(** Write a message in the console (if not called in silent mode) *)
val console2 : string -> unit Lwt.t

(** Use that function for all impossible cases in exception handlers
   ([try ... with ... | e -> unexpected_exception ...] or [Lwt.catch ...]).
   A message will be written in [warnings.log].
   Put something in the string to help locating the problem (usually the name
   of the function where is has been called).
 *)
val unexpected_exception : exn -> string -> unit Lwt.t


(**/**)

val access : string * Lwt_chan.out_channel ref * Lwt_unix.file_descr ref
val warningfile : string * Lwt_chan.out_channel ref * Lwt_unix.file_descr ref
val error : string * Lwt_chan.out_channel ref * Lwt_unix.file_descr ref

val open_files : unit -> unit
val bip : int -> unit
