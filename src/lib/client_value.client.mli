(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2011 Grégoire Henry
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

(** {2 Client and shared values}

    See the {{!page-"eliom-language"}manual}. *)

type 'a t = 'a
(** An ['a] client value on the client is just an ['a].  See also {{!Client_value.t}the abstract representation on
    the server}. *)

exception Exception_on_server of string
(** This exception is raised (in Lwt) on the client if a call to a
    server function {!Client.server_function} fails (in Lwt) on the server
    side.

    The argument describes the original exception by
    {!Printexc.to_string}. *)

(** Event handlers like {!Content.Html.F.a_onclick} may raise [False] to cancel the event (as if the JavaScript
    function returned [false]). *)

(* Cannot re-export exception Lib.False,
   cf. http://caml.inria.fr/mantis/view.php?id=5778 *)
(* (\** See {!Lib.False}. *\) *)
exception False

(**/**)

type injection_datum = Runtime.injection_datum
type global_data2 (* Global data only needed while unwrapping *)
