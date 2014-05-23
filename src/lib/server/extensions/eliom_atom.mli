(* Ocsigen
 * Copyright (C) 2010 Archibald Pontier
 *
 * This source file is part of Ocsigen < http://ocsigen.org/ >
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

(** Register an atom feed *)
module Reg : "sigs/eliom_reg.mli"
  subst type page      := Atom_feed.feed
    and type options := unit
    and type return  := Eliom_registration.http_service
    and type returnB := [> Eliom_registration.http_service ]
    and type returnT := [< Eliom_registration.http_service ]
    and type result  := Eliom_registration.browser_content Eliom_registration.kind

(** Needed when used with Pubsubhubbub *)
type feed = { notify_updates : unit -> unit }

(** Add the needed <link rel="hub" ...> for each hub in the feed, and
 communicate with the hub *)
val register_feed :
  path:string list -> hubs:Atom_feed.uri list -> string ->
  (unit -> Atom_feed.feed Lwt.t) -> feed
