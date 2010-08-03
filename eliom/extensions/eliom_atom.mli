(* 
 * Copyright (C) 2010 Archibald Pontier
 *
 * This source file is part of Ocsigen < http://ocsigen.org/ >
 *
 * atom is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * atom is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with atom; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 *)

(** Register an atom feed *)
module Reg : Eliom_mkreg.ELIOMREGSIG
        with type page = Atom_feed.feed

(** Needed when used with Pubsubhubbub *)        
type feed = { notify_updates : unit -> unit }

(** Add the needed <link rel="hub" ...> for each hub in the feed, and 
 communicate with the hub *)
val register_feed :
  path:string list -> hubs:Atom_feed.uri list -> string -> 
  (unit -> Atom_feed.feed Lwt.t) -> feed 
