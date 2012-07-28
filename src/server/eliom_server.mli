(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2012 Benedikt Becker
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


(** Server representation of client values.
    Developer-visible functions should always operate on the synonym
    {% <<a_api subproject="server" | type Eliom_lib.client_value >> %}.
  *)
module Client_value : sig

  type +'a t
  (**/**)
  val create: closure_id:int64 -> instance_id:int -> _ t
  val closure_id: _ t -> int64
  val instance_id: _ t -> int
end
