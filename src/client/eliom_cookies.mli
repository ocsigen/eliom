(* Ocsigen
 * Copyright (C) 2011 Pierre Chambart
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

open Eliom_pervasives

type cookie =
  | OSet of float option * string * bool
  | OUnset

type cookieset

val empty_cookieset : cookieset

val add_cookie : Url.path -> string -> cookie -> cookieset -> cookieset

val remove_cookie : Url.path -> string -> cookieset -> cookieset

val add_cookies : cookieset -> cookieset -> cookieset
(** [add_cookies newcookies oldcookies] adds the cookies from
    [newcookies] to [oldcookies]. If cookies are already bound in
    oldcookies, the previous binding disappear. *)
