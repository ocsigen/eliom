(* Ocsigen
 * Copyright (C) 2010 Vincent Balat
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

(** Cookies table manipulation (Duplicate of Ocsigen_cookies). *)

open Eliom_lib

(** Map adressed by URL path, used for {!cookieset}. *)
module Cookies : Map.S with type key = Url.path

(** HTTP cookies representation. *)
type cookie = Ocsigen_cookies.cookie =
  | OSet of float option * string * bool
  (** Install a cookies on the client. The float option is the
      timestamp for the expiration date. The string is the value.  If
      the bool is true and the protocol is https, the cookie will be
      secure (it will ask the browser to send it only through secure
      connections).*)
  | OUnset (** Removes cookies from the client. *)

(** Set of cookie, grouped by path. *)
type cookieset = cookie String.Table.t Cookies.t

(** Empty set of cookies. *)
val empty_cookieset : cookie String.Table.t Cookies.t

(** The function [add_cookie path c v cookie_table] adds the cookie
    [c] to the table [cookie_table].  If the cookie is already bound,
    the previous binding disappear. *)
val add_cookie : Url.path -> string -> cookie ->
  cookie String.Table.t Cookies.t ->
  cookie String.Table.t Cookies.t

(** The function [remove_cookie c cookie_table] removes the cookie [c]
    from the table [cookie_table]. {e Warning: it is not equivalent to
    [add_cookie ... OUnset ...]).}
*)
val remove_cookie : Url.path -> string ->
  cookie String.Table.t Cookies.t ->
  cookie String.Table.t Cookies.t

(** The function[add_cookies newcookies oldcookies] adds the cookies
    from [newcookies] to [oldcookies]. If cookies are already bound in
    oldcookies, the previous binding disappear. *)
val add_cookies :
  cookie String.Table.t Cookies.t ->
  cookie String.Table.t Cookies.t ->
  cookie String.Table.t Cookies.t

