(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2010 Vincent Balat
 * Copyright (C) 2011 Jérôme Vouillon, Grégoire Henry, Pierre Chambart
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

(** Pages of the application, the navigation history, and the data
    associated to each state of the History API. Internal module. *)

open Js_of_ocaml

(** {2 History states} *)

type state = {template : string option; position : Mod_dom.position}
(** Data stored in the session storage for each state of the History
    API. *)

type state_id = {session_id : int; state_index : int  (** point in history *)}

type saved_state = state_id * string [@@deriving json]
(** The value of [history.state]: the state id and the URL. *)

val session_id : int
(** A random number identifying the current session of the browser
    tab. *)

val history_state : state_id -> string -> Js.js_string Js.t Js.Opt.t
(** [history_state id uri] is the [history.state] value of the page with
    state [id] at [uri]. *)

val get_state : state_id -> state
(** Reads the data of a state from the session storage.
    Raises [Not_found] if there is none. *)

val update_state : unit -> unit
(** Saves the template and the scroll position of the active page in
    the session storage. *)

(** {2 Pages} *)

val section_page : Logs.src

(** See {!Client.Page_status}. *)
module Page_status : sig
  type t = Generating | Active | Cached | Dead

  val signal : unit -> t React.S.t

  module Events : sig
    val active : unit -> unit React.E.t
    val cached : unit -> unit React.E.t
    val dead : unit -> unit React.E.t
    val inactive : unit -> unit React.E.t
  end

  val onactive :
     ?now:bool
    -> ?once:bool
    -> ?stop:unit React.E.t
    -> (unit -> unit)
    -> unit

  val oncached : ?once:bool -> ?stop:unit React.E.t -> (unit -> unit) -> unit
  val ondead : ?stop:unit React.E.t -> (unit -> unit) -> unit
  val oninactive : ?once:bool -> ?stop:unit React.E.t -> (unit -> unit) -> unit

  val while_active :
     ?now:bool
    -> ?stop:unit React.E.t
    -> (unit -> unit Lwt.t)
    -> unit
end

type page =
  { page_unique_id : int
  ; mutable page_id : state_id
  ; mutable url : string
  ; page_status : Page_status.t React.S.t
  ; mutable previous_page : int option
  ; set_page_status : ?step:React.step -> Page_status.t -> unit
  ; mutable dom : Dom_html.bodyElement Js.t option
    (** The DOM of the page, when it is cached *)
  ; mutable reload_function : (unit -> unit -> Service.result Lwt.t) option }

val active_page : page ref
(** The page being displayed. *)

val set_active_page : page -> unit
(** Makes a page the active one, and retires the previous one. *)

val get_this_page : unit -> page
(** The page the running code is generating, or the active page outside
    of a page generation. *)

val with_new_page :
   ?state_id:state_id
  -> ?old_page:page
  -> replace:bool
  -> unit
  -> (unit -> 'a)
  -> 'a
(** [with_new_page ~replace () f] runs [f] in the context of a new page,
    which [get_this_page] returns. With [~replace:true], the new page
    takes the state id of the active page. *)

val advance_page : unit -> unit
(** Makes the page being generated the active one, and adds it to the
    history if it is not already there. *)

(** {2 History} *)

module History : sig
  val find_by_state_index : int -> page option

  val replace : page -> unit
  (** Replaces the page with the same state index. *)

  val past : unit -> string list
  val future : unit -> string list

  val max_num_doms : int option ref
  (** The maximum distance from the active page of the pages whose DOM
      is kept in the cache. *)

  val garbage_collect_doms : unit -> unit
  (** Removes the DOMs too far from the active page from the cache. *)
end
