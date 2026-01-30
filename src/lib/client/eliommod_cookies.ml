(* Ocsigen
 * http://www.ocsigen.org
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

open Js_of_ocaml
open Eliom_lib
include Eliom_cookies_base

(* CCC The tables are indexed by the hostname, not the port appear.
   there are no particular reason. If needed it is possible to add it *)
let cookie_tables :
  (float option * string * bool) Ocsigen_cookie_map.Map_inner.t
    Ocsigen_cookie_map.Map_path.t
    Jstable.t
  =
  Jstable.create ()

module Map (Ord : sig
    type key [@@deriving json]

    val compare : key -> key -> int
  end) =
struct
  type 'a t =
    | Empty
    | Node of {l : 'a t; v : Ord.key; d : 'a; r : 'a t; h : int}
  [@@deriving json]

  let height = function Empty -> 0 | Node {h; _} -> h

  let create l x d r =
    let hl = height l and hr = height r in
    Node {l; v = x; d; r; h = (if hl >= hr then hl + 1 else hr + 1)}

  let bal l x d r =
    let hl = height l and hr = height r in
    if hl > hr + 2
    then
      match l with
      | Empty -> invalid_arg "Map.bal"
      | Node {l = ll; v = lv; d = ld; r = lr; _} -> (
          if height ll >= height lr
          then create ll lv ld (create lr x d r)
          else
            match lr with
            | Empty -> invalid_arg "Map.bal"
            | Node {l = lrl; v = lrv; d = lrd; r = lrr; _} ->
                create (create ll lv ld lrl) lrv lrd (create lrr x d r))
    else if hr > hl + 2
    then
      match r with
      | Empty -> invalid_arg "Map.bal"
      | Node {l = rl; v = rv; d = rd; r = rr; _} -> (
          if height rr >= height rl
          then create (create l x d rl) rv rd rr
          else
            match rl with
            | Empty -> invalid_arg "Map.bal"
            | Node {l = rll; v = rlv; d = rld; r = rlr; _} ->
                create (create l x d rll) rlv rld (create rlr rv rd rr))
    else Node {l; v = x; d; r; h = (if hl >= hr then hl + 1 else hr + 1)}

  let rec add x data = function
    | Empty -> Node {l = Empty; v = x; d = data; r = Empty; h = 1}
    | Node {l; v; d; r; h} as m ->
        let c = Ord.compare x v in
        if c = 0
        then if d == data then m else Node {l; v = x; d = data; r; h}
        else if c < 0
        then
          let ll = add x data l in
          if l == ll then m else bal ll v d r
        else
          let rr = add x data r in
          if r == rr then m else bal l v d rr

  let rec fold f m accu =
    match m with
    | Empty -> accu
    | Node {l; v; d; r; _} -> fold f r (f v d (fold f l accu))

  let empty = Empty
end

[@@@warning "-39"]

module Map_path = Map (struct
    type key = string list [@@deriving json]

    let compare = compare
  end)

module Map_inner = Map (struct
    type key = string [@@deriving json]

    let compare = compare
  end)

[@@@warning "+39"]

let json_cookies =
  [%json: (float option * string * bool) Map_inner.t Map_path.t]

let extern_cookies c =
  Ocsigen_cookie_map.Map_path.fold
    (fun path inner m ->
       Map_path.add path
         (Ocsigen_cookie_map.Map_inner.fold Map_inner.add inner Map_inner.empty)
         m)
    c Map_path.empty

let intern_cookies c =
  Map_path.fold
    (fun path inner m ->
       Ocsigen_cookie_map.Map_path.add path
         (Map_inner.fold Ocsigen_cookie_map.Map_inner.add inner
            Ocsigen_cookie_map.Map_inner.empty)
         m)
    c Ocsigen_cookie_map.Map_path.empty

(** [in_local_storage] implements cookie substitutes for iOS WKWebView *)
let get_table ?(in_local_storage = false) = function
  | None -> Ocsigen_cookie_map.Map_path.empty
  | Some host ->
      if in_local_storage
      then
        let host = Js.string (host ^ "/substitutes") in
        Js.Optdef.case
          Dom_html.window##.localStorage
          (fun () -> Ocsigen_cookie_map.Map_path.empty)
          (fun st ->
             Js.Opt.case
               st##(getItem host)
               (fun () -> Ocsigen_cookie_map.Map_path.empty)
               (fun v ->
                  intern_cookies (of_json ~typ:json_cookies (Js.to_string v))))
      else
        Js.Optdef.get
          (Jstable.find cookie_tables (Js.string host))
          (fun () -> Ocsigen_cookie_map.Map_path.empty)

(** [in_local_storage] implements cookie substitutes for iOS WKWebView *)
let set_table ?(in_local_storage = false) host t =
  match host with
  | None -> ()
  | Some host ->
      if in_local_storage
      then
        let host = Js.string (host ^ "/substitutes") in
        Js.Optdef.case
          Dom_html.window##.localStorage
          (fun () -> ())
          (fun st ->
             st##(setItem host
                    (Js.string (to_json ~typ:json_cookies (extern_cookies t)))))
      else Jstable.add cookie_tables (Js.string host) t

let now () =
  let date = new%js Js.date_now in
  Js.to_float date##getTime /. 1000.

(** [in_local_storage] implements cookie substitutes for iOS WKWebView *)
let update_cookie_table ?(in_local_storage = false) host cookies =
  let now = now () in
  Ocsigen_cookie_map.Map_path.iter
    (fun path table ->
       Ocsigen_cookie_map.Map_inner.iter
         (fun name -> function
            | OSet (Some exp, _, _) when exp <= now ->
                set_table ~in_local_storage host
                  (Ocsigen_cookie_map.Poly.remove ~path name
                     (get_table ~in_local_storage host))
            | OUnset ->
                set_table ~in_local_storage host
                  (Ocsigen_cookie_map.Poly.remove ~path name
                     (get_table ~in_local_storage host))
            | OSet (exp, value, secure) ->
                set_table ~in_local_storage host
                  (Ocsigen_cookie_map.Poly.add ~path name (exp, value, secure)
                     (get_table ~in_local_storage host)))
         table)
    cookies

(** [in_local_storage] implements cookie substitutes for iOS WKWebView *)
let get_cookies_to_send ?(in_local_storage = false) host https path =
  let now = now () in
  Ocsigen_cookie_map.Map_path.fold
    (fun cpath t cookies_to_send ->
       if
         Url.is_prefix_skip_end_slash
           (Url.remove_slash_at_beginning cpath)
           (Url.remove_slash_at_beginning path)
       then
         Ocsigen_cookie_map.Map_inner.fold
           (fun name (exp, value, secure) cookies_to_send ->
              match exp with
              | Some exp when exp <= now ->
                  set_table ~in_local_storage host
                    (Ocsigen_cookie_map.Poly.remove ~path:cpath name
                       (get_table ~in_local_storage host));
                  cookies_to_send
              | _ ->
                  if (not secure) || https
                  then (name, value) :: cookies_to_send
                  else cookies_to_send)
           t cookies_to_send
       else cookies_to_send)
    (get_table ~in_local_storage host)
    []

let make_new_session_id () =
  failwith
    "Cannot define anonymous coservices on client side. Ask their values to the server."
