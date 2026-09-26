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

open Js_of_ocaml
open Lib

let section = Client_core.section

(* == Associate data to state of the History API.

   We store an 'id' in the state, and store data in an association
   table in the session storage. This allows avoiding "replaceState"
   that has not a coherent behaviour between Chromium and Firefox
   (2012/03).

   Storing the scroll position in the state is not required with
   Chrome or Firefox: they automatically store and restore the
   correct scrolling while browsing the history. However this
   behaviour in not required by the HTML5 specification (only
   suggested). *)

[@@@warning "-39"]

type state =
  { (* TODO store cookies_info in state... *)
    template : string option
  ; position : Mod_dom.position }
[@@deriving json]

type state_id = {session_id : int; state_index : int (* point in history *)}
[@@deriving json]

type saved_state = state_id * string [@@deriving json]

[@@@warning "+39"]

let random_int =
  if
    Js.Optdef.test Js.Unsafe.global##.crypto
    && Js.Optdef.test Js.Unsafe.global##.crypto##.getRandomValues
  then
    fun () ->
      let a =
        Js.Unsafe.global##.crypto##(getRandomValues
                                      (new%js Typed_array.int16Array 2))
      in
      (Typed_array.unsafe_get a 0 lsl 16) lor Typed_array.unsafe_get a 1
  else fun () -> truncate (4294967296. *. Js.to_float Js.math##random)

let section_page = Logs.Src.create "eliom:client:page"

(* The history.state value of the page [page_id] at [uri] *)
let history_state page_id uri =
  Js.Opt.return (Js.string (to_json ~typ:[%json: saved_state] (page_id, uri)))

module Page_status_t = struct
  type t = Generating | Active | Cached | Dead

  let to_string st =
    match st with
    | Generating -> "Generating"
    | Active -> "Active"
    | Cached -> "Cached"
    | Dead -> "Dead"
end

type page =
  { page_unique_id : int
  ; mutable page_id : state_id
  ; mutable url : string
  ; page_status : Page_status_t.t React.S.t
  ; mutable previous_page : int option
  ; set_page_status : ?step:React.step -> Page_status_t.t -> unit
  ; mutable dom : Dom_html.bodyElement Js.t option
  ; mutable reload_function : (unit -> unit -> Service.result Lwt.t) option }

let string_of_page p =
  Printf.sprintf "%d/%d %s %s %d %b" p.page_unique_id p.page_id.state_index
    p.url
    (Page_status_t.to_string @@ React.S.value p.page_status)
    (Option.value p.previous_page ~default:0)
    (Option.is_some p.dom)

let set_page_status p st =
  Logs.debug ~src:section_page (fun fmt ->
    fmt "Set page status %d/%d: %s" p.page_unique_id p.page_id.state_index
      (Page_status_t.to_string st));
  p.set_page_status st

let retire_page p =
  set_page_status p @@ match p.dom with Some _ -> Cached | None -> Dead

let session_id = random_int ()

let next_state_id =
  let last = ref 0 in
  fun () ->
    incr last;
    {session_id; state_index = !last}

let last_page_id = ref (-1)

let mk_page ?(state_id = next_state_id ()) ?url ?previous_page ~status () =
  incr last_page_id;
  Logs.debug ~src:section_page (fun fmt ->
    fmt "Create page %d/%d" !last_page_id state_id.state_index);
  let page_status, set_page_status = React.S.create status in
  (* protect page_status from React.S.stop ~strong:true *)
  ignore @@ React.S.map (fun _ -> ()) page_status;
  { page_unique_id = !last_page_id
  ; page_id = state_id
  ; url =
      (match url with
      | Some u -> u
      | None ->
          fst
            (Url.split_fragment
               (Js.to_string Dom_html.window##.location##.href)))
  ; page_status
  ; previous_page
  ; set_page_status
  ; dom = None
  ; reload_function = None }

let active_page = ref @@ mk_page ~status:Active ()

let set_active_page p =
  Logs.debug ~src:section_page (fun fmt ->
    fmt "Set active page %d/%d" p.page_unique_id p.page_id.state_index);
  retire_page !active_page;
  active_page := p;
  set_page_status !active_page Active

(* This key serves as a hook to access the page the currently running code is
   generating. *)
let this_page : page Lwt.key = Lwt.new_key ()

let get_this_page () =
  match Lwt.get this_page with
  | Some p -> p
  | None ->
      Logs.debug ~src:section_page (fun fmt -> fmt "No page in context");
      !active_page

let with_new_page ?state_id ?old_page ~replace () f =
  let state_id = if replace then Some !active_page.page_id else state_id in
  let url, previous_page =
    match old_page with
    | Some o -> Some o.url, o.previous_page
    | None -> None, None
  in
  let page = mk_page ?state_id ?url ?previous_page ~status:Generating () in
  Lwt.with_value this_page (Some page) f

module Page_status = struct
  include Page_status_t

  let signal () =
    let p = get_this_page () in
    p.page_status

  module Events = struct
    let changes () = React.S.changes (signal ())

    let active () =
      changes () |> React.E.fmap @@ function Active -> Some () | _ -> None

    let cached () =
      changes () |> React.E.fmap @@ function Cached -> Some () | _ -> None

    let dead () =
      changes () |> React.E.fmap @@ function Dead -> Some () | _ -> None

    let inactive () = React.E.select [cached (); dead ()]
  end

  let maybe_just_once ~once e = if once then React.E.once e else e

  let stop_event ?(stop = React.E.never) e =
    Dom_reference.retain_generic (get_this_page ()) ~keep:e;
    Dom_reference.retain_generic e
      ~keep:(React.E.map (fun () -> React.E.stop ~strong:true e) stop)

  let onactive ?(now = true) ?(once = false) ?stop action =
    let on_event () =
      stop_event ?stop @@ React.E.map action @@ maybe_just_once ~once
      @@ Events.active ()
    in
    if now && React.S.value (signal ()) = Active
    then (
      action ();
      if not once then on_event ())
    else on_event ()

  let oncached ?(once = false) ?stop action =
    stop_event ?stop @@ React.E.map action @@ maybe_just_once ~once
    @@ Events.cached ()

  let ondead ?stop action =
    stop_event ?stop @@ React.E.map action (Events.dead ())

  let oninactive ?(once = false) ?stop action =
    stop_event ?stop @@ React.E.map action @@ maybe_just_once ~once
    @@ Events.inactive ()

  let while_active ?now ?(stop = React.E.never) action =
    let thread = ref Lwt.return_unit in
    onactive ?now ~stop (fun () -> thread := action ());
    oninactive ~stop (fun () -> Lwt.cancel !thread);
    Dom_reference.retain_generic (get_this_page ())
      ~keep:(React.E.map (fun () -> Lwt.cancel !thread) stop)
end

module History = struct
  let section = Logs.Src.create "eliom:client:history"

  let get, set =
    let history = ref [!active_page] in
    let set h =
      Logs.debug ~src:section (fun fmt ->
        fmt "setting history:\n%s"
          (String.concat "\n" @@ List.map string_of_page !history));
      history := h
    in
    (fun () -> !history), set

  let find_by_state_index i =
    try Some (List.find (fun p -> p.page_id.state_index = i) (get ()))
    with Not_found -> None

  let split_rev_past_future index =
    let rec loop past = function
      | [] -> past, []
      | x :: future when x.page_id.state_index = index -> x :: past, future
      | x :: l -> loop (x :: past) l
    in
    loop [] (get ())

  let advance n =
    let new_history, future =
      match n.previous_page with
      | None -> get (), []
      | Some pp ->
          let rev_past, future = split_rev_past_future pp in
          List.rev (n :: rev_past), future
    in
    List.iter (fun p -> set_page_status p Dead) future;
    set new_history

  let replace n =
    let maybe_replace p =
      if p.page_id.state_index = n.page_id.state_index
      then (set_page_status p Dead; n)
      else p
    in
    set @@ List.map maybe_replace @@ get ()

  let past () =
    let index = !active_page.page_id.state_index in
    let rev_past, _ = split_rev_past_future index in
    List.map (fun p -> p.url)
    @@ match rev_past with _present :: past -> past | [] -> []

  let future () =
    let index = !active_page.page_id.state_index in
    let _, future = split_rev_past_future index in
    List.map (fun p -> p.url) future

  let max_num_doms = ref None

  let garbage_collect_doms () =
    match !max_num_doms with
    | None -> ()
    | Some max_num_doms ->
        let interleave l r =
          let take_from_l = ref false in
          let alternate _ _ =
            take_from_l := not !take_from_l;
            if !take_from_l then -1 else 1
          in
          List.merge alternate l r
        in
        let rev_past, future =
          split_rev_past_future !active_page.page_id.state_index
        in
        let pages_ordered_by_distance_from_present =
          interleave rev_past future
        in
        let num_doms = ref 0 in
        let maybe_delete_dom p =
          match p.dom with
          | None -> ()
          | Some _ ->
              num_doms := !num_doms + 1;
              if !num_doms > max_num_doms
              then (
                p.dom <- None;
                set_page_status p Dead)
        in
        List.iter maybe_delete_dom pages_ordered_by_distance_from_present
end

let advance_page () =
  let new_page = get_this_page () in
  if new_page != !active_page
  then (
    new_page.previous_page <- Some !active_page.page_id.state_index;
    (match History.find_by_state_index new_page.page_id.state_index with
    | Some _ -> ()
    | None -> History.advance new_page);
    set_active_page new_page)

let state_key {session_id; state_index} =
  Js.string (Printf.sprintf "state_history_%x_%x" session_id state_index)

let get_state state_id : state =
  Js.Opt.case
    (Js.Optdef.case
       Dom_html.window##.sessionStorage
       (fun () ->
          (* Session storage is available wherever the History API
             is. *)
          raise_error ~section "sessionStorage not available")
       (fun s -> s##(getItem (state_key state_id))))
    (fun () -> raise Not_found)
    (fun s -> of_json ~typ:[%json: state] (Js.to_string s))

let set_state i (v : state) =
  Js.Optdef.case
    Dom_html.window##.sessionStorage
    (fun () -> ())
    (fun s ->
       s##(setItem (state_key i) (Js.string (to_json ~typ:[%json: state] v))))

let update_state () =
  set_state !active_page.page_id
    { template = Request_info.get_request_template ()
    ; position = Mod_dom.getDocumentScroll () }
