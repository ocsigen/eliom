(* Ocsigen
 * http://www.ocsigen.org
 * Module server.ml
 * Copyright (C) 2010
 * Raphaël Proust
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

(* This module is a mashup of often needed react function. *)

module E :
sig

  val create_half_primitive : 'a React.E.t -> ('a React.E.t * ('a -> unit))
    (* [create_half_primitive e] is a tupple with an event and a callback. The
     * event is triggered by both [e]'s occurrences and calls to the callback *)

  val branch : ('a -> 'b * 'c) -> 'a React.E.t -> ('b React.E.t * 'c React.E.t)
    (* [branch f e] evaluates to two events, each of them being triggered
     * whenever [e] has an occurence. Typing gives a obvious clue about values
     * carried by resulting events. *)

  module Bundle :
  sig

    type 'a id
      (* The type of bundled events identifier *)

    type 'a bundle
      (* The type of a bundle of event *)

    val create : unit -> 'a bundle
      (* [create ()] returns an event with originally no events inside *)

    val create_in_bundle : 'a bundle -> ('a React.E.t * 'a id)
      (* [create_in_bundle b] creates a new event in the bundle [b]. Returned
       * value is the event itself and an identifier. *)

    val send_to_one : 'a id -> 'a bundle -> 'a -> unit
      (* [send_to_one i b x] sends [x] on the event identified by [i] in the
       * bundle [b]. [i] must be an identifier for an event in the bundle [b]
       * and not from another bundle.
       * One use of this is to create a "normal" push function :
       * [
       *  let (e, i) = create_in_bundle b
       *  let p = send_to_one i b
       * ]
       * note that in this example, [p] holds no pointer to [e] and that
       * [e] might be collected by the GC even if [p] can't.
       *)

    val send_to_bundle : 'a bundle -> ('a * 'a id) list -> unit
      (* [send_to_bundle b [(v1, i1); (v2, i2); ...]] sends [v1] on the event
       * identified by [i1], [v2] on the event identified by [i2] and so on. All
       * these occurrences happens simultaneously. *)

  end

end = struct

  let create_half_primitive evt =
    let (prim_evt, prim_push) = React.E.create () in
    (React.E.select [evt ; prim_evt], prim_push)

  let branch f e =
    let ee = React.E.map f e in
    (React.E.map fst ee, React.E.map snd ee)

  module Bundle =
  struct

    type 'a id = int
    type 'a bundle =
        {
          b_event    : ('a * 'a id) list React.E.t ;
          b_push     : ('a * 'a id) list -> unit ;
          b_id_gen   : unit -> int ;
        }

    let create () =
      let (e, s) = React.E.create () in
        {
          b_event    = e ;
          b_push     = s ;
          b_id_gen   = (let r = ref 0 in fun () -> incr r ; !r) ;
        }

    let create_in_bundle b =
      let id = b.b_id_gen () in
      (React.E.fmap
         (fun l ->
            try Some (fst (List.find (fun (_, i) -> i = id) l))
            with Not_found -> None
         )
         b.b_event,
       id)

    let send_to_one i { b_push = push } x = push [ (x, i) ]

    let send_to_bundle { b_push = push } l = push l

  end

end
