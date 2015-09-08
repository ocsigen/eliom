(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2015 Vasilis Papavasileiou
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

let get_storage s =
  Js.Optdef.case s
    (fun () -> failwith "Browser storage not supported")
    (fun v -> v)

module type Get_storage_object = sig
  val get : unit -> Dom_html.storage Js.t
end

module type PREFIXED_STORAGE = sig
  include Eliom_storage_sigs.STORAGE
  val create_key :
    prefix:string -> string -> 'a Deriving_Json.t -> 'a key
end

module Make_prefixed_storage (G : Get_storage_object) :

  PREFIXED_STORAGE =

struct

  type 'a key = Js.js_string Js.t * 'a Deriving_Json.t

  let length () = (G.get ())##length

  let create_key ~prefix id j =
    Js.string (prefix ^ id), j

  let find (id, j) =
    let v = (G.get ())##getItem(id)
    and f v = Deriving_Json.from_string j (Js.to_string v)
    and g () = raise Not_found in
    Js.Opt.case v g f

  let set (id, j) v =
    let v = Js.string (Deriving_Json.to_string j v) in
    (G.get ())##setItem(id, v)

  let remove (id, _) =
    (G.get ())##removeItem(id)

  let clear () =
    (G.get ())##clear()

end

module Make_table (S : PREFIXED_STORAGE) :

  Eliom_storage_sigs.TABLE =

struct

  type ('a, 'b) t =
    string * 'a Deriving_Json.t * 'b Deriving_Json.t

  let create s j j' =
    (* TODO: check s against regexp to make sure it becomes a unique
       prefix *)
    Printf.sprintf "__%s_" s, j, j'

  let wrap_key (prefix, j, j') key =
    S.create_key ~prefix (Deriving_Json.to_string j key) j'

  let find t key =
    wrap_key t key |> S.find

  let set t key v =
    S.set (wrap_key t key) v

  let remove t key =
    wrap_key t key |> S.remove

end

module Make_all (G : Get_storage_object) = struct

  module Prefixed_storage = Make_prefixed_storage(G)

  module Storage = struct
    include Prefixed_storage
    let create_key s j = create_key ~prefix:"_storage_" s j
  end

  module Table = Make_table(Prefixed_storage)

end

module Local = struct
  module G = struct
    let get () = get_storage (Dom_html.window##localStorage)
  end
  include Make_all(G)
end

module Session = struct
  module G = struct
    let get () = get_storage (Dom_html.window##sessionStorage)
  end
  include Make_all(G)
end
