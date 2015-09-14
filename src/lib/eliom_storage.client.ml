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

let valid_name s =
  match
    let re = Regexp.regexp "^[A-Za-z0-9]+$" in
    Regexp.string_match re s 0
  with
  | Some _ ->
    true
  | None ->
    false

module type GET_STORAGE = sig
  val get : unit -> Dom_html.storage Js.t
end

module type PREFIXED_RAW = sig
  type key
  val length : unit -> int
  val find : key -> string
  val set : key -> string -> unit
  val remove : key -> unit
  val clear : unit -> unit
  val create_key : prefix:string -> string -> key
end

module type PREFIXED_JSON = sig
  include Eliom_storage_sigs.STORAGE
  val create_key :
    prefix:string -> string -> 'a Deriving_Json.t -> 'a key
end

module type PREFIXED_POLY = sig
  include Eliom_storage_sigs.STORAGE
  val create_key : prefix:string -> string -> 'a key
end

module Make_prefixed_raw (G : GET_STORAGE) : PREFIXED_RAW = struct

  type key = Js.js_string Js.t

  let length () = (G.get ())##length

  let create_key ~prefix key =
    Js.bytestring (Printf.sprintf "__%s_%s" prefix key)

  let find key =
    let v = (G.get ())##getItem(key)
    and f = Js.to_bytestring
    and g () = raise Not_found in
    Js.Opt.case v g f

  let set key v =
    (G.get ())##setItem(key, Js.bytestring v)

  let remove key = (G.get ())##removeItem(key)

  let clear () = (G.get ())##clear()

end

module Make_prefixed_json (R : PREFIXED_RAW) : PREFIXED_JSON = struct

  type 'a key = R.key * 'a Deriving_Json.t

  let length = R.length

  let create_key ~prefix key j =
    R.create_key ~prefix key, j

  let find (key, j) =
    R.find key |> Deriving_Json.from_string j

  let set (key, j) v =
    Deriving_Json.to_string j v |> R.set key

  let remove (key, _) =
    R.remove key

  let clear = R.clear

end

module Make_prefixed_poly (R : PREFIXED_RAW) : PREFIXED_POLY = struct

  type 'a key = R.key

  let length = R.length

  let create_key = R.create_key

  let find key =
    let v = R.find key in
    Marshal.from_string v 0

  let set key v =
    Marshal.to_string v [] |> R.set key

  let remove = R.remove

  let clear = R.clear

end

module Make_table_json (S : PREFIXED_JSON) = struct

  type ('a, 'b) t =
    string * 'a Deriving_Json.t * 'b Deriving_Json.t

  let create s j j' =
    if valid_name s then
      s, j, j'
    else
      failwith "create: invalid name"

  let wrap_key (prefix, j, j') key =
    S.create_key ~prefix (Deriving_Json.to_string j key) j'

  let find t key =
    wrap_key t key |> S.find

  let set t key v =
    S.set (wrap_key t key) v

  let remove t key =
    wrap_key t key |> S.remove

end

module Make_table_poly (S : PREFIXED_POLY) = struct

  type ('a, 'b) t = string

  let create prefix =
    if valid_name prefix then
      prefix
    else
      failwith "create: invalid name"

  let wrap_key ~prefix key =
    Marshal.to_string key [] |> S.create_key ~prefix

  let find prefix key =
    let v = wrap_key ~prefix key |> S.find in
    Marshal.from_string v 0

  let set prefix key v =
    let key = wrap_key ~prefix key
    and v = Marshal.to_string v [] in
    S.set key v

  let remove prefix key =
    wrap_key ~prefix key |> S.remove

end

module Make_all (G : GET_STORAGE) : Eliom_storage_sigs.ALL = struct

  module Raw_ = Make_prefixed_raw(G)

  module Raw = struct
    include Raw_
    let create_key s = create_key ~prefix:"_raw_" s
  end

  module Json = struct
    module Prefixed_storage = Make_prefixed_json(Raw_)
    module Storage = struct
      include Prefixed_storage
      let create_key s j = create_key ~prefix:"_storage_" s j
    end
    module Table = Make_table_json(Prefixed_storage)
  end

  module Poly = struct
    module Prefixed_storage = Make_prefixed_poly(Raw_)
    module Storage = struct
      include Prefixed_storage
      let create_key s = create_key ~prefix:"_storage_" s
    end
    module Table = Make_table_poly(Prefixed_storage)
  end

end

module Local : Eliom_storage_sigs.ALL = struct
  module G = struct
    let get () = get_storage (Dom_html.window##localStorage)
  end
  include Make_all(G)
end

module Session : Eliom_storage_sigs.ALL = struct
  module G = struct
    let get () = get_storage (Dom_html.window##sessionStorage)
  end
  include Make_all(G)
end
