(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2016 Vasilis Papavasileiou
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
include Eliom_parameter_base

type raw_post_data = unit

module M : sig
  type 'a t

  val remove : 'a t -> string -> ('a * 'a t) option
  val of_assoc_list : (string * 'a) list -> 'a t
end = struct
  module Raw = Map.Make (struct
      type t = string

      let compare = compare
    end)

  type 'a t = 'a list Raw.t

  let remove m id =
    try
      match Raw.find id m with
      | [h] -> Some (h, Raw.remove id m)
      | h :: t -> Some (h, Raw.add id t m)
      | [] -> None
    with Not_found -> None

  let of_assoc_list l =
    let f acc (id, v) =
      try
        let l = Raw.find id acc in
        Raw.add id (v :: l) acc
      with Not_found -> Raw.add id [v] acc
    in
    List.fold_left f Raw.empty l
end

let reconstruct_atom ~f m name =
  try
    match M.remove m name with
    | Some (`String v, m) ->
        let v = f (Js.to_string v) in
        Some (v, m)
    | _ -> None
  with _ -> None

let ( >>= ) x f = match x with Some x -> f x | None -> None

let rec reconstruct_set : type a c.
  a list * Form.form_elt M.t
  -> (a, _, c) params_type
  -> a list * Form.form_elt M.t
  =
 fun ((acc, m) as p) y ->
  match reconstruct_params_form m y with
  | Some (v, m) -> reconstruct_set (v :: acc, m) y
  | None -> p

and reconstruct_params_form : type a c.
  Form.form_elt M.t
  -> (a, [`WithoutSuffix], c) params_type
  -> (a * Form.form_elt M.t) option
  =
 fun m -> function
  | TAtom (name, TBool) -> (
    match M.remove m name with
    | Some (_, m) -> Some (true, m)
    | None -> Some (false, m))
  | TAtom (name, y) -> reconstruct_atom ~f:(atom_of_string y) m name
  | TProd (TList _, _) -> failwith "Lists or sets in suffixes must be last"
  | TProd (TSet _, _) -> failwith "Lists or sets in suffixes must be last"
  | TProd (y1, y2) ->
      reconstruct_params_form m y1 >>= fun (x1, m) ->
      reconstruct_params_form m y2 >>= fun (x2, m) -> Some ((x1, x2), m)
  | TUnit -> Some ((), m)
  | TOption ((TAtom (_, TString) as y), _) -> (
    match reconstruct_params_form m y with
    | Some ("", m) -> Some (None, m)
    | Some (s, m) -> Some (Some s, m)
    | None -> Some (None, m))
  | TOption (y, _) -> (
    match reconstruct_params_form m y with
    | Some (x, m) -> Some (Some x, m)
    | None -> Some (None, m))
  | TSet (TAtom (_, TBool) as y) ->
      reconstruct_params_form m y >>= fun (x, m) -> Some ([x], m)
  | TSet y -> Some (reconstruct_set ([], m) y)
  | TSum (y1, y2) -> (
    match reconstruct_params_form m y1 with
    | Some (x, m) -> Some (Inj1 x, m)
    | None -> reconstruct_params_form m y2 >>= fun (x, m) -> Some (Inj2 x, m))
  | TCoord name ->
      let f = int_of_string in
      reconstruct_atom ~f m (name ^ ".x") >>= fun (abscissa, m) ->
      reconstruct_atom ~f m (name ^ ".y") >>= fun (ordinate, m) ->
      Some ({abscissa; ordinate}, m)
  | TUserType (name, {of_string = f; _}) -> reconstruct_atom ~f m name
  | _ -> None

let user_type ~of_string ~to_string n = TUserType (n, {of_string; to_string})

let all_suffix_user ~of_string ~to_string n =
  TESuffixu (n, {of_string; to_string})

let reconstruct_params_form l y =
  reconstruct_params_form (M.of_assoc_list l) y >>= fun (v, _) -> Some v

let get_non_localized_get_parameters {name; param; _} =
  (* Simplified version of the server-side code that
     - only deals with GET params
     - doesn't cache the result
     - doesn't deal with files *)
  try
    Some
      (reconstruct_params_ param
         (try
            Eliom_lib.String.Table.find name
              (Eliom_request_info.get_sess_info ()).si_nl_get_params
          with Not_found -> [])
         [] false None)
  with Eliom_common.Eliom_Wrong_parameter | Not_found -> None
