[@@@warning "-39"]

type cookie = Ocsigen_cookie_map.cookie =
  | OSet of float option (* exp date *) * string (* value *) * bool (* secure *)
  | OUnset
[@@deriving json]

type cookie_array = (string array * (string * cookie) array) array
[@@deriving json]

[@@@warning "+39"]

(** changes to cookieset_to_json must be completed
    by corresponding changes in cookieset_of_json *)
let cookieset_to_json set =
  let cookietable_array set =
    let add key v l = (key, v) :: l in
    Array.of_list (Ocsigen_cookie_map.Map_inner.fold add set [])
  in
  let add key v l = (Array.of_list key, cookietable_array v) :: l in
  let a = Array.of_list (Ocsigen_cookie_map.Map_path.fold add set []) in
  Deriving_Json.to_string [%json: cookie_array] a

let cookieset_of_json json =
  let array = Deriving_Json.from_string [%json: cookie_array] json in
  let cookietable_array array =
    Array.fold_left
      (fun set (name, cookie) ->
         Ocsigen_cookie_map.Map_inner.add name cookie set)
      Ocsigen_cookie_map.Map_inner.empty array
  in
  Array.fold_left
    (fun set (path, cookietable) ->
       let path = Array.to_list path in
       Ocsigen_cookie_map.Map_path.add path (cookietable_array cookietable) set)
    Ocsigen_cookie_map.empty array
