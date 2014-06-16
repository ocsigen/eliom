open Eliom_lib
open Ocsigen_cookies

type cookie = Ocsigen_cookies.cookie =
  | OSet of float option * string * bool
  | OUnset
deriving (Json)

type cookie_array =
    ( string array * (( string * cookie ) array )) array
deriving (Json)

(** changes to cookieset_to_json must be completed
    by corresponding changes in cookieset_of_json *)
let cookieset_to_json set =
  let cookietable_array set =
    let add key v l = (key, v)::l in
    Array.of_list (CookiesTable.fold add set [])
  in
  let add key v l = (Array.of_list key, cookietable_array v)::l in
  let a = Array.of_list (Cookies.fold add set []) in
  Deriving_Json.to_string Json.t<cookie_array> a

let cookieset_of_json json =
  let array = Deriving_Json.from_string Json.t<cookie_array> json in
  let cookietable_array array =
    Array.fold_left (fun set (name, cookie) -> CookiesTable.add name cookie set)
      CookiesTable.empty array
  in
  Array.fold_left (fun set (path, cookietable) ->
    let path = Array.to_list path in
    Cookies.add path (cookietable_array cookietable) set)
    Cookies.empty array
