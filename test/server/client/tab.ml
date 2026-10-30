open Lwt.Syntax
module Browser = Eliom_test_server.Browser

type t = {browser : Browser.t; mutable tab_cookies : (string * string) list}

let create browser = {browser; tab_cookies = []}
let tab_cookies t = t.tab_cookies
let set_tab_cookies t cookies = t.tab_cookies <- cookies

(* The tab cookies set by a response, as by the client-side program *)
let store t (r : Browser.response) =
  match Browser.header r Eliom.Common_base.set_tab_cookies_header_name with
  | None -> ()
  | Some json ->
      Ocsigen_cookie_map.Map_path.iter
        (fun _path cookies ->
           Ocsigen_cookie_map.Map_inner.iter
             (fun name cookie ->
                let others = List.remove_assoc name t.tab_cookies in
                t.tab_cookies <-
                  (match cookie with
                  | Ocsigen_cookie_map.OSet (_, value, _) ->
                      (name, value) :: others
                  | Ocsigen_cookie_map.OUnset -> others))
             cookies)
        (Eliom.Cookies_base.cookieset_of_json json)

let headers ?(expecting_process_page = false) t =
  let tab_cookies =
    ( Eliom.Common_base.tab_cookies_header_name
    , Deriving_Json.to_string [%json: (string * string) list] t.tab_cookies )
  in
  if expecting_process_page
  then
    (* As the client-side program asking for a page of the application *)
    [ tab_cookies
    ; "Accept", "application/xhtml+xml"
    ; ( Eliom.Common_base.expecting_process_page_name
      , Deriving_Json.to_string [%json: bool] true ) ]
  else [tab_cookies]

let get ?expecting_process_page t url =
  let+ r =
    Browser.get ~headers:(headers ?expecting_process_page t) t.browser url
  in
  store t r; r

let post t url params =
  let+ r = Browser.post ~headers:(headers t) t.browser url params in
  store t r; r
