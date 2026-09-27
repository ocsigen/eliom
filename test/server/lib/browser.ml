open Lwt.Syntax

type cookie =
  { name : string
  ; value : string
  ; path : string
  ; expires : float option
  ; secure : bool }

type t = {ctx : Cohttp_lwt_unix.Net.ctx; mutable jar : cookie list}
type response = {status : int; headers : Cohttp.Header.t; body : string}

let host = "eliom-test"

let create server =
  let hosts = Hashtbl.create 1 in
  Hashtbl.add hosts host (`Unix_domain_socket (Server_harness.socket server));
  { ctx = Cohttp_lwt_unix.Net.init ~resolver:(Resolver_lwt_unix.static hosts) ()
  ; jar = [] }

(* Dates of cookies, as written by Ocsigen Server:
   "Thu, 01 Jan 1970 00:00:00 GMT". *)
let months =
  [ "Jan"
  ; "Feb"
  ; "Mar"
  ; "Apr"
  ; "May"
  ; "Jun"
  ; "Jul"
  ; "Aug"
  ; "Sep"
  ; "Oct"
  ; "Nov"
  ; "Dec" ]

(* [days_from_civil y m d] is the number of days from 1970-01-01 to [y-m-d]
   (proleptic Gregorian calendar, [m] from 1 to 12). *)
let days_from_civil y m d =
  let y = if m <= 2 then y - 1 else y in
  let era = (if y >= 0 then y else y - 399) / 400 in
  let yoe = y - (era * 400) in
  let doy = (((153 * if m > 2 then m - 3 else m + 9) + 2) / 5) + d - 1 in
  let doe = (yoe * 365) + (yoe / 4) - (yoe / 100) + doy in
  (era * 146097) + doe - 719468

let month_index mon =
  let rec index i = function
    | [] -> None
    | m :: l -> if m = mon then Some i else index (i + 1) l
  in
  index 1 months

let parse_date s =
  match
    Scanf.sscanf s " %_s %d %s %d %d:%d:%d GMT" (fun d mon y h mi sec ->
      Option.map
        (fun m ->
           float_of_int
             ((days_from_civil y m d * 86400) + (h * 3600) + (mi * 60) + sec))
        (month_index mon))
  with
  | date -> date
  | exception (Scanf.Scan_failure _ | Failure _ | End_of_file) -> None

let split_at c s =
  match String.index_opt s c with
  | Some i ->
      ( String.trim (String.sub s 0 i)
      , String.trim (String.sub s (i + 1) (String.length s - i - 1)) )
  | None -> String.trim s, ""

let parse_set_cookie header =
  match String.split_on_char ';' header with
  | [] -> None
  | nv :: attributes ->
      let name, value = split_at '=' nv in
      let cookie = {name; value; path = "/"; expires = None; secure = false} in
      Some
        (List.fold_left
           (fun c a ->
              match split_at '=' a with
              | n, v when String.lowercase_ascii n = "path" -> {c with path = v}
              | n, v when String.lowercase_ascii n = "expires" ->
                  {c with expires = parse_date v}
              | n, v when String.lowercase_ascii n = "max-age" ->
                  { c with
                    expires =
                      Option.map
                        (fun s -> Unix.time () +. float_of_int s)
                        (int_of_string_opt v) }
              | n, _ when String.lowercase_ascii n = "secure" ->
                  {c with secure = true}
              | _ -> c)
           cookie attributes)

let expired now c = match c.expires with Some e -> e <= now | None -> false

let store b headers =
  let now = Unix.time () in
  List.iter
    (fun h ->
       match parse_set_cookie h with
       | None -> ()
       | Some c ->
           let others =
             List.filter
               (fun c' -> c'.name <> c.name || c'.path <> c.path)
               b.jar
           in
           b.jar <- (if expired now c then others else c :: others))
    (Cohttp.Header.get_multi headers "set-cookie")

(* RFC 6265, 5.1.4 *)
let path_matches ~cookie_path path =
  let n = String.length cookie_path in
  path = cookie_path
  || String.starts_with ~prefix:cookie_path path
     && (cookie_path.[n - 1] = '/' || path.[n] = '/')

let cookie_header b url =
  let path = fst (split_at '?' url) in
  let now = Unix.time () in
  match
    List.filter
      (fun c ->
         (not c.secure)
         && (not (expired now c))
         && path_matches ~cookie_path:c.path path)
      b.jar
  with
  | [] -> []
  | l ->
      [ ( "cookie"
        , String.concat "; " (List.map (fun c -> c.name ^ "=" ^ c.value) l) ) ]

let call ?(headers = []) ?body meth b url =
  let headers = Cohttp.Header.of_list (cookie_header b url @ headers) in
  let uri = Uri.of_string ("http://" ^ host ^ url) in
  let* resp, body =
    Cohttp_lwt_unix.Client.call ~ctx:b.ctx ~headers ?body meth uri
  in
  let* body = Cohttp_lwt.Body.to_string body in
  let headers = Cohttp.Response.headers resp in
  store b headers;
  Lwt.return
    { status = Cohttp.Code.code_of_status (Cohttp.Response.status resp)
    ; headers
    ; body }

let get ?headers b url = call ?headers `GET b url

let post ?(headers = []) b url params =
  let body =
    Cohttp_lwt.Body.of_string
      (Uri.encoded_of_query (List.map (fun (n, v) -> n, [v]) params))
  in
  call
    ~headers:(("content-type", "application/x-www-form-urlencoded") :: headers)
    ~body `POST b url

let cookies b = List.sort compare (List.map (fun c -> c.name, c.value) b.jar)
let header r name = Cohttp.Header.get r.headers name
