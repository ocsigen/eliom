open Lwt.Syntax

type t = {ctx : Cohttp_lwt_unix.Net.ctx; mutable jar : Cookie_jar.t}
type response = {status : int; headers : Cohttp.Header.t; body : string}

let host = "eliom-test"

let create server =
  let hosts = Hashtbl.create 1 in
  Hashtbl.add hosts host (`Unix_domain_socket (Server_harness.socket server));
  { ctx = Cohttp_lwt_unix.Net.init ~resolver:(Resolver_lwt_unix.static hosts) ()
  ; jar = Cookie_jar.empty }

let path_of_url url =
  match String.index_opt url '?' with
  | Some i -> String.sub url 0 i
  | None -> url

let call ?(headers = []) ?body meth b url =
  let path = path_of_url url in
  let cookie =
    Option.map
      (fun c -> "cookie", c)
      (Cookie_jar.header ~now:(Unix.time ()) ~path b.jar)
  in
  let headers = Cohttp.Header.of_list (Option.to_list cookie @ headers) in
  let uri = Uri.of_string ("http://" ^ host ^ url) in
  let* resp, body =
    Cohttp_lwt_unix.Client.call ~ctx:b.ctx ~headers ?body meth uri
  in
  let* body = Cohttp_lwt.Body.to_string body in
  let headers = Cohttp.Response.headers resp in
  b.jar <-
    Cookie_jar.store ~now:(Unix.time ()) ~path
      (Cohttp.Header.get_multi headers "set-cookie")
      b.jar;
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

let cookies b = Cookie_jar.cookies ~now:(Unix.time ()) b.jar
let header r name = Cohttp.Header.get r.headers name
