(* The test server of test.ml: coservices of global scope. The handlers
   answer in plain text. *)

open Eliom
module P = Parameter

let text s = Lwt.return (s, "text/plain")
let string service f = Registration.String.register ~service f

(* A fallback says whether it replaces a coservice not found. *)
let fallback name =
  let service =
    Service.create ~path:(Service.Path [name]) ~meth:(Service.Get P.unit) ()
  in
  string service (fun () () ->
    text
      (if Request_info.get_link_too_old ()
       then name ^ ", link too old"
       else name));
  service

let main = fallback "main"
let other = fallback "other"

(* Attached coservices *)

let anonymous = Service.create_attached_get ~fallback:main ~get_params:P.unit ()
let () = string anonymous (fun () () -> text "anonymous")

let named =
  Service.create_attached_get ~name:"named" ~fallback:main
    ~get_params:P.(int "n")
    ()

(* The GET parameters without the prefix of the coservice are left to the
   page. *)
let () =
  string named (fun n () ->
    text
      (String.concat " "
         (("named " ^ string_of_int n)
         :: List.map
              (fun (n, v) -> n ^ "=" ^ v)
              (Request_info.get_other_get_params ()))))

let attached_post =
  Service.create_attached_post ~fallback:main ~post_params:P.(string "v") ()

let () = string attached_post (fun () v -> text ("attached post " ^ v))

let once =
  Service.create_attached_get ~max_use:1 ~fallback:main ~get_params:P.unit ()

let () = string once (fun () () -> text "once")

(* Non-attached coservices *)

let na_anonymous =
  Service.create ~path:Service.No_path ~meth:(Service.Get P.unit) ()

let () = string na_anonymous (fun () () -> text "na anonymous")

let na_named =
  Service.create ~name:"na_named" ~path:Service.No_path
    ~meth:(Service.Get P.(int "n"))
    ()

let () = string na_named (fun n () -> text ("na named " ^ string_of_int n))

let na_post =
  Service.create ~path:Service.No_path
    ~meth:(Service.Post (P.unit, P.(string "v")))
    ()

let () = string na_post (fun () v -> text ("na post " ^ v))

let na_once =
  Service.create ~max_use:1 ~path:Service.No_path ~meth:(Service.Get P.unit) ()

let () = string na_once (fun () () -> text "na once")

(* A non-attached coservice attached to the path of another service *)
let na_attached = Service.attach ~fallback:other ~service:na_named ()

(* CSRF-safe coservices, registered for the whole site: each link to one
   registers a new coservice of the session of the request (the default
   [~csrf_scope]), or of its client process *)

let csrf =
  Service.create_attached_get ~csrf_safe:true ~fallback:main ~get_params:P.unit
    ()

let () = string csrf (fun () () -> text "csrf")

let csrf_once =
  Service.create_attached_get ~csrf_safe:true ~max_use:1 ~fallback:main
    ~get_params:P.unit ()

let () = string csrf_once (fun () () -> text "csrf once")

let csrf_post =
  Service.create_attached_post ~csrf_safe:true ~fallback:main
    ~post_params:P.(string "v")
    ()

let () = string csrf_post (fun () v -> text ("csrf post " ^ v))

let na_csrf =
  Service.create ~csrf_safe:true ~path:Service.No_path
    ~meth:(Service.Get P.unit) ()

let () = string na_csrf (fun () () -> text "na csrf")

let na_csrf_post =
  Service.create ~csrf_safe:true ~path:Service.No_path
    ~meth:(Service.Post (P.unit, P.(string "v")))
    ()

let () = string na_csrf_post (fun () v -> text ("na csrf post " ^ v))

let csrf_tab =
  Service.create_attached_get ~csrf_safe:true
    ~csrf_scope:Common.default_process_scope ~fallback:main ~get_params:P.unit
    ()

let () = string csrf_tab (fun () () -> text "csrf of the tab")

(* Links to the coservices. The URL of a POST service is followed by its POST
   parameters, one per line, the name and the value separated by a tab. *)

let get_uri service params =
  Eliom_uri.make_string_uri ~absolute_path:true ~service params

let post_uri service post_params =
  let path, get_params, _, post_params =
    Eliom_uri.make_post_uri_components ~absolute_path:true ~service ()
      post_params
  in
  String.concat "\n"
    (Eliom_uri.make_string_uri_from_components (path, get_params, None)
    :: List.map (fun (n, v) -> n ^ "\t" ^ v) post_params)

(* Coservices created by a request, with the options [~max_use] or
   [~timeout] *)
let create_coservice ?max_use ?timeout () =
  let service =
    Service.create_attached_get ?max_use ?timeout ~fallback:main
      ~get_params:P.unit ()
  in
  string service (fun () () -> text "created");
  get_uri service ()

let () =
  string
    (Service.create ~path:(Service.Path ["link"])
       ~meth:(Service.Get P.(string "to"))
       ())
    (fun target () ->
       text
         (match target with
         | "anonymous" -> get_uri anonymous ()
         | "named" -> get_uri named 3
         | "attached_post" -> post_uri attached_post "x"
         | "once" -> get_uri once ()
         | "na_anonymous" -> get_uri na_anonymous ()
         | "na_named" -> get_uri na_named 4
         | "na_post" -> post_uri na_post "y"
         | "na_once" -> get_uri na_once ()
         | "na_attached" -> get_uri na_attached 5
         | "created_once" -> create_coservice ~max_use:1 ()
         | "created_timeout" -> create_coservice ~timeout:1. ()
         | "csrf" -> get_uri csrf ()
         | "csrf_once" -> get_uri csrf_once ()
         | "csrf_post" -> post_uri csrf_post "x"
         | "na_csrf" -> get_uri na_csrf ()
         | "na_csrf_post" -> post_uri na_csrf_post "y"
         | "csrf_tab" -> get_uri csrf_tab ()
         | _ -> invalid_arg target))

let () = Eliom_test_server.Server_harness.start [App.run ()]
