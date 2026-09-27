(* The test server of test.ml. Most services answer in plain text, for simple
   assertions. *)

open Eliom
module P = Parameter

let text s = Lwt.return (s, "text/plain")

let get ?priority path params =
  Service.create ?priority ~path:(Service.Path path) ~meth:(Service.Get params)
    ()

let post path get_params post_params =
  Service.create ~path:(Service.Path path)
    ~meth:(Service.Post (get_params, post_params))
    ()

let string ?options ?code ?headers service f =
  Registration.String.register ?options ?code ?headers ~service f

(* Dispatch and parameters *)

let hello = get ["hello"] P.unit
let () = string hello (fun () () -> text "hello")

let () =
  string
    (get ["params"] P.(int "i" ** string "s"))
    (fun (i, s) () -> text (Printf.sprintf "i=%d s=%s" i s))

let () =
  string
    (get ["suffix"] P.(suffix (int "i" ** string "s")))
    (fun (i, s) () -> text (Printf.sprintf "i=%d s=%s" i s))

let () =
  string
    (get ["opt"] P.(opt (int "i")))
    (fun i () ->
       text (match i with None -> "none" | Some i -> string_of_int i))

let () =
  string
    (get ["list"] P.(list "l" (string "x")))
    (fun l () -> text (String.concat "," l))

let () =
  string
    (get ["set"] P.(set int "i"))
    (fun l () ->
       text (String.concat "," (List.map string_of_int (List.sort compare l))))

let form = get ["form"] P.unit
let () = string form (fun () () -> text "get")

let () =
  string (post ["form"] P.unit P.(string "v")) (fun () v -> text ("post " ^ v))

let () = string (get ["dir"; ""] P.unit) (fun () () -> text "directory")

(* Two services on the same path, tried by decreasing priority *)
let () =
  string
    (get ~priority:1 ["priority"] P.(int "i"))
    (fun i () -> text ("int " ^ string_of_int i))

let () =
  string (get ["priority"] P.(string "s")) (fun s () -> text ("string " ^ s))

let temporary = get ["temporary"] P.unit
let () = string temporary (fun () () -> text "temporary")

let () =
  string (get ["unregister"] P.unit) (fun () () ->
    Service.unregister temporary;
    text "unregistered")

let () =
  string (get ["failure"] P.unit) (fun () () -> failwith "handler failure")

(* Outputs *)

let () =
  Registration.Html.register ~service:(get ["html"] P.unit) (fun () () ->
    Lwt.return
      Content.Html.F.(
        html (head (title (txt "title")) []) (body [p [txt "body"]])))

let () =
  Registration.Html_text.register ~service:(get ["html_text"] P.unit)
    (fun () () -> Lwt.return "<p>text</p>")

let () =
  Registration.CssText.register ~options:3600 ~service:(get ["css"] P.unit)
    (fun () () -> Lwt.return "p {}")

let () =
  string ~options:0 (get ["no_cache"] P.unit) (fun () () -> text "no cache")

let () =
  string ~code:201 ~headers:(Cohttp.Header.init_with "X-Test" "yes")
    (get ["code"] P.unit) (fun () () -> text "created")

let () =
  Registration.Unit.register ~service:(get ["unit"] P.unit) (fun () () ->
    Lwt.return_unit)

let () =
  Registration.Action.register ~options:`NoReload
    ~service:(post ["action"] P.unit P.(string "v"))
    (fun () _ -> Lwt.return_unit)

let () =
  Registration.Redirection.register ~service:(get ["redirect"] P.unit)
    (fun () () -> Lwt.return (Registration.Redirection hello))

let () =
  Registration.Redirection.register ~options:`MovedPermanently
    ~service:(get ["moved"] P.unit) (fun () () ->
    Lwt.return (Registration.Redirection hello))

let () =
  Registration.String_redirection.register
    ~service:(get ["redirect_string"] P.unit) (fun () () ->
    Lwt.return "http://example.org/elsewhere")

let () =
  Registration.File.register ~options:60 ~service:(get ["file"] P.unit)
    (fun () () -> Lwt.return "file.txt")

let () =
  Registration.File.register ~service:(get ["missing_file"] P.unit)
    (fun () () -> Lwt.return "missing.txt")

let () =
  Registration.Any.register
    ~service:(get ["any"] P.(bool "html"))
    (fun html () ->
       if html
       then Registration.Html_text.send "<p>any</p>"
       else
         Lwt.map Registration.cast_unknown_content_kind
           (Registration.String.send ("any", "text/plain")))

(* The file served by /file, in the directory of the server *)
let () =
  Out_channel.with_open_bin
    (Filename.concat Sys.argv.(1) "file.txt")
    (fun oc -> output_string oc "file content")

let () = Eliom_test_server.Server_harness.start [App.run ()]
