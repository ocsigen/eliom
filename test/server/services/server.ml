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

let suffix_service = get ["suffix"] P.(suffix (int "i" ** string "s"))

let () =
  string (get ["suffix_link"] P.unit) (fun () () ->
    text
      (Eliom_uri.make_string_uri ~absolute_path:true ~service:suffix_service
         (3, "x/y")))

let () =
  string
    (get ["files"] P.(suffix (all_suffix "path")))
    (fun path () -> text (String.concat "|" path))

let () =
  string
    (get ["relative"] P.(suffix (string "s")))
    (fun _ () -> text (Eliom_uri.make_string_uri ~service:hello ()))

let () =
  string
    (get ["tree"] P.(suffix (all_suffix_string "path")))
    (fun path () -> text path)

let () =
  string
    (get ["info"] P.(suffix (string "s")))
    (fun _ () ->
       text
         (String.concat " "
            (List.map (String.concat "|")
               [ Request_info.get_current_sub_path ()
               ; Request_info.get_original_full_path () ])))

let () =
  string suffix_service (fun (i, s) () -> text (Printf.sprintf "i=%d s=%s" i s))

let form = get ["form"] P.unit
let () = string form (fun () () -> text "get")

let () =
  string (post ["form"] P.unit P.(string "v")) (fun () v -> text ("post " ^ v))

let () =
  string
    (post ["post_int"] P.unit P.(int "n"))
    (fun () n -> text (string_of_int n))

let () = string (get ["dir"; ""] P.unit) (fun () () -> text "directory")

let () =
  string
    (get ["prod"] P.(suffix_prod (int "i") (string "q")))
    (fun (i, q) () -> text (Printf.sprintf "i=%d q=%s" i q))

(* Two services on the same path: when the parameters do not match the first
   one, the next one is tried. *)
let () =
  string
    (get ["alternatives"] P.(int "i"))
    (fun i () -> text ("int " ^ string_of_int i))

let () =
  string
    (get ["alternatives"] P.(string "s"))
    (fun s () -> text ("string " ^ s))

(* Two services accepting the same parameters: the one with the higher
   priority is tried first, although it is registered after the other. *)
let () =
  string
    (get ["priority"] P.(int "x"))
    (fun i () -> text ("int " ^ string_of_int i))

let () =
  string
    (get ~priority:1 ["priority"] P.(string "x"))
    (fun s () -> text ("string " ^ s))

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
  Registration.File_ct.register ~options:60 ~service:(get ["file_ct"] P.unit)
    (fun () () -> Lwt.return ("file.txt", "text/x-test"))

let () =
  Registration.Any.register
    ~service:(get ["any"] P.(bool "html"))
    (fun html () ->
       if html
       then Registration.Html_text.send "<p>any</p>"
       else
         Lwt.map Registration.cast_unknown_content_kind
           (Registration.String.send ("any", "text/plain")))

(* Non-localized parameters: [persistent_nl] is kept by default in the links
   of the pages that receive it, [transient_nl] only with
   [~keep_nl_params:`All] *)

let persistent_nl =
  P.make_non_localized_parameters ~prefix:"test" ~name:"persistent"
    ~persistent:true
    P.(string "v")

let transient_nl =
  P.make_non_localized_parameters ~prefix:"test" ~name:"transient"
    P.(string "v")

let nl_target = get ["nl"; "target"] P.unit

let () =
  string nl_target (fun () () ->
    let value nl =
      Option.value ~default:"none" (P.get_non_localized_get_parameters nl)
    in
    text
      (Printf.sprintf "persistent=%s transient=%s" (value persistent_nl)
         (value transient_nl)))

(* /nl/link is a link to /nl/target with both parameters; /nl/link?keep=
   is a link to it without parameters, keeping those of the request as
   [keep] says *)
let () =
  string
    (get ["nl"; "link"] P.(opt (string "keep")))
    (fun keep () ->
       let uri ?keep_nl_params ?nl_params () =
         Eliom_uri.make_string_uri ~absolute_path:true ?keep_nl_params
           ?nl_params ~service:nl_target ()
       in
       text
         (match keep with
         | None ->
             uri
               ~nl_params:
                 P.(
                   add_nl_parameter
                     (add_nl_parameter empty_nl_params_set persistent_nl "p")
                     transient_nl "t")
               ()
         | Some "default" -> uri ()
         | Some "all" -> uri ~keep_nl_params:`All ()
         | Some "persistent" -> uri ~keep_nl_params:`Persistent ()
         | Some "none" -> uri ~keep_nl_params:`None ()
         | Some k -> invalid_arg k))

(* The file served by /file, in the directory of the server *)
let () =
  Out_channel.with_open_bin
    (Filename.concat Sys.argv.(1) "file.txt")
    (fun oc -> output_string oc "file content")

let () = Eliom_test_server.Server_harness.start [App.run ()]
