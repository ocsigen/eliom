(* The test server of test.ml: services sending files of its directory. *)

open Eliom
module P = Parameter
module Charset_mime = Ocsigen_http.Charset_mime

let dir = Sys.argv.(1)

(* The files, created when the server starts *)
let () =
  List.iter
    (fun (name, content) ->
       Out_channel.with_open_bin (Filename.concat dir name) (fun oc ->
         Out_channel.output_string oc content))
    ["page.foo", "a page"; "image.png", "an image"; "text.txt", "0123456789"]

(* [file path] registers on [path] a service sending the file of its
   parameter [name] *)
let file ?options ?charset ?content_type ?headers path =
  Registration.File.register ?options ?charset ?content_type ?headers
    ~service:
      (Service.create ~path:(Service.Path [path])
         ~meth:(Service.Get P.(string "name"))
         ())
    (fun name () -> Lwt.return (Filename.concat dir name))

let () = file "file"

let () =
  file ~options:60
    ~headers:(Cohttp.Header.init_with "x-test" "yes")
    "file_options"

let () = file ~content_type:"text/x-test" "file_content_type"

let () =
  file ~content_type:"text/x-test"
    ~headers:(Cohttp.Header.init_with "content-type" "text/x-header")
    "file_content_type_headers"

let () = file ~charset:"iso-8859-1" "file_charset"

let () =
  Registration.File_ct.register
    ~service:
      (Service.create ~path:(Service.Path ["file_ct"])
         ~meth:(Service.Get P.unit) ()) (fun () () ->
    Lwt.return (Filename.concat dir "text.txt", "application/x-test"))

(* The configuration gives the type of the extension foo, and a charset to
   all files. *)
let () =
  Eliom_test_server.Server_harness.start
    ~mime_assoc:
      (Charset_mime.update_mime_ext
         (Charset_mime.default_mime_assoc ())
         "foo" "text/x-foo")
    ~charset_assoc:(Charset_mime.empty_charset_assoc ~default:"utf-8" ())
    [App.run ()]
