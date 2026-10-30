(* Writes the data scripts of a page of the test application as a JavaScript
   file that runs them in the global scope, as a browser does. *)

let open_cdata = "//<![CDATA[\n"
let close_cdata = "\n//]]>"

let find s sub from =
  let n = String.length s and m = String.length sub in
  let rec loop i =
    if i + m > n
    then None
    else if String.sub s i m = sub
    then Some i
    else loop (i + 1)
  in
  loop from

(* The contents of the CDATA sections of [page] *)
let rec cdata_sections page from =
  match find page open_cdata from with
  | None -> []
  | Some i -> (
      let start = i + String.length open_cdata in
      match find page close_cdata start with
      | None -> failwith "unterminated CDATA section"
      | Some stop ->
          String.sub page start (stop - start)
          :: cdata_sections page (stop + String.length close_cdata))

let () =
  Eliom_test_server.Server_harness.with_server "./server.exe" @@ fun server ->
  let b = Eliom_test_server.Browser.create server in
  let r = Lwt_main.run (Eliom_test_server.Browser.get b "/page") in
  cdata_sections r.body 0
  |> List.iter (fun script ->
    Printf.printf "require(\"vm\").runInThisContext(%s);\n"
      (Js_literal.string script))
