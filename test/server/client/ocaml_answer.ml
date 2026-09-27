let decode (r : Eliom_test_server.Browser.response) =
  if r.status <> 200
  then Printf.ksprintf failwith "Ocaml_answer: status %d" r.status;
  if
    Eliom_test_server.Browser.header r "content-type"
    <> Some Eliom.Service.eliom_appl_answer_content_type
  then failwith "Ocaml_answer: not an answer of an OCaml service";
  let _, data =
    (Marshal.from_string (Eliom.Lib.Url.decode r.body) 0
     : _ * _ Eliom.Runtime.eliom_caml_service_data)
  in
  data.Eliom.Runtime.ecs_data
