(*ocamlmklib -I ../ ../eliom_client_stubs.o ../eliom_client_unwrap.ml -o lib; ocamlfind ocamlc -linkpkg -package js_of_ocaml -syntax camlp4o -package js_of_ocaml.syntax lib.cma -I ../ unwrap.ml -o test; js_of_ocaml -pretty -noinline ../eliom_client.js test *)

open Eliom_client_unwrap

let mark = (Obj.magic "test":mark)

let id = id_of_int 42

type unwrapper =
    { id : unwrap_id;
      umark : mark; }

let unwrapper =
  { id = id;
    umark = mark }

let v = (13,unwrapper)

let () = register_unwrapper id (fun (a,b) -> (a+1,string_of_int a))

let v' = unwrap (mark,v)

let debug f =
  Printf.ksprintf
    (fun s -> Dom.appendChild (Dom_html.document##body)
      (Dom_html.document##createTextNode
	 (Js.string (s^"\n")))) f

open Lwt
let f _ =
  debug "result: %i %s = 14 13 ?" (fst v') (snd v');
  Js._true

let () = Dom_html.window##onload <- (Dom_html.handler f)
