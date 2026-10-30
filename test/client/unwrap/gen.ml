(* Writes the values of the cases as the server sends them, and as the server
   prints them, then values marked for the unwrappers of the client. *)

let marked n s id = n, s, Eliom.Wrap.create_unwrapper (Eliom.Wrap.id_of_int id)

let () =
  print_endline "let cases = [|";
  List.iter
    (fun (Values.Case {name = _; value; show}) ->
       Printf.printf "  (%S, %S);\n"
         (Marshal.to_string (Eliom.Wrap.wrap value) [])
         (show value))
    Values.cases;
  print_endline "|]";
  let shared = marked 2 "shared" 1000 in
  Printf.printf "let unwrapped = %S\nlet shared = %S\nlet not_registered = %S\n"
    (Marshal.to_string (Eliom.Wrap.wrap (marked 1 "one" 1000)) [])
    (Marshal.to_string (Eliom.Wrap.wrap [shared; shared]) [])
    (Marshal.to_string (Eliom.Wrap.wrap (marked 3 "three" 1001)) [])
