(* Reads back the values sent by the server, and prints them; a value printed
   differently on the server is reported. *)

let unwrap s = Eliom.Unwrap.unwrap s 0

let print name f =
  match f () with
  | shown -> Printf.printf "%s: %s\n" name shown
  | exception Failure message -> Printf.printf "%s: Failure %S\n" name message
  | exception Js_of_ocaml.Js_error.Exn e ->
      Printf.printf "%s: JavaScript error %S\n" name
        (Js_of_ocaml.Js_error.to_string e)

let () =
  List.iteri
    (fun n (Values.Case {name; value = _; show}) ->
       let encoded, on_server = Encoded.cases.(n) in
       print name (fun () ->
         match show (unwrap encoded) with
         | shown when shown = on_server -> shown
         | shown -> Printf.sprintf "%s, NOT %s on the server" shown on_server))
    Values.cases

(* The unwrapper registered for a mark replaces the marked value, once even if
   the value is shared. A value whose mark has no unwrapper is kept. *)
let calls = ref 0

let () =
  Eliom.Unwrap.register_unwrapper (Eliom.Unwrap.id_of_int 1000)
    (fun (n, s, (_ : unit)) -> incr calls; Printf.sprintf "%d %s" n s);
  print "unwrapped" (fun () -> unwrap Encoded.unwrapped);
  calls := 0;
  print "shared" (fun () ->
    let a, b =
      match unwrap Encoded.shared with
      | [a; b] -> a, b
      | _ -> failwith "not a list of two values"
    in
    Printf.sprintf "%s, %s, shared: %b, unwrapped %d time(s)" a b (a == b)
      !calls);
  print "not registered" (fun () ->
    let n, s, (_ : unit) = unwrap Encoded.not_registered in
    Printf.sprintf "%d %s" n s);
  print "not a value" (fun () -> unwrap "not a marshalled value")

let () =
  print "big-endian float array" (fun () ->
    let (a : float array), b = unwrap Encoded.big_endian in
    Printf.sprintf "%d floats, last %h, shared: %b" (Array.length a) a.(255)
      (a == b))
