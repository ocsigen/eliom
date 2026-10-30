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

(* The message of a big-endian server: a long float array, shared. It is the
   one of this server with the code of the array and the bytes of each float
   reversed. *)
let big_endian =
  let floats = Array.init 256 float_of_int in
  let s = Marshal.to_string (Eliom.Wrap.wrap (floats, floats)) [] in
  (* CODE_DOUBLE_ARRAY32_LITTLE, then the length, 256 *)
  let code = "\x07\x00\x00\x01\x00" in
  let n = String.length code in
  let rec find i =
    if i + n > String.length s
    then failwith "no long float array in the message"
    else if String.sub s i n = code
    then i
    else find (i + 1)
  in
  let i = find 0 in
  let b = Bytes.of_string s in
  (* CODE_DOUBLE_ARRAY32_BIG *)
  Bytes.set b i '\x0f';
  for k = 0 to 255 do
    for j = 0 to 7 do
      Bytes.set b (i + n + (8 * k) + j) s.[i + n + (8 * k) + 7 - j]
    done
  done;
  Bytes.to_string b

let () = Printf.printf "let big_endian = %S\n" big_endian
