(* Writes the values of the cases as the server sends them, and as the server
   prints them, then values marked for the unwrappers of the client, then
   floats as a big-endian server sends them. *)

let marked n s id = n, s, Eliom.Wrap.create_unwrapper (Eliom.Wrap.id_of_int id)

(* [print_cases name cases] defines [name], the array of the messages of
   [cases] and of their printing on the server; each message is changed by the
   function paired with its case. *)
let print_cases name cases =
  Printf.printf "let %s = [|\n" name;
  List.iter
    (fun (change, Values.Case {name = _; value; show}) ->
       Printf.printf "  (%S, %S);\n"
         (change (Marshal.to_string (Eliom.Wrap.wrap value) []))
         (show value))
    cases;
  print_endline "|]"

let () =
  print_cases "cases" (List.map (fun case -> Fun.id, case) Values.cases);
  let shared = marked 2 "shared" 1000 in
  Printf.printf "let unwrapped = %S\nlet shared = %S\nlet not_registered = %S\n"
    (Marshal.to_string (Eliom.Wrap.wrap (marked 1 "one" 1000)) [])
    (Marshal.to_string (Eliom.Wrap.wrap [shared; shared]) [])
    (Marshal.to_string (Eliom.Wrap.wrap (marked 3 "three" 1001)) [])

(* The object of [floats] as a little-endian or a big-endian server sends it:
   its code, then its length for an array, then the floats. *)
let float_object floats ~little =
  let int32 n =
    let b = Bytes.create 4 in
    Bytes.set_int32_be b 0 (Int32.of_int n);
    Bytes.to_string b
  in
  let code, header, values =
    match floats with
    | Values.Boxed f ->
        (* CODE_DOUBLE_LITTLE, CODE_DOUBLE_BIG *)
        (if little then '\x0c' else '\x0b'), "", [f]
    | Values.Float_array a when Array.length a < 0x100 ->
        (* CODE_DOUBLE_ARRAY8_LITTLE, CODE_DOUBLE_ARRAY8_BIG *)
        ( (if little then '\x0e' else '\x0d')
        , String.make 1 (Char.chr (Array.length a))
        , Array.to_list a )
    | Values.Float_array a ->
        (* CODE_DOUBLE_ARRAY32_LITTLE, CODE_DOUBLE_ARRAY32_BIG *)
        ( (if little then '\x07' else '\x0f')
        , int32 (Array.length a)
        , Array.to_list a )
  in
  let float f =
    let b = Bytes.create 8 in
    (if little then Bytes.set_int64_le else Bytes.set_int64_be)
      b 0 (Int64.bits_of_float f);
    Bytes.to_string b
  in
  String.make 1 code ^ header ^ String.concat "" (List.map float values)

(* The message [s] of this server, with the object of [floats] as a big-endian
   server sends it. *)
let big_endian floats s =
  if Sys.big_endian
  then s
  else
    let little = float_object floats ~little:true in
    let n = String.length little in
    let rec find i =
      if i + n > String.length s
      then failwith "no such floats in the message"
      else if String.sub s i n = little
      then i
      else find (i + 1)
    in
    let i = find 0 in
    String.sub s 0 i
    ^ float_object floats ~little:false
    ^ String.sub s (i + n) (String.length s - i - n)

let () =
  print_cases "big_endian"
    (List.map (fun (floats, case) -> big_endian floats, case) Values.big_endian)
