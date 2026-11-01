type case = Case : {name : string; value : 'a; show : 'a -> string} -> case

let case name value show = Case {name; value; show}
let int name n = case name n string_of_int

type record = {i : int; s : string; f : float}

type variant =
  | Constant
  | Other_constant
  | Args of int * string
  | Record of record

type many_constructors =
  | C0 of int
  | C1 of int
  | C2 of int
  | C3 of int
  | C4 of int
  | C5 of int
  | C6 of int
  | C7 of int
  | C8 of int
  | C9 of int
  | C10 of int
  | C11 of int
  | C12 of int
  | C13 of int
  | C14 of int
  | C15 of int
  | C16 of int

type floats = {x : float; y : float}

let show_float = Printf.sprintf "%h"
let show_floats a = String.concat " " (List.map show_float (Array.to_list a))
let show_record {i; s; f} = Printf.sprintf "{i = %d; s = %S; f = %h}" i s f

let show_variant = function
  | Constant -> "Constant"
  | Other_constant -> "Other_constant"
  | Args (i, s) -> Printf.sprintf "Args (%d, %S)" i s
  | Record r -> "Record " ^ show_record r

let show_many = function
  | C0 n -> Printf.sprintf "C0 %d" n
  | C1 n -> Printf.sprintf "C1 %d" n
  | C2 n -> Printf.sprintf "C2 %d" n
  | C3 n -> Printf.sprintf "C3 %d" n
  | C4 n -> Printf.sprintf "C4 %d" n
  | C5 n -> Printf.sprintf "C5 %d" n
  | C6 n -> Printf.sprintf "C6 %d" n
  | C7 n -> Printf.sprintf "C7 %d" n
  | C8 n -> Printf.sprintf "C8 %d" n
  | C9 n -> Printf.sprintf "C9 %d" n
  | C10 n -> Printf.sprintf "C10 %d" n
  | C11 n -> Printf.sprintf "C11 %d" n
  | C12 n -> Printf.sprintf "C12 %d" n
  | C13 n -> Printf.sprintf "C13 %d" n
  | C14 n -> Printf.sprintf "C14 %d" n
  | C15 n -> Printf.sprintf "C15 %d" n
  | C16 n -> Printf.sprintf "C16 %d" n

let show_list show l = "[" ^ String.concat "; " (List.map show l) ^ "]"
let all_bytes = String.init 256 Char.chr

let cases =
  [ (* Integers, at the limits of each encoding and of the client integers:
       32 bits in JavaScript, 31 bits in WebAssembly. A 64-bit server encodes
       the integers beyond 31 bits on 64 bits, which the client refuses. *)
    int "zero" 0
  ; int "small" 63
  ; int "int8" (-128)
  ; int "int16" 32767
  ; int "int16 min" (-32768)
  ; int "int, 32-bit encoding" 0x10000
  ; int "max 31 bits" ((1 lsl 30) - 1)
  ; int "min 31 bits" (-(1 lsl 30))
  ; int "above 31 bits" (1 lsl 30)
  ; int "below 31 bits" (-(1 lsl 30) - 1)
  ; int "max 32 bits" ((1 lsl 31) - 1)
  ; int "min 32 bits" (-(1 lsl 31))
  ; int "above 32 bits" (1 lsl 31)
  ; case "int32" Int32.min_int Int32.to_string
  ; case "int64" Int64.max_int Int64.to_string
  ; case "int64 min" Int64.min_int Int64.to_string
  ; case "nativeint" 1n Nativeint.to_string
  ; (* Floats *)
    case "float" 1.5 show_float
  ; case "negative zero" (-0.) show_float
  ; case "nan" Float.nan show_float
  ; case "infinity" Float.infinity show_float
  ; case "negative infinity" Float.neg_infinity show_float
  ; case "max float" Float.max_float show_float
  ; case "subnormal" 5e-324 show_float
  ; case "float array" [|1.; 2.5|] show_floats
  ; case "long float array" (Array.init 300 float_of_int) (fun a ->
      Printf.sprintf "%d floats, last %h" (Array.length a) a.(299))
  ; case "float record" {x = 1.; y = -2.} (fun {x; y} ->
      Printf.sprintf "{x = %h; y = %h}" x y)
  ; (* Strings, at the limits of each encoding *)
    case "empty string" "" (Printf.sprintf "%S")
  ; case "string of 31 bytes" (String.make 31 'a') (fun s ->
      Printf.sprintf "%d bytes" (String.length s))
  ; case "string of 32 bytes" (String.make 32 'b') (fun s ->
      Printf.sprintf "%d bytes" (String.length s))
  ; case "string of 256 bytes" (String.make 256 'c') (fun s ->
      Printf.sprintf "%d bytes" (String.length s))
  ; case "UTF-8" "é漢🙂" (Printf.sprintf "%S")
  ; case "all bytes" all_bytes (fun s -> string_of_bool (s = all_bytes))
  ; (* Blocks *)
    case "list" [1; 2; 3] (show_list string_of_int)
  ; case "option" (Some "x") (function
      | None -> "None"
      | Some s -> Printf.sprintf "Some %S" s)
  ; case "variants"
      [Constant; Other_constant; Args (1, "a"); Record {i = 2; s = "b"; f = 3.}]
      (show_list show_variant)
  ; case "tags 0 to 16"
      [ C0 0
      ; C1 1
      ; C2 2
      ; C3 3
      ; C4 4
      ; C5 5
      ; C6 6
      ; C7 7
      ; C8 8
      ; C9 9
      ; C10 10
      ; C11 11
      ; C12 12
      ; C13 13
      ; C14 14
      ; C15 15
      ; C16 16 ]
      (show_list show_many)
  ; case "8 fields" (1, 2, 3, 4, 5, 6, 7, 8) (fun (a, b, c, d, e, f, g, h) ->
      show_list string_of_int [a; b; c; d; e; f; g; h])
  ; case "polymorphic variants"
      [`A; `B 1; `C ("c", 2.)]
      (show_list (function
         | `A -> "`A"
         | `B n -> Printf.sprintf "`B %d" n
         | `C (s, f) -> Printf.sprintf "`C (%S, %h)" s f))
  ; (* Sharing and cycles *)
    (let l = [1; 2] in
     case "sharing" (l, l) (fun (a, b) ->
       Printf.sprintf "%s, shared: %b" (show_list string_of_int a) (a == b)))
  ; (* Each kind of object counts in the table of shared objects *)
    (let short = "s"
     and medium = String.make 40 'm'
     and long = String.make 300 'l'
     and float = 1.5
     and floats = [|1.; 2.|]
     and int64 = 1L in
     case "objects shared"
       ( (short, medium, long, float, floats, int64)
       , (short, medium, long, float, floats, int64) )
       (fun ((a, b, c, d, e, f), (a', b', c', d', e', f')) ->
          Printf.sprintf "%d %d %d %h %h %Ld, shared: %b" (String.length a)
            (String.length b) (String.length c) d e.(1) f
            (a == a' && b == b' && c == c' && d == d' && e == e' && f == f')))
  ; (let rec l = 1 :: 2 :: l in
     case "cycle" l (fun l ->
       Printf.sprintf "cycle of 2: %b" (List.tl (List.tl l) == l))) ]
