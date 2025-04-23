(*
   Parse __eliom_request_data marshalled data.

ocaml str.cma ./extract_request_data.ml < /tmp/index.html | ocaml ./parse_request_data.ml
*)

let prefix_small_block = 0x80
let prefix_small_int = 0x40
let prefix_small_string = 0x20
let code_int8 = 0x00
let code_int16 = 0x01
let code_int32 = 0x02
let code_int64 = 0x03
let code_shared8 = 0x04
let code_shared16 = 0x05
let code_shared32 = 0x06
let code_block32 = 0x08
let code_block64 = 0x13
let code_string8 = 0x09
let code_string32 = 0x0a
let code_double_big = 0x0b
let code_double_little = 0x0c
let code_double_array8_big = 0x0d
let code_double_array8_little = 0x0e
let code_double_array32_big = 0x0f
let code_double_array32_little = 0x07
let code_codepointer = 0x10
let code_infixpointer = 0x11
let code_custom = 0x12
let c = stdin
let pos () = pos_in c
let read8u () = input_byte c

let read8s () =
  let i = read8u () in
  if i > 0x7f then 0x100 - i else i

let read16u () =
  let i1 = input_byte c in
  let i0 = input_byte c in
  (i1 lsl 8) lor i0

let read16s () =
  let i = read16u () in
  if i > 0x7fff then 0x10000 - i else i

let read32u () =
  let i3 = input_byte c in
  let i2 = input_byte c in
  let i1 = input_byte c in
  let i0 = input_byte c in
  (i3 lsl 24) lor (i2 lsl 16) lor (i1 lsl 8) lor i0

let read32s () =
  let i = read32u () in
  if i > 0x7fffffff then 0x100000000 - i else i

let read64u () =
  let i1 = read32u () in
  let i0 = read32u () in
  (i1 lsl 32) lor i0

let readstr len = really_input_string c len

type desc =
  | Block of int * t array * int
  | Int of int
  | String of string
  | Ref of int
  | Float of float
  | Int64 of Int64.t
  | Int32 of Int32.t
  | Nativeint of Nativeint.t

and t = { desc : desc; start : int; mutable fin : int }

let double_little () =
  let start = pos () in
  let a = ref 0L in
  for i = 0 to 7 do
    a :=
      Int64.logor !a (Int64.shift_left (Int64.of_int (read8u ())) ((7 - i) * 8))
  done;
  let fin = pos () in
  { desc = Float (Int64.to_float !a); start; fin }

let double_big () =
  let start = pos () in
  let a = ref 0L in
  for i = 0 to 7 do
    a := Int64.logor !a (Int64.shift_left (Int64.of_int (read8u ())) (i * 8))
  done;
  let fin = pos () in
  { desc = Float (Int64.to_float !a); start; fin }

let intern () =
  let magic = read32u () in
  let block_len = read32u () in
  let num_objects = read32u () in
  let size_32 = read32u () in
  let size_64 = read32u () in
  assert (magic = 0x8495a6be);
  ignore (size_32, size_64, block_len, num_objects);
  let obj_counter = ref 0 in
  let dummy = { desc = Int 0; start = 0; fin = 0 } in
  let rec intern_rec () =
    let start = pos () in
    let code = read8u () in
    if code >= prefix_small_int then
      if code >= prefix_small_block then (
        let tag = code land 0xF in
        let size = (code lsr 4) land 0x7 in
        let v = Array.make size dummy in
        let c = !obj_counter in
        if size > 0 then incr obj_counter;
        for i = 0 to size - 1 do
          v.(i) <- intern_rec ()
        done;
        let fin = pos () in
        { desc = Block (tag, v, c); start; fin })
      else { desc = Int (code land 0x3F); start; fin = start + 1 }
    else if code >= prefix_small_string then (
      let len = code land 0x1F in
      incr obj_counter;
      let s = readstr len in
      let fin = pos () in
      { desc = String s; start; fin })
    else
      match code with
      | 0x00 -> { desc = Int (read8s ()); start; fin = start + 2 }
      | 0x01 -> { desc = Int (read16s ()); start; fin = start + 3 }
      | 0x02 -> { desc = Int (read32s ()); start; fin = start + 5 }
      | 0x03 -> assert false
      | 0x04 ->
          let ofs = read8u () in
          { desc = Ref (!obj_counter - ofs); start; fin = start + 2 }
      | 0x05 ->
          let ofs = read16u () in
          { desc = Ref (!obj_counter - ofs); start; fin = start + 3 }
      | 0x06 ->
          let ofs = read32u () in
          { desc = Ref (!obj_counter - ofs); start; fin = start + 5 }
      | 0x08 ->
          let header = read32u () in
          let tag = header land 0xFF in
          let size = header lsr 10 in
          let v = Array.make size dummy in
          let c = !obj_counter in
          if size > 0 then incr obj_counter;
          for i = 0 to size - 1 do
            v.(i) <- intern_rec ()
          done;
          let fin = pos () in
          { desc = Block (tag, v, c); start; fin }
      | 0x13 -> assert false
      | 0x09 ->
          let len = read8u () in
          incr obj_counter;
          { desc = String (readstr len); start; fin = start + len + 2 }
      | 0x0a ->
          let len = read32u () in
          incr obj_counter;
          { desc = String (readstr len); start; fin = start + len + 5 }
      | 0x0c ->
          incr obj_counter;
          double_little ()
      | 0x0b ->
          incr obj_counter;
          double_big ()
      | 0x0e ->
          let size = read8u () in
          let tag = 254 in
          let c = !obj_counter in
          if size > 0 then incr obj_counter;
          let v = Array.make size dummy in
          incr obj_counter;
          for i = 0 to size - 1 do
            v.(i) <- double_little ()
          done;
          let fin = pos () in
          { desc = Block (tag, v, c); start; fin }
      | 0x0d ->
          let size = read8u () in
          let tag = 254 in
          if size > 0 then incr obj_counter;
          let v = Array.make size dummy in
          incr obj_counter;
          for i = 0 to size - 1 do
            v.(i) <- double_big ()
          done;
          let fin = pos () in
          { desc = Block (tag, v, !obj_counter - 1); start; fin }
      | 0x07 ->
          let header = read32u () in
          let tag = header land 0xFF in
          let size = header lsr 10 in
          if size > 0 then incr obj_counter;
          let v = Array.make size dummy in
          incr obj_counter;
          for i = 0 to size - 1 do
            v.(i) <- double_little ()
          done;
          let fin = pos () in
          { desc = Block (tag, v, !obj_counter - 1); start; fin }
      | 0x0f ->
          let header = read32u () in
          let tag = header land 0xFF in
          let size = header lsr 10 in
          if size > 0 then incr obj_counter;
          let v = Array.make size dummy in
          incr obj_counter;
          for i = 0 to size - 1 do
            v.(i) <- double_big ()
          done;
          let fin = pos () in
          { desc = Block (tag, v, !obj_counter - 1); start; fin }
      | 0x10 | 0x11 -> assert false
      | 0x12 | 0x19 -> (
          incr obj_counter;
          let ch = input_char c in
          assert (ch = '_');
          let ch = input_char c in
          assert (ch <> '\000');
          let z = input_char c in
          assert (z = '\000');
          match ch with
          | 'j' ->
              let a = ref 0L in
              for _ = 0 to 7 do
                a :=
                  Int64.logor (Int64.shift_left !a 8) (Int64.of_int (read8u ()))
              done;
              { desc = Int64 !a; start; fin = start + 9 }
          | 'i' ->
              let a = ref 0l in
              for _ = 0 to 3 do
                a :=
                  Int32.logor (Int32.shift_left !a 8) (Int32.of_int (read8u ()))
              done;
              { desc = Int32 !a; start; fin = start + 5 }
          | 'n' ->
              let c = read8u () in
              assert (c = 1);
              let a = ref 0n in
              for _ = 0 to 7 do
                a :=
                  Nativeint.logor
                    (Nativeint.shift_left !a 8)
                    (Nativeint.of_int (read8u ()))
              done;
              { desc = Nativeint !a; start; fin = start + 10 }
          | _ -> assert false)
      | _ -> assert false
  in
  intern_rec ()

let rec is_list v =
  match v.desc with
  | Block (0, [| _; r |], _) -> r.desc = Int 0 || is_list r
  | _ -> false

let rec print f v =
  if is_list v then
    match v.desc with
    | Block (_, a, _) ->
        Format.fprintf f "@[<1>[%a%a]#%d@]" print a.(0) print_list a.(1)
          (v.fin - v.start)
    | _ -> assert false
  else
    match v.desc with
    | Block (t, [||], _n) -> Format.fprintf f "@[<1>{%d:}@]" t
    | Block (t, a, n) ->
        Format.fprintf f "@[<1>(%d){%d:" n t;
        Array.iter (fun v -> Format.fprintf f "@ %a" print v) a;
        Format.fprintf f "}#%d@]" (v.fin - v.start)
    | Int i -> Format.fprintf f "%d" i
    | String s -> Format.fprintf f "\"%s\"" (String.escaped s)
    | Ref i -> Format.fprintf f "@@%d" i
    | Float d -> Format.fprintf f "%f" d
    | Int64 i -> Format.fprintf f "%LdL" i
    | Int32 i -> Format.fprintf f "%ldl" i
    | Nativeint i -> Format.fprintf f "%ndn" i

and print_list f v =
  match v.desc with
  | Block (_, a, _) -> Format.fprintf f ";@ %a%a" print a.(0) print_list a.(1)
  | _ -> ()

let _ =
  let v = intern () in
  match v.desc with
  | Block (0, [| mark; v |], _) -> (
      Format.printf "ZZ mark@.";
      Format.printf "@[%a@]@." print mark;
      match v.desc with
      | Block
          ( 0,
            [|
              global_data;
              request_data;
              event_handlers;
              client_attribs;
              sess_info;
            |],
            _ ) ->
          let pr s v =
            Format.printf "ZZ %s (%d)@." s (v.fin - v.start);
            Format.printf "@[%a@]@." print v
          in
          pr "global_data" global_data;
          pr "request_data" request_data;
          pr "event_handlers" event_handlers;
          pr "client_attribs" client_attribs;
          pr "sess_info" sess_info
      | _ -> assert false)
  | _ -> assert false
