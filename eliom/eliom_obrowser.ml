let jsmarshal v =
  let s = Marshal.to_string v [] in
  let rec pp i =
    if i < 0 then ""
    else if i = 0 then Printf.sprintf "0x%02X" (Char.code s.[i])
    else pp (pred i) ^ "," ^ Printf.sprintf "0x%02X" (Char.code s.[i])
  in "[" ^ pp (String.length s - 1) ^ "]"

let fresh_id = 
  let c = ref 0 in
  fun () -> c := !c+1; "id"^string_of_int !c

