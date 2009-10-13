let string_map f s =
  let r = ref [] in
  for i = String.length s - 1 downto 0 do
    r := f s.[i] :: !r
  done;
  !r

let jsmarshal v =
  let s = Marshal.to_string v [] in
  let s' = string_map (fun c -> Printf.sprintf "0x%02X" (Char.code c)) s in
  Printf.sprintf "[%s]" (String.concat "," s')


let fresh_id = 
  let c = ref 0 in
  fun () -> c := !c+1; "id"^string_of_int !c

