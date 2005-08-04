let lwtbip i = 
  let s = "bip"^(string_of_int i)^"\n" in
  Lwt_unix.write Unix.stderr s 0 (String.length s)

let lwtwarning s = 
  let s = s^"\n" in
  Lwt_unix.write Unix.stderr s 0 (String.length s)

let warning s = prerr_endline s

let bip i = prerr_endline ("bip"^(string_of_int i))

