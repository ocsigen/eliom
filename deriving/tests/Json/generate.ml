(*pp ${DERIVING} *)

let p : Decl.all Deriving_Json.t = Json.t<Decl.all>

let json = Deriving_Json.to_string p Decl.all

let _ =
  print_endline "### Initial value";
  print_endline (Decl.print_all Decl.all);
  print_endline "### Json value";
  print_endline json

let _ =
  let out = open_out "data.json" in
  output_string out json;
  close_out out

let _ =
  let v = Deriving_Json.from_string p json in
  print_endline "### Unmarshaled value";
  print_endline (Decl.print_all v);
  if (v = Decl.all) then print_endline "OK" else (print_endline "KO"; exit 1)

