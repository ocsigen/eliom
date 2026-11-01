(* JavaScript string literals. Besides the quote and the backslash, only the
   characters below 0x20 are escaped: Node.js reads page.js in UTF-8, as a
   browser reads the page. *)

let string s =
  let b = Buffer.create (String.length s + 2) in
  Buffer.add_char b '"';
  String.iter
    (function
      | '"' -> Buffer.add_string b "\\\""
      | '\\' -> Buffer.add_string b "\\\\"
      | c when Char.code c < 0x20 -> Printf.bprintf b "\\u%04x" (Char.code c)
      | c -> Buffer.add_char b c)
    s;
  Buffer.add_char b '"';
  Buffer.contents b
