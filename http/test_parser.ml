let parse_file f =
  let input = open_in f in
  let lexbuf = Lexing.from_channel input in
  try
  Http_parser.header Http_lexer.token lexbuf
  with
  Parsing.Parse_error -> failwith ("erreur vers "^ (Lexing.lexeme lexbuf))
  |e -> Http_frame.Http_error.display_http_exception e;failwith "erreur"

let _ = 
  parse_file Sys.argv.(1)
