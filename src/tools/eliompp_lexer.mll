(** This lexer attempts to tokenize sections of an eliom file *)
{
  type token =
    | RAW of string
    | CHAR of char
    | SERVER_SECTION of (int * string)
    | SHARED_SECTION of (int * string)
    | CLIENT_SECTION of (int * string)

  let section_def = `Server
  let section_idt : [`Server | `Client | `Shared] ref = ref section_def

  let comment_ref_count = ref 0
  let section_ref_count = ref 0

  let in_comment () = !comment_ref_count <> 0
  let in_section () = !section_ref_count <> 0

  let line = ref 1

  let buf = Buffer.create 256
  let add_char c = Buffer.add_char buf c
  let add_string s = Buffer.add_string buf s
  let reset_buf () = Buffer.clear buf

  let get_buf () =
    let s = Buffer.contents buf in
    reset_buf ();
    s

  exception Unterminated_string
  exception Unterminated_comment
  exception Unterminated_section
  exception Forbidden_inner_section
  exception Unknown_section_id

  let start_cstring ?(reset = false) cstring lexbuf =
    if reset then reset_buf ();
    add_char '"';
    cstring lexbuf

  let start_comment ?(reset = false) comment lexbuf =
    if reset then reset_buf ();
    add_string "(*";
    incr comment_ref_count;
    comment lexbuf

  let we_are_ppx = ref false

  let string_of_section = function
    | `Server -> "server"
    | `Client -> "client"
    | `Shared -> "shared"

  let start_section_ppx side =
    let loc = !line in
    reset_buf ();
    we_are_ppx := true;
    add_string ("[%%" ^ string_of_section side ^ ".start]");
    section_idt := side;
    match side with
    | `Client -> CLIENT_SECTION (loc, get_buf ())
    | `Server -> SERVER_SECTION (loc, get_buf ())
    | `Shared -> SHARED_SECTION (loc, get_buf ())

  let start_section ?(reset = false) ~idt section lexbuf =
    if reset then reset_buf ();
    add_char '{';
    add_string idt;
    add_char '{';
    (match idt with
     | "server" ->
         if not (in_section ())
         then section_idt := `Server
         else raise Forbidden_inner_section
     | "client" ->
         if not (in_section ())
         then section_idt := `Client
         else raise Forbidden_inner_section
     | "shared" ->
         if not (in_section ())
         then section_idt := `Shared
         else raise Forbidden_inner_section
     | _ -> ());
    incr section_ref_count;
    section lexbuf
}

let ident = [' ' '\t' 'a'-'z' 'A'-'Z' '0'-'9' '(' ')']+

rule token = parse
  | '"'                         {
      start_cstring cstring lexbuf;
      RAW (get_buf ())
  }
  | "(*)"                       { RAW ("(*)") }
  | "(*"                        {
      start_comment ~reset:true comment lexbuf;
      RAW (get_buf ())
  }
  | '{' (ident as idt) '{'      {
      let loc = !line in
      start_section ~reset:true ~idt section lexbuf;
      let section_tk =
        match !section_idt with
        | `Client -> CLIENT_SECTION (loc, get_buf ())
        | `Server -> SERVER_SECTION (loc, get_buf ())
        | `Shared -> SHARED_SECTION (loc, get_buf ())
      in
      section_idt := section_def;
      section_tk
  }
  | "[%%server.start]" { start_section_ppx `Server }
  | "[%%client.start]" { start_section_ppx `Client }
  | "[%%shared.start]" { start_section_ppx `Shared }
  | [^ '"' '{' '(' '\n']+
    as raw                      { RAW raw }
  | '\n'                        { incr line; CHAR '\n' }
  | '{'                         { CHAR '{' }
  | '('                         { CHAR '(' }
  | '*'                         { CHAR '*' }
  | eof                         { raise End_of_file }
and cstring = parse
  | '"'                         { add_char '"' }
  | '\\' '"'                    { add_string "\\\""; cstring lexbuf }
  | '\\' '\n'                   { add_string "\\\n"; incr line; cstring lexbuf }
  | ('\\' _) as s               { add_string s; cstring lexbuf }
  | '\n'                        { add_char '\n'; incr line; cstring lexbuf }
  | [^ '"' '\\' '\n']+ as s     { add_string s; cstring lexbuf }
  | eof                         { raise Unterminated_string }
and comment = parse
  | "(*"                        { start_comment comment lexbuf; }
  | "*)"                        { add_string "*)";
                                  decr comment_ref_count;
                                  if not (in_comment ())
                                  then ()
                                  else comment lexbuf
                                }
  | "(*)"                       { add_string "(*)"; comment lexbuf }
  | '"'                         {
      try
        start_cstring cstring lexbuf;
        comment lexbuf
      with Unterminated_string -> raise Unterminated_comment
  }
  | '\n'                        { add_char '\n'; incr line; comment lexbuf }
  | '*'                         { add_char '*'; comment lexbuf }
  | '('                         { add_char '('; comment lexbuf }
  | [^ '"' '*' '(' '\n']+ as s  { add_string s; comment lexbuf }
  | eof                         { raise Unterminated_comment }
and section = parse
  | '{' (ident as idt) '{'      { start_section ~idt section lexbuf }
  | "}}"                        {
      add_string "}}";
      decr section_ref_count;
      if in_section ()
      then section lexbuf
      else ()
  }
  | "(*)"                       { add_string "(*)"; section lexbuf }
  | "(*"                        {
      try
        start_comment comment lexbuf;
        section lexbuf
      with Unterminated_comment -> raise Unterminated_section
  }
  | '"'                         {
      try
        start_cstring cstring lexbuf;
        section lexbuf
      with Unterminated_string -> raise Unterminated_section
  }
  | '\n'                        { add_char '\n'; incr line; section lexbuf }
  | '('                         { add_char '('; section lexbuf }
  | '}'                         { add_char '}'; section lexbuf }
  | '{'                         { add_char '{'; section lexbuf }
  | [^ '"' '(' '\n' '}' '{']+
    as s                        { add_string s; section lexbuf }
  | eof                         {
      if not !we_are_ppx then raise Unterminated_section
    }
