{
open Http_parser
open Http_frame
}

let blank = [' ' '\t']
let strin=[^ ':' ' ' '\n' '\r' '\t']*
let integer = ['0'-'9']*
let proto =['h' 'H'] ['t' 'T'] ['t' 'T'] ['p' 'P'] '/' integer '.' integer

rule token =
  parse
  |blank                {print_string " ";token lexbuf}
  |"GET"                {print_string "GET";METHOD "GET"}
  |"POST"               {METHOD "POST"}
  |"HEAD"               {METHOD "HEAD"}
  |"PUT"                {METHOD "PUT"}
  |"DELETE"             {METHOD "DELETE"}
  |"TRACE"              {METHOD "TRACE"}
  |"OPTIONS"            {METHOD "OPTIONS"}
  |"CONNECT"            {METHOD "CONNECT"}
  |"LINK"               {METHOD "LINK"}
  |"UNLINK"             {METHOD "UNLINK"}
  |"PATCH"              {METHOD "PATCH"}
  |"\r\n"               {print_endline "";EOL}
  |":"                  {print_string ":";COLON}
  |"\n"                 {print_endline "";EOL}
  |integer              {print_string "<code>";CODE (int_of_string (Lexing.lexeme lexbuf))}
  |proto                {print_string "<proto>";PROTO (Lexing.lexeme lexbuf)}
  |strin                {print_string (Lexing.lexeme lexbuf);
			 STRING (Lexing.lexeme lexbuf)}
  |eof                  {raise (Http_error.Http_exception (Some 400,["Unexpected end of
                                file"]))}
  |_                    {raise (Http_error.Http_exception (Some 400,["lexer error"
                        ^(Lexing.lexeme lexbuf)]))}
