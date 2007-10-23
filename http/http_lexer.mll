{
(* Ocsigen
 * http://www.ocsigen.org
 * http_lexer.mll Copyright (C) 2005 Denis Berthod
 * Laboratoire PPS - CNRS Université Paris Diderot
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception; 
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

open Http_parser
open Http_frame
}

let blank = [' ' '\t']
let strin=[^ ':' ' ' '\n' '\r' '\t']*
let integer = ['0'-'9']+
let proto =['h' 'H'] ['t' 'T'] ['t' 'T'] ['p' 'P'] '/' integer '.' integer

(* RFC 2616, sect. 2.2 *)
let octet = _
let char = ['\000'-'\127']
let upalpha = ['A'-'Z']
let loalpha = ['a'-'z']
let alpha = upalpha | loalpha
let digit = ['0'-'9']
let ctl = ['\000'-'\031' '\127']

let crlf = "\r?\n"
let lws = crlf? [' ' '\t'] +
let text = _ # ctl
  (* RFC:
     A CRLF is allowed in the definition of TEXT only as part of a
     header field continuation. It is expected that the folding LWS
     will be replaced with a single SP before interpretation of the
     TEXT value. *)
let hex = ['A'-'F' 'a'-'f'] | digit
let separators =
  ['(' ')' '<' '>' '@' ',' ';' ':' '\\' '\"'
   '/' '[' ']' '?' '=' '{' '}' ' ' '\t']
let token = (char # ctl) # separators
let quoted_pair = "\\" char
let qdtext = text # '\"'
let quoted_string = '\"' (qdtext | quoted_pair)* '\"'

rule token =
  parse
  |blank                {Messages.debug_noel " "; token lexbuf}
  |"GET"                
  |"POST"               
  |"HEAD"               
  |"PUT"                
  |"DELETE"             
  |"TRACE"              
  |"OPTIONS"            
  |"CONNECT"            
  |"LINK"               
  |"UNLINK"             
  |"PATCH"              {Messages.debug_noel (Lexing.lexeme lexbuf); 
                         METHOD (Lexing.lexeme lexbuf)}
  |"\r\n"               {Messages.debug ""; EOL}
  |":"                  {Messages.debug_noel ":";COLON}
  |"\n"                 {Messages.debug ""; EOL}
  |integer              {Messages.debug_noel (Lexing.lexeme lexbuf);
			 CODE (Lexing.lexeme lexbuf)}
  |proto                {Messages.debug_noel (Lexing.lexeme lexbuf);
			 PROTO (Lexing.lexeme lexbuf)}
  |strin                {Messages.debug_noel (Lexing.lexeme lexbuf);
			 STRING (Lexing.lexeme lexbuf)}
  |eof                  {raise (Http_error.Http_exception (400, Some "unexpected end of file"))}
  |_                    {raise (Http_error.Http_exception (400, Some ("unexpected character "
                        ^ Lexing.lexeme lexbuf)))}
