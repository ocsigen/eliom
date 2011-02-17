{
(* Ocsigen
 * http://www.ocsigen.org
 * http_lexer.mll Copyright (C) 2005 Denis Berthod
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

  open Ocsigen_http_frame
  open Http_header


  let meth_of_string =
    function
      | "GET" -> GET
      | "POST" -> POST
      | "HEAD" -> HEAD
      | "PUT" -> PUT
      | "DELETE" -> DELETE
      | "TRACE" -> TRACE
      | "OPTIONS" -> OPTIONS
      | "CONNECT" -> CONNECT
      | "LINK" -> LINK
      | "UNLINK" -> UNLINK
      | "PATCH" -> PATCH
      | s -> raise (Http_error.Http_exception (405, Some ("unknown method "^s), None))

  let proto_of_string s = match String.uppercase s with
    | "HTTP/1.1" -> HTTP11
    | "HTTP/1.0" -> HTTP10
    | s -> raise (Http_error.Http_exception (505, None, None))

  let add_header (n, v) h =
    let field = String.concat " " (List.rev v) in
    { h with headers = Http_headers.add n field h.headers }

  let handle_eof lexbuf =
    raise (Http_error.Http_exception (400, Some "unexpected end of file", None))
  let handle_other lexbuf =
    raise (Http_error.Http_exception
             (400, Some ("unexpected character " ^ Lexing.lexeme lexbuf), None))
}

(* RFC 2616, sect. 2.2 *)
let char = ['\000'-'\127']
let ctl = ['\000'-'\031' '\127']
let lowalpha = ['a'-'z']
let upalpha  = ['A'-'Z']
let alpha = upalpha | lowalpha
let digit = ['0'-'9']
let alpha = lowalpha | upalpha
let blank = [' ' '\t']
let crlf = "\r"? "\n"
let separators =
  ['(' ')' '<' '>' '@' ',' ';' ':' '\\' '\"'
   '/' '[' ']' '?' '=' '{' '}' ' ' '\t']
let lws = crlf? [' ' '\t'] +

let method_ = alpha*

(* it is more general than what RFC request *)
let request_URI = (_ #ctl #[' '] )*
let http_version = ['h' 'H'] ['t' 'T'] ['t' 'T'] ['p' 'P'] "/" digit+ "." digit+

let token          = ((char #separators) #ctl)+
(* token           = 1*<any CHAR except CTLs or separators> *)

let field_name     = token
let field_content  = (_ #ctl #[' '] )*
(* <the OCTETs making up the field-value
   and consisting of either *TEXT or combinations
   of token, separators, and quoted-string> *)

let status_code    = digit digit digit
let reason_phrase  = (_ #ctl)*

let sp = blank+

rule header = parse
  | (method_ as meth) sp (request_URI as uri) sp (http_version as version) crlf
      {	nofirstline
	  { proto = proto_of_string(version);
	    mode = Query ( meth_of_string(meth), uri );
	    headers = Http_headers.empty }
	  lexbuf }
  | (http_version as version) sp ( status_code as status_code ) sp reason_phrase crlf
      {	nofirstline
	  { proto = proto_of_string(version);
	    mode = Answer ( int_of_string status_code );
	    headers = Http_headers.empty }
	  lexbuf }
  | eof                            { handle_eof lexbuf }
  | _                              { handle_other lexbuf }

and nofirstline h = parse
  | crlf                           { h }
  | (field_name as field_name) ":" { line field_name [] h lexbuf }
  | eof                            { handle_eof lexbuf }
  | _                              { handle_other lexbuf }

and line name content h = parse
  | crlf crlf              { add_header (Http_headers.name name,content) h }
  | crlf (field_name as field_name) ":"
                           { line field_name [] (add_header (Http_headers.name name,content) h) lexbuf }
  | lws                    { line name content h lexbuf }
  | blank                  { line name content h lexbuf }
  | ( field_content as c ) { line name (c::content) h lexbuf }
  | eof                    { handle_eof lexbuf }
  | _                      { handle_other lexbuf }
