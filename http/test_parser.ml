(* Ocsigen
 * test_parser.ml Copyright (C) 2005 Denis Berthod
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

let parse_file f =
  let input = open_in f in
  let lexbuf = Lexing.from_channel input in
  try
  Http_parser.header Http_lexer.token lexbuf
  with
  Parsing.Parse_error -> failwith ("erreur vers "^ (Lexing.lexeme lexbuf))
  |e -> Ocsigen_http_frame.Http_error.display_http_exception e;failwith "erreur"

let _ =
  parse_file Sys.argv.(1)
