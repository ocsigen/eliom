(* Ocsigen
 * Copyright (C) 2007 Gabriel Kerneis
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
(*
   Parseur camlp4 pour XML sans antiquotations
*)
open Camlp4.PreCast;

type xml =
  [ Element of (string * (list (string * string)) * (list xml))
  | PCData of string ];

exception Xml_parser_error of string ;

value nocaml_msg =
        "Caml code not allowed in configuration file. Use $$ to escape $." ;


module B = Xmllexer.BasicTypes;

    type state = {
      stream : Stream.t (B.token * Loc.t);
      stack : Stack.t B.token;
      loc : Loc.t
    };
    type error_msg =
      [ EndOfTagExpected of string
      | EOFExpected ];
    exception Internal_error of error_msg;

   (* Stack - the type of s is state *)
    value pop s =
      try ((Stack.pop s.stack), s)
      with
      [ Stack.Empty ->
          let (t, l) = Stream.next s.stream
          in (t, {  stream = s.stream; stack = s.stack; loc = l; }) ];
    value push t s = Stack.push t s.stack;

   (* Convert a stream of tokens into an xml tree list *)
    value rec read_nodes s acc =
      match pop s with
      [ (B.Comment _, s) -> read_nodes s acc
      | (B.Whitespace _, s) -> read_nodes s acc
      | (B.PCData pcdata, s) -> read_nodes s [(PCData pcdata)::acc]
      | (B.Tag (tag, attlist, closed), s) ->
                  match closed with
          [ True -> read_nodes s [Element (tag, (read_attlist s attlist), [])::acc]
          | False ->read_nodes s
                           [Element (tag, (read_attlist s attlist), (read_elems ~tag s))::acc]
                  ]
      | (B.CamlExpr _, _) | (B.CamlString _, _)|(B.CamlList _, _) ->
                        raise (Xml_parser_error nocaml_msg)
          | (B.Eof, _)|(B.Endtag _,_) as t ->
                        do { push (fst t) s; List.rev acc}
          ]

   and read_elems ?tag s =
      let elems = read_nodes s [] in
        match pop s with
        [ (B.Endtag s, _) when (Some s) = tag -> elems
        | (B.Eof, _) when tag = None -> elems
        | (t, loc) ->
            match tag with
            [ None -> raise (Internal_error EOFExpected)
            | Some s -> raise (Internal_error (EndOfTagExpected s)) ] ]

   and read_attlist s =
      fun
      [ [] -> []
      | [ `Attribute (`Attr a, `Val v) :: l ] ->
          [ (a,v) :: (read_attlist s l) ]
      | [ `Attribute (`CamlAttr _, `Val _) :: _ ] |
          [ `Attribute (_, `CamlVal _) :: _ ] | [ `CamlList _ :: _ ] ->
                  raise (Xml_parser_error nocaml_msg)];

    value to_expr_taglist stream loc =
      let s = {  stream = stream; stack = Stack.create (); loc = loc; } in
          read_nodes s [];

value rawxmlparser_file s =
  let chan = open_in s in
  let loc = Loc.mk s in
  let tree = to_expr_taglist (Xmllexer.from_stream loc True (Stream.of_channel chan)) loc
  in do { close_in chan; tree };

value rawxmlparser_string s =
  let loc = Loc.ghost in
  to_expr_taglist (Xmllexer.from_string loc True s) loc;

value xmlparser rawxmlparser s = try (rawxmlparser s)
with
[ Xmllexer.Error.ParseException (e, loc) ->
    raise (Xml_parser_error (Xmllexer.Error.to_string e loc))
| Internal_error EOFExpected ->
    raise (Xml_parser_error "EOF expected")
| Internal_error (EndOfTagExpected s) ->
    raise (Xml_parser_error ("End of tag expected: "^s))]
;

value xmlparser_file = xmlparser rawxmlparser_file;

value xmlparser_string = xmlparser rawxmlparser_string;
