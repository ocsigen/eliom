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


(* We raise this error when parsing Ocsigen configuration files. *)
exception ParseException of (Xmllexer.lexing_error * Camlp4.PreCast.Loc.t) ;

value parse_error_to_string x loc =
  Printf.sprintf "%s (%s)"
    (Xmllexer.lex_error_to_string x) (Loc.to_string loc);


module LexerArg = struct
  value error loc e = ParseException (e, loc);

  type attr_name =  [ = `AttrName of string ];
  type attr_value = [ = `AttrVal of string ];
  type attribute =  [ = `Attribute of (attr_name * attr_value) ];

  type token = [
  = `Tag of (string * (list attribute) * bool)
  | `PCData of string
  | `Endtag of string
  | `Comment of string
  | `Whitespace of string
  | `Eof
  ];

  value parse_dollar_attrname  c loc lexbuf =
      raise (ParseException (Xmllexer.EAttributeNameExpected, loc));
  value parse_dollar_attribute c loc lexbuf =
      raise (ParseException (Xmllexer.EAttributeValueExpected, loc));
  value parse_dollar_attrvalue = parse_dollar_attrname;
  value parse_dollar_token c lexbuf = `PCData "$";

end;

module Xmllexer = Xmllexer.Make (LexerArg);

type state = {
  stream : Stream.t (LexerArg.token * Loc.t);
  stack : Stack.t LexerArg.token;
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
    [ (`Comment _, s) -> read_nodes s acc
    | (`Whitespace _, s) -> read_nodes s acc
    | (`PCData pcdata, s) -> read_nodes s [(PCData pcdata)::acc]
    | (`Tag ("xi:include",
             [`Attribute (`AttrName "href", `AttrVal v)], True), s)->
        let l = rawxmlparser_file v in
        let acc = List.rev_append l acc in
        read_nodes s acc
    | (`Tag ("xi:include", _, _), s) ->
        raise (Xml_parser_error "Invalid syntax for inclusion directive")
    | (`Tag (tag, attlist, closed), s) ->
        match closed with
        [ True -> read_nodes s [Element (tag, (read_attlist attlist), [])::acc]
        | False ->read_nodes s
            [Element (tag, (read_attlist attlist), (read_elems ~tag s))::acc]
        ]
    | (`Eof, _)|(`Endtag _,_) as t ->
      do { push (fst t) s; List.rev acc}
]

and read_elems ?tag s =
  let elems = read_nodes s [] in
  match pop s with
  [ (`Endtag s, _) when (Some s) = tag -> elems
  | (`Eof, _) when tag = None -> elems
  | (t, loc) ->
      match tag with
      [ None -> raise (Internal_error EOFExpected)
      | Some s -> raise (Internal_error (EndOfTagExpected s)) ] ]

and read_attlist = List.map (fun [`Attribute (`AttrName a, `AttrVal v) -> (a,v)])

and to_expr_taglist stream loc =
  let s = {  stream = stream; stack = Stack.create (); loc = loc; } in
  read_nodes s []

and rawxmlparser_file s =
  let chan = open_in s in
  try
    let loc = Loc.mk s in
    let tree = to_expr_taglist (Xmllexer.from_stream loc True (Stream.of_channel chan)) loc
    in do { close_in chan; tree }
 with [ e ->
        do { close_in chan;
             match e with
             [ Sys_error s -> raise (Xml_parser_error s)
             | _ -> raise e
             ]
           } ]

and rawxmlparser_string s =
  let loc = Loc.ghost in
  to_expr_taglist (Xmllexer.from_string loc True s) loc;

value xmlparser rawxmlparser s = try (rawxmlparser s)
with
[ ParseException (e, loc) ->
    raise (Xml_parser_error (parse_error_to_string e loc))
| Internal_error EOFExpected ->
    raise (Xml_parser_error "EOF expected")
| Internal_error (EndOfTagExpected s) ->
    raise (Xml_parser_error ("End of tag expected: "^s))]
;

value xmlparser_file = xmlparser rawxmlparser_file;

value xmlparser_string = xmlparser rawxmlparser_string;
