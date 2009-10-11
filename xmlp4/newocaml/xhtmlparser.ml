(* Ocsigen
 * Copyright (C) 2007 Gabriel Kerneis
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

open Camlp4.PreCast;


(* For Camlp4 error handling when parsing xhtml inlined in OCaml code *)
module Error = struct

  type t = Xmllexer.lexing_error;

  value to_string = Xmllexer.lex_error_to_string;

  value tloc_to_string x loc =
    Printf.sprintf "%s (%s)"
      (to_string x) (Loc.to_string loc);


  exception E of t;

  value print ppf e = Format.fprintf ppf "%s" (to_string e);

end;

module EH = Camlp4.ErrorHandler.Register Error;

module LexerArg = struct
  value error loc e = Loc.Exc_located(loc, Error.E e);

  type attr_name =  [ = `AttrName of string | `CamlAttrName of string ];
  type attr_value = [ = `AttrVal of string | `CamlAttrVal of string ];
  type attribute =  [ = `Attribute of (attr_name * attr_value)
                      | `CamlAttributes of string ];

  type token = [
  = `Tag of (string * (list attribute) * bool)
  | `PCData of string
  | `Endtag of string
  | `Comment of string
  | `CamlString of string
  | `CamlList of string
  | `CamlExpr of string
  | `Whitespace of string
  | `Eof
  ];

  value parse_dollar_attribute = Camllexer.attribute_dollar;
  value parse_dollar_attrvalue = Camllexer.attr_data_dollar;
  value parse_dollar_attrname = Camllexer.attr_name_dollar;
  value parse_dollar_token = Camllexer.token_dollar;

end;

module Make (Syntax: Camlp4.Sig.Camlp4Syntax with module Loc = Loc and module Ast = Ast) =
struct
  module Xmllexer =
    Xmllexer.Make(LexerArg);

  value blocktags = [ "fieldset"; "form"; "address"; "body"; "head";
                      "blockquote"; "div"; "html";
                      "h1"; "h2"; "h3"; "h4"; "h5"; "h6";
                      "p"; "dd"; "dl"; "li"; "ol";
                      "ul"; "colgroup"; "table"; "tbody"; "tfoot";
                      "thead"; "td"; "th"; "tr" ] ;

  value semiblocktags = [ "pre"; "style"; "title" ] ;

  type state = {
    stream : Stream.t (LexerArg.token * Loc.t)  ;
    stack :  Stack.t LexerArg.token;
    loc : Loc.t ;
  } ;



  exception CamlListExc of string ;


  (* Error report *)
  module Error = struct

    type t =
        [ EndOfTagExpected of string
        | EOFExpected
        | NoMoreTagExpected
        | NoMoreData ] ;

    exception E of t ;

    open Format ;

    value print ppf = fun
      [ NoMoreData  -> fprintf ppf "No more data : empty quotation ?"
      | EndOfTagExpected tag ->
          fprintf ppf "Missing end of tag %S" tag
      | EOFExpected -> fprintf ppf "End of file expected"
      | NoMoreTagExpected -> fprintf ppf "End of quotation expected"
      ];

    value to_string x =
        let b = Buffer.create 50 in
        let () = bprintf b "%a" print x in Buffer.contents b ;
  end;


  value err error loc = do {
    Format.eprintf "Error: %a: %a@." Loc.print loc Error.print error ;
    raise(Loc.Exc_located(loc, Error.E error))
  };

  open Error ;

  (* Stack - the type of s is state *)
  value pop s =
    try (Stack.pop s.stack, s)
    with [Stack.Empty ->
            let (t,l) = Stream.next s.stream in
            (t, {stream = s.stream ; stack = s.stack ; loc = l })];

  value push t s =
    Stack.push t s.stack ;


  value rec expr_of_list loc = fun
    [ [] -> <:expr< [] >>
    | [(`Elt a)::l] ->
        <:expr< [ $a$ :: $expr_of_list loc l$ ] >>
    | [(`List a)::l] ->
        <:expr< $a$ @ $expr_of_list loc l$ >>
    ];

  value parse = Xmllexer.from_string ;

  (* To parse antiquotations *)

  value get_expr v loc =
    Syntax.Gram.parse_string Syntax.expr_eoi loc v;


  (*
    Nicolas Pouillard 20080218:

    In  the  antiquotation $str:s$ of <:expr<...>> the 's' string is supposed
    to  be  properly  escaped,  that's  not  any OCaml string, that's any litteral
    OCaml   string   (some   chars   between   double   quote).   You  should  use
    $str:String.escaped s$ or the shortcut for it $`str:s$.

  *)

  (* Convert a stream of tokens into an xhtml tree *)
  value rec read_node s =
    let loc = s.loc in
    match pop s with
      [ (`PCData s, _) ->
          <:expr< ((XHTML.M.tot (XML.EncodedPCDATA $str:String.escaped s$))
                     : XHTML.M.elt [> Xhtmltypes.pcdata ]) >>
      | (`CamlString s, _) ->
          <:expr< ((XHTML.M.tot (XML.EncodedPCDATA $get_expr s loc$))
                     : XHTML.M.elt [> Xhtmltypes.pcdata ]) >>
      | (`CamlList s, _) -> raise (CamlListExc s)
      | (`CamlExpr s, _) -> get_expr s loc
      | (`Whitespace s, _) ->
          <:expr< XHTML.M.tot (XML.Whitespace $str:String.escaped s$) >>
      | (`Comment s, _) ->
          <:expr< XHTML.M.tot (XML.Comment $str:String.escaped s$) >>
      | (`Tag (tag, attlist, closed), s) ->
          let constr =
            if List.mem tag blocktags
            then "BlockElement"
            else (if List.mem tag semiblocktags
                  then "SemiBlockElement"
                  else "Element")
          in
          match closed with
          [ True ->
              <:expr< ((XHTML.M.tot (XML.$uid:constr$ $str:tag$
                                       $read_attlist s attlist$ []))
                         : XHTML.M.elt [> `$uid: String.capitalize tag$])>>
          | False ->
              let foo = <:expr<
                ($read_elems ~tag s$ :>
                   list (XHTML.M.elt [< Xhtmltypes.$lid:tag^"_content"$]))>>
              in
              <:expr< ((XHTML.M.tot (XML.$uid:constr$ $str:tag$
                                       $read_attlist s attlist$
                                       (XHTML.M.toeltl $foo$) ))
                         : XHTML.M.elt [> `$uid: String.capitalize tag$])
              >>
          ]
      | ((`Endtag _ | `Eof as t),_) ->
        do {push t s;
            raise (E NoMoreData)}
      ]

  and read_elems ?tag s =
    let elems = ref [] in
    let loc = s.loc in
    let _ =
      try
        while True do {
          try
            match (read_node s, elems.val) with [
              (* TODO: concaténer les retours à la ligne et $ des PCData en ajoutant :
                 | (PCData c , [(PCData c2) :: q]) ->
                 elems.val := [PCData (Printf.sprintf "%s\n%s" c2 c) :: q]
                 il faut traduire les PCData du pattern matching et de l'expression en
                 leur équivalent Ast.*)
              (x,l) -> elems.val := [(`Elt x) :: l] ]
          with [
            CamlListExc e ->
              let l = get_expr e s.loc in
              elems.val := [ (`List l) :: elems.val ]
          ]
        }
      with
      [E NoMoreData -> ()]
    in
    match pop s with
    [ (`Endtag s,_) when Some s = tag ->
        <:expr< $expr_of_list loc (List.rev elems.val) $ >>
    | (`Eof,_) when tag = None ->
        <:expr< $expr_of_list loc (List.rev elems.val) $ >>
    | (t,s) ->
        match tag with
        [ None -> err EOFExpected s.loc
        | Some t -> err (EndOfTagExpected t) s.loc
        ]
    ]

  and read_attlist s =
    let loc = s.loc in fun
    [ [] -> <:expr< [] >>
    | [`Attribute (`AttrName a, `AttrVal v)::l] ->
        <:expr< [ (XML.string_attrib $str:a$ $str:v$) :: $read_attlist s l$ ] >>
    | [`Attribute (`CamlAttrName a, `AttrVal v)::l] ->
        <:expr< [ (XML.string_attrib $get_expr a loc$ $str:v$) ::
                    $read_attlist s l$ ] >>
    | [`Attribute (`AttrName a, `CamlAttrVal v)::l] ->
        <:expr< [ (XML.string_attrib $str:a$ $get_expr v loc$) ::
                    $read_attlist s l$ ] >>
    | [`Attribute (`CamlAttrName a, `CamlAttrVal v)::l] ->
        <:expr< [ (XML.string_attrib $get_expr a loc$ $get_expr v loc$) ::
                    $read_attlist s l$ ] >>
    | [`CamlAttributes cl ::l] ->
        <:expr< [ (XHTML.M.to_xmlattribs $get_expr cl loc$) ::
                    $read_attlist s l$ ] >>
    ];

    (* FIXED ? please report any problem with this function *)
    (* remove the white spaces at the begining of a stream *)
  value rec clean_ws s =
    match Stream.next s.stream with
    [ (`Whitespace _,l) -> clean_ws {(s) with loc = l }
    | (t,l) -> let _ = push t s in {(s) with loc = l } ] ;


  value  to_expr stream loc =
    let s = {stream = stream; stack = Stack.create() ; loc = loc } in
    try
      let v = read_node (clean_ws s) in
      try match pop s with
        [ (`Eof, _) -> v
        | (_, s) -> err NoMoreTagExpected s.loc
        ]
      with [ _ -> v ]
    with [E NoMoreData -> err NoMoreData loc];

  value to_expr_taglist stream loc =
    let s = {stream = stream; stack = Stack.create() ; loc = loc } in
    try <:expr< $read_elems (clean_ws s)$  >>
    with [E NoMoreData -> err NoMoreData loc];

  (* remove the white spaces at the end of a string *)
  value remove_ws s =
    let rec end_index i = match s.[i] with
        ['\n'|'\t'|' '|'\r' -> end_index (i-1)
        |_ -> i]
    in
    let sub i j = String.sub s i ( j - i + 1) in
    try sub 0 (end_index (String.length s - 1))
    with [Invalid_argument _ -> ""];

  value xml_exp loc (x : option string) s =
    to_expr (parse loc False (remove_ws s)) loc;

  value xml_expl loc (x : option string) s =
    to_expr_taglist (parse loc False (remove_ws s)) loc;

 end ;
