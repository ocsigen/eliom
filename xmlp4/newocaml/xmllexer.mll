(****************************************************************************)
(*                                                                          *)
(*                              Objective Caml                              *)
(*                                                                          *)
(*                            INRIA Rocquencourt                            *)
(*                                                                          *)
(*  Copyright 2006-2006 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed under   *)
(*  the terms of the GNU Library General Public License, with the special   *)
(*  exception on linking described in LICENSE at the top of the Objective   *)
(*  Caml source tree.                                                       *)
(*                                                                          *)
(****************************************************************************)

(* Authors:
 * - Daniel de Rauglaudre: initial version
 * - Nicolas Pouillard: refactoring (Lexer.mll)
 * - Gabriel Kerneis : merging with xml-light => xmllexer.mll
 * - Boris Yakobowski : simplification of the code, better handling of
     XML/Xhtml with quotations mode
 *)


(* The lexer definition *)


{

type lexing_error =
    | EUnterminatedComment
    | EUnterminatedString
    | EIdentExpected
    | ECloseExpected
    | ENodeExpected
    | EAttributeNameExpected
    | EAttributeValueExpected
    | EUnterminatedEntity
    | ECamlIdentExpected
    | WNestedComments


let lex_error_to_string = function
  | EUnterminatedComment -> "Unterminated Comment"
  | EUnterminatedString  -> "Unterminated String"
  | EIdentExpected -> "Ident Expected"
  | ECloseExpected -> "Close Expected"
  | ENodeExpected -> "Node Expected"
  | EAttributeNameExpected -> "Attribute Name Expected"
  | EAttributeValueExpected -> "Attribute Value Expected"
  | EUnterminatedEntity -> "Unterminated Entity"
  | ECamlIdentExpected -> "Caml Ident Expected"
  | WNestedComments -> "Nested comments (non valid XML)"



open Camlp4
module Loc = Camlp4.PreCast.Loc
open Lexing


(* To store some context information:
     *   loc    : position of the beginning of a string, quotation and comment
     *   entity : shall we translate entities or not
*)

type context = {
  loc        : Loc.t    ;
  lexbuf     : lexbuf   ;
  buffer     : Buffer.t ;
  entity     : bool ;
}

let default_context lb = {
  loc        = Loc.ghost ;
  lexbuf     = lb        ;
  buffer     = Buffer.create 256 ;
  entity     = true ;
}

(* To buffer string literals *)

let store c = Buffer.add_string c.buffer (Lexing.lexeme c.lexbuf)
let istore_char c i = Buffer.add_char c.buffer (Lexing.lexeme_char c.lexbuf i)
let buff_contents c =
        let contents = Buffer.contents c.buffer in
        Buffer.reset c.buffer; contents

let loc c = Loc.merge c.loc (Loc.of_lexbuf c.lexbuf)

let with_curr_loc f c = f { (c) with loc = Loc.of_lexbuf c.lexbuf } c.lexbuf
let parse f c = f c c.lexbuf

let lexeme c = Lexing.lexeme c.lexbuf
let store_string c s = Buffer.add_string c.buffer s


(* Deal with entities  *)
let idents = Hashtbl.create 0

let _ = begin
        Hashtbl.add idents "gt;" ">";
        Hashtbl.add idents "lt;" "<";
        Hashtbl.add idents "amp;" "&";
        Hashtbl.add idents "apos;" "'";
        Hashtbl.add idents "quot;" "\"";
end


(* Update the current location with file name and line number. *)
let update_loc c line absolute chars =
        let lexbuf = c.lexbuf in
        let pos = lexbuf.lex_curr_p in
        lexbuf.lex_curr_p <- { pos with
        pos_lnum = if absolute then line else pos.pos_lnum + line;
        pos_bol = pos.pos_cnum - chars;
        }


(* The lexer is parameterized to handle both Xml and Xhtml with
   inlined Caml code modes. *)
module type LexerArgSig = sig
  (* We do not raise the same exception in both lexers *)
  val error : Camlp4.PreCast.Loc.t -> lexing_error -> exn

  type attr_name =  private [> `AttrName of string ]
  type attr_value =  private [> `AttrVal of string ]
  type attribute = private [> `Attribute of (attr_name * attr_value) ]

  type token = private [>
  | `Tag of (string * (attribute list) * bool)
  | `PCData of string
  | `Endtag of string
  | `Comment of string
  | `Whitespace of string
  | `Eof
  ]

  val parse_dollar_token :     context ->          lexbuf -> token
  val parse_dollar_attrname :  context -> Loc.t -> lexbuf -> attr_name
  val parse_dollar_attrvalue : context -> Loc.t -> lexbuf -> attr_value
  val parse_dollar_attribute : context -> Loc.t -> lexbuf -> attribute

end


module Make (X : LexerArgSig) = struct

let err error loc =
  raise (X.error loc error)


}

(******************************************************************************)

(* NB : ignore(buff_contents c) flushes the buffer c *)

let newline = ['\n']
let break = ['\r']
let space = [' ' '\t']
let identchar =  ['A'-'Z' 'a'-'z' '_' '0'-'9' ':' '-']
let entitychar = ['A'-'Z' 'a'-'z']
let pcchar = [^ '\r' '\n' '<' '>' '&' '$']
let camlidentchar =  [^ '$' ]

rule token c = parse
  | newline { update_loc c 1 false 0; `Whitespace (lexeme c) }
  | (space|break) + { `Whitespace (lexeme c) }
      (* WARNING: on considère les commentaires comme des noeuds ; c'est mal, mais
         c'est la seule solution si on ne veut pas les ignorer. Autre solution :
         les ignorer et rajouter un commentaire de remplissage au niveau
         du parser si on se retrouve avec une liste de noeuds vide (donc
         si le seul noeud à analyser était un commentaire *)
  | "<!--" { `Comment(comment c [Loc.of_lexbuf c.lexbuf] lexbuf)}
  | "<?" { header c lexbuf;  token c lexbuf }
  | '<' space* '/' space* {
      let tag = ident_name c lexbuf in
      ignore_spaces c lexbuf;
      close_tag c lexbuf;
      `Endtag tag
    }
  | '<' space* {
      let tag = ident_name c lexbuf in
      ignore_spaces c lexbuf;
      let attribs, closed = attributes c lexbuf in
      `Tag(tag, attribs, closed)
    }
  | "&#" {
      ignore(buff_contents c);
      store c ;
      `PCData (pcdata c lexbuf)
    }
  | '&' {
      ignore (buff_contents c);
      store_string c (entity c lexbuf);
      `PCData (pcdata c lexbuf)
    }
  | "$" {
      ignore (buff_contents c);
      X.parse_dollar_token c lexbuf
    }
  | space* pcchar+ {
      ignore (buff_contents c);
      store c ;
      `PCData (pcdata c lexbuf)
    }
  | eof { `Eof }
  | _ { err ENodeExpected (Loc.of_lexbuf lexbuf) }

and ignore_spaces c = parse
  | newline {
      update_loc c 1 false 0;
      ignore_spaces c lexbuf
    }
  | (space | break) + { ignore_spaces c lexbuf }
  | "" { () }


(* l is a list of the locations of the opened comments, which is used
   to keep track of nesting. comment should never be called with an empty
   list. *)
and comment c l = parse
(* WARNING: selon la norme XML, le symbole "--" est INTERDIT dans un commentaire.
 * Néanmoins, on choisit de gérer les commentaires imbriqués, pour faciliter
 * l'écriture des fichiers de configuration. *)
  | newline {
      update_loc c 1 false 0;
      comment c l lexbuf
    }
  | "<!--" {
(*    warn WNestedComments (Loc.of_lexbuf lexbuf) ; *)
      store_string c "<!--" ;
      ignore (comment c (loc c :: l) lexbuf);
      store_string c "-->" ;
      comment c l lexbuf
    }
  | "-->" {
      match l with
        | [] (* Should not occur... *)
        | [_] -> buff_contents c
        | _ -> ""
    }
  | eof {
      err EUnterminatedComment
        (match l with | [] -> Loc.of_lexbuf lexbuf
                      | l :: _ -> l)
    }
  | _ { store c ; comment c l lexbuf }

and header c = parse
  (* Le header est ignoré *)
  | newline {
      update_loc c 1 false 0;
      header c lexbuf
    }
  | "?>" { () }
  | eof { err ECloseExpected (loc c) }
  | _ { header c lexbuf }

and pcdata c = parse
  | pcchar+ {
      store c ;
      pcdata c lexbuf
    }
  | "&#" {
      store c ;
      pcdata c lexbuf;
    }
  | '&' {
      store_string c (entity c lexbuf);
      pcdata c lexbuf
    }
  | "$$" { store_string c "$" ; pcdata c lexbuf }
  | "" { buff_contents c }

and entity c = parse
  | entitychar+ ';' {
      let ident = lexeme c in
      if not c.entity then "&" ^ ident else
        try Hashtbl.find idents (String.lowercase ident)
        with Not_found -> "&" ^ ident
    }
  | _ | eof { err EUnterminatedEntity (Loc.of_lexbuf lexbuf) }

and ident_name c = parse
  | identchar+ { lexeme c }
  | _ | eof { err EIdentExpected (loc c)}

and close_tag c = parse
  | '>' { () }
  | _ | eof { err ECloseExpected (loc c)}

and attributes c = parse
  | '>'  { [], false }
  | "/>" { [], true }
  | "$list:" {
      let attribute = X.parse_dollar_attribute c (Loc.of_lexbuf lexbuf) lexbuf in
      ignore_spaces c lexbuf;
      let others, closed = attributes c lexbuf in
      attribute :: others, closed
    }
  | ""  {
      let attribute =
        let a = attr_name c lexbuf in
        let data = attr_data c lexbuf in
        `Attribute (a, data)
      in
      ignore_spaces c lexbuf;
      let others, closed = attributes c lexbuf in
      attribute :: others, closed
    }

and attr_name c = parse
  | identchar+ { `AttrName (lexeme c) }

  | '$' {
      X.parse_dollar_attrname c (loc c) lexbuf
    }

  | _ | eof { err EAttributeNameExpected (loc c)}

and attr_data c = parse
  | space* '=' space* ('$' | "$str:") {
      ignore(buff_contents c) ;
      X.parse_dollar_attrvalue c (loc c) lexbuf
    }
  | space* '=' space* '"' {
      ignore(buff_contents c) ;
      `AttrVal(dq_string c lexbuf)
    }
  | space* '=' space* '\'' {
      ignore(buff_contents c) ;
      `AttrVal(q_string c lexbuf)
    }
  | _ | eof { err EAttributeValueExpected (loc c)}

and dq_string c = parse
  | '"' { buff_contents c }
  | '\\' [ '"' '\\' ] {
      istore_char c 1;
      dq_string c lexbuf
    }
  | eof { err EUnterminatedString (Loc.of_lexbuf lexbuf) }
  | _ {
      istore_char c 0;
      dq_string c lexbuf
    }

and q_string c = parse
  | '\'' { buff_contents c }
  | '\\' [ '\'' '\\' ] {
      istore_char c 1;
      q_string c lexbuf
    }
  | eof  { err EUnterminatedString (Loc.of_lexbuf lexbuf) }
  | _ {
      istore_char c 0;
      q_string c lexbuf
    }


(******************************************************************************)
{
let lexing_store s buff max =
        let rec self n s =
                if n >= max then n
                else
                        match Stream.peek s with
                        | Some x ->
                                        Stream.junk s;
                                        buff.[n] <- x;
                                        succ n
                        | _ -> n
                in
                self 0 s

let from_context c  =
        let next _ =
                let tok = with_curr_loc token c in
                let loc = Loc.of_lexbuf c.lexbuf in
                Some ((tok, loc))
                in Stream.from next

let from_lexbuf lb e =
        let c = { (default_context lb) with
        loc        = Loc.of_lexbuf lb;
                entity = e }
        in from_context c

let setup_loc lb loc =
        let start_pos = Loc.start_pos loc in
        lb.lex_abs_pos <- start_pos.pos_cnum;
        lb.lex_curr_p  <- start_pos

let from_string loc entity str =
        let lb = Lexing.from_string str in
        setup_loc lb loc;
        from_lexbuf lb entity

let from_stream loc entity strm =
        let lb = Lexing.from_function (lexing_store strm) in
        setup_loc lb loc;
        from_lexbuf lb entity

end

}
