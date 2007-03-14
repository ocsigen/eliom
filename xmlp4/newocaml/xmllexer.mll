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
 * - Gabriel Kerneis : merging with xml-light => Xml_lexer.mll
 *)


(* $Id: Lexer.mll,v 1.6 2007/02/07 10:09:21 ertai Exp $ *)

(* The lexer definition *)


{

(** A lexical analyzer. *)


open Camlp4
open BasicTypes

  module Loc = Camlp4.PreCast.Loc
  
  open Lexing

  (* Error report *)
  module Error = struct

    type t =
	| EUnterminatedComment
	| EUnterminatedString
  	| EIdentExpected
  	| ECloseExpected
  	| ENodeExpected
  	| EAttributeNameExpected
  	| EAttributeValueExpected
  	| EUnterminatedEntity
  	| ECamlIdentExpected

    exception E of t

    open Format

    let print ppf =
      function
  	  | EUnterminatedComment ->
    	  fprintf ppf "Lexing error : UnterminatedComment"
  	  | EUnterminatedString  ->
    	  fprintf ppf "Lexing error : UnterminatedString"
    	| EIdentExpected ->
    	  fprintf ppf "Lexing error : IdentExpected"
    	| ECloseExpected ->
    	  fprintf ppf "Lexing error : CloseExpected"
    	| ENodeExpected ->
    	  fprintf ppf "Lexing error : NodeExpected"
    	| EAttributeNameExpected ->
    	  fprintf ppf "Lexing error : AttributeNameExpected"
    	| EAttributeValueExpected ->
    	  fprintf ppf "Lexing error : AttributeValueExpected"
    	| EUnterminatedEntity ->
    	  fprintf ppf "Lexing error : UnterminatedEntity"
    	| ECamlIdentExpected ->
    	  fprintf ppf "Lexing error : CamlIdentExpected"

    let to_string x =
      let b = Buffer.create 50 in
      let () = bprintf b "%a" print x in Buffer.contents b
  end;;

  let module M = ErrorHandler.Register(Error) in ()

  open Error

  (* To store some context information:
  *   loc       : position of the beginning of a string, quotation and comment
  *)

  type context =
  { loc        : Loc.t    ;
    lexbuf     : lexbuf   ;
    buffer     : Buffer.t ;
  }

  let default_context lb =
  { loc        = Loc.ghost ;
    lexbuf     = lb        ;
    buffer     = Buffer.create 256 ;
  }

  (* To buffer string literals *)

  let store c = Buffer.add_string c.buffer (Lexing.lexeme c.lexbuf)
  let istore_char c i = Buffer.add_char c.buffer (Lexing.lexeme_char c.lexbuf i)
  let buff_contents c =
    let contents = Buffer.contents c.buffer in
    Buffer.reset c.buffer; contents

  let loc c = Loc.merge c.loc (Loc.of_lexbuf c.lexbuf)
  let set_start_p c = c.lexbuf.lex_start_p <- Loc.start_pos c.loc
  let move_start_p shift c =
    let p = c.lexbuf.lex_start_p in
    c.lexbuf.lex_start_p <- { (p) with pos_cnum = p.pos_cnum + shift }

  let with_curr_loc f c = f { (c) with loc = Loc.of_lexbuf c.lexbuf } c.lexbuf
  let parse_nested f c =
    with_curr_loc f c;
    set_start_p c;
    buff_contents c
  let shift n c = { (c) with loc = Loc.move `both n c.loc }
  let store_parse f c = store c ; f c c.lexbuf
  let parse f c = f c c.lexbuf
  
  let lexeme c = Lexing.lexeme c.lexbuf
  let store_string c s = Buffer.add_string c.buffer s
  let cut_last s = String.sub s 0 (String.length s - 1)
  let extract_caml s prefix = 
    let i = String.index s '$'  in
    let j = String.index_from s (i+1) '$' in 
    let k = (if prefix then  String.index_from s (i+1) ':' else i) 
    in if k<j then String.sub s (k+1) (j-k-1) else failwith "Error : extract_caml"
  
  (* Deal with entities *)
  let idents = Hashtbl.create 0

  let _ = begin
  	Hashtbl.add idents "gt;" ">";
  	Hashtbl.add idents "lt;" "<";
  	Hashtbl.add idents "amp;" "&";
  	Hashtbl.add idents "apos;" "'";
  	Hashtbl.add idents "quot;" "\"";
  end

  (* Update the current location with file name and line number. *)

  let update_loc c file line absolute chars =
    let lexbuf = c.lexbuf in
    let pos = lexbuf.lex_curr_p in
    let new_file = match file with
                  | None -> pos.pos_fname
                  | Some s -> s
    in
    lexbuf.lex_curr_p <- { pos with
      pos_fname = new_file;
      pos_lnum = if absolute then line else pos.pos_lnum + line;
      pos_bol = pos.pos_cnum - chars;
    }

  let err error loc =
    raise(Loc.Exc_located(loc, Error.E error))

  let warn error loc =
    Format.eprintf "Warning: %a: %a@." Loc.print loc Error.print error
  }

(******************************************************************************)

  let newline = ['\n']
  let break = ['\r']
  let space = [' ' '\t']
  let identchar =  ['A'-'Z' 'a'-'z' '_' '0'-'9' ':' '-']
  let entitychar = ['A'-'Z' 'a'-'z']
  let pcchar = [^ '\r' '\n' '<' '>' '&' '$']
  let camlidentchar =  [^ '$']
      (* FIXME: C'est "un peu" violent - à raffiner *)

  rule token c = parse
    | newline                    { update_loc c None 1 false 0; token c lexbuf }
    | (space|break) +                                          {token c lexbuf }
    (* FIXME: on considère les commentaires comme des noeuds ; c'est mal, mais
     * c'est la seule solution si on ne veut pas les ignorer. Autre solution :
             * les ignorer et rajouter un commentaire de remplissage au niveau
             * du parser si on se retrouve avec une liste de noeuds vide (donc
             * si le seul noeud à analyser était un commentaire *)
	  | "<!--"                                
                { Comment(comment c lexbuf)}
	  | "<?"                                  { header c lexbuf;  token c lexbuf }
	  | '<' space* '/' space* 
	    {
			let tag = ident_name c lexbuf in
			ignore_spaces c lexbuf;
			close_tag c lexbuf;
			Endtag tag                                                               }
	| '<' space*
		{
			let tag = ident_name c lexbuf in
			ignore_spaces c lexbuf;
			let attribs, closed = attributes c lexbuf in
			Tag(tag, attribs, closed)                                                }
	| "&#"
		{
			ignore(buff_contents c);
			store c ;
			PCData (pcdata c lexbuf)
		}
	| '&'
		{
			ignore (buff_contents c);
			store_string c (entity c lexbuf);
			PCData (pcdata c lexbuf)
		}
	| "$$"       
	  {
	    ignore (buff_contents c); 
	    store_string c "$" ; 
	    PCData (pcdata c lexbuf) 
	  }
	| space* pcchar+
		{
			ignore (buff_contents c);
			store c ;
			PCData (pcdata c lexbuf)
		}
	| eof { Eof }
  |"$str:" camlidentchar+ '$' 
    {CamlString (extract_caml (lexeme c) true)}
  |"$list:" camlidentchar+ '$' 
    {CamlList (extract_caml (lexeme c) true)}
  |'$' camlidentchar+ '$' 
    {CamlExpr (extract_caml (lexeme c) false)}
  | _
		{ err ENodeExpected (Loc.of_lexbuf lexbuf) }

and ignore_spaces c = parse
	| newline
		{
			update_loc c None 1 false 0;
			ignore_spaces c lexbuf
		}
	| (space | break) +
		{ ignore_spaces c lexbuf }
	| ""
		{ () }

and comment c = parse
(* FIXME: selon la norme XML, le symbole "--" est INTERDIT dans un commentaire.
 * Néanmoins, on choisit de gérer les commentaires imbriqués, pour faciliter
 * l'écriture des fichiers de configuration. C'est sous-optimal en matière de
 * gestion mémoire, on fait des recopies de chaines inutiles *)
        | newline
		{
			update_loc c None 1 false 0;
			comment c lexbuf
		}
        | "<!--" {let buff = buff_contents c in 
                 let in_comment = comment c lexbuf in
                 store_string c buff ; 
                 store_string c "<!--" ;
                 store_string c in_comment ;
                 store_string c "-->" ;
                 comment c lexbuf}
        | "-->"                                	{ buff_contents c }
	| eof
		{ raise (E EUnterminatedComment) }
        | _     { store c ; comment c lexbuf }
        
and header c = parse
(* Le header est ignoré *)
    | newline
		{
			update_loc c None 1 false 0;
			header c lexbuf
		}
	| "?>"
		{ () }
	| eof
		{ err ECloseExpected (loc c) }
	| _
		{ header c lexbuf }		

and pcdata c = parse
	| pcchar+
		{
			store c ;
			pcdata c lexbuf
		}
	| "&#"
		{
			store c ;
			pcdata c lexbuf;
		}
	| '&'
		{
			store_string c (entity c lexbuf);
			pcdata c lexbuf
		}
	| "$$"                                { store_string c "$" ; pcdata c lexbuf }
	| ""
		{ buff_contents c }

and entity c = parse
	| entitychar+ ';'
		{
			let ident = lexeme c in
			try
				Hashtbl.find idents (String.lowercase ident)
			with
				Not_found -> "&" ^ ident
		}
	| _ | eof
		{ raise (E EUnterminatedEntity) }

and ident_name c = parse
	| identchar+
		{ lexeme c }
	| _ | eof
		{ err EIdentExpected (loc c)}

and close_tag c = parse
	| '>'
		{ () }
	| _ | eof
		{ err ECloseExpected (loc c)}

and attributes c = parse
	| '>'
		{ [], false }
	| "/>"
		{ [], true }
	| "" (* do not read a char ! *)
		{
			let (attribute:attribute) = 
			  (match attribute c lexbuf with
			  | `Attr s as a  -> 
			    let (data:valeur) = attribute_data c lexbuf in
			    (`Attribute (a, data))
			  |`CamlAttr s as a ->
			    let (data:valeur) = attribute_data c lexbuf in
			    (`Attribute (a, data))			  
			  | (`CamlList s) as a -> a
			  ) in
			ignore_spaces c lexbuf;
			let others, closed = attributes c lexbuf in
			attribute :: others, closed
		}

and attribute c = parse
	| identchar+
		{`Attr (lexeme c) }
	| "$list:" camlidentchar+ '$'
	  {`CamlList (extract_caml(lexeme c) true)}	
	| '$' camlidentchar+ '$'                                 
	  {`CamlAttr (extract_caml(lexeme c) false)}
	| _ | eof
		{ err EAttributeNameExpected (loc c)}

and attribute_data c = parse
	| space* '=' space* "$str:" camlidentchar+ '$'
	  {`CamlVal (extract_caml(lexeme c) true)}	
	| space* '=' space* '"'
		{
			ignore(buff_contents c) ;
			`Val(dq_string c lexbuf)
		}
	| space* '=' space* '\''
		{
			ignore(buff_contents c) ;
			`Val(q_string c lexbuf)
		}
	| _ | eof
		{ err EAttributeValueExpected (loc c)}

and dq_string c = parse
	| '"'
		{ buff_contents c }
	| '\\' [ '"' '\\' ]
		{
			istore_char c 1;
			dq_string c lexbuf
		}
	| eof
		{ raise (E EUnterminatedString) }
	| _
		{ 
			istore_char c 0;
			dq_string c lexbuf
		}

and q_string c = parse
	| '\''
		{ buff_contents c }
	| '\\' [ '\'' '\\' ]
		{
			istore_char c 1;
			q_string c lexbuf
		}
	| eof
		{ raise (E EUnterminatedString) }
	| _
		{ 
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

  let from_context c =
    let next _ =
      let tok = with_curr_loc token c in
      let loc = Loc.of_lexbuf c.lexbuf in
      Some ((tok, loc))
    in Stream.from next

  let from_lexbuf lb =
    let c = { (default_context lb) with
              loc        = Loc.of_lexbuf lb; }
    in from_context c

  let setup_loc lb loc =
    let start_pos = Loc.start_pos loc in
    lb.lex_abs_pos <- start_pos.pos_cnum;
    lb.lex_curr_p  <- start_pos

  let from_string loc str =
    let lb = Lexing.from_string str in
    setup_loc lb loc;
    from_lexbuf lb

  let from_stream loc strm =
    let lb = Lexing.from_function (lexing_store strm) in
    setup_loc lb loc;
    from_lexbuf lb
    
  let mk () loc strm =
    from_stream loc strm

}
