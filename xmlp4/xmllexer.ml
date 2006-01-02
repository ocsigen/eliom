(* camlp4r *)

(* xmllexer.ml provide a generic XML lexer for Camlp4. *)
(* Manuel.Maarek@macs.hw.ac.uk (Heriot-Watt University) *)
(* Manuel.Maarek@spi.lip6.fr (LIP6) *)
(* Modif by VB for xmlp4 *)


(* derived from the default lexer of Camlp4: plexer.ml *)
(***********************************************************************)
(*                                                                     *)
(*                             Camlp4                                  *)
(*                                                                     *)
(*        Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt     *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)
(* based on: plexer.ml,v 1.10 2002/07/19 *)

open Stdpp;
open Token;


(* The string buffering machinery *)

value buff = ref (String.create 80);
value store len x =
  do {
    if len >= String.length buff.val then
      buff.val := buff.val ^ String.create (String.length buff.val)
    else ();
    buff.val.[len] := x;
    succ len
  }
;
value mstore len s =
  add_rec len 0 where rec add_rec len i =
    if i == String.length s then len else add_rec (store len s.[i]) (succ i)
;
value get_buff len = String.sub buff.val 0 len;


value string_of_space_char = fun
  [ ' ' -> " "
  | '\n' -> "\n"
  | '\t' -> "\t"
  | _ -> "" ];

(* The lexer *)

value rec ident len =
  parser
  [ [: `('A'..'Z' | 'a'..'z' | '\192'..'\214' | '\216'..'\246' |
         '\248'..'\255' | '0'..'9' | '_' | ''' as
         c)
        ;
       s :] ->
      ident (store len c) s
  | [: :] -> len ]
and ident2 len =
  parser
  [ [: `('!' | '?' | '~' | '=' | '@' | '^' | '&' | '+' | '-' | '*' | '/' |
         '%' | '.' | ':' | '<' | '>' | '|' | '$' as
         c)
        ;
       s :] ->
      ident2 (store len c) s
  | [: :] -> len ]
and ident3 len =
  parser
  [ [: `('0'..'9' | 'A'..'Z' | 'a'..'z' | '\192'..'\214' | '\216'..'\246' |
         '\248'..'\255' | '_' | '!' | '%' | '&' | '*' | '+' | '-' | '.' |
         '/' | ':' | '<' | '=' | '>' | '?' | '@' | '^' | '|' | '~' | ''' |
         '$' as
         c)
        ;
       s :] ->
      ident3 (store len c) s
  | [: :] -> len ]
;

value error_on_unknown_keywords = ref False;
value err loc msg = raise_with_loc loc (Token.Error msg);

value ((begin_attr, end_attr), waiting_for_attr) =
  let b = ref False in
  (((fun () -> b.val := True),
   (fun () -> b.val := False)),
  (fun () -> b.val));


value ((push_tag,pop_tag),(size_tag_stack,string_of_tag_stack)) =
  let stack = Stack.create () in
  ((
   (fun t -> Stack.push t stack)
     ,
   (fun () -> try Stack.pop stack with [Stack.Empty -> "" ])
  ),
   ((fun () -> Stack.length stack)
      ,
    (fun () ->
      let s = ref "" in
      do {
       Stack.iter
         (fun tag -> s.val := (if (s.val == "") then " " else s.val) ^ tag)
         stack;
         s.val}
     )
   ));

value next_token_fun find_kwd fname lnum bolpos =
  let make_pos p =
    do{
     {Lexing.pos_fname = fname.val; Lexing.pos_lnum = lnum.val;
      Lexing.pos_bol = bolpos.val; Lexing.pos_cnum = p}} in
  let mkloc (bp, ep) = (make_pos bp, make_pos ep) in
  let keyword_or_error (bp,ep) s =
    let loc = mkloc (bp, ep) in
      try (("", find_kwd s), loc) with
      [ Not_found ->
        if error_on_unknown_keywords.val then err loc ("illegal token: " ^ s)
        else (("", s), loc) ]
  in
  let rec next_token =
    ( fun s ->
      if waiting_for_attr () then
	next_token_attr s
      else
	next_token_normal s )
  and next_token_attr =
    parser bp
    [ [: `(' ' | '\t'); s :] -> next_token s
      | [: `('\013' | '\010'); s :] ep -> 
        do {bolpos.val := ep; incr lnum; next_token s}
      |	[: `('a'..'z' | 'A'..'Z' | '_' | ':' as c); s
	 :] -> name_attribut bp (store 0 c) s
      |	[: `'=' :] ep -> (("","="),mkloc (bp,ep))
      |	[: `'"'; s :] ep ->
	  (("VALUE", get_buff (value_attribut_in_double_quote bp 0 s)),mkloc (bp,ep))
      |	[: `'''; s :] ep ->
	  (("VALUE", get_buff (value_attribut_in_quote bp 0 s)),mkloc (bp,ep))
      |	[: `'$'; s :] -> (* for caml expressions *) camlexprattr bp 0 s
      |	[: `'/'; `'>' :] ep ->
	  let name = pop_tag () in
	  do { 
	  end_attr ();
	  (("GAT",name),mkloc (bp,ep)) }
      |	[: `'>'; s :] -> do { end_attr (); next_token s}
    ]
  and next_token_normal =
    parser bp
    [ [: `(' ' | '\t' | '\026' | '\012' as c); s :] ep ->
      let c_string = string_of_space_char c in
      (("WHITESPACE", 
	get_buff (whitespaces bp (mstore 0 c_string) s)),mkloc (bp,ep))
    | [: `('\013' | '\010' as c); s :] ep ->
      let c_string = string_of_space_char c in
      do {bolpos.val := ep; incr lnum;
      (("WHITESPACE", 
	get_buff (whitespaces bp (mstore 0 c_string) s)),mkloc (bp,ep))}
    | [: `('A'..'Z' | '\192'..'\214' | '\216'..'\222' | 'a'..'z' |
      '\223'..'\246' | '\248'..'\255' | '_' | '1'..'9' | '0' | '\'' | '"' |
      (* '$' | *) 
      '!' | '=' | '@' | '^' | '&' | '+' | '-' | '*' | '/' | '%' | '~' |
      '?' | ':' | '|' | '[' | ']' | '{' | '}' | '.' | ';' | ',' | '\\' | '(' |
      ')' | '#' | '>' | '`'
	as c); s :] ep -> 
	  (("DATA", get_buff (data bp (store 0 c) s)),mkloc (bp,ep))
    | [: `'$'; s :] -> (* for caml expr *) camlexpr bp s
    | [: `'<'; s :] ->
	match s with parser
      [	[: `'?'; `'x'; `'m'; `'l';
	 len = question_mark_tag bp 0
	   :] ep -> (("XMLDECL", get_buff len), mkloc (bp, ep))
      |	[: `'!'; s :] ->
	  match s with parser
	    [ [: `'-'; `'-'; ct :] ep -> 
		(("COMMENT", get_buff (comment_tag 0 bp 0 ct)),
		 mkloc (bp,ep))
	    | [: len = exclamation_mark_tag bp 0
		 :] ep -> (("DECL", get_buff len),mkloc (bp,ep)) ]
      |	[: `'/'; `('a'..'z' | 'A'..'Z' | '_' | ':' as c); s
	   :] -> end_tag bp (store 0 c) s
      |	[: `('a'..'z' | 'A'..'Z' | '_' (* | ':'*) as c); s
	   :] -> start_tag bp (store 0 c) s ] 
    | [: `c :] ep -> keyword_or_error (bp,ep) (String.make 1 c)
    | [: _ = Stream.empty :] ep ->
	let size = size_tag_stack () in
	if (size == 0)
	then
	  do {lnum.val := 1;
	    (("EOI", ""), mkloc (bp, succ bp))}
	else
	  err (mkloc (bp, ep)) ( (string_of_int size) ^ " tag" ^ (if size == 1 then "" else "s") ^ " not terminated (" ^ (string_of_tag_stack ()) ^ ").")
    ]

(* XML *)
  and question_mark_tag bp len =
    parser
    [ [: `'?'; `'>' :] -> len
    | [: `c; s :] -> question_mark_tag bp (store len c) s
    | [: :] ep -> err (mkloc (bp, ep)) "XMLDecl tag (<? ?>) not terminated" ]
  and comment_tag prof bp len =
    parser
    [ [: `'-'; s :] -> dash_in_comment_tag prof bp len s
      |	[: `'<'; s :] -> comment_tag2 prof bp len s
      |	[: `c; s :] -> comment_tag prof bp (store len c) s
      | [: :] ep -> err (mkloc (bp, ep)) "comment (<!-- -->) not terminated" ]
  and comment_tag2 prof bp len =
    parser
    [ [: `'!'; s :] -> comment_tag3 prof bp len s
      |	[: s :] -> comment_tag prof bp (store len '<') s ]
  and comment_tag3 prof bp len =
    parser
    [ [: `'-'; s :] -> comment_tag4 prof bp len s
      |	[: s :] -> comment_tag prof bp (mstore len "<!") s ]
  and comment_tag4 prof bp len =
    parser
    [ [: `'-'; s :] -> comment_tag (prof+1) bp (mstore len "<!--") s
      |	[: s :] -> comment_tag prof bp (mstore len "<!-") s ]
  and dash_in_comment_tag prof bp len =
    parser
    [ [: `'-'; s :] -> dash_in_comment_tag2 prof bp len s
    | [: a = comment_tag prof bp (store len '-') :] -> a ]
  and dash_in_comment_tag2 prof bp len =
    parser
    [ [: `'>'; s :] -> if prof = 0 
                    then len 
                    else comment_tag (prof-1) bp (mstore len "-->") s
    | [: a = comment_tag prof bp (mstore len "--") :] -> a ]
  and exclamation_mark_tag bp len =
    parser bp_sb
    [ [: `'>' :] -> len
    | [: `('[' as c); s :] ->
	sq_bracket_in_exclamation_mark_tag bp_sb (store len c) s
    | [: `c; s :] -> exclamation_mark_tag bp (store len c) s
    | [: :] ep -> err (mkloc (bp, ep)) "Decl tag (<! >) not terminated" ]
  and sq_bracket_in_exclamation_mark_tag bp len =
    parser
      [ [: `(']' as c ); s :] ->
	  exclamation_mark_tag bp (store len c) s
      | [: `c; s :] -> sq_bracket_in_exclamation_mark_tag bp (store len c) s
      | [: :] ep -> err (mkloc (bp, ep))
	    "Decl tag with open square bracket (<! ... [) not terminated" ]
  and end_tag bp len =
    parser
    [ [: `'>' :] ep ->
      let name = get_buff len in
      let last_open = pop_tag () in
      if last_open = name then
	(("GAT",name), mkloc (bp,ep))
      else
       	err (mkloc (bp, ep)) ("bad end tag: </"
		      ^ name
		      ^ ">"
		      ^ (
			if last_open = "" then
			  ""
			else
			  " (</" ^last_open ^ "> expected)"
		       ))
      | [: `('a'..'z' | 'A'..'Z' | '1'..'9' | '0' |
	'.' | '-' | '_' | ':'  as c); s :] -> end_tag bp (store len c) s
      |	[: :] ep -> err (mkloc (bp, ep)) "end tag (</ >) not terminated" ]
  and start_tag bp len =
    parser
    [ [: `'>' :] ep ->
      let name = get_buff len in
      do { push_tag name;
	   (("TAG", name), mkloc (bp,ep)) }
      |	[: `('\013' | ' ' | '\t') :] ep ->
	  let name = get_buff len in
          do { push_tag name;
	       begin_attr ();
	       (("TAG", name), mkloc (bp,ep)) }
      |	[: `('\010') :] ep ->
	  let name = get_buff len in
          do { bolpos.val := ep; incr lnum;
               push_tag name;
	       begin_attr ();
	       (("TAG", name), mkloc (bp,ep)) }
      | [: `( 'a'..'z' | 'A'..'Z' | '1'..'9' | '0' |
	'.' | '-' | '_' | ':' as c); s :] ->
	    start_tag bp (store len c) s
      |	[: :] ep ->
	  let name = get_buff len in
          do { push_tag name;
	       begin_attr ();
	       (("TAG", name), mkloc (bp,ep)) } ]
 (*     |	[: :] ep -> err (mkloc (bp, ep)) "start tag (< > or < />) not terminated" ] *)
  and name_attribut bp len =
    parser
    [ [: `('\013' | ' ' | '\t') :] ep ->
      (("ATTR",get_buff len), mkloc (bp,ep))
      |	[: `('\010') :] ep ->
      do { bolpos.val := ep; incr lnum ; (("ATTR",get_buff len), mkloc (bp,ep))}
      |	[: `( 'a'..'z' | 'A'..'Z' | '1'..'9' | '0' |
    '.' | '-' | '_' | ':'
      as c); s :] -> name_attribut bp (store len c) s
      |	[: s :] ep -> (("ATTR",get_buff len), mkloc (bp,ep))
      |	[: :] ep -> err (mkloc (bp, ep)) "start tag (< > or < />) not terminated" ]
  and value_attribut_in_double_quote bp len =
    parser
    [ [: `'"' :] -> len
      |	[: `'\\'; `'"'; s :] ->
	  value_attribut_in_double_quote bp (store (store len '\\') '"') s
      |	[: `c; s :] ->
	  value_attribut_in_double_quote bp (store len c) s
      |	[: :] ep -> err (mkloc (bp, ep)) "attribut value not terminated" ]
  and camlexpr bp =
    parser
    [ [: `('a'..'z' | '\223'..'\246' | '\248'..'\255' | '_' as c); s :] ->
      antiquotname bp ((ident (store 0 c)) s) s
    | [: `'$'; s :] ep -> (("DATA", "$"), mkloc (bp,ep))
    | [: s :] ep -> (("CAMLEXPRXML", get_buff (camlexpr2 bp 0 s)), mkloc (bp,ep)) ]
  and antiquotname bp len =
    parser bp2 
    [ [: `':'; s :] ep -> let buff = get_buff len in 
    let bp2 = bp2+2 in
    if buff = "list" 
    then (("CAMLEXPRXMLL", get_buff (camlexpr2 bp2 0 s)), mkloc (bp2,ep))
    else if buff = "str" 
    then (("CAMLEXPRXMLS", get_buff (camlexpr2 bp2 0 s)), mkloc (bp2,ep))
    else err (mkloc (bp, ep)) "unknown antiquotation"
    | [: `'$'; s :] ep -> (("CAMLEXPRXML", (get_buff len)), mkloc (bp,ep))
    | [: s :] ep -> (("CAMLEXPRXML", get_buff (camlexpr2 bp len s)), mkloc (bp,ep)) ]
  and camlexpr2 bp len =
    parser
    [ [: `'$'; s :] -> len
    | [: `('\010' as c) ; s :] ep -> 
     do { bolpos.val := ep; incr lnum; 
     camlexpr2 bp (store len c) s}
    | [: `(_ as c) ; s :] -> camlexpr2 bp (store len c) s ]



  and camlexprattr bp len =
    parser
    [ [: `('a'..'z' | '\223'..'\246' | '\248'..'\255' | '_' as c); s :] ->
      antiquotname_attr bp ((ident (store 0 c)) s) s
    | [: `'$'; s :] ep -> (("DATA", "$"), mkloc (bp,ep))
    | [: s :] ep -> (("CAMLEXPR", get_buff (camlexpr2 bp 0 s)), mkloc (bp,ep)) ]
  and antiquotname_attr bp len =
    parser
    [ [: `':'; s :] ep -> let buff = get_buff len in 
	if buff = "list" 
	then (("CAMLEXPRL", get_buff (camlexpr2 bp 0 s)), mkloc (bp,ep))
	else err (mkloc (bp, ep)) "unknown antiquotation"
    | [: `'$'; s :] ep -> (("CAMLEXPR", get_buff len), mkloc (bp,ep))
    | [: s :] ep -> (("CAMLEXPR", get_buff (camlexpr2 bp len s)), mkloc (bp,ep)) ]

  and value_attribut_in_quote bp len =
    parser
    [ [: `''' :] -> len
      |	[: `'\\'; `'''; s :] ->
	  value_attribut_in_quote bp (store (store len '\\') ''') s
      |	[: `c; s :] ->
	  value_attribut_in_quote bp (store len c) s
      |	[: :] ep -> err (mkloc (bp, ep)) "attribut value not terminated" ]
  and data bp len =
    parser
    [ (* [: `(' ' | '\n' | '\t' as c); s :] ->
      let c_string = string_of_space_char c in
      spaces_in_data bp len c_string s
    | *) [: `('A'..'Z' | '\192'..'\214' | '\216'..'\222' | 'a'..'z' |
      '\223'..'\246' | '\248'..'\255' | '_' | '1'..'9' | '0' | '\'' | '"' |
      (* '$' | *)
      '!' | '=' | '@' | '^' | '&' | '+' | '-' | '*' | '/' | '%' | '~' |
      '?' | ':' | '|' | '[' | ']' | '{' | '}' | '.' | ';' | ',' | '\\' | '(' |
      ')' | '#' | '>' | '`'
	as c) ; s :] -> data bp (store len c) s
    | [: s :] -> len ]
(*  and spaces_in_data bp len spaces =
    parser
    [ [: `('\n' as c); s :] ep ->
      let c_string = string_of_space_char c in
      do { bolpos.val := ep; incr lnum ; spaces_in_data bp len (spaces ^ c_string) s}
    | [: `(' ' | '\t' as c); s :] ->
      let c_string = string_of_space_char c in
      spaces_in_data bp len (spaces ^ c_string) s
    | [: `('A'..'Z' | '\192'..'\214' | '\216'..'\222' | 'a'..'z' |
      '\223'..'\246' | '\248'..'\255' | '_' | '1'..'9' | '0' | '\'' | '"' |
      (* '$' | *) 
      '!' | '=' | '@' | '^' | '&' | '+' | '-' | '*' | '/' | '%' | '~' |
      '?' | ':' | '|' | '[' | ']' | '{' | '}' | '.' | ';' | ',' | '\\' | '(' |
      ')' | '#' | '`'
	as c) ; s :] ->
       	  data bp (store (mstore len spaces) c) s
    | [: s :] -> len ] *)
  and whitespaces bp len =
    parser
    [ [: `('\013' | ' ' | '\t' | '\026' | '\012' as c); s :] ->
      let c_string = string_of_space_char c in
      whitespaces bp (mstore len c_string) s
    | [: `('\010' as c); s :] ep ->
      let c_string = string_of_space_char c in
      do { bolpos.val := ep; incr lnum;
           whitespaces bp (mstore len c_string) s}
    | [: s :] -> len ]
(* /XML *)

  in
  fun cstrm ->
    try
      next_token cstrm
    with
    [ Stream.Error str ->
        err (mkloc (Stream.count cstrm, Stream.count cstrm + 1)) str ]
;

value func kwd_table =
  let bolpos = ref 0 in
  let lnum = ref 1 in
  let fname = ref "" in
  let find = Hashtbl.find kwd_table in
  Token.lexer_func_of_parser (next_token_fun find fname lnum bolpos)
;

value rec check_keyword_stream =
  parser [: _ = check; _ = Stream.empty :] -> True
and check =
  parser
  [ [: `'A'..'Z' | 'a'..'z' | '\192'..'\214' | '\216'..'\246' | '\248'..'\255'
        ;
       s :] ->
      check_ident s
  | [: `'!' | '?' | '~' | '=' | '@' | '^' | '&' | '+' | '-' | '*' | '/' |
        '%' | '.'
        ;
       s :] ->
      check_ident2 s
  | [: `'<'; s :] ->
      match Stream.npeek 1 s with
      [ [':' | '<'] -> ()
      | _ -> check_ident2 s ]
  | [: `':';
       _ =
         parser
         [ [: `']' | ':' | '=' | '>' :] -> ()
         | [: :] -> () ] :] ->
      ()
  | [: `'>' | '|';
       _ =
         parser
         [ [: `']' | '}' :] -> ()
         | [: a = check_ident2 :] -> a ] :] ->
      ()
  | [: `'[' | '{'; s :] ->
      match Stream.npeek 2 s with
      [ ['<'; '<' | ':'] -> ()
      | _ ->
          match s with parser
          [ [: `'|' | '<' | ':' :] -> ()
          | [: :] -> () ] ]
  | [: `';';
       _ =
         parser
         [ [: `';' :] -> ()
         | [: :] -> () ] :] ->
      ()
  | [: `_ :] -> () ]
and check_ident =
  parser
  [ [: `'A'..'Z' | 'a'..'z' | '\192'..'\214' | '\216'..'\246' |
        '\248'..'\255' | '0'..'9' | '_' | '''
        ;
       s :] ->
      check_ident s
  | [: :] -> () ]
and check_ident2 =
  parser
  [ [: `'!' | '?' | '~' | '=' | '@' | '^' | '&' | '+' | '-' | '*' | '/' |
        '%' | '.' | ':' | '<' | '>' | '|'
        ;
       s :] ->
      check_ident2 s
  | [: :] -> () ]
;

value check_keyword s =
  try check_keyword_stream (Stream.of_string s) with _ -> False
;

value error_no_respect_rules p_con p_prm =
  raise
     (Token.Error
         ("the token " ^
          (if p_con = "" then "\"" ^ p_prm ^ "\""
           else if p_prm = "" then p_con
           else p_con ^ " \"" ^ p_prm ^ "\"") ^
          " does not respect Plexer rules"))
;

value error_ident_and_keyword p_con p_prm =
  raise
    (Token.Error
        ("the token \"" ^ p_prm ^ "\" is used as " ^ p_con ^
         " and as keyword"))
;

value using_token kwd_table ident_table (p_con, p_prm) =
  match p_con with
  [ "" ->
      if not (Hashtbl.mem kwd_table p_prm) then
        if check_keyword p_prm then
          if Hashtbl.mem ident_table p_prm then
            error_ident_and_keyword (Hashtbl.find ident_table p_prm) p_prm
          else Hashtbl.add kwd_table p_prm p_prm
        else error_no_respect_rules p_con p_prm
      else ()
  | "QUOTATION" |
    "CAMLEXPR" | "CAMLEXPRXML" | 
    "CAMLEXPRL" | "CAMLEXPRXMLL" |  
    "CAMLEXPRXMLS" | "COMMENT" |
    "TAG" | "GAT" | "ATTR" | "VALUE" | "XMLDECL" |
    "DECL" | "DATA" | "WHITESPACE" | "EOI" ->
      ()
  | _ ->
      raise
        (Token.Error
           ("the constructor \"" ^ p_con ^
              "\" is not recognized by Plexer")) ]
;

value removing_token kwd_table ident_table (p_con, p_prm) =
  match p_con with
  [ "" -> Hashtbl.remove kwd_table p_prm
  | "LIDENT" | "UIDENT" ->
      if p_prm <> "" then Hashtbl.remove ident_table p_prm
      else ()
  | _ -> () ]
;

value text =
  fun
  [ ("", t) -> "'" ^ t ^ "'"
  | ("CAMLEXPR", k) -> "camlexpr \"" ^ k ^ "\""
  | ("CAMLEXPRXML", k) -> "camlvarxml \"" ^ k ^ "\""
  | ("CAMLEXPRL", k) -> "camlexprl \"" ^ k ^ "\""
  | ("CAMLEXPRXMLL", k) -> "camlvarxmll \"" ^ k ^ "\""
  | ("COMMENT", k) -> "comment \"" ^ k ^ "\""

  | ("TAG","") -> "tag"
  | ("TAG",t) -> "tag \"" ^ t ^ "\""
  | ("GAT","") -> "end tag"
  | ("ATTR","") -> "attribut"
  | ("ATTR",a) -> "attribut \"" ^ a ^ "\""
  | ("VALUE","") -> "value"
  | ("VALUE",v) -> "value \"" ^ v ^ "\""
  | ("XMLDECL","") -> "XML declaration"
  | ("DECL","") -> "declaration"
  | ("DATA","") -> "data"
  | ("WHITESPACE","") -> "whitespace"

  | ("EOI", "") -> "end of input"
  | (con, "") -> con
  | (con, prm) -> con ^ " \"" ^ prm ^ "\"" ]
;

value tok_match =
  fun tok -> Token.default_match tok
;

value gmake () =
  let kwd_table = Hashtbl.create 301 in
  let id_table = Hashtbl.create 301 in
  {tok_func = func kwd_table; tok_using = using_token kwd_table id_table;
   tok_removing = removing_token kwd_table id_table;
   tok_match = tok_match; tok_text = text;
   tok_comm = None}
;

value tparse =
  fun _ -> None
;

value make () =
  let kwd_table = Hashtbl.create 301 in
  let id_table = Hashtbl.create 301 in
  {func = func kwd_table; using = using_token kwd_table id_table;
   removing = removing_token kwd_table id_table; tparse = tparse;
   text = text}
;

(* $Id: xmllexer.ml,v 1.4 2005/06/04 21:02:26 balat Exp $ *)
