(* This is the complementary parser to Xmllexer, used in mode Xhtml with
   inline Ocaml code. The rules below are used to parse subexpressions
   starting with '$' *)

{ open Xmllexer }

let camlidentchar =  [^ '$' ]
let newline = ['\n']

rule camlident c  = parse
  | newline {
      update_loc c 1 false 0 ;
      store c ;
      camlident c lexbuf
    }
  | camlidentchar+ {
      store c ;
      camlident c lexbuf
    }
  | '$' { buff_contents c }

and token_dollar c = parse
  |"str:" {
      `CamlString (camlident c lexbuf)
    }
  |"list:"  {
      `CamlList (camlident c lexbuf)
    }
  | "$" { `PCData "$" }
  | ""  {
      `CamlExpr (camlident c lexbuf)
    }

and attribute_dollar c loc = parse
  | "" {
      ignore (buff_contents c);
      `CamlAttributes (camlident c lexbuf)
    }

and attr_name_dollar c loc = parse
  | ""  {
      ignore (buff_contents c) ;
      `CamlAttrName (camlident c lexbuf)
    }

and attr_data_dollar c loc = parse
  | "" {
      `CamlAttrVal (camlident c lexbuf)
    }
