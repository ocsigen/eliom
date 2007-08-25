%{
(* Ocsigen
 * http://www.ocsigen.org
 * http_parser.mly Copyright (C) 2005 Denis Berthod
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

  open Http_frame
  open Http_header
  
  let mode = ref Nofirstline
  let proto = ref HTTP11
  let headers = ref []

  let reset_header () = headers:=[]
  
  let make_header() =
    {mode = !mode; 
     proto= !proto;
     headers= !headers}

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
      | s -> raise
            (Http_error.Http_exception (Some 400,[("Unknown method: "^s)]))

  let proto_of_string =
    function
      | "HTTP/1.1" -> HTTP11
      | "HTTP/1.0" -> HTTP10
      | s -> raise
            (Http_error.Http_exception (Some 400,
                                        [("Unsupported protocol: "^s)]))

(*
  let split_string s =
    try
    let ind = String.index s ':' in
    (String.lowercase (String.sub s 0 ind )),
      String.lowercase (String.sub s (ind+1) ((String.length s) - ind-1) )
    with Not_found  -> 
      raise (Http_error.Http_exception (Some 400,["bad header format"]))
*)
%}

%token COLON EOL 
%token <string>METHOD
%token <string>PROTO
%token <string>STRING
%token <string>CODE

%start header nofirstline
%type <Http_frame.Http_header.http_header>header
%type <Http_frame.Http_header.http_header>nofirstline

%%
header :
  | firstline EOL                {make_header()}
  | firstline lines EOL          {make_header()}

firstline :
  | METHOD STRING PROTO EOL      {reset_header ();
                                 mode := Query (meth_of_string($1), $2);
                                 proto:=(proto_of_string $3)}
  | PROTO CODE strings EOL       {reset_header ();
				 mode := Answer (int_of_string $2);
				 proto:=(proto_of_string $1)
			       }

nofirstline :
  | EOL                          {mode := Nofirstline; 
				  proto := HTTP11;
				  make_header()}
  | lines EOL                    {mode := Nofirstline; 
				  proto := HTTP11;
				  make_header()}

lines :
  | line                         {headers:=$1::!headers}
  | line lines                   {headers:=$1::!headers;$2}
  | strings EOL                  {}
  | strings EOL lines            {$3}

line :
  | STRING COLON strings EOL    {(String.lowercase($1),$3)}
  | CODE COLON strings EOL      {($1,$3)}
  | STRING COLON EOL            {(String.lowercase($1),"")}
  | CODE COLON EOL              {($1,"")}
  /* EOL                  {split_string $1}*/

strings :
  | COLON                        {":"}
  | STRING                       {$1}
  | STRING COLON strings         {$1^":"^$3}
  | STRING strings               {$1^" "^$2}
  | PROTO                        {$1}
  | PROTO COLON strings          {$1^":"^$3}
  | PROTO strings                {$1^" "^$2}
  | METHOD                       {$1}
  | METHOD COLON strings         {$1^":"^$3}
  | METHOD strings               {$1^" "^$2}
  | CODE                         {$1}
  | CODE strings                 {$1^" "^$2}
  | CODE COLON strings           {$1^":"^$3}

%%

let nofirstline a =
  reset_header ();
  nofirstline a


