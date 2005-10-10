%{
  open Http_frame
  open Http_header
  
  let mode = ref Query
  let meth = ref None
  let url = ref None
  let code = ref None
  let proto = ref ""
  let headers = ref []

  let reset_header () = headers:=[]
  
  let make_header() =
    {mode = !mode; meth= !meth; url = !url; code = !code ;
    proto= !proto;headers= !headers}

  let meth_of_string =
    function
      |"GET" -> GET
      |"POST" -> POST
      |"HEAD" -> HEAD
      |"PUT" -> PUT
      |"DELETE" -> DELETE
      |"TRACE" -> TRACE
      |"OPTIONS" -> OPTIONS
      |"CONNECT" -> CONNECT
      |"LINK" -> LINK
      |"UNLINK" -> UNLINK
      |"PATCH" -> PATCH
      |s -> raise (Http_error.Http_exception (Some 400,[("Unknown method :"^s)]))

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
%token <int>CODE

%start header
%type <Http_frame.Http_header.http_header>header

%%
header :
  |firstline EOL                {make_header()}
  |firstline lines EOL          {make_header()}

firstline :
  |METHOD STRING PROTO EOL      {reset_header ();
      mode := Query;meth:=Some(meth_of_string($1));
      url:=Some $2;proto:=$3}
  |PROTO CODE strings EOL       {reset_header ();
				 mode := Answer;proto:=$1;code:= Some $2}

lines :
  |line                         {headers:=$1::!headers}
  |line lines                   {headers:=$1::!headers;$2}

line :
  STRING COLON strings EOL    {(String.lowercase($1),$3)}
  /* EOL                  {split_string $1}*/

strings :
  |COLON                        {":"}
  |STRING                       {$1}
  |STRING COLON strings         {$1^":"^$3}
  |STRING strings               {$1^" "^$2}
  |CODE                         {string_of_int $1}
  |CODE strings                 {(string_of_int $1)^" "^$2}
  |CODE COLON strings           {(string_of_int $1)^":"^$3}
