open Ocsigen_http_frame
open Framepp

module H = Http_header

module C =
  struct
    type t = string
    let string_of_content c = c
    let content_of_string s = s
  end
  

module Http = FHttp_frame (C)

module PP = Fframepp(C)

let hd1= 
  {
    H.mode = H.Query;
    H.meth = Some H.GET;
    H.url = Some "pop";
    H.code = None;
    H.proto = "HTML/1.0";
    headers = []
  }


  

let frame = {Http.header = hd1; Http.content = Some (C.content_of_string "Bonjour")}

let _ =
  print_endline (PP.string_of_http_frame frame)

