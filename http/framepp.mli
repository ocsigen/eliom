
val string_of_header : Ocsigen_http_frame.Http_header.http_header -> string

val string_of_method : Ocsigen_http_frame.Http_header.http_method -> string

val method_of_string : string -> Ocsigen_http_frame.Http_header.http_method

val string_of_proto : Ocsigen_http_frame.Http_header.proto -> string

val proto_of_string : string -> Ocsigen_http_frame.Http_header.proto
