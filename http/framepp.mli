
val string_of_header : Http_frame.Http_header.http_header -> string

val string_of_method : Http_frame.Http_header.http_method -> string

val method_of_string : string -> Http_frame.Http_header.http_method

val string_of_proto : Http_frame.Http_header.proto -> string

val proto_of_string : string -> Http_frame.Http_header.proto
