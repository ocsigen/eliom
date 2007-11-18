(** Ocsigen's pretty printer for xhtml. [html_compat] is an option to set
   if you want to print with a syntax closer to html (not xml).
 *)
val xhtml_print : 
    ?version:[< `HTML_v03_02 | `HTML_v04_01 | `XHTML_01_00 | `XHTML_01_01 > `XHTML_01_01 ] ->
      ?width:int -> ?encode:(string -> string) ->
        ?html_compat:bool ->
          [ `Html ] XHTML.M.elt -> string
            
(** Ocsigen's pretty printer for xhtml portions. 
   [html_compat] is an option to set
   if you want to print with a syntax closer to html (not xml). *)
val xhtml_list_print : 
    ?version:[< `HTML_v03_02 | `HTML_v04_01 | `XHTML_01_00 | `XHTML_01_01 > `XHTML_01_01 ] ->
      ?width:int -> ?encode:(string -> string) -> 
        ?html_compat:bool ->
          'a XHTML.M.elt list -> string

(** Ocsigen's pretty printer for xhtml. [html_compat] is an option to set
   if you want to print with a syntax closer to html (not xml).
 *)
val xhtml_stream : 
    ?version:[< `HTML_v03_02 | `HTML_v04_01 | `XHTML_01_00 | `XHTML_01_01 > `XHTML_01_01 ] ->
      ?width:int -> ?encode:(string -> string) ->
        ?html_compat:bool ->
          [ `Html ] XHTML.M.elt -> string Ocsistream.t
            
(** Ocsigen's pretty printer for xhtml portions. 
   [html_compat] is an option to set
   if you want to print with a syntax closer to html (not xml). *)
val xhtml_list_stream : 
    ?version:[< `HTML_v03_02 | `HTML_v04_01 | `XHTML_01_00 | `XHTML_01_01 > `XHTML_01_01 ] ->
      ?width:int -> ?encode:(string -> string) -> 
        ?html_compat:bool ->
          'a XHTML.M.elt list -> string Ocsistream.t

