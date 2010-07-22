(** XHTML "compact printing" (no pretty printing, no line breaks added) *)


module MakeCompact (Format : sig
  type 'a elt
  type doctypes
  val emptytags : string list
  val need_name : string list
  val doctype : doctypes -> string
  val default_doctype : doctypes
  val toeltl : 'a elt list -> XML.elt list
  val toelt : 'a elt -> XML.elt end) : sig
 
(** Ocsigen's compact printer for xhtml. [html_compat] is an option to set
    if you want to print with a syntax closer to html (not xml).
*)
  val xhtml_print :
    ?header:string ->
    ?version:Format.doctypes ->
    ?encode:(string -> string) ->
    ?html_compat:bool ->
    [ `Html ] Format.elt -> string

(** Ocsigen's compact printer for xhtml portions.
    [html_compat] is an option to set
   if you want to print with a syntax closer to html (not xml). *)
  val xhtml_list_print :
    ?header:string ->
    ?version: Format.doctypes ->
    ?encode:(string -> string) ->
    ?html_compat:bool ->
    'a Format.elt list -> string
    

(**/**)
  val emptytags : string list
end
