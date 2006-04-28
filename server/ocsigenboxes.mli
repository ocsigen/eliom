(** Predefined boxes *)

val menu : ?classe:XHTML.M.nmtoken list ->
       ((unit,unit, [<`Internal_Url of [<`Public_Url | `State_Url] | `External_Url])
        Ocsigen.url * Xhtmltypes.a_content XHTML.M.elt list)
       list ->
       (unit,unit, [<`Internal_Url of [<`Public_Url | `State_Url] | `External_Url]) Ocsigen.url ->
       Ocsigen.current_url -> [> `Ul ] XHTML.M.elt
(** Creates a menu 

   Example:

  [menu ~classe:["mainmenu"]
    [
     (home, <:xmllist< Home >>);
     (infos, <:xmllist< More infos >>)
   ] current current_url]

   Tip: How to make a menu with different kinds of urls (external, internal...)?

   You need to coerce each of them. For example
   [(home :> (('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, Ocsigen.url_kind) url))]

*)
