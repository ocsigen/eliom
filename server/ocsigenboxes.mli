(** Predefined boxes *)

val menu : ?classe:XHTML.M.nmtoken list ->
       (('a, Ocsigen.xhformcontl,
         Xhtmltypes.xha XHTML.M.elt, 'c, 'd, 'e, 'f, 'g, [<`Internal_Url of [<`Public_Url | `State_Url] | `External_Url])
        Ocsigen.url * Xhtmltypes.xhacont XHTML.M.elt list)
       list ->
       ('a, Ocsigen.xhformcontl, Xhtmltypes.xha XHTML.M.elt, 'c, 'd, 'e, 'f, 'g, [<`Internal_Url of [<`Public_Url | `State_Url] | `External_Url]) Ocsigen.url ->
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
