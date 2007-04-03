(** Predefined boxes for Eliommod *)
open Eliom

val menu : ?classe:XHTML.M.nmtoken list ->
  ((unit, unit, [< get_service_kind ],
    [ `WithoutSuffix ],
    unit param_name, unit param_name, [< registrable ])
     service * Xhtmltypes.a_content XHTML.M.elt list)
  ->
    ((unit, unit, [< get_service_kind ],
      [ `WithoutSuffix ],
      unit param_name, unit param_name, [< registrable])
       service * Xhtmltypes.a_content XHTML.M.elt list)
      list ->
        (unit, unit, [< get_service_kind ], [ ` WithoutSuffix ], 
         unit param_name, unit param_name, 
         [< registrable ]) service ->
           server_params -> [> `Ul ] XHTML.M.elt

(** Creates a menu 

   Example:

  [menu ~classe:["mainmenu"]
    [
     (home, <:xmllist< Home >>);
     (infos, <:xmllist< More infos >>)
   ] current sp]

   Tip: How to make a menu with different kinds of services (external, internal...)?

   You need to coerce each of them. For example
   [(home :> (('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, service_kind) service))]

*)
