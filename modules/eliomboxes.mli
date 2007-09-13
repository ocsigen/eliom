(** Predefined boxes for Eliommod *)
open Eliomservices
open Eliomparameters
open Eliomsessions

val menu : ?classe:XHTML.M.nmtoken list ->
  ((unit, unit, [< get_service_kind ],
    [ `WithoutSuffix ],
    unit, unit, [< registrable ])
     service * Xhtmltypes.a_content XHTML.M.elt list)
  ->
    ((unit, unit, [< get_service_kind ],
      [ `WithoutSuffix ],
      unit, unit, [< registrable])
       service * Xhtmltypes.a_content XHTML.M.elt list)
      list ->
        (unit, unit, [< get_service_kind ], [ ` WithoutSuffix ], 
         unit, unit, 
         [< registrable ]) service ->
           Eliommod.server_params -> [> `Ul ] XHTML.M.elt

(** Creates a menu 

   Example:

  [menu ~classe:["mainmenu"]
    [
     (home, <:xmllist< Home >>);
     (infos, <:xmllist< More infos >>)
   ] current sp]

*)
