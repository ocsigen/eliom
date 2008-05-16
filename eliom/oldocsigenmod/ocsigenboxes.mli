(** Predefined boxes for Ocsigenmod *)

val menu : ?classe:XHTML.M.nmtoken list ->
       ((unit,unit, [<`Internal_Service of [<`Public_Service | `Local_Service] | `External_Service],[<`WithSuffix|`WithoutSuffix] as 'tipo,unit Ocsigen.param_name, unit Ocsigen.param_name)
        Ocsigen.service * Xhtmltypes.a_content XHTML.M.elt list)
        ->
       ((unit,unit, [<`Internal_Service of [<`Public_Service | `Local_Service] | `External_Service],[<`WithSuffix|`WithoutSuffix] as 'tipo,unit Ocsigen.param_name, unit Ocsigen.param_name)
        Ocsigen.service * Xhtmltypes.a_content XHTML.M.elt list)
       list ->
       (unit,unit, [<`Internal_Service of [<`Public_Service | `Local_Service] | `External_Service],'tipo, unit Ocsigen.param_name, unit Ocsigen.param_name) Ocsigen.service ->
       Ocsigen.server_params -> [> `Ul ] XHTML.M.elt

(** Creates a menu

   Example:

  [menu ~classe:["mainmenu"]
    [
     (home, <:xmllist< Home >>);
     (infos, <:xmllist< More infos >>)
   ] current sp]

   Tip: How to make a menu with different kinds of services (external, internal...)?

   You need to coerce each of them. For example
   [(home :> (('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, Ocsigen.service_kind) service))]

*)
