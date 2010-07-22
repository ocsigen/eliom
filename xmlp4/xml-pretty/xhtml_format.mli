open XML

module type Info = 
sig
  val emptytags : string list
  val blocktags : string list
  val semiblocktags : string list
  type +'a elt
  type doctypes
  val default_doctype : doctypes
  val doctype : doctypes -> string
  val toeltl : 'a elt list -> XML.elt list
  val toelt : 'a elt -> XML.elt
  val ocsigenadv: string
  val need_name : string list
 end

module Xhtml5Info : Info with type doctypes = XHTML5.M.doctypes with type +'a elt = 'a XHTML5.M.elt
module XhtmlInfo : Info with type doctypes = XHTML.M.doctypes with type +'a elt = 'a XHTML.M.elt
