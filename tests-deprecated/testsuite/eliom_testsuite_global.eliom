let eref : string option Eliom_reference.eref =
  Eliom_reference.eref ~scope:Eliom_common.site_scope None

let eref' : string option Eliom_reference.eref =
  Eliom_reference.eref ~scope:Eliom_common.site_scope ~persistent:"eref2" None
