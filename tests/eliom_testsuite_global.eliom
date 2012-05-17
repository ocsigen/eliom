
let eref : string option Eliom_reference.eref =
  Eliom_reference.eref ~scope:Eliom_common.site None

let eref' : string option Eliom_reference.eref =
  Eliom_reference.eref ~scope:Eliom_common.site ~persistent:"eref2" None

