
let eref : string option Eliom_references.eref =
  Eliom_references.eref ~scope:Eliom_common.site None

let eref' : string option Eliom_references.eref =
  Eliom_references.eref ~scope:Eliom_common.site ~persistent:"eref2" None

