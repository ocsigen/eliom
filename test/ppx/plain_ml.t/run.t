A plain .ml file goes through the PPX unchanged:

  $ ppx_eliom_server --impl plain.ml
  let x = 1

An empty file is left untouched, whatever its extension:

  $ ppx_eliom_server --impl empty.ml

  $ ppx_eliom_server --impl empty.eliom
