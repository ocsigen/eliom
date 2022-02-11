open Ppx_eliom_type [@@warning "-33"]

let () = Ppxlib.Driver.run_as_ppx_rewriter ()
