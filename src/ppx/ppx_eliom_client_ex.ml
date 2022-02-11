open Ppx_eliom_client [@@warning "-33"]

let () = Ppxlib.Driver.run_as_ppx_rewriter ()
