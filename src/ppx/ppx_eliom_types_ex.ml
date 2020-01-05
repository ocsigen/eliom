open Ppx_eliom_type [@@warning "-33"]
let () = Migrate_parsetree.Driver.run_as_ppx_rewriter ()
