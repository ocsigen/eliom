(** Tests of Eliom references on a site, without request: site references
    have a value for each site, global references one for all, and
    references of other scopes can be created but neither read nor
    written. *)

val suite : string * unit Alcotest.test_case list
