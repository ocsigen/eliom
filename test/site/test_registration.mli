(** Tests of the registration of services on a site: services left
    unregistered at the end of the initialisation of the site, and services
    registered twice. *)

val suite : string * unit Alcotest.test_case list
