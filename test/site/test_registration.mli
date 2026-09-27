(** Tests of the registration of services on a site: services left
    unregistered at the end of the initialisation of the site, which is an
    error for services with a path and a warning for non-attached
    coservices, and services registered twice. *)

val suite : string * unit Alcotest.test_case list
