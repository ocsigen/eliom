(** Tests of the registration of services on a site: services left
    unregistered at the end of the initialisation of the site, which is an
    error for services with a path and a warning for non-attached
    coservices, services registered twice or on the same path and
    parameters, paths used both as a page and as a directory, and
    registrations that need a request. *)

val suite : string * unit Alcotest.test_case list
