(** Tests of services created on a site: their paths, the URLs built for them
    without a request, and the site configuration they read. *)

val suite : string * unit Alcotest.test_case list
