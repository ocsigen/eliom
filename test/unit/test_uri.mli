(** Tests of URL construction that does not depend on a site or a request:
    relative paths, assembly of components, absolute prefixes, and URLs of
    external services. *)

val suite : string * unit Alcotest.test_case list
