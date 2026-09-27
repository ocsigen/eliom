(** Tests of persistent Eliom references of site and global scope, without
    request. They need a database. *)

val suite : string * unit Alcotest.test_case list
