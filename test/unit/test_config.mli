(** Tests of the global Eliom configuration read from the server
    configuration file: accepted options, their effect on the defaults, and
    errors. *)

val suite : string * unit Alcotest.test_case list
