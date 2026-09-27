(** Tests of typed page parameters: encoding of OCaml values into HTTP
    parameters and URL suffixes, decoding back, and names used in forms. *)

val suite : string * unit Alcotest.test_case list
