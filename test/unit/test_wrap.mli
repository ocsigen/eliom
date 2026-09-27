(** Tests of the serialisation of server values for the client: wrapping
    ({!Eliom.Wrap}), the encoding of Eliom data, and the escaping of
    marshalled data embedded in pages. *)

val suite : string * unit Alcotest.test_case list
