(** Tests of state cookies: serialisation of cookie sets sent to the client,
    hashing of cookie values, session identifiers, and the names of state
    cookies, which encode the scope of the state. *)

val suite : string * unit Alcotest.test_case list
