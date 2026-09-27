(** Tests of HTML content generated on the server: printing of functional
    and DOM nodes, element identifiers, custom data, and the error pages
    shown for wrong parameters. *)

val suite : string * unit Alcotest.test_case list
