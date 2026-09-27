(** Tests of volatile session groups, on each level (tab sessions of a
    browser session, browser sessions of a group, groups of a site) and for
    each kind of sessions (service and data): closing a state, emptying or
    evicting a group, or lowering a limit closes exactly the states it holds,
    and never the states of the other kind. *)

val suite : string * unit Alcotest.test_case list
