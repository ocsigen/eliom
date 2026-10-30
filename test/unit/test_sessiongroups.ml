module Common = Eliom.Common
module Groups = Eliom.Mod_sessiongroups
module Dlist = Ocsigen_base.Cache.Dlist

(* Volatile states are kept in bounded dlists, on three levels:
   - the tab sessions of a browser session are in the dlist of the group of
     level [`Client_process] named after the browser session;
   - the browser sessions of a session group are in the dlist of the group
     of level [`Session];
   - the session groups of a site are entries of the group of groups of the
     site, shared by the two kinds of sessions (service and data).
   Removing a node closes what it holds, and lowering the maximum of a
   dlist closes its oldest nodes. *)

(* The operations of one kind of sessions *)
type kind =
  { name : string
  ; add :
      Common.sitedata
      -> string
      -> Common.cookie_level Common.sessgrp
      -> string Dlist.node
  ; remove : 'a. 'a Dlist.node -> unit
  ; set_max : 'a. 'a Dlist.node -> int -> unit
  ; set_group_max : Common.cookie_level Common.sessgrp -> int -> unit
  ; size : Common.cookie_level Common.sessgrp -> int
  ; entry :
      Common.cookie_level Common.sessgrp
      -> Common.group_of_groups_entry Dlist.node option }

let service =
  { name = "service"
  ; add = (fun sitedata id g -> Groups.Serv.add sitedata id g)
  ; remove = Groups.Serv.remove
  ; set_max = Groups.Serv.set_max
  ; set_group_max = Groups.Serv.set_group_max
  ; size = Groups.Serv.group_size
  ; entry =
      (fun g -> Option.map snd (Groups.Serv.find_node_in_group_of_groups g)) }

let data =
  { name = "data"
  ; add = (fun sitedata id g -> Groups.Data.add sitedata id g)
  ; remove = Groups.Data.remove
  ; set_max = Groups.Data.set_max
  ; set_group_max = Groups.Data.set_group_max
  ; size = Groups.Data.group_size
  ; entry = Groups.Data.find_node_in_group_of_groups }

let other k = if k == service then data else service

(* Each test has a site of its own: the group tables are global, and their
   keys contain the site directory. *)
let new_sitedata dir =
  Eliom.Mod_main.create_sitedata [] [dir]
    (Ocsigen.Extensions.default_config_info ())

let group sitedata name =
  Groups.make_full_named_group_name_ ~cookie_level:`Session sitedata name

let tabs sitedata browser_session =
  Groups.make_full_named_group_name_ ~cookie_level:`Client_process sitedata
    browser_session

(* The states of one kind in a site:
   - group g1: browser sessions b11 (tabs t111, t112) and b12 (tab t121);
   - group g2: browser session b21 (tab t211). *)
type states =
  { b11 : string Dlist.node
  ; b12 : string Dlist.node
  ; t111 : string Dlist.node
  ; t112 : string Dlist.node }

let id k name = k.name ^ "-" ^ name

let populate sitedata k =
  let browser g name = k.add sitedata (id k name) (group sitedata g) in
  let tab b name = k.add sitedata (id k name) (tabs sitedata (id k b)) in
  let b11 = browser "g1" "b11" in
  let b12 = browser "g1" "b12" in
  ignore (browser "g2" "b21");
  let t111 = tab "b11" "t111" in
  let t112 = tab "b11" "t112" in
  ignore (tab "b12" "t121");
  ignore (tab "b21" "t211");
  {b11; b12; t111; t112}

(* Number of browser sessions of each group, and of tabs of each browser
   session *)
let snapshot sitedata k =
  [ "g1", k.size (group sitedata "g1")
  ; "g2", k.size (group sitedata "g2")
  ; "b11 tabs", k.size (tabs sitedata (id k "b11"))
  ; "b12 tabs", k.size (tabs sitedata (id k "b12"))
  ; "b21 tabs", k.size (tabs sitedata (id k "b21")) ]

let everything = ["g1", 2; "g2", 1; "b11 tabs", 2; "b12 tabs", 1; "b21 tabs", 1]

(* [expect sitedata k msg ~changed] checks that the states of [k] are those
   created by [populate], but for the numbers in [changed], and that the
   states of the other kind are untouched. *)
let expect sitedata k msg ~changed =
  let expected =
    List.map
      (fun (name, n) ->
         name, Option.value (List.assoc_opt name changed) ~default:n)
      everything
  in
  Alcotest.(check (list (pair string int)))
    (Printf.sprintf "%s: %s states" msg k.name)
    expected (snapshot sitedata k);
  Alcotest.(check (list (pair string int)))
    (Printf.sprintf "%s: %s states" msg (other k).name)
    everything
    (snapshot sitedata (other k))

let entry_exn k g =
  match k.entry g with
  | Some node -> node
  | None -> Alcotest.fail "no entry in the group of groups"

(* Closing *)

let close_tab sitedata k s =
  k.remove s.t111;
  expect sitedata k "after closing t111" ~changed:["b11 tabs", 1]

let close_browser_session sitedata k s =
  k.remove s.b11;
  expect sitedata k "after closing b11" ~changed:["g1", 1; "b11 tabs", 0]

let close_all_browser_sessions sitedata k s =
  k.remove s.b11;
  k.remove s.b12;
  expect sitedata k "after closing b11 and b12"
    ~changed:["g1", 0; "b11 tabs", 0; "b12 tabs", 0]

let close_group sitedata k _ =
  k.remove (entry_exn k (group sitedata "g1"));
  expect sitedata k "after closing g1"
    ~changed:["g1", 0; "b11 tabs", 0; "b12 tabs", 0]

let evict_group sitedata k _ =
  let groups = sitedata.Common.group_of_groups in
  ignore (Dlist.set_maxsize groups (Dlist.size groups));
  (* The group of groups is full: a new group evicts its oldest entry, the
     group g1 of [k]. *)
  ignore (k.add sitedata (id k "b31") (group sitedata "g3"));
  expect sitedata k "after evicting g1"
    ~changed:["g1", 0; "b11 tabs", 0; "b12 tabs", 0]

(* Limits: [set_max] on a node sets the maximum of the dlist holding it,
   which is what State.set_max_*_states_in_parent do. *)

let limit_tabs sitedata k s =
  k.set_max s.t112 1;
  expect sitedata k "after limiting b11 to one tab" ~changed:["b11 tabs", 1]

let limit_browser_sessions sitedata k s =
  k.set_max s.b12 1;
  expect sitedata k "after limiting g1 to one browser session"
    ~changed:["g1", 1; "b11 tabs", 0]

let limit_groups sitedata k _ =
  let groups = sitedata.Common.group_of_groups in
  k.set_max (entry_exn k (group sitedata "g2")) (Dlist.size groups - 1);
  expect sitedata k "after limiting the number of groups"
    ~changed:["g1", 0; "b11 tabs", 0; "b12 tabs", 0]

(* [run name f k] runs [f] on a new site holding the states of both kinds,
   those of [k] created first, so that the oldest entry of the group of
   groups is the group g1 of [k]. *)
let run name f k () =
  let sitedata = new_sitedata (name ^ " " ^ k.name) in
  let states = populate sitedata k in
  ignore (populate sitedata (other k));
  expect sitedata k "before" ~changed:[];
  f sitedata k states

let scenarios =
  [ "close a tab", close_tab
  ; "close a browser session", close_browser_session
  ; "close all the browser sessions of a group", close_all_browser_sessions
  ; "close a group", close_group
  ; "evict a group", evict_group
  ; "limit the tabs of a browser session", limit_tabs
  ; "limit the browser sessions of a group", limit_browser_sessions
  ; "limit the groups of a site", limit_groups ]

(* The sessions without a group of a site are in one group, which is not a
   group of the site: when the site has too many groups, they are not closed
   with the oldest one. *)
let without_group k () =
  let sitedata = new_sitedata ("without a group " ^ k.name) in
  let anonymous =
    Groups.make_full_group_name ~cookie_level:`Session ~sitedata None
  in
  ignore (k.add sitedata (id k "a1") anonymous);
  ignore (k.add sitedata (id k "a2") anonymous);
  Alcotest.(check bool)
    "not in the group of groups" true
    (Option.is_none (k.entry anonymous));
  ignore (Dlist.set_maxsize sitedata.Common.group_of_groups 1);
  ignore (k.add sitedata (id k "b11") (group sitedata "g1"));
  ignore (k.add sitedata (id k "b21") (group sitedata "g2"));
  Alcotest.(check int) "oldest group closed" 0 (k.size (group sitedata "g1"));
  Alcotest.(check int) "sessions without a group" 2 (k.size anonymous);
  (* A new limit for the group applies at once *)
  k.set_group_max anonymous 1;
  Alcotest.(check int) "after a new limit" 1 (k.size anonymous)

(* The list of the sessions of a persistent group, in the database *)
let persistent_up () =
  let sitedata = new_sitedata "persistent groups" in
  let g =
    Common.make_persistent_full_group_name ~cookie_level:`Session
      (Common.get_site_dir_string sitedata)
      (Some "g")
  in
  let check msg expected =
    Lwt.map (Alcotest.(check (list string)) msg expected) (Groups.Pers.find g)
  in
  Lwt_main.run
    (let open Lwt.Syntax in
     let* _ = Groups.Pers.add None "s1" g in
     let* _ = Groups.Pers.add None "s2" g in
     let* () = check "newest first" ["s2"; "s1"] in
     let* () = Groups.Pers.up "s1" g in
     let* () = check "used, moved up" ["s1"; "s2"] in
     let* () = Groups.Pers.remove sitedata "s1" g in
     (* A request of a session that has left the group, during its
        closing, does not put it back. *)
     let* () = Groups.Pers.up "s1" g in
     check "left the group" ["s2"])

let suite =
  ( "session groups"
  , List.concat_map
      (fun k ->
         Alcotest.test_case
           (Printf.sprintf "sessions without a group (%s)" k.name)
           `Quick (without_group k)
         :: List.map
              (fun (name, f) ->
                 Alcotest.test_case
                   (Printf.sprintf "%s (%s)" name k.name)
                   `Quick (run name f k))
              scenarios)
      [service; data]
    @ [Alcotest.test_case "persistent group, use" `Quick persistent_up] )
