module Reference = Eliom.Reference
module Common = Eliom.Common
open Lwt.Syntax

(* [on_site ?config_info ~app site_dir f] runs [f ()] during the
   initialisation of a site, where site references can be used. *)
let on_site ?config_info ~app site_dir f =
  Site.init ?config_info ~site_dir ~app (fun () -> Lwt_main.run (f ()))

let site_eref name default =
  Reference.eref ~scope:Common.site_scope
    ~persistent:(name, [%json: int])
    default

let test_site_scope () =
  let r = site_eref "test_site_scope" 0 in
  let default, modified =
    on_site ~app:"persistent-a" ["a"] (fun () ->
      let* default = Reference.get r in
      let* () = Reference.set r 1 in
      let* () = Reference.modify r succ in
      let+ modified = Reference.get r in
      default, modified)
  in
  Alcotest.(check int) "default value" 0 default;
  Alcotest.(check int) "modified" 2 modified;
  Alcotest.(check int)
    "other site" 0
    (on_site ~app:"persistent-b" ["b"] (fun () -> Reference.get r));
  (* The site is initialised again, as when the server is restarted. *)
  let kept, unset =
    on_site ~app:"persistent-a-again" ["a"] (fun () ->
      let* kept = Reference.get r in
      let* () = Reference.unset r in
      let+ unset = Reference.get r in
      kept, unset)
  in
  Alcotest.(check int) "same site" 2 kept;
  Alcotest.(check int) "unset" 0 unset

let test_modify_later () =
  (* A modification started during the initialisation of a site, for
     instance by [Lwt.async], can end after it. *)
  let r = site_eref "test_modify_later" 0 in
  let modification =
    Site.init ~site_dir:["m"] ~app:"modify-later" (fun () ->
      Reference.modify r succ)
  in
  Lwt_main.run modification;
  Alcotest.(check int)
    "modified" 1
    (on_site ~app:"modify-later-again" ["m"] (fun () -> Reference.get r))

let test_site_identity () =
  (* A site is identified by its directory only: its values are kept when the
     default host name changes, with the machine or the configuration. *)
  let r = site_eref "test_site_identity" 0 in
  on_site ~app:"identity" ["s"] (fun () -> Reference.set r 1);
  let other_host =
    {Site.config_info with Ocsigen.Extensions.default_hostname = "example.com"}
  in
  Alcotest.(check int)
    "other default host name" 1
    (on_site ~config_info:other_host ~app:"identity-other-host" ["s"] (fun () ->
       Reference.get r))

let test_default_stored () =
  (* The default value is computed once, at the first read, and stored. *)
  let calls = ref 0 in
  let default () = incr calls; 10 * !calls in
  let r =
    Reference.eref_from_fun ~scope:Common.site_scope
      ~persistent:("test_default_stored", [%json: int])
      default
  in
  Alcotest.(check int) "not called at creation" 0 !calls;
  let read app = on_site ~app ["stored"] (fun () -> Reference.get r) in
  Alcotest.(check int) "first read" 10 (read "stored");
  Alcotest.(check int) "read again" 10 (read "stored-again");
  Alcotest.(check int) "calls" 1 !calls

let test_global_scope () =
  let r =
    Reference.eref ~scope:Common.global_scope
      ~persistent:("test_global_scope", [%json: int])
      0
  in
  Lwt_main.run (Reference.set r 3);
  Alcotest.(check int)
    "on a site" 3
    (on_site ~app:"persistent-global" ["g"] (fun () -> Reference.get r));
  Alcotest.(check int)
    "modified" 4
    (Lwt_main.run
       (let* () = Reference.modify r succ in
        Reference.get r));
  Alcotest.(check int)
    "unset" 0
    (Lwt_main.run
       (let* () = Reference.unset r in
        Reference.get r))

let test_unreadable () =
  (* A stored value that cannot be decoded, for instance after a change of
     type, is replaced by the default value. References with the same name
     share their value. *)
  let check kind scope run =
    let name = "test_unreadable_" ^ kind in
    let as_string =
      Reference.eref ~scope ~persistent:(name, [%json: string]) "default"
    in
    let as_int = Reference.eref ~scope ~persistent:(name, [%json: int]) 7 in
    let as_int_value, as_string_value =
      run (fun () ->
        let* () = Reference.set as_string "x" in
        let* as_int_value = Reference.get as_int in
        let+ as_string_value = Reference.get as_string in
        as_int_value, as_string_value)
    in
    Alcotest.(check int) "reset" 7 as_int_value;
    (* The value is stored, then unreadable in turn. *)
    Alcotest.(check string) "reset again" "default" as_string_value
  in
  check "site" Common.site_scope (on_site ~app:"unreadable" ["u"]);
  check "global" Common.global_scope (fun f -> Lwt_main.run (f ()))

let suite =
  ( "persistent references"
  , [ Alcotest.test_case "site scope" `Quick test_site_scope
    ; Alcotest.test_case "modification ending after the initialisation" `Quick
        test_modify_later
    ; Alcotest.test_case "site identity" `Quick test_site_identity
    ; Alcotest.test_case "default value stored" `Quick test_default_stored
    ; Alcotest.test_case "global scope" `Quick test_global_scope
    ; Alcotest.test_case "unreadable value" `Quick test_unreadable ] )
