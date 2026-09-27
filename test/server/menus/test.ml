(* Tests of the menus of Eliom.Tools, printed by the pages of the server:
   the current page of a menu is found from the URL of the request. *)

open Eliom_test_server
open Lwt.Syntax

(* The menus of a page, in the order of server.ml *)
type menus =
  { menu : string
  ; menu_of_b : string
  ; depth_first : string
  ; whole_tree : string
  ; breadth_first : string }

let menus b url =
  let+ r = Browser.get b url in
  if r.status <> 200 then Alcotest.failf "%s: status %d" url r.status;
  match String.split_on_char '\n' r.body with
  | [menu; menu_of_b; depth_first; whole_tree; breadth_first] ->
      {menu; menu_of_b; depth_first; whole_tree; breadth_first}
  | _ -> Alcotest.failf "%s: unexpected answer %S" url r.body

(* [case server name f] is a test that runs [f b] with a new browser [b]. *)
let case server name f =
  Alcotest.test_case name `Quick (fun () ->
    Lwt_main.run (f (Browser.create server)))

let check = Alcotest.(check string)

let simple server =
  let case = case server in
  ( "menu"
  , [ case "current page" (fun b ->
        let+ m = menus b "/a" in
        check "menu"
          {|<ul class="eliomtools_menu"><li class="eliomtools_current eliomtools_first">A</li><li><a href="b">B</a></li><li class="eliomtools_last"><a href="c">C</a></li></ul>|}
          m.menu)
    ; case "page not in the menu" (fun b ->
        (* The links are relative to the page. *)
        let+ m = menus b "/section/x" in
        check "menu"
          {|<ul class="eliomtools_menu"><li class="eliomtools_first"><a href="../a">A</a></li><li><a href="../b">B</a></li><li class="eliomtools_last"><a href="../c">C</a></li></ul>|}
          m.menu)
    ; case "given service" (fun b ->
        let+ m = menus b "/a" in
        check "menu"
          {|<ul class="eliomtools_menu"><li class="eliomtools_first"><a href="a">A</a></li><li class="eliomtools_current">B</li><li class="eliomtools_last"><a href="c">C</a></li></ul>|}
          m.menu_of_b) ] )

let level0 ?(a = "") ?(section = "") ?(submenu = "") prefix =
  Printf.sprintf
    {|<ul class="eliomtools_menu eliomtools_level0"><li class="eliomtools_first%s"><a href="%sa">A</a></li><li%s><a href="%s">Section</a>%s</li><li class="eliomtools_disabled eliomtools_last">Off</li></ul>|}
    a prefix section
    (if prefix = "" then "section/" else "./")
    submenu

let level1 ?(x = "") prefix =
  Printf.sprintf
    {|<ul class="eliomtools_menu eliomtools_level1"><li class="eliomtools_first%s"><a href="%sx">X</a></li><li class="eliomtools_last"><a href="%sy">Y</a></li></ul>|}
    x prefix prefix

let hierarchical server =
  let case = case server in
  let current = " eliomtools_current" in
  ( "hierarchical menu"
  , [ case "depth first" (fun b ->
        let* m = menus b "/a" in
        (* The submenus of the other sections are collapsed. *)
        check "page of the first level" (level0 ~a:current "") m.depth_first;
        let+ m = menus b "/section/x" in
        check "page of a section"
          (level0 ~section:{| class="eliomtools_current_path"|}
             ~submenu:(level1 ~x:current "") "../")
          m.depth_first)
    ; case "main page of a section" (fun b ->
        let+ m = menus b "/section/" in
        check "section"
          (level0 ~section:{| class="eliomtools_current"|} ~submenu:(level1 "")
             "../")
          m.depth_first)
    ; case "whole tree" (fun b ->
        let+ m = menus b "/a" in
        check "all the submenus"
          (level0 ~a:current ~submenu:(level1 "section/") "")
          m.whole_tree)
    ; case "breadth first" (fun b ->
        (* One list for each level, down to the current page *)
        let* m = menus b "/a" in
        check "page of the first level" (level0 ~a:current "") m.breadth_first;
        let+ m = menus b "/section/x" in
        check "page of a section"
          (level0 ~section:{| class="eliomtools_current_path"|} "../"
          ^ level1 ~x:current "")
          m.breadth_first) ] )

let () =
  Server_harness.with_server "./server.exe" (fun server ->
    Alcotest.run ~and_exit:false "eliom-server-menus"
      [simple server; hierarchical server])
