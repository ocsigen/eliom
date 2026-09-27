(* The test server of test.ml: pages answering with their menus, printed, one
   per line. *)

open Eliom
module H = Content.Html

let page path =
  Service.create ~path:(Service.Path path) ~meth:(Service.Get Parameter.unit) ()

let home = page []
let a = page ["a"]
let b = page ["b"]
let c = page ["c"]
let section = page ["section"; ""]
let x = page ["section"; "x"]
let y = page ["section"; "y"]
let print elt = Format.asprintf "%a" (H.Printer.pp_elt ()) elt
let menu = [a, [H.F.txt "A"]; b, [H.F.txt "B"]; c, [H.F.txt "C"]]

let leaf text service =
  [H.F.txt text], Tools.Site_tree (Tools.Main_page (Tools.Srv service), [])

let site =
  ( Tools.Main_page (Tools.Srv home)
  , [ leaf "A" a
    ; ( [H.F.txt "Section"]
      , Tools.Site_tree
          (Tools.Main_page (Tools.Srv section), [leaf "X" x; leaf "Y" y]) )
    ; [H.F.txt "Off"], Tools.Disabled ] )

let menus () =
  String.concat "\n"
    [ print (Tools.F.menu menu ())
    ; print (Tools.F.menu menu ~service:b ())
    ; String.concat ""
        (List.map print (Tools.F.hierarchical_menu_depth_first site ()))
    ; String.concat ""
        (List.map print
           (Tools.F.hierarchical_menu_depth_first ~whole_tree:true site ()))
    ; String.concat ""
        (List.map print (Tools.F.hierarchical_menu_breadth_first site ()))
    ; String.concat "" (List.map print (Tools.F.structure_links site ())) ]

let () =
  List.iter
    (fun service ->
       Registration.String.register ~service (fun () () ->
         Lwt.return (menus (), "text/plain")))
    [home; a; b; c; section; x; y]

let () = Eliom_test_server.Server_harness.start [App.run ~xhr_links:false ()]
