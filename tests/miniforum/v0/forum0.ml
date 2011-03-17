open XHTML.M
open Eliomservices
open Eliomparameters
open Eliompredefmod
open Eliompredefmod.Xhtml

let mainpage = new_service ~path:[] ~get_params:unit ()

let () = register mainpage
    (fun sp () () -> 
      Mylib.create_page sp
        "Messages"
        [Mylib.display_message_list ()])

