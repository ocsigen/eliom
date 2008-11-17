open XHTML.M
open Eliom_services
open Eliom_parameters
open Eliom_predefmod


let mainpage = new_service ~path:[] ~get_params:unit ()
let msgpage = new_service  ~path:[] ~get_params:(int "n") ()

let () = Xhtml.register mainpage
    (fun sp () () -> 
      Mylib.create_page sp
        "Messages"
        [Mylib.display_message_list ()])

let () = Xhtml.register msgpage
    (fun sp n () -> 
      Mylib.create_page sp
        ("Message "^(string_of_int n))
        [Mylib.display_message n])

