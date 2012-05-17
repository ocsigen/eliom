open XHTML.M
open Eliom_service
open Eliom_parameter
open Eliom_predefmod
open Eliom_predefmod.Xhtml

let mainpage = new_service ~path:[] ~get_params:unit ()
let msgpage = new_service  ~path:[] ~get_params:(int "n") ()
let addmsgpage = 
  new_post_service ~fallback:mainpage ~post_params:(string "msg") ()
let newmsgpage = new_service ~path:["newmessage"] ~get_params:unit ()

let () = register mainpage
    (fun sp () () -> 
      Mylib.create_page sp
        "Messages"
        [Mylib.display_message_list ();
         p [a newmsgpage sp [pcdata "Create a new message"] ()]])

let () = register msgpage
    (fun sp n () -> 
      Mylib.create_page sp
        ("Message "^(string_of_int n))
        [Mylib.display_message n])

let () = register addmsgpage
    (fun sp () msg -> 
      let ok = 
        new_coservice ~max_use:1 ~fallback:mainpage ~get_params:unit () 
      in
      register ~sp ~service:ok
        (fun sp () () ->
          Mylib.register_message msg;
          Mylib.create_page sp
            "Message added"
            [Mylib.display_message_list ()]);
      Mylib.create_page sp
        "Confirm this Message?"
        [p [pcdata msg];
         p [
         a ok sp [pcdata "Yes"] (); pcdata " ";
         a mainpage sp [pcdata "Cancel"] ()]
       ]
    )

let () = register newmsgpage
    (fun sp () () -> 
      Mylib.create_page sp
        "New Message"
        [post_form addmsgpage sp
          (fun fieldname -> 
            [p [pcdata "Write your message: "; br ();
                textarea ~name:fieldname ~rows:10 ~cols:80 (); br ();
                string_input ~input_type:`Submit ~value:"Enter" ()]]) ()])
