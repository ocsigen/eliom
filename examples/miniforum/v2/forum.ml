open Lwt
open XHTML.M
open Eliom_predefmod
open Eliom_services
open Eliom_parameters
open Eliom_predefmod.Xhtml
open Services

let () = register mainpage
    (fun sp () () -> 
      Mylib.create_page sp
        "Messages"
        [Mylib.display_message_list sp;
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
      Actions.register ~sp ~service:ok
        (fun sp () () ->
          Mylib.register_message msg;
          Lwt.return []);
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

let () = Actions.register disconnect_action
    (fun sp () () -> 
      Eliom_sessions.close_session ~sp () >>= fun () -> 
      Lwt.return [])

let () = Actions.register connect_action
    (fun sp () login ->
      Eliom_sessions.close_session ~sp () >>= fun () -> 
      Eliom_sessions.set_volatile_data_session_group
          ~set_max:(Some 10) ~sp login;
      Lwt.return [])
