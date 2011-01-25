open XHTML5.M
open Eliom_parameters

let (>>=) = Lwt.bind

(* Services *)
let main_service = Eliom_services.service ~path:[""] ~get_params:unit ()

let user_service =
  Eliom_services.service
    ~path:["users"] ~get_params:(suffix (string "name")) ()

let connection_service =
  Eliom_services.post_service
    ~fallback:main_service
    ~post_params:(string "name" ** string "password")
    ()

let disconnection_service = Eliom_services.post_coservice' ~post_params:unit ()

let new_user_form_service = Eliom_services.service ~path:["create account"] ~get_params:unit ()

let account_confirmation_service =
  Eliom_services.post_coservice ~fallback:new_user_form_service ~post_params:(string "name" ** string "password") ()




(* User names and passwords: *)
let users = ref [("Calvin", "123"); ("Hobbes", "456")]

let user_links () =
  ul (List.map (fun (name, _) -> li [Eliom_output.Xhtml5.a ~service:user_service [pcdata name] name]) !users)

let check_pwd name pwd = try List.assoc name !users = pwd with Not_found -> false



(* Eliom references *)
let username = Eliom_references.eref ~scope:`Session None

let wrong_pwd = Eliom_references.eref ~scope:`Request false



(* Page widgets: *)
let disconnect_box () =
  Eliom_output.Xhtml5.post_form disconnection_service
    (fun _ -> [p [Eliom_output.Xhtml5.string_input
                    ~input_type:`Submit ~value:"Log out" ()]]) ()

let connection_box () =
  Eliom_references.get username >>= fun u ->
  Eliom_references.get wrong_pwd >>= fun wp ->
  Lwt.return
    (match u with
      | Some s -> div [p [pcdata "You are connected as "; pcdata s; ];
                       disconnect_box () ]
      | None ->
        let l =
          [Eliom_output.Xhtml5.post_form ~service:connection_service
            (fun (name1, name2) ->
              [p [pcdata "login: ";
                  Eliom_output.Xhtml5.string_input ~input_type:`Text ~name:name1 ();
                  br ();
                  pcdata "password: ";
                  Eliom_output.Xhtml5.string_input ~input_type:`Password ~name:name2 ();
                  br ();
                  Eliom_output.Xhtml5.string_input ~input_type:`Submit ~value:"Connect" ()
                 ]]) ();
             p [Eliom_output.Xhtml5.a new_user_form_service [pcdata "Create an account"] ()]]
        in
        if wp
        then div ((p [em [pcdata "Wrong user or password"]])::l)
        else div l
    )

let create_account_form () =
  Eliom_output.Xhtml5.post_form ~service:account_confirmation_service
    (fun (name1, name2) ->
      [p [pcdata "login: ";
          Eliom_output.Xhtml5.string_input ~input_type:`Text ~name:name1 ();
          br ();
          pcdata "password: ";
          Eliom_output.Xhtml5.string_input ~input_type:`Password ~name:name2 ();
          br ();
          Eliom_output.Xhtml5.string_input ~input_type:`Submit ~value:"Connect" ()
         ]]) ()




(* Registration of services *)
let _ = 
  Eliom_output.Xhtml5.register
    ~service:main_service
    (fun () () ->
      connection_box () >>= fun cf ->
      Lwt.return
        (html (head (title (pcdata "")) [])
              (body [h1 [pcdata "Hello"];
                     cf;
                     user_links ()])));

  Eliom_output.Any.register
    ~service:user_service
    (fun name () ->
      if List.exists (fun (n, _) -> n = name) !users
      then begin
        connection_box () >>= fun cf ->
        Eliom_output.Xhtml5.send
          (html (head (title (pcdata name)) [])
             (body [h1 [pcdata name];
                    cf;
                    p [Eliom_output.Xhtml5.a ~service:main_service [pcdata "Home"] ()]]))
      end
      else
        Eliom_output.Xhtml5.send
          ~code:404
          (html (head (title (pcdata "404")) [])
             (body [h1 [pcdata "404"];
                    p [pcdata "That page does not exist"]]))
    );

  Eliom_output.Action.register
    ~service:connection_service
    (fun () (name, password) ->
      if check_pwd name password
      then Eliom_references.set username (Some name)
      else Eliom_references.set wrong_pwd true >>= fun () -> Lwt.return ());

  Eliom_output.Action.register
    ~service:disconnection_service
    (fun () () ->
      Eliom_state.close_session () >>= fun () ->
      Lwt.return ());

  Eliom_output.Xhtml5.register
    ~service:new_user_form_service
    (fun () () ->
      Lwt.return
        (html (head (title (pcdata "")) [])
              (body [h1 [pcdata "Create an account"];
                     create_account_form ();
                    ])));

  Eliom_output.Xhtml5.register
    ~service:account_confirmation_service
    (fun () (name, pwd) ->
      let create_account_service =
        Eliom_output.Action.register_coservice
          ~fallback:main_service
          ~get_params:Eliom_parameters.unit
          ~timeout:60.
          (fun () () ->
            users := (name, pwd)::!users;
            Lwt.return ())
      in
      Lwt.return
        (html (head (title (pcdata "")) [])
              (body [h1 [pcdata "Confirm account creation for "; pcdata name];
                     p [Eliom_output.Xhtml5.a ~service:create_account_service [pcdata "Yes"] ();
                        pcdata " ";
                        Eliom_output.Xhtml5.a ~service:main_service [pcdata "No"] ()]
                    ])))
