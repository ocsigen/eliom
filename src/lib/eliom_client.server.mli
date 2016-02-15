(** Returns whether the application is sent by a server or started on
    client side. If called on server side, always returns [false].
    Otherwise, it tests the presence of JS variables added automatically by
    Eliom when the page is sent by a server.
    Example:
    {[ if not (Eliom_client.is_client_app ())
 then Eliom_client.init_client_app ... ]}
*)
val is_client_app : unit -> bool
