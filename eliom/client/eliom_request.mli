exception Looping_redirection
exception Failed_request of int
exception Program_terminated
exception External_service

val redirect_get : string -> unit
val redirect_post : string -> (string * string) list -> unit

val send :
  ?cookies_info:bool * string list ->
  ?get_args:(string * string) list ->
  ?post_args:(string * string) list -> string -> string Lwt.t

val http_get :
  ?cookies_info:bool option *
                ('a, 'b,
                 [< `Attached of ([> `External ], 'c) Eliom_services.a_s
                  | `Nonattached of 'd ],
                 [< `WithSuffix | `WithoutSuffix ], 'e, 'f, 'g, 'h)
                Eliom_services.service * 'a ->
  string ->
  (string * string) list -> string Lwt.t

val http_post :
  ?cookies_info:bool option *
                ('a, 'b,
                 [< `Attached of ([> `External ], 'c) Eliom_services.a_s
                  | `Nonattached of 'd ],
                 [< `WithSuffix | `WithoutSuffix ], 'e, 'f, 'g, 'h)
                Eliom_services.service * 'a ->
  string ->
  (string * string) list -> string Lwt.t

val get_eliom_appl_result : string -> Eliom_client_types.eliom_appl_answer

val get_cookie_info_for_uri_js : Js.js_string Js.t -> bool * string list
