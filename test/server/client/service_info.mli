(** What a client needs to know about a POST service with one parameter, such
    as the service writing on a bus: computed by the test server, sent to the
    test in JSON. *)

type t =
  { url : string  (** The URL of the service *)
  ; post_params : (string * string) list
    (** The hidden POST parameters of the service *)
  ; param : string  (** The name of the POST parameter *) }

val of_service :
   ( unit
     , 'a
     , Eliom.Service.post
     , _
     , _
     , _
     , _
     , [`WithoutSuffix]
     , unit
     , [`One of 'b] Eliom.Parameter.param_name
     , _ )
     Eliom.Service.t
  -> t
(** [of_service s] is the information about [s]. *)

val post : Tab.t -> t -> string -> Eliom_test_server.Browser.response Lwt.t
(** [post tab s v] sends the value [v] of the parameter of [s] from [tab]. *)

val to_string : t -> string
val of_string : string -> t
