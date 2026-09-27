(** Services of the test servers of test.ml, which answer in plain text:
    - [/set?v=] and [/get] set and read a session reference,
    - [/count] is the number of volatile data sessions of the server. *)

val text : string -> (string * string) Lwt.t
(** [text s] is the plain text [s], as a result of a String service. *)

val get :
   string list
  -> ('a, [`WithoutSuffix], 'b) Eliom.Parameter.params_type
  -> ( 'a
       , unit
       , Eliom.Service.get
       , Eliom.Service.att
       , Eliom.Service.non_co
       , Eliom.Service.non_ext
       , Eliom.Service.reg
       , [`WithoutSuffix]
       , 'b
       , unit
       , Eliom.Service.non_ocaml )
       Eliom.Service.t
(** [get path params] is a GET service at [path]. *)

val register :
   ( 'a
     , unit
     , Eliom.Service.get
     , Eliom.Service.att
     , Eliom.Service.non_co
     , Eliom.Service.non_ext
     , Eliom.Service.reg
     , [`WithoutSuffix]
     , 'b
     , unit
     , Eliom.Service.non_ocaml )
     Eliom.Service.t
  -> ('a -> unit -> (string * string) Lwt.t)
  -> unit
(** [register service f] registers [f] as the handler of [service]. *)

val session_value : string Eliom.Reference.Volatile.eref
(** The session reference of [/set] and [/get]. *)

val start : unit -> unit
(** [start ()] registers the common services and starts the server. *)
