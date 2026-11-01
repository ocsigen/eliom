(** Helpers of the test servers: services answering in plain text. *)

val text : string -> (string * string) Lwt.t
(** [text s] is the answer [s], in plain text. *)

val get :
   ?priority:int
  -> Eliom.Lib.Url.path
  -> ('a, ([< `WithSuffix | `WithoutSuffix] as 'b), 'c) Eliom.Service.params
  -> ( 'a
       , unit
       , Eliom.Service.get
       , Eliom.Service.att
       , Eliom.Service.non_co
       , Eliom.Service.non_ext
       , Eliom.Service.reg
       , 'b
       , 'c
       , unit
       , Eliom.Service.non_ocaml )
       Eliom.Service.t
(** [get ?priority path params] is a new GET service on [path]. *)

val string :
   ?options:Eliom.Registration.String.options
  -> ?code:int
  -> ?headers:Cohttp.Header.t
  -> ( 'a
       , 'b
       , 'c
       , 'd
       , 'e
       , Eliom.Service.non_ext
       , Eliom.Service.reg
       , [< `WithSuffix | `WithoutSuffix]
       , 'f
       , 'g
       , Eliom.Registration.String.return )
       Eliom.Service.t
  -> ('a -> 'b -> Eliom.Registration.String.page Lwt.t)
  -> unit
(** [string service f] registers [f] as the handler of [service], with
    {!Eliom.Registration.String}. *)
