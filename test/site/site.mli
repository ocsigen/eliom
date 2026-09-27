(** A site for tests, without server. *)

val hostname : string
(** The default host name of the sites. *)

val http_port : int
(** The default HTTP port of the sites. *)

val https_port : int
(** The default HTTPS port of the sites. *)

val config_info : Ocsigen.Extensions.config_info
(** The default configuration of the host of the sites. *)

val init :
   ?config_info:Ocsigen.Extensions.config_info
  -> ?site_dir:string list
  -> ?run:(app:string -> Ocsigen.Server.instruction)
  -> app:string
  -> (unit -> 'a)
  -> 'a
(** [init ~config_info ~site_dir ~run ~app f] is the result of [f ()], run as
    the initialisation of the Eliom module [app] of a site at [site_dir]
    (default [[]]) of a host of configuration [config_info] (default
    {!config_info}), as Ocsigen Server does for an application linked
    statically: by [run ~app], default [Eliom.App.run ~app ()], which then
    checks that all the services of the site are registered. As in the
    toplevel code of an Eliom module, the client values created by [f] are
    global.

    The exceptions raised by [f] or by the check are raised again.

    @raise Invalid_argument if [app] was already used. *)

val register :
   ( 'get
     , 'post
     , _
     , _
     , _
     , Eliom.Service.non_ext
     , Eliom.Service.reg
     , _
     , _
     , _
     , Eliom.Service.non_ocaml )
     Eliom.Service.t
  -> unit
(** [register service] registers an empty page as the handler of [service]. *)
