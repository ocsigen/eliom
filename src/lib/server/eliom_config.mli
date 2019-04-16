(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2010 Vincent Balat
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

(** This module makes also possible get information from the configuration
file.

    It make also possible to add configuration options
    in configuration file for your Eliom modules.
    Use {!Eliom_config.parse_config} for that.

    Example:

{v
     <eliommodule ...>
        <myoption myattr="...">myvalue</myoption>
     </eliommodule>
v}


*)


(** The function [get_default_hostname ()]returns the hostname
    declared in the config file ([<host defaulthostname="...">]) or
    the default machine hostname.
    In that case, absolute URL will use that hostname. *)
val get_default_hostname : unit -> string

(** The function [get_default_port ()] returns the port number
    declared in the config file ([<host defaulthttpport="...">]) or
    80 if undeclared.
*)
val get_default_port : unit -> int

(** The function [get_default_sslport ()] returns the https port
    number declared in the config file ([<host
    defaulthttpsport="...">]) or 443 if undeclared.
*)
val get_default_sslport : unit -> int

(** The function [default_protocol_is_https ()] returns [true]
    if there is option ([<host
    defaultprotocol="https">])or false otherwise.
    In that case, absolute links will use https protocol even if
    the current request is http.
*)
val default_protocol_is_https : unit -> bool

(** The function [get_config_default_charset ()] returns the default
    charset for this site. *)
val get_config_default_charset : unit -> string

(** The provided value serves as a default value for the optional parameter
    [~xhr] in the functions [Eliom_registration.*.{a, get_form, post_form,
    lwt_get_form, lwt_post_form}] (cf. {!Eliom_registration.Html.a} et al.).
    This value can also be set in the
    {{:http://ocsigen.org/eliom/dev/manual/config#h5o-25}config file}.  *)
val set_default_links_xhr : ?override_configfile:bool -> bool -> unit

(**/**)
val get_default_links_xhr : unit -> bool
(**/**)

(** The function [get_config ()] returns the information of the
    configuration file concerning that site (between [<eliommodule>] and
    [</eliommodule>] or [<eliom>] and [</eliom>]).

    {e Warning: You must call that function during the initialisation of
    your module (not during a Lwt thread or a service)
    otherwise it will raise the exception
    {!Eliom_common.Eliom_site_information_not_available}.
    If you want to build a statically linkable module, you must call this
    function inside the initialisation function given to
    {!Eliom_service.register_eliom_module}.}
*)
val get_config : unit -> Xml.xml list

(** Process the configuration
    (same as the one returned by
    {% <<a_api module="Eliom_config" | val get_config>> %})
    by a given specification (cf. {% <<a_api project="ocsigenserver" | type Ocsigen_extensions.Configuration.element >> %}) *)
val parse_config : ?pcdata:(string -> unit) -> ?other_elements:(string -> (string * string) list -> Xml.xml list -> unit) -> Ocsigen_extensions.Configuration.element list -> unit

(** The function [get_config_info ()] returns the information
    concerning the current request from the configuration files
    (must be called during a request).
    The configuration may have been modified by previous Ocsigen server
    extensions.
*)
val get_config_info : unit -> Ocsigen_extensions.config_info

(** Same as [Ocsigen_config.get_debugmode].
    On client side, returns [false] for now. *)
val get_debugmode : unit -> bool

(**/**)
val get_config_info_sp :
  Eliom_common.server_params -> Ocsigen_extensions.config_info

val get_config_default_charset_sp :
  Eliom_common.server_params -> string
