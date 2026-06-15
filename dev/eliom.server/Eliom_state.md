
# Module `Eliom_state`

Storing server-side values for your applications or sessions.

**Please read the [Eliom manual](./../server-state.md) before this page to learn how server side state works.**

` <<outline| <<header| **Table of contents** >> >>`


### Managing the state of an application


#### Closing sessions, removing state data and services

```ocaml
val discard : 
  scope:[< Eliom_common.user_scope | Eliom_common.request_scope ] ->
  ?secure:bool ->
  unit ->
  unit Lwt.t
```
Delete server-side (volatile and persistent) state data and services for a session, a group of sessions, a client process or a request.

Use that function to close a session (using scope `Eliom_common.default_session_scope`).

Closing a group of sessions will close all sessions in the group.

By default will remove both secure and unsecure data and services, unless `~secure` is present.

*Warning: you may also want to unset some request-scoped Eliom references when discarding a state.*

```ocaml
val discard_all_scopes : ?secure:bool -> unit -> unit Lwt.t
```
```ocaml
val discard_data : 
  ?persistent:bool ->
  scope:[< Eliom_common.user_scope | Eliom_common.request_scope ] ->
  ?secure:bool ->
  unit ->
  unit Lwt.t
```
Remove current state data.

If the optional parameter `?persistent` is not present, will remove both volatile and persistent data. Otherwise only volatile or persistent data.

```ocaml
val discard_services : 
  scope:[< Eliom_common.user_scope ] ->
  ?secure:bool ->
  unit ->
  unit
```
Remove all services registered for the given scope (the default being ``Session`).


#### State status

The following functions return the current state of the state for a given scope:

- `Alive_state` means that data has been recorded for this scope
- `Empty_state` means that there is no data for this scope
- `Expired_state` means that data for this scope has been removed because the timeout has been reached.
The default scope is ``Session`.

```ocaml
type state_status = 
  | Alive_state
  | Empty_state
  | Expired_state
```
```ocaml
val service_state_status : 
  scope:[< Eliom_common.user_scope ] ->
  ?secure:bool ->
  unit ->
  state_status
```
```ocaml
val volatile_data_state_status : 
  scope:[< Eliom_common.user_scope ] ->
  ?secure:bool ->
  unit ->
  state_status
```
```ocaml
val persistent_data_state_status : 
  scope:[< Eliom_common.user_scope ] ->
  ?secure:bool ->
  unit ->
  state_status Lwt.t
```

#### User cookies

If you want to store a client-side state, and ask the browser to send it back with each request, you can set manually your own cookies. Usual cookies correspond to scope ``Session` (that is, one browser). The browser send them with each request to the same Web site. But Eliom also implements client-side process cookies (scope ``Client_process`), that behave in the same way, but for one instance of the client-side Eliom program (if there is one).

Cookies can be limited to a subsite using the `?path` optional parameter. This path is relative to the main path of your Web site. (It is not possible to set a cookie for a subsite larger than your current Web site).

Cookies can have an expiration date, specified (in seconds since the 1st of January 1970\) in the optional parameter `?exp` (see how to set default expiration dates below).

Secure cookies are sent by the browser only with HTTPS (default: `false`).

```ocaml
val set_cookie : 
  ?cookie_level:Eliom_common.cookie_level ->
  ?path:string list ->
  ?exp:float ->
  ?secure:bool ->
  name:string ->
  value:string ->
  unit ->
  unit
```
Ask the browser to record a cookie.

```ocaml
val unset_cookie : 
  ?cookie_level:Eliom_common.cookie_level ->
  ?path:string list ->
  name:string ->
  unit ->
  unit
```
Ask the browser to remove a cookie.


### Session groups

If your Web site has users, it is a good idea to group together all the sessions for one user. Otherwise, you may want to group sessions according to another criterion.

Session groups may be used for example to limit the number of sessions one user can open at the same time, or to implement a "close all your sessions" feature. Usually, the group is the user name.


#### Putting a session in a group, removing a session from a group

```ocaml
val set_service_session_group : 
  ?set_max:int ->
  ?scope:Eliom_common.session_scope ->
  ?secure:bool ->
  string ->
  unit
```
sets the group to which belong the service session.

If the optional `?set_max` parameter is present, also sets the maximum number of sessions in the group. Default: follow current configuration for the group or default configuration if the group does not exist.

If `~secure` is true, it will affect the secure session (secure cookies), otherwise (default), the unsecure one (behavior change in Eliom 4\).

```ocaml
val unset_service_session_group : 
  ?set_max:int ->
  ?scope:Eliom_common.session_scope ->
  ?secure:bool ->
  unit ->
  unit
```
Remove the session from its group. Will not close the session if it contains data.

```ocaml
val get_service_session_group : 
  ?scope:Eliom_common.session_scope ->
  ?secure:bool ->
  unit ->
  string option
```
returns the group to which belong the service session. If the session does not belong to any group, or if no session is opened, return `None`.

```ocaml
val get_service_session_group_size : 
  ?scope:Eliom_common.session_scope ->
  ?secure:bool ->
  unit ->
  int option
```
returns the number of sessions in the group. If he session does not belong to any group or if no session is opened, returns `None`

```ocaml
val set_volatile_data_session_group : 
  ?set_max:int ->
  ?scope:Eliom_common.session_scope ->
  ?secure:bool ->
  string ->
  unit
```
sets the group to which belong the volatile data session.

If the optional `?set_max` parameter is present, also sets the maximum number of sessions in the group. Default: follow current configuration for the group or default configuration if the group does not exist.

```ocaml
val unset_volatile_data_session_group : 
  ?set_max:int ->
  ?scope:Eliom_common.session_scope ->
  ?secure:bool ->
  unit ->
  unit
```
Remove the session from its group. Will not close the session if it contains data.

```ocaml
val get_volatile_data_session_group : 
  ?scope:Eliom_common.session_scope ->
  ?secure:bool ->
  unit ->
  string option
```
returns the group to which belong the data session. If the session does not belong to any group, or if no session is opened, return `None`.

```ocaml
val get_volatile_data_session_group_size : 
  ?scope:Eliom_common.session_scope ->
  ?secure:bool ->
  unit ->
  int option
```
returns the number of sessions in the group. If he session does not belong to any group or if no session is opened, returns `None`

```ocaml
val set_persistent_data_session_group : 
  ?set_max:int option ->
  ?scope:Eliom_common.session_scope ->
  ?secure:bool ->
  string ->
  unit Lwt.t
```
sets the group to which belong the persistent session.

If the optional `?set_max` parameter is present, also sets the maximum number of sessions in the group. When `~set_max:None` is present, the number of session is unlimited. Default: follow current configuration for the group or default configuration if the group does not exist.

```ocaml
val unset_persistent_data_session_group : 
  ?scope:Eliom_common.session_scope ->
  ?secure:bool ->
  unit ->
  unit Lwt.t
```
Remove the session from its group. Will not close the session if it contains data.

```ocaml
val get_persistent_data_session_group : 
  ?scope:Eliom_common.session_scope ->
  ?secure:bool ->
  unit ->
  string option Lwt.t
```
returns the group to which belong the persistent session. If the session does not belong to any group, or if no session is opened, return `None`.


#### Maximum group size

The following functions of this section set the maximum number of sessions in a session group, for the different kinds of session. This won't modify existing groups. That value will be used only as default value if you do not specify the optional parameter `?set_max` of function [`Eliom_state.set_volatile_data_session_group`](./#val-set_volatile_data_session_group).

If there is no group, the number of sessions is limitated by sub network (which can be a problem for example if the server is behind a reverse proxy). It is highly recommended to use session groups\!

- Default number of sessions in a group: 5
- Default number of sessions in a sub network: 1000000
- Default IPV4 sub network: /16
- Default IPV6 sub network: /56
These default can be changed from configuration file and/or using these functions.

If `~override_configfile` is `true` (default (`false`), then the function will set the value even if it has been modified in the configuration file. It means that by default, these functions have no effect if there is a value in the configuration file. This gives the ability to override the values chosen by the module in the configuration file. Use `~override_configfile:true` for example if your Eliom module wants to change the values afterwards (for example in the site configuration Web interface).

```ocaml
val set_default_max_service_sessions_per_group : 
  ?override_configfile:bool ->
  int ->
  unit
```
Sets the maximum number of service sessions in a session group (see above).

```ocaml
val set_default_max_volatile_data_sessions_per_group : 
  ?override_configfile:bool ->
  int ->
  unit
```
Sets the maximum number of volatile data sessions in a session group (see above).

```ocaml
val set_default_max_persistent_data_sessions_per_group : 
  ?override_configfile:bool ->
  int option ->
  unit
```
Sets the maximum number of persistent data sessions in a session group (see above). `None` means "no limitation".

```ocaml
val set_default_max_volatile_sessions_per_group : 
  ?override_configfile:bool ->
  int ->
  unit
```
Sets the maximum number of volatile sessions (data and service) in a session group (see above).

```ocaml
val set_default_max_service_sessions_per_subnet : 
  ?override_configfile:bool ->
  int ->
  unit
```
Sets the maximum number of service sessions in a subnet (see above).

```ocaml
val set_default_max_volatile_data_sessions_per_subnet : 
  ?override_configfile:bool ->
  int ->
  unit
```
Sets the maximum number of volatile data sessions in a subnet (see above).

```ocaml
val set_default_max_volatile_sessions_per_subnet : 
  ?override_configfile:bool ->
  int ->
  unit
```
Sets the maximum number of volatile sessions (data and service) in a subnet (see above).

```ocaml
val set_default_max_service_tab_sessions_per_group : 
  ?override_configfile:bool ->
  int ->
  unit
```
Sets the maximum number of tab service sessions in a session group (see above).

```ocaml
val set_default_max_volatile_data_tab_sessions_per_group : 
  ?override_configfile:bool ->
  int ->
  unit
```
Sets the maximum number of volatile data tab sessions in a session group (see above).

```ocaml
val set_default_max_persistent_data_tab_sessions_per_group : 
  ?override_configfile:bool ->
  int option ->
  unit
```
Sets the maximum number of persistent data tab sessions in a session group (see above).

```ocaml
val set_default_max_volatile_tab_sessions_per_group : 
  ?override_configfile:bool ->
  int ->
  unit
```
Sets the maximum number of volatile tab sessions (data and service) in a session group (see above).

```ocaml
val set_ipv4_subnet_mask : ?override_configfile:bool -> int -> unit
```
Sets the mask for subnet (IPV4).

```ocaml
val set_ipv6_subnet_mask : ?override_configfile:bool -> int -> unit
```
Sets the mask for subnet (IPV6).

```ocaml
val set_max_service_states_for_group_or_subnet : 
  scope:Eliom_common.user_scope ->
  ?secure:bool ->
  int ->
  unit
```
Sets the maximum number of service sessions in the current session group (or for the client sub network, if there is no group).

```ocaml
val set_max_volatile_data_states_for_group_or_subnet : 
  scope:Eliom_common.user_scope ->
  ?secure:bool ->
  int ->
  unit
```
Sets the maximum number of volatile data sessions in the current session group (or for the client sub network, if there is no group).

```ocaml
val set_max_volatile_states_for_group_or_subnet : 
  scope:Eliom_common.user_scope ->
  ?secure:bool ->
  int ->
  unit
```
Sets the maximum number of volatile sessions (both data and service sessions) in the current group (or for the client sub network, if there is no group).


### Expiration of cookies and timeouts


#### Cookie expiration

The functions in this section ask the browser to set the state cookie expiration date, for the different kinds of session, in seconds, since the 1st of January 1970\. `None` means the cookie will expire when the browser is closed. There is no way to set cookies for an infinite time on browsers. By default, Eliom sets default expiration date to 10 years after opening the session.

By default, it will affect regular browser cookies (sessions). But if you set `~cookie_level:`Client_process`, it will only affect the client-side Eliom process (if there is one), which simulates some kind of "tab cookies".

```ocaml
val set_service_cookie_exp_date : 
  cookie_scope:Eliom_common.cookie_scope ->
  ?secure:bool ->
  float option ->
  unit
```
Sets the cookie expiration date for the current service state (see above).

```ocaml
val set_volatile_data_cookie_exp_date : 
  cookie_scope:Eliom_common.cookie_scope ->
  ?secure:bool ->
  float option ->
  unit
```
Sets the cookie expiration date for the current data state (see above).

```ocaml
val set_persistent_data_cookie_exp_date : 
  cookie_scope:Eliom_common.cookie_scope ->
  ?secure:bool ->
  float option ->
  unit Lwt.t
```
Sets the cookie expiration date for the persistent state (see above).


#### Global configuration of state timeouts

The following functions set the timeout for states, for the different kinds of states. States will be closed after this amount of time of inactivity from the user. `None` means no timeout.

The optional parameter `?recompute_expdates` is `false` by default. If you set it to `true`, the expiration dates for all states in the table will be recomputed with the new timeout. That is, the difference between the new timeout and the old one will be added to their expiration dates (asynchronously, by another Lwt thread, as this can take a long time). States whose timeout has been set individually with functions like [`Eliom_state.set_volatile_data_state_timeout`](./#val-set_volatile_data_state_timeout) won't be affected.

If `~scope_hierarchy` is not present, it is the default for all scope hierarchies, and in that case `recompute_expdates` is ignored. `~scope_hierarchy:None` means the default scope hierarchy.

If `~override_configfile` is `true` (default (`false`), then the function will set the timeout even if it has been modified in the configuration file. It means that by default, these functions have no effect if there is a value in the configuration file. This gives the ability to override the values chosen by the module in the configuration file. Use `~override_configfile:true` for example if your Eliom module wants to change the values afterwards (for example in the site configuration Web interface).

```ocaml
val set_global_volatile_state_timeout : 
  cookie_scope:[< Eliom_common.cookie_scope ] ->
  ?secure:bool ->
  ?recompute_expdates:bool ->
  ?override_configfile:bool ->
  float option ->
  unit
```
Sets the (server side) timeout for volatile (= "in memory") sessions (both service session and volatile data session).

```ocaml
val set_global_service_state_timeout : 
  cookie_scope:[< Eliom_common.cookie_scope ] ->
  ?secure:bool ->
  ?recompute_expdates:bool ->
  ?override_configfile:bool ->
  float option ->
  unit
```
Sets the (server side) timeout for service states.

```ocaml
val set_default_global_service_state_timeout : 
  cookie_level:[< Eliom_common.cookie_level ] ->
  ?override_configfile:bool ->
  float option ->
  unit
```
```ocaml
val set_global_volatile_data_state_timeout : 
  cookie_scope:[< Eliom_common.cookie_scope ] ->
  ?secure:bool ->
  ?recompute_expdates:bool ->
  ?override_configfile:bool ->
  float option ->
  unit
```
Sets the (server side) timeout for volatile (= "in memory") data states.

```ocaml
val set_default_global_volatile_data_state_timeout : 
  cookie_level:[< Eliom_common.cookie_level ] ->
  ?override_configfile:bool ->
  float option ->
  unit
```
```ocaml
val set_global_persistent_data_state_timeout : 
  cookie_scope:[< Eliom_common.cookie_scope ] ->
  ?secure:bool ->
  ?recompute_expdates:bool ->
  ?override_configfile:bool ->
  float option ->
  unit
```
Sets the (server side) timeout for persistent states.

```ocaml
val set_default_global_persistent_data_state_timeout : 
  cookie_level:[< Eliom_common.cookie_level ] ->
  ?override_configfile:bool ->
  float option ->
  unit
```
```ocaml
val get_global_service_state_timeout : 
  ?secure:bool ->
  cookie_scope:[< Eliom_common.cookie_scope ] ->
  unit ->
  float option
```
Returns the (server side) timeout for service states.

```ocaml
val get_global_volatile_data_state_timeout : 
  ?secure:bool ->
  cookie_scope:[< Eliom_common.cookie_scope ] ->
  unit ->
  float option
```
Returns the (server side) timeout for "volatile data" states.

```ocaml
val get_global_persistent_data_state_timeout : 
  ?secure:bool ->
  cookie_scope:[< Eliom_common.cookie_scope ] ->
  unit ->
  float option
```
Returns the (server side) timeout for persistent states.


#### Personalizing timeouts for current state

```ocaml
val set_service_state_timeout : 
  cookie_scope:Eliom_common.cookie_scope ->
  ?secure:bool ->
  float option ->
  unit
```
sets the timeout for service state (server side) for current user, in seconds. `None` \= no timeout

```ocaml
val unset_service_state_timeout : 
  cookie_scope:[< Eliom_common.cookie_scope ] ->
  ?secure:bool ->
  unit ->
  unit
```
remove the service state timeout for current user (and turn back to the default).

```ocaml
val get_service_state_timeout : 
  cookie_scope:[< Eliom_common.cookie_scope ] ->
  ?secure:bool ->
  unit ->
  float option
```
returns the timeout for current service state. `None` \= no timeout

```ocaml
val set_volatile_data_state_timeout : 
  cookie_scope:[< Eliom_common.cookie_scope ] ->
  ?secure:bool ->
  float option ->
  unit
```
sets the (server side) timeout for volatile data state for current user, in seconds. `None` \= no timeout

```ocaml
val unset_volatile_data_state_timeout : 
  cookie_scope:[< Eliom_common.cookie_scope ] ->
  ?secure:bool ->
  unit ->
  unit
```
remove the "volatile data" state timeout for current user (and turn back to the default).

```ocaml
val get_volatile_data_state_timeout : 
  cookie_scope:[< Eliom_common.cookie_scope ] ->
  ?secure:bool ->
  unit ->
  float option
```
returns the timeout for current volatile data state. `None` \= no timeout

```ocaml
val set_persistent_data_state_timeout : 
  cookie_scope:[< Eliom_common.cookie_scope ] ->
  ?secure:bool ->
  float option ->
  unit Lwt.t
```
sets the (server side) timeout for persistent state for current user, in seconds. `None` \= no timeout

```ocaml
val unset_persistent_data_state_timeout : 
  cookie_scope:[< Eliom_common.cookie_scope ] ->
  ?secure:bool ->
  unit ->
  unit Lwt.t
```
remove the persistent state timeout for current user (and turn back to the default).

```ocaml
val get_persistent_data_state_timeout : 
  cookie_scope:[< Eliom_common.cookie_scope ] ->
  ?secure:bool ->
  unit ->
  float option Lwt.t
```
returns the persistent state timeout for current user. `None` \= no timeout


### Administrating server side state

*Warning: Most these functions must be called when the site information is available, that is, either during a request or during the initialisation phase of the site. Otherwise, it will raise the exception [`Eliom_common.Eliom_site_information_not_available`](./Eliom_common.md#exception-Eliom_site_information_not_available). If you are using static linking, you must delay the call to this function until the configuration file is read, using [`Eliom_service.register_eliom_module`](./Eliom_service.md#val-register_eliom_module). Otherwise you will also get this exception.*

```ocaml
type 'a volatile_table
```
The type of (volatile) state data tables.

```ocaml
type 'a persistent_table
```
The type of persistent state data tables.

```ocaml
val discard_everything : unit -> unit Lwt.t
```
Discard all services and persistent and volatile data for every scopes.

```ocaml
val discard_all : 
  scope:Eliom_common.user_scope ->
  ?secure:bool ->
  unit ->
  unit Lwt.t
```
Discard all services and persistent and volatile data for one scope.

```ocaml
val discard_all_data : 
  ?persistent:bool ->
  scope:Eliom_common.user_scope ->
  ?secure:bool ->
  unit ->
  unit Lwt.t
```
Discard server side data for all clients, for the given scope.

If the optional parameter `?persistent` is not present, both the persistent and volatile data will be removed.

```ocaml
val discard_all_services : 
  scope:Eliom_common.user_scope ->
  ?secure:bool ->
  unit ->
  unit Lwt.t
```
Remove all services registered for clients for the given scope.

```ocaml
module Ext : sig ... end
```