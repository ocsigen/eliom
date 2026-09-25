# Sessions and server side state

**Summary**

*Eliom references* allow to store data on server for one user (session data). The interface is very similar to regular OCaml references, with extra parameters `?scope` (session, client process, group of sessions) and `?persistent` (for keeping the values on hard disk). For example if you create an Eliom reference of *scope* session, its value will be different for each session (one session \= one browser process).

To create an Eliom reference containing initial value `None`:

```ocaml
let myref = Eliom.Reference.Volatile.eref ~scope None
```
Where `~scope` may be (amongst others):

- `Eliom.Common.default_session_scope` if you want to store server side data for one browser (one session),
- `Eliom.Common.default_process_scope` if you want to store server side data for one tab of a browser (one client process),
- `Eliom.Common.request_scope` if you want to store server side data during one request,
- `Eliom.Common.default_group_scope` if you want to store server side data for a group of sessions (for example all browsers belonging to the same user \-- see below).
Setting this reference, during a request:

```ocaml
...
Eliom.Reference.Volatile.Ext.set myref (Some "user data")
```
Getting the value of this reference (different for each user):

```ocaml
  Eliom.Reference.Volatile.Ext.get myref
```
Most of the time, what we want is to store data for one user, not for one browser instance. To do that, we use scope "group of sessions". A session is attached to a group of session by calling function [`Eliom.State.set_volatile_data_session_group`](./eliom.server/Eliom-State.md#val-set_volatile_data_session_group) (for non-persistent groups). The name of the group may be for example the user id. This will automatically create a session (and set a cookie) if needed.

Example:

```ocaml
let open_session login password =
  let%lwt b = check_password login password in
  if b then
    Eliom.State.set_volatile_data_session_group
      ~scope:Eliom.Common.default_session_scope
      (Int64.to_string (get_userid login))
  else
    ...
```

## Introduction

 Different scopes for data and services

The server-side state of an application refers to server-side data that can be shared by all clients, or that can be specific to a limited *scope*, such as:

- the site,
- a session,
- a group of sessions (for example all sessions of the same user),
- a client-side process (when programming an Eliom app), or
- the current request.
For a given scope, server-side state consists of:

- *services* registered with this specific scope, and
- data encapsulated within an *Eliom reference* created with this specific scope.
- Bus, channels, etc. (actually implemented using services and Eliom references)
States disappear either when the associated scope is explicitely discarded, or after a timeout.

 Scope and cookies

From a technical point of view, sessions and groups of sessions are implemented automatically within Eliom by asking the browser to send a session identifier in a cookie. Client-side processes also send an identifier with each request, using a kind of "client-side process-cookie".

It is possible to create different scopes of the same level if you want several states for the same application (advanced use). Each scope uses its own cookies, and you can discard data for a single scope.

 Secure cookies

States can be secure or not. Secure means that the state data or service will be associated to a secure cookie, that is a cookie that is sent by the browser only if the protocol is HTTPS. Use secure states to access confidential data, that you do not want to send through HTTP.

 Three kind of states

In the current implementation, because of a limitation in OCaml's serialization mechanism, there are three kinds of states (for each scope):

- volatile data states,
- volatile service states,
- persistent data states.
Volatile states will not survive after relaunching the server. There is no persistent service state. Be very careful if you use both persistent state data and service state, as your session may become inconsistent. (Use service states only for volatile services, like coservices with timeouts.)

We hope to simplify this when OCaml's serialization mechanism evolves. In the meantime, be very careful where you store your data. To avoid shutting down the server, note that it is possible to ask the server to dynamically load new versions of your site (see [command `reload`](./workflow-configuration.md#reload)).

## Scopes, cookies and sessions

### Basics

Eliom uses a notion of *scopes* to restrict the visibility of server-side data to a set of clients. Together with [Eliom references](./#eref), this is a very convenient way to implement a session mechanism for your Web site.

There are two categories of scopes:

- *Eliom scopes*, which are:
- [`Eliom.Common.global_scope`](./eliom.server/Eliom-Common.md#type-global_scope): for data shared by all clients,
- [`Eliom.Common.site_scope`](./eliom.server/Eliom-Common.md#type-site_scope): for data and services shared by all clients of an Eliom site, and
- [`Eliom.Common.request_scope`](./eliom.server/Eliom-Common.md#type-request_scope): for data specific to the current request.
- *User scopes*: used to restrict the accessibility of a piece of data or a service to a subset of the clients.
Within the user scopes, Eliom distinguishes three scopes that differ with respect to how Eliom associates clients and data.

- Data and dynamic services created with scope [`Eliom.Common.default_session_scope`](./eliom.server/Eliom-Common.md#val-default_session_scope) are only visible to the client belonging to a same session (all tabs of a single browser).
- Data and dynamic services created with scope [`Eliom.Common.default_group_scope`](./eliom.server/Eliom-Common.md#val-default_group_scope) are only visible to the clients whose sessions belong to the same session group. See section [Session groups](./#session_groups) for more information.
- Data and dynamic services created with scope [`Eliom.Common.default_process_scope`](./eliom.server/Eliom-Common.md#val-default_process_scope) are only visible to a specific instance of the Eliom application (i.e., a single tab, running the Eliom client process). See section [Eliom applications](./clientserver-applications.md) for more information.
User scopes are organised in a *hierarchy*: client processes belong to a session, and sessions belong to a group of sessions.

Data and dynamic services with a user scope can be discarded explicitely or via a timeout. See sections [Closing session](./#closing_sessions) and [Timeouts and session duration](./#timeouts) for more information.

If you want to handle multiple sessions for the same site \---~ e.g. several different data sessions that could be created and discarded independently~ \--- you can create new users scopes that will use different cookies. See section [Hierarchies of scopes](./#new_scope) for more information.

### Creating sessions and scopes

#### Automatic session creation

Eliom automatically creates a session \---~ and sets the corresponding cookie on the client~ \--- when you first modify an Eliom reference of scope [session](./eliom.server/Eliom-Common.md#val-default_session_scope), when you register a service with this scope or when you enter a session group.

By default, Eliom is using three cookies for sessions (and session groups):

- one for session services,
- one for volatile session data,
- one for persistent session data.
For client side processes, it uses the same three kinds of client side process cookies.

#### Hierarchies of scopes and multiple sessions (advanced use)

If you want to handle multiple sessions for the same site that can be created and discarded independently, you can create a new hierarchy of users scope that will use different cookies, with the function [`Eliom.Common.create_scope_hierarchy`](./eliom.server/Eliom-Common.md#val-create_scope_hierarchy).

```ocaml
let custom_session_hierarchy = Eliom.Common.create_scope_hierarchy "custom"
let custom_session = `Session custom_session_hierarchy
```
Then, the value `custom_session` can replace the usual [`Eliom.Common.default_session_scope`](./eliom.server/Eliom-Common.md#val-default_session_scope) for the `~scope` parameter of the functions [`Eliom.Reference.eref`](./eliom.server/Eliom-Reference.md#type-eref), [`Eliom.Registration.Html.register`](./eliom.server/Eliom-Registration-Html.md#val-register), [`Eliom.State.discard`](./eliom.server/Eliom-State.md#val-discard),~ ...

(Same for ``Client_process` or ``Session_group` scope levels).

The function [`Eliom.Common.create_scope_hierarchy`](./eliom.server/Eliom-Common.md#val-create_scope_hierarchy) will prevent you from creating two scope hierarchies with the same name.

##### Example of use:

A typical use of hierarchies is to have one hierarchy for connected users (the default hierarchy), and one independent from connection. Use the first hierarchy to save user-related content. It will be discarded when the user logs out. Use the second hierarchy to save data corresponding to a tab or browser, (the user being connected or not), for example because this data must be available before the user logs in.

### Closing sessions (and other states)

To discard a state, use the [`Eliom.State.discard`](./eliom.server/Eliom-State.md#val-discard) function. It will remove all server-side services and data (persistent or not) for the given scope. Used with `~scope:Eliom.Common.default_session_scope`, this will close a session. Used with `~scope:Eliom.Common.default_group_scope`, this will close all sessions in the group.

It is also possible to selectively discard only services, persistent data, or volatile data (see the [`Eliom.State`](./eliom.server/Eliom-State.md) module). But this may be periculous. Be very careful when doing this, as you are desynchronizing the three kinds of sessions.

The behaviour of [`Eliom.State.discard`](./eliom.server/Eliom-State.md#val-discard) on a session group is subject to discussion and may evolve in future versions.

Warnings:

- It is a good idea to close the session when a user tries to connect, even if it is already connected.
- You may also want to unset some request-scoped Eliom references when discarding a state,
- If your state data contains opened file descriptors, they won't be closed by OCaml's garbage collector. Close them yourself\! (for example using Gc.finalise).

### Timeouts and session duration

The default timeout for sessions is one hour. Sessions will be automatically closed after this amount of time of inactivity from the user. You can change the timeout for your whole site using the [`Eliom.State.set_global_volatile_state_timeout`](./eliom.server/Eliom-State.md#val-set_global_volatile_state_timeout).

It is also possible to change the default value for Eliom through the configuration file, like this:

```ocaml
<extension findlib-package="ocsigenserver.ext.eliom">
  <volatiletimeout value="7200"/>
</extension>
```
In the configuration files the value `"infinity"` means no timeout.

This default may be overriden by each site using [`Eliom.State.set_global_volatile_state_timeout`](./eliom.server/Eliom-State.md#val-set_global_volatile_state_timeout) (or, for all sites at once, `set_default_volatile_session_timeout` and `set_default_volatile_data_session_timeout`). If you want your user to be able to set the default in the configuration file for your site (between `<site>` and `</site>`), you must parse the configuration (using [`Eliom.Config.get_config`](./eliom.server/Eliom-Config.md#val-get_config) function). You can also change the timeout for a specific user only with the following functions: [`Eliom.State.set_volatile_data_state_timeout`](./eliom.server/Eliom-State.md#val-set_volatile_data_state_timeout). For more details, see the [`Eliom.State`](./eliom.server/Eliom-State.md) module's interface.

### Secure session

By default, data and services saved in a session are available to requests using both HTTP and HTTPS. If you want to keep some state in a *secure session* that is visible only to a client accessing with the HTTPS protocol, you may provide the optional parameter `~secure:true` when calling functions like [`Eliom.Reference.eref`](./eliom.server/Eliom-Reference.md#val-eref), [`Eliom.Registration.Html.register`](./eliom.server/Eliom-Registration-Html.md#val-register), etc.

The default can be set in the configuration file:

```ocaml
<extension findlib-package="eliom.server">
   <securecookies value="true" />
</extension>
```
This option can also be set inside a `<eliom>` tag, but be careful: in that case it will affect only the actions performed after setting this option, in the same site (and NOT the top-level instructions of the modules loaded before).

Secure sessions are using secure cookies, i.e., Ocsigen Server will ask the browsers to send the cookie only if the protocol is HTTPS. Thus it is not possible to access secure references and services if the user is using HTTP.

The server does not check the protocol currently used, neither to send or receive the cookies, which means that it will work even if your server is using HTTP behind a local proxy.

### Session groups

#### Sharing data between a group of sessions

Session group is a kind of scope that allows sharing data or services between a set of sessions, typically all sessions for given user. For example, using persistent Eliom references with scope [`Eliom.Common.default_group_scope`](./eliom.server/Eliom-Common.md#val-default_group_scope) is a convenient way to store data about a user without having to explicitly use an external database. (Persistent session group states are not discarded when all the sessions are closed).

A session group is identified by a name. The current session could be attached to a group of sessions using one of the following functions \---~ depending on the nature of the data you want to share. They take the session group name as parameter:

- [`Eliom.State.set_service_session_group`](./eliom.server/Eliom-State.md#val-set_service_session_group)
- [`Eliom.State.set_volatile_data_session_group`](./eliom.server/Eliom-State.md#val-set_volatile_data_session_group)
- [`Eliom.State.set_persistent_data_session_group`](./eliom.server/Eliom-State.md#val-set_persistent_data_session_group)
A session could be only attached to one group at a time, but it is possible to create multiple sessions for a same client attached to different groups, see section [Hierarchies of scopes](./#new_scope) for more information.

It's possible to fetch the current session group name of a session, if any, or to detach a session from a group. See the module [`Eliom.State`](./eliom.server/Eliom-State.md) for more information.

#### Limit the number of session within a group

The number of sessions in a group is limited. If all sessions are in a group you will prevent malicious users from opening too many sessions. If you do not use session groups, the number of sessions is limited by IP address, which can be a problem for example if the server is behind a reverse proxy or if many user are using the same NAT. That's why we always recommend to set the session group (usually it's the user name or id).

 Explain how to change the maximum number of sessions in a group.

#### Close all session of a group

Session groups allows the implementation of features like "close all sessions" for one user (even those opened on other browsers). Consider the following scenario: a user logs in from home using a "Remember me on this computer" feature, which sets an (almost) no-expiration cookie on his browser and session timeouts of infinity on the server. The user goes on vacation, and while logging from a cyber-café, he also sets the "Remember me" option. Back home he realises the mistake, and wishes to do a "global logout", i.e., closing all existing sessions associated with his user name.

For this kind of usage, it is highly recommended to set a group for each of the three kinds of states you use (services, volatile data and persistent data).

To close all sessions from a group, close the group.

See section [Closing sessions](./#closing_sessions) for more information.

## Eliom references

### Principles

*Eliom references* (*erefs*) are a kind of references with a (possibly) limited [scope](./#scopes). You declare a reference with an initial value and a scope. Then, when you change the associated value, it actually changes only for the scope you specified.

Eliom references can be persistent or not (that is: can survive after relaunching the server or not).

Eliom references are used for example:

- to store session data, server side data for a client process, or user data (scope: session, client process, session group),
- or to keep some information about the current request (scope: request), for example to give information to the service taking in charge the request after an action,
- to implement persistent references (scope: global)
- for caching functions ([`Eliom.Reference.eref_from_fun`](./eliom.server/Eliom-Reference.md#val-eref_from_fun)).
Non persistent global Eliom references are equivalent to regular OCaml references.

Eliom references are either created using the function [`Eliom.Reference.eref`](./eliom.server/Eliom-Reference.md#val-eref), that works like the usual Ocaml `ref` function, but with at least one additional scope parameter. Or they may be created by the function [`Eliom.Reference.eref_from_fun`](./eliom.server/Eliom-Reference.md#val-eref_from_fun): Its argument function is evaluated the first time the reference is accessed (through [`Eliom.Reference.get`](./eliom.server/Eliom-Reference.md#val-get)), within one scope or after the reference has been reset.

The [`Eliom.Reference`](./eliom.server/Eliom-Reference.md) module also defines functions to [get](./eliom.server/Eliom-Reference.md#val-get) the value, [set](./eliom.server/Eliom-Reference.md#val-set), [modify](./eliom.server/Eliom-Reference.md#val-modify) it (by applying a function to its content), and [unset](./eliom.server/Eliom-Reference.md#val-unset) it, this is reset to the initial value.

### Persistent references

Persistent references are Eliom references that survives after relaunching the server. They are implemented using the [`Ocsipersist`](./../ocsipersist/ocsipersist/Ocsipersist.md) module for which Ocsigenserver provides two implementations, one based on `SQLite`, the other one based on `DBM`.

Persistent references are created by adding the `~persistent` parameter to the [`Eliom.Reference.eref`](./eliom.server/Eliom-Reference.md#type-eref) function calls. Since Eliom 13, the value of this parameter is a pair `(name, codec)`: `name` is the name of the reference in the database, and `codec` is a `Deriving_Json` codec used to serialise the stored value:

```ocaml
type user_pref = { lang : string; theme : string } [@@deriving json]

let prefs =
  Eliom.Reference.eref
    ~persistent:("user_prefs", [%json: user_pref])
    ~scope:Eliom.Common.default_group_scope
    { lang = "en"; theme = "light" }
```
The type stored in the reference must be annotated with `[@@deriving json]` (or any equivalent way of producing a `Deriving_Json.t` value, e.g. `Deriving_Json.convert` for an abstract type, or one of the `Deriving_Json.Json_xxx` functors for parametric types).

Before Eliom 13, the value parameter of `~persistent` was just the table name (a `string`), and Eliom used OCaml's unsafe `Stdlib.Marshal` module to serialise persistent data, which led to the following limitations:

- On-disk values were unsafe to read across OCaml versions (Marshal gives no inter-version stability guarantee on certain types);
- It was not possible to serialise closures or services (as we are using dynamic linking);
- If you ever changed the type of serialised data, you had to also change the persistent reference name, or the server would crash while deserialising.
The Deriving\_Json migration removes all of these limitations: serialised data is human-readable, stable across OCaml versions, and the `[@@deriving json]` annotation forces the codec to match the stored type at compile time.

**Upgrading an existing site to Eliom 13 resets all persistent data.** The JSON tables are given new names (a `_json_` prefix for references and per-session/site state; new version suffixes for cookies and session groups), so the pre-13 Marshal tables are left orphaned and never read. Persistent references start again from their default value and users are logged out once. The old tables are kept on disk (they are not deleted), so their content can still be inspected or recovered by hand if needed. A stored value that cannot be deserialised is treated as an absent value rather than raising, so an incomplete migration never turns into a server error.

Beware that this reset concerns more than sessions: persistent references with the session-group scope are commonly keyed by user (the group name being the user id) and used to store *durable per-user data* such as preferences; those also restart from their default value, silently. If your application keeps data it cannot afford to lose in persistent references (group, site or global scope), export it *before* upgrading, with the Eliom 12 version of your application (for example into your SQL database), and re-import it through the new API afterwards. Reading the orphaned Marshal tables later remains possible in theory, but requires code built with the exact types the old application stored. Once the upgrade is validated, the old tables can be dropped from the storage backend to reclaim space.

#### Volatile references

The module [`Eliom.Reference.Volatile`](./eliom.server/Eliom-Reference-Volatile.md) allows the creation of non-persistent Eliom references, which can then be used through a non-Lwt interface.

As [`Eliom.Reference.Volatile.eref`](./eliom.server/Eliom-Reference-Volatile.md#type-eref) is a subtype of [`Eliom.Reference.eref`](./eliom.server/Eliom-Reference.md#type-eref), a volatile reference `eref` may be used as `(eref : _ Eliom.Reference.eref)` with the Lwt-interface of [`Eliom.Reference`](./eliom.server/Eliom-Reference.md) alike.

## Accessing other states

Sometimes, it is useful to access other states. For example if you want to send a notification to another user, you may want to find the communication channel registered for this user. It can probably be found as an Eliom reference in the group corresponding to this user.

Use module [`Eliom.State.Ext`](./eliom.server/Eliom-State-Ext.md) to get the state corresponding to a group name. Use [`Eliom.State.Ext.iter_sub_states`](./eliom.server/Eliom-State-Ext.md#val-iter_sub_states) to iterate on all sessions in a group, or on all client processes in a session.

Use [`Eliom.Reference.Ext`](./eliom.server/Eliom-Reference-Ext.md) to access Eliom references belonging to another state.

## Low-level cookies manipulation

Eliom references are used to store data on the server-side. It is also possible to ask the browser or the client-side process to record some piece of data and send it back to the server with each request.

This is implemented using the usual browser cookies for sessions \---~ and a simulation of browser cookies by Eliom client side processes~ \--- with the function [`Eliom.State.set_cookie`](./eliom.server/Eliom-State.md#val-set_cookie); the cookies sent by the client can be read back with [`Eliom.Request_info.get_cookies`](./eliom.server/Eliom-Request_info.md#val-get_cookies).
