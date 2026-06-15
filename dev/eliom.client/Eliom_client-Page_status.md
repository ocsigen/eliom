
# Module `Eliom_client.Page_status`

```ocaml
type t = 
  | Generating
  | Active
  | Cached
  | Dead
```
a page can be in one of the following states:

- `Generating`: page is currently being generated and not yet instated as the active page
- `Active`: page is currently being displayed
- `Cached`: page is in the browser history with its DOM stashed in the cache
- `Dead`: page is in the browser history without its DOM being cached
```ocaml
val signal : unit -> t React.S.t
```
retrieves a react signal for the status of the current page; note that the \`current page' is not necessarily the page currently being displayed, but rather the page in whose context the current code is executed.

```ocaml
module Events : sig ... end
```
convenience functions for retrieving a react event for the current page that is triggered whenever it reaches the respective status

```ocaml
val onactive : 
  ?now:bool ->
  ?once:bool ->
  ?stop:unit React.E.t ->
  (unit -> unit) ->
  unit
```
`onactive` is convenience function that attaches a handler to `Events.active`, which behaves exactly like `fun f -> React.E.map f Events.active`. If `now` is `true` (default) and the page is currently active the function is also invoked right away. This is useful to ensure that the function is invoked also on server-generated pages which are active right from the start and thus have no transition to the active state. If `once` is `true` (`false` by default) the action is executed only once. By `stop` one can supply an event that will deinstall the handler when triggered.

Typical use cases for this function are processes that need to run continually while a page is being viewed. Such processes (including event listeners of `Dom_html.window`) are killed on a page change and not automatically restored with the DOM (contrary to event listeners attached to DOM elements).

```ocaml
val oncached : ?once:bool -> ?stop:unit React.E.t -> (unit -> unit) -> unit
```
```ocaml
val ondead : ?stop:unit React.E.t -> (unit -> unit) -> unit
```
```ocaml
val oninactive : ?once:bool -> ?stop:unit React.E.t -> (unit -> unit) -> unit
```
```ocaml
val while_active : 
  ?now:bool ->
  ?stop:unit React.E.t ->
  (unit -> unit Lwt.t) ->
  unit
```
`while_active` initiates an action as `onactive` but cancels it whenever the page is not active anymore.
