# Module `Eliom.Client_history`

Pages of the application, the navigation history, and the data associated to each state of the History API. Internal module.

### History states

```ocaml
type state = {
  template : string option;
  position : Mod_dom.position;
}
```
Data stored in the session storage for each state of the History API.

```ocaml
type state_id = {
  session_id : int;
  state_index : int; (* point in history *)
}
```
```ocaml
type saved_state = state_id * string
```
The value of `history.state`: the state id and the URL.

```ocaml
val saved_state_of_json : Deriving_Json_lexer.lexbuf -> saved_state
```
```ocaml
val saved_state_to_json : Buffer.t -> saved_state -> unit
```
```ocaml
val saved_state_json : saved_state Deriving_Json.t
```
```ocaml
val session_id : int
```
A random number identifying the current session of the browser tab.

```ocaml
val history_state : 
  state_id ->
  string ->
  Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t Js_of_ocaml.Js.Opt.t
```
`history_state id uri` is the `history.state` value of the page with state `id` at `uri`.

```ocaml
val get_state : state_id -> state
```
Reads the data of a state from the session storage. Raises `Not_found` if there is none.

```ocaml
val update_state : unit -> unit
```
Saves the template and the scroll position of the active page in the session storage.

### Pages

```ocaml
val section_page : Logs.src
```
```ocaml
module Page_status : sig ... end
```
See [`Client.Page_status`](./Eliom-Client-Page_status.md).

```ocaml
type page = {
  page_unique_id : int;
  mutable page_id : state_id;
  mutable url : string;
  page_status : Page_status.t React.S.t;
  mutable previous_page : int option;
  set_page_status : ?step:React.step -> Page_status.t -> unit;
  mutable dom : Js_of_ocaml.Dom_html.bodyElement Js_of_ocaml.Js.t option; (* The DOM of the page, when it is cached *)
  mutable reload_function : (unit -> unit -> Service.result Lwt.t) option;
}
```
```ocaml
val active_page : page ref
```
The page being displayed.

```ocaml
val set_active_page : page -> unit
```
Makes a page the active one, and retires the previous one.

```ocaml
val get_this_page : unit -> page
```
The page the running code is generating, or the active page outside of a page generation.

```ocaml
val with_new_page : 
  ?state_id:state_id ->
  ?old_page:page ->
  replace:bool ->
  unit ->
  (unit -> 'a) ->
  'a
```
`with_new_page ~replace () f` runs `f` in the context of a new page, which `get_this_page` returns. With `~replace:true`, the new page takes the state id of the active page.

```ocaml
val advance_page : unit -> unit
```
Makes the page being generated the active one, and adds it to the history if it is not already there.

### History

```ocaml
module History : sig ... end
```
