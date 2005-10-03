(* Copyright Vincent Balat *)

(** Type of answers from modules (web pages) *)
type page = Xhtmlpp.xhtml

(** Type of formulars *)
type form = Xhtmlpp.xhtmlcont
type formorlink = Xhtmlpp.xhtmlcont
type insideform = Xhtmlpp.insideform
type insideforml = insideform list

(** Typed URLs *)
type public_url
type state_url
type 'a internal_url
type external_url
type ('a,'b,'c,'d,'e,'f,'g) url

type url_string = string list
(** Url matches the exact string
    Url_Prefix matches any string with this prefix
   (for ex to make the string "wiki/" activate all pages like "wiki/toto")
 *)
type url_activator = Url of url_string | Url_Prefix of url_string

(** Type of http parameters *)
type http_params = {url_suffix: string;
		    current_url: string list;
		    useragent: string;
		    ip: Unix.inet_addr;
		    get_params: (string * string) list;
		    post_params: (string * string) list}

(** Type for names of page parameters *)
type 'a name

(** Type for parameters of a web page *)
type ('a,'b,'c,'d) parameters

val _noparam : ('a, 'a, 'b -> 'b, formorlink) parameters
val _int :
    string ->
    (int -> 'a, 'a, (int name -> 'b) -> 'b, int -> formorlink) parameters
val _unit : (unit -> 'a, 'a, 'b -> 'b, formorlink) parameters
val _string :
  string ->
  (string -> 'a, 'a, (string name -> 'b) -> 'b, string -> formorlink) 
    parameters
val _user_type :
  (string -> 'c) -> ('c -> string) -> string ->
  ('c -> 'a, 'a, ('c name -> 'b) -> 'b, 'c -> formorlink) parameters
val _useragent :
  ('a, 'b, 'c, 'd) parameters -> (string -> 'a, 'b, 'c, 'd) parameters
val _ip :
  ('a, 'b, 'c, 'd) parameters -> (string -> 'a, 'b, 'c, 'd) parameters
val _current_url :
  ('a, 'b, 'c, 'd) parameters -> (string list -> 'a, 'b, 'c, 'd) parameters
val _url_suffix :
  ('a, 'b, 'c, 'd) parameters ->
  (string -> 'a, 'b, 'c, string -> 'd) parameters
val _http_params :
  ('a, 'b, 'c, 'd) parameters ->
  (http_params -> 'a, 'b, 'c, 'd) parameters
val ( ** ) :
    ('a, 'b, 'c -> 'd, 'e -> formorlink) parameters ->
    ('b, 'g, 'd -> 'h, 'i -> 'j) parameters ->
    ('a, 'g, 'c -> 'h, 'e -> 'i -> 'j) parameters

(** Register an url in the server with the associated action.
   register_url url t f will associate the url url to the function f.
   f is usually a function that takes any number of parameters of
   any types and that creates a page.
   t is a function that will translate f to a function from http_params
   to page. t can be written using _unit _int (++) etc.
 *)
val new_url :
    name:url_activator ->
    params:('a, page, 'b -> insideforml, 'c) parameters ->
    ('b, insideforml, 'c, 'a, page, page, public_url internal_url) url

val new_external_url :
  name:url_activator ->
  params:('a, page, 'b -> insideforml, 'c) parameters ->
  ('b, insideforml, 'c, 'a, page, page, external_url) url

val new_state_url :
  fallback:('b, insideforml, 'c, 'a, page, page, public_url internal_url) url ->
  ('b, insideforml, 'c, 'a, page, page, state_url internal_url) url

val register_url :
  url:('a, insideforml, 'b, 'c, 'd, 'e, 'f internal_url) url -> action:'c -> unit

val register_session_url :
    url:('a, insideforml, 'b, 'c, 'd, 'e, 'f internal_url) url -> action:'c -> unit

(**  Create a new URL and register it in the server with the associated action.
   [register_new_url url t f] will associate the url [url] to the function [f].
   [f] is usually a function that takes any number of parameters of
   any types and that creates a page.
   [t] is a function that will translate f to a function from http_params
   to page. [t] can be written using [_unit _int (++)] etc.
*)
val register_new_url :
  name:url_activator ->
  params:('a, page, 'b -> insideforml, 'c) parameters ->
  action:'a -> ('b, insideforml, 'c, 'a, page, page, public_url internal_url) url

val register_new_session_url :
    fallback:('a, insideforml, 'b, 'c, page, page, public_url internal_url) url ->
    action:'c -> ('a, insideforml, 'b, 'c, page, page, state_url internal_url) url

val new_post_url :
  fallback:('a, 'b, 'c, 'd, 'e, 'f, public_url internal_url) url ->
  post_params:('h, 'i, 'j -> insideforml, 'k) parameters ->
  ('a, 'j, 'c, 'd, 'h, 'i, public_url internal_url) url

val new_external_post_url :
  name:url_activator ->
  params:('a, page, 'b -> insideforml, 'c) parameters ->
  post_params:('h, 'i, 'j -> insideforml, 'k) parameters ->
  ('b, 'j, 'c, 'a, 'h, 'i, external_url) url

val new_post_state_url :
  fallback:('a, 'b, 'c, 'd, 'e, 'f, public_url internal_url) url ->
  post_params:('g, 'h, 'i -> insideforml, 'j) parameters ->
  ('a, 'i, 'c, 'd, 'g, 'h, state_url internal_url) url

val register_post_url :
    url:('a, 'b -> 'c, 'd, 'e, 'f, 'e, 'g internal_url) url -> action:'f -> unit

val register_post_session_url :
    url:('a, 'b -> 'c, 'd, 'e, 'f, 'e, 'g internal_url) url -> action:'f -> unit

val register_new_post_url :
    fallback:('a, 'b, 'c, 'd, 'e, 'f, public_url internal_url) url ->
    post_params:('h, 'd, ('i -> 'j) -> insideforml, 'k) parameters ->
    action:'h -> ('a, 'i -> 'j, 'c, 'd, 'h, 'd, public_url internal_url) url

val register_new_post_session_url :
    fallback:('a, 'b, 'c, 'd, 'e, 'f, public_url internal_url) url ->
    post_params:('g, 'd, ('h -> 'i) -> insideforml, 'j) parameters ->
    action:'g -> ('a, 'h -> 'i, 'c, 'd, 'g, 'd, state_url internal_url) url

(** to close a session: *)
val close_session : unit -> unit

(** Functions to create web pages: *)

val link : string -> string list -> 
  ('a, insideforml, 'c, 'd, 'e, 'f, 'g) url -> 'c

(** Link a registrated URL with the function that takes the url and
    names of the parameters, and creates a form for these parameters
*)
val form_get : string list -> 
  ('a, insideforml, 'c, 'd, 'e, 'f, 'g) url -> 'a -> form
val form_post : string list -> 
  ('a, 'b, 'c, 'd, 'e, 'f, 'g) url -> 'b -> 'c
val int_box : int name -> insideform
val string_box : string name -> insideform
val button : string -> insideform


(** return a page from an url and parameters *)
val get_page :
  url_string * int option * (string * string) list *
  (string * string) list * string -> 
  Unix.sockaddr -> string option -> string option * page

(** loads a module in the server *)
val load_aaaaa_module : dir:url_string -> cmo:string -> unit


exception Aaaaa_error_while_loading of string
val state_param_name : string
