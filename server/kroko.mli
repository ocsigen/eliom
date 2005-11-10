(* Kroko
 * Copyright (C) 2005 Vincent Balat
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)


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

val counter : unit -> int

(** Type of http parameters *)
type http_params = {url_suffix: string;
		    full_url: string;
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
  fallback:('b, insideforml, 'c, 'a, page, page, public_url internal_url) url 
  -> ('b, insideforml, 'c, 'a, page, page, state_url internal_url) url

val register_url :
  url:('a, insideforml, 'b, 'c, 'd, 'e, 'f internal_url) url -> page:'c -> unit

val register_session_url :
    url:('a, insideforml, 'b, 'c, 'd, 'e, 'f internal_url) url -> page:'c -> unit

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
  page:'a -> ('b, insideforml, 'c, 'a, page, page, public_url internal_url) url

val register_new_session_url :
    fallback:('a, insideforml, 'b, 'c, page, page, public_url internal_url) url ->
    page:'c -> ('a, insideforml, 'b, 'c, page, page, state_url internal_url) url

val new_post_url :
  fallback:('a, 'b, 'c, 'd, 'e, 'f, public_url internal_url) url ->
  post_params:('h, 'd, 'j -> insideforml, 'k) parameters ->
  ('a, 'j, 'c, 'd, 'h, 'd, public_url internal_url) url

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
    url:('a, 'b -> 'c, 'd, 'e, 'f, 'e, 'g internal_url) url -> page:'f -> unit

val register_post_session_url :
    url:('a, 'b -> 'c, 'd, 'e, 'f, 'e, 'g internal_url) url -> page:'f -> unit

val register_new_post_url :
    fallback:('a, 'b, 'c, 'd, 'e, 'f, public_url internal_url) url ->
    post_params:('h, 'd, ('i -> 'j) -> insideforml, 'k) parameters ->
    page:'h -> ('a, 'i -> 'j, 'c, 'd, 'h, 'd, public_url internal_url) url

val register_new_post_session_url :
    fallback:('a, 'b, 'c, 'd, 'e, 'f, public_url internal_url) url ->
    post_params:('g, 'd, ('h -> 'i) -> insideforml, 'j) parameters ->
    page:'g -> ('a, 'h -> 'i, 'c, 'd, 'g, 'd, state_url internal_url) url


(* actions (new 10/05) *)
type ('a,'b) actionurl

val new_actionurl :
  params:('a, unit, 'b -> insideforml, 'c) parameters -> ('b, 'a) actionurl

val register_actionurl : actionurl:('a, 'b) actionurl -> action:'b -> unit

val register_session_actionurl :
  actionurl:('a, 'b) actionurl -> action:'b -> unit


val register_new_actionurl :
  params:('a, unit, 'b -> insideforml, 'c) parameters ->
  action:'a 
  -> ('b, 'a) actionurl

val register_new_session_actionurl :
  params:('a, unit, 'b -> insideforml, 'c) parameters ->
  action:'a 
  -> ('b, 'a) actionurl

(** static pages (new 10/05) *)
val register_new_static_directory :
    name:url_string ->
    location:string -> 
  (insideforml, insideforml, string -> formorlink, page, page, page, 
   public_url internal_url) url

val register_new_session_static_directory :
    name:url_string ->
    location:string -> 
  (insideforml, insideforml, string -> formorlink, page, page, page, 
   public_url internal_url) url

(** to close a session: *)
val close_session : unit -> unit

(** Functions to create web pages: *)

val link : string -> url_string -> 
  ('a, insideforml, 'c, 'd, 'e, 'f, 'g) url -> 'c

val css_link : url_string -> 
  ('a, insideforml, string -> Xhtmlpp.xhtmlcont, 'd, 'e, 'f, 'g) url -> 
  string -> Xhtmlpp.xhtmlcont

val js_link : url_string -> 
  ('a, insideforml, string -> Xhtmlpp.xhtmlcont, 'd, 'e, 'f, 'g) url -> 
  string -> Xhtmlpp.xhtmlcont

(** Link a registrated URL with the function that takes the url and
    names of the parameters, and creates a form for these parameters
*)
val form_get : string list -> 
  ('a, insideforml, 'c, 'd, 'e, 'f, 'g) url -> 'a -> form
val form_post : string list -> 
  ('a, 'b, 'c, 'd, 'e, 'f, 'g) url -> 'b -> 'c
val int_box : int name -> insideform
val hidden_int_box : int name -> int -> insideform
val string_box : string name -> insideform
val button : string -> insideform

val action_link : 
  ?reload:bool ->
  string -> 
  http_params -> 
  (insideforml, unit -> unit) actionurl -> formorlink

val action_form :
    ?reload:bool ->
      ?classe:string ->
	?id:string ->
	  http_params ->
	    ('a, 'b) actionurl ->
	      'a -> form

(** return a page from an url and parameters *)
val get_page :
  url_string * string * int option * (string * string) list *
  (string * string) list * string -> 
  Unix.sockaddr -> string option -> string option * page * string

val make_action :
  string -> (string * string) list ->
  url_string * string * int option * (string * string) list *
  (string * string) list * string -> 
  Unix.sockaddr -> string option -> string option * unit * string

(** loads a module in the server *)
val load_kroko_module : dir:url_string -> cmo:string -> unit


exception Static_File of string
exception Kroko_error_while_loading of string
exception Kroko_404
val state_param_name : string
val action_prefix : string
val action_name : string
val action_reload : string
