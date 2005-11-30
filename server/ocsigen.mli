(* Ocsigen
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

open Xhtml

(** Type of answers from modules (web pages) *)
type page = Xhtml.xhtml t

(** Type of formulars *)
type xhformcontl = xhformcont t list

(** Typed URLs *)
type public_url
type state_url
type 'a internal_url
type external_url
type (-'a, -'b, +'c, +'d, +'e, +'f, +'g, -'h, -'i, +'j, 'k) url


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
type ('a,'b,'c,'dalink, 'dform, 'dimg, 'dheadlink, 'dscript) parameters

val _noparam : 
    ('a, 'a, 'b -> 'b, [>xhalink] t, [>xhform] t, [>xhimg] t, [>xhheadlink] t, [>xhscript] t) parameters
val _int :
    string ->
    (int -> 'a, 'a, (int name -> 'b) -> 'b, 
      int -> [>xhalink] t, int -> [>xhform] t, int -> [>xhimg] t, int -> [>xhheadlink] t, int -> [>xhscript] t) parameters
val _unit : (unit -> 'a, 'a, 'b -> 'b, [>xhalink] t, [>xhform] t, [>xhimg] t, [>xhheadlink] t, [>xhscript] t) parameters
val _string :
  string ->
  (string -> 'a, 'a, (string name -> 'b) -> 'b, 
     string -> [>xhalink] t, string -> [>xhform] t, string -> [>xhimg] t, string -> [>xhheadlink] t, string -> [>xhscript] t) parameters
val _user_type :
  (string -> 'c) -> ('c -> string) -> string ->
  ('c -> 'a, 'a, ('c name -> 'b) -> 'b, 
    'c -> [>xhalink] t, 'c -> [>xhform] t, 'c -> [>xhimg] t, 'c -> [>xhheadlink] t, 'c -> [>xhscript] t) parameters
val _useragent :
  ('a, 'b, 'c, 'dalink, 'dform, 'dimg, 'dheadlink, 'dscript) parameters -> 
    (string -> 'a, 'b, 'c,  'dalink, 'dform, 'dimg, 'dheadlink, 'dscript) parameters
val _ip :
  ('a, 'b, 'c, 'dalink, 'dform, 'dimg, 'dheadlink, 'dscript) parameters -> 
    (string -> 'a, 'b, 'c, 'dalink, 'dform, 'dimg, 'dheadlink, 'dscript) parameters
val _current_url :
  ('a, 'b, 'c,  'dalink, 'dform, 'dimg, 'dheadlink, 'dscript) parameters -> 
    (string list -> 'a, 'b, 'c,  'dalink, 'dform, 'dimg, 'dheadlink, 'dscript) parameters
val _url_suffix :
    ('a, 'b, 'c,  'dalink, 'dform, 'dimg, 'dheadlink, 'dscript) parameters ->
      (string -> 'a, 'b, 'c, 
	string -> 'dalink, string -> 'dform, string -> 'dimg, string -> 'dheadlink, string -> 'dscript) parameters
val _http_params :
  ('a, 'b, 'c, 'dalink, 'dform, 'dimg, 'dheadlink, 'dscript) parameters ->
  (http_params -> 'a, 'b, 'c, 'dalink, 'dform, 'dimg, 'dheadlink, 'dscript) parameters
val ( ** ) :
    ('a, 'b, 'c -> 'd, 'e -> xhalink t, 'e -> xhform t,  'e -> xhimg t,
      'e -> xhheadlink t, 'e -> xhscript t) parameters ->
      ('b, 'g, 'd -> 'h,  'i -> 'jalink, 'i -> 'jform, 'i -> 'jimg, 'i -> 'jheadlink, 'i -> 'jscript) parameters ->
	('a, 'g, 'c -> 'h, 'e -> 'i -> 'jalink, 'e -> 'i -> 'jform, 'e -> 'i -> 'jimg, 'e -> 'i -> 'jheadlink, 'e -> 'i -> 'jscript) parameters


(** Register an url in the server with the associated action.
   register_url url t f will associate the url url to the function f.
   f is usually a function that takes any number of parameters of
   any types and that creates a page.
   t is a function that will translate f to a function from http_params
   to page. t can be written using _unit _int (++) etc.
 *)
val new_url :
    name:url_activator ->
    params:('a, page, 'b -> xhformcontl, 
      'calink, 'cform, 'cimg, 'cheadlink, 'cscript) parameters ->
    ('b, xhformcontl, 'calink,'cform,'cimg,'cheadlink,'cscript, 'a, 
      page, page, public_url internal_url) url

val new_external_url :
  name:url_activator ->
  params:('a, page, 'b -> xhformcontl, 
    'calink, 'cform, 'cimg, 'cheadlink, 'cscript) parameters ->
  ('b, xhformcontl, 'calink,'cform,'cimg,'cheadlink,'cscript, 'a, page, page, external_url) url

val new_state_url :
  fallback:('b, xhformcontl, 'calink,'cform,'cimg,'cheadlink,'cscript, 'a, page, page, public_url internal_url) url 
  -> ('b, xhformcontl, 'calink,'cform,'cimg,'cheadlink,'cscript, 'a, page, page, state_url internal_url) url

val register_url :
  url:('a, xhformcontl, 'calink,'cform,'cimg,'cheadlink,'cscript, 'c, 'd, 'e, 'f internal_url) url -> page:'c -> unit

val register_session_url :
    url:('a, xhformcontl, 'calink,'cform,'cimg,'cheadlink,'cscript, 'c, 'd, 'e, 'f internal_url) url -> page:'c -> unit

(**  Create a new URL and register it in the server with the associated action.
   [register_new_url url t f] will associate the url [url] to the function [f].
   [f] is usually a function that takes any number of parameters of
   any types and that creates a page.
   [t] is a function that will translate f to a function from http_params
   to page. [t] can be written using [_unit _int (++)] etc.
*)
val register_new_url :
  name:url_activator ->
  params:('a, page, 'b -> xhformcontl, 
    'calink, 'cform, 'cimg, 'cheadlink, 'cscript) parameters ->
  page:'a -> ('b, xhformcontl, 'calink,'cform,'cimg,'cheadlink,'cscript, 'a, 
	      page, page, public_url internal_url) url

val register_new_session_url :
    fallback:('a, xhformcontl, 'calink,'cform,'cimg,'cheadlink,'cscript, 'c, page, page, public_url internal_url) url ->
    page:'c -> ('a, xhformcontl, 'calink,'cform,'cimg,'cheadlink,'cscript, 'c, page, page, state_url internal_url) url

val new_post_url :
  fallback:('a, 'b, 'calink,'cform,'cimg,'cheadlink,'cscript, 'd, 'e, 'f, public_url internal_url) url ->
  post_params:('h, 'd, 'j -> xhformcontl, 'dalink, 'dform, 'dimg, 'dheadlink, 'dscript) parameters ->
  ('a, 'j, 'calink,'cform,'cimg,'cheadlink,'cscript, 'd, 'h, 'd, public_url internal_url) url

val new_external_post_url :
  name:url_activator ->
  params:('a, page, 'b -> xhformcontl, 
    'calink, 'cform,'cimg, 'cheadlink, 'cscript) parameters ->
  post_params:('h, 'i, 'j -> xhformcontl, 
    'kalink, 'kform, 'kimg, 'kheadlink, 'kscript) parameters ->
  ('b, 'j, 'calink,'cform,'cimg,'cheadlink,'cscript, 'a, 'h, 'i, external_url) url

val new_post_state_url :
  fallback:('a, 'b, 'calink,'cform,'cimg,'cheadlink,'cscript, 'd, 'e, 'f, public_url internal_url) url ->
  post_params:('g, 'h, 'i -> xhformcontl, 
    'dalink, 'dform, 'dimg, 'dheadlink, 'dscript) parameters ->
  ('a, 'i, 'calink,'cform,'cimg,'cheadlink,'cscript, 'd, 'g, 'h, state_url internal_url) url

val register_post_url :
    url:('a, 'b -> 'c, 'calink,'cform,'cimg,'cheadlink,'cscript, 'e, 'f, 'e, 'g internal_url) url -> page:'f -> unit

val register_post_session_url :
    url:('a, 'b -> 'c, 'calink,'cform,'cimg,'cheadlink,'cscript, 'e, 'f, 'e, 'g internal_url) url -> page:'f -> unit

val register_new_post_url :
    fallback:('a, 'b, 'calink,'cform,'cimg,'cheadlink,'cscript, 'd, 'e, 'f, public_url internal_url) url ->
    post_params:('h, 'd, ('i -> 'j) -> xhformcontl, 
      'kalink, 'kform, 'kimg, 'kheadlink, 'kscript) parameters ->
    page:'h -> ('a, 'i -> 'j, 'calink,'cform,'cimg,'cheadlink,'cscript, 'd, 'h, 'd, public_url internal_url) url

val register_new_post_session_url :
    fallback:('a, 'b, 'calink,'cform,'cimg,'cheadlink,'cscript, 'd, 'e, 'f, public_url internal_url) url ->
    post_params:('g, 'd, ('h -> 'i) -> xhformcontl, 
      'dalink, 'dform, 'dimg, 'dheadlink, 'dscript) parameters ->
    page:'g -> ('a, 'h -> 'i, 'calink,'cform,'cimg,'cheadlink,'cscript, 'd, 'g, 'd, state_url internal_url) url


(* actions (new 10/05) *)
type ('a,'b) actionurl

val new_actionurl :
  params:('a, unit, 'b -> xhformcontl, 
    'dalink, 'dform, 'dimg, 'dheadlink, 'dscript) parameters -> ('b, 'a) actionurl

val register_actionurl : actionurl:('a, 'b) actionurl -> action:'b -> unit

val register_session_actionurl :
  actionurl:('a, 'b) actionurl -> action:'b -> unit


val register_new_actionurl :
  params:('a, unit, 'b -> xhformcontl, 
    'dalink, 'dform, 'dimg, 'dheadlink, 'dscript) parameters ->
  action:'a 
  -> ('b, 'a) actionurl

val register_new_session_actionurl :
  params:('a, unit, 'b -> xhformcontl, 
    'dalink, 'dform, 'dimg, 'dheadlink, 'dscript) parameters ->
  action:'a 
  -> ('b, 'a) actionurl

(** static pages (new 10/05) *)
val register_new_static_directory :
    name:url_string ->
    location:string -> 
  (xhformcontl, xhformcontl, 
   string -> [>xhalink] t, string -> [>xhform] t, string -> [>xhimg] t, string -> [>xhheadlink] t, string -> [>xhscript] t, 
   page, page, page, 
   public_url internal_url) url

val register_new_session_static_directory :
    name:url_string ->
    location:string -> 
  (xhformcontl, xhformcontl, 
   string -> [>xhalink] t, string -> [>xhform] t, string -> [>xhimg] t, string -> [>xhheadlink] t, string -> [>xhscript] t, 
   page, page, page, 
   public_url internal_url) url

(** to close a session: *)
val close_session : unit -> unit

(** Functions to create web pages: *)

val a : xhacont t list -> url_string -> 
  ('a, xhformcontl, 'calink,'cform,'cimg,'cheadlink,'cscript, 'd, 'e, 'f, 'g) url -> 'calink

val css_link : url_string -> 
  ('a, xhformcontl, 'calink,'cform,'cimg,'cheadlink,'cscript, 'd, 'e, 'f, 'g) url -> 'cheadlink

val js_link : url_string -> 
  ('a, xhformcontl, 'calink,'cform,'cimg,'cheadlink,'cscript, 'd, 'e, 'f, 'g) url -> 'cscript

(** Link a registrated URL with the function that takes the url and
    names of the parameters, and creates a form for these parameters
*)
val form_get : url_string -> 
  ('a, xhformcontl, 'calink,'cform,'cimg,'cheadlink,'cscript, 'd, 'e, 'f, 'g) url -> 
    'a -> [> xhform ] t
val form_post : url_string -> 
  ('a, 'b, 'calink,'cform,'cimg,'cheadlink,'cscript, 'd, 'e, 'f, 'g) url -> 'b 
    -> 'cform
val img : 
  ?id:string -> ?classe:string list -> ?alt:string -> ?title:string ->
    url_string -> 
  ('a, xhformcontl, 'calink,'cform,'cimg,'cheadlink,'cscript, 'd, 'e, 'f, 'g) url -> 'cimg


val int_input : ?size:int -> ?maxlength:int -> 
  ?classe:string list -> ?id:string -> ?title:string -> 
    ?accesskey:string -> ?disabled:bool -> ?readonly:bool -> int name -> 
      [> xhinput ] t
val hidden_int_input : int name -> int -> [> xhinput ] t
val string_input : ?size:int -> ?maxlength:int -> 
  ?classe:string list -> ?id:string -> ?title:string -> 
    ?accesskey:string -> ?disabled:bool -> ?readonly:bool -> string name -> 
      [> xhinput ] t
val password_input : ?size:int -> ?maxlength:int -> 
  ?classe:string list -> ?id:string -> ?title:string -> 
    ?accesskey:string -> ?disabled:bool -> ?readonly:bool -> string name -> 
      [> xhinput ] t
val submit_input : ?classe:string list -> ?id:string -> ?title:string -> 
  ?accesskey:string -> ?disabled:bool -> ?readonly:bool -> string -> [> xhinput ] t
val reset_input : ?classe:string list -> ?id:string -> ?title:string -> 
  ?accesskey:string -> ?disabled:bool -> ?readonly:bool -> string -> [> xhinput ] t
val checkbox_input : ?classe:string list -> ?id:string -> ?title:string -> 
  ?accesskey:string -> ?disabled:bool -> ?readonly:bool -> ?checked:bool -> string -> [> xhinput ] t
val radio_input : ?classe:string list -> ?id:string -> ?title:string -> 
  ?accesskey:string -> ?disabled:bool -> ?readonly:bool -> ?checked:bool -> string -> [> xhinput ] t
val textarea : ?classe:string list -> ?id:string -> ?title:string -> ?accesskey:string ->
  ?disabled:bool -> ?readonly:bool -> rows:int -> cols:int ->
    ?dir:[`Rtl|`Ltr] -> string name -> [> xhtextarea ] t

val action_link : 
  ?reload:bool ->
  string -> 
  http_params -> 
  (xhformcontl, unit -> unit) actionurl -> [> xhform] t

val action_form :
    ?reload:bool ->
      ?classe:string list ->
	?id:string ->
	  http_params ->
	    ('a, 'b) actionurl ->
	      'a -> [> xhform] t

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
val load_ocsigen_module : dir:url_string -> cmo:string -> unit


exception Static_File of string
exception Ocsigen_error_while_loading of string
exception Ocsigen_404
val state_param_name : string
val action_prefix : string
val action_name : string
val action_reload : string

(** Profiling *)
val number_of_sessions : unit -> int
