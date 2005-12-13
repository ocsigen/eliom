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

open XHTML.M
open Xhtmltypes

(** Type of answers from modules (web pages) *)
type page = xhtml elt

(** Type of formulars *)
type xhformcontl = xhformcont elt list

(** Typed URLs *)
type public_url
type state_url
type 'a internal_url
type external_url
type (-'a, -'b, +'c, +'e, +'u (*, +'f, +'g*), -'h, -'i, +'j, 'k) url


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
type ('a,'b,'c,'da, 'dform, 'duri (* 'dimg, 'dlink, 'dscript *)) parameters

val _noparam : 
    ('a, 'a, 'b -> 'b, [>xha] elt, [>xhform] elt, uri (*, [>xhimg] elt, [>xhlink] elt, [>xhscript] elt*)) parameters
val _int :
    string ->
    (int -> 'a, 'a, (int name -> 'b) -> 'b, 
      int -> [>xha] elt, int -> [>xhform] elt, int -> uri (*, int -> [>xhimg] elt, int -> [>xhlink] elt, int -> [>xhscript] elt *)) parameters
val _unit : (unit -> 'a, 'a, 'b -> 'b, [>xha] elt, [>xhform] elt, uri (*, [>xhimg] elt, [>xhlink] elt, [>xhscript] elt *)) parameters
val _string :
  string ->
  (string -> 'a, 'a, (string name -> 'b) -> 'b, 
     string -> [>xha] elt, string -> [>xhform] elt, string -> uri (*, string -> [>xhimg] elt, string -> [>xhlink] elt, string -> [>xhscript] elt *)) parameters
val _user_type :
  (string -> 'c) -> ('c -> string) -> string ->
  ('c -> 'a, 'a, ('c name -> 'b) -> 'b, 
    'c -> [>xha] elt, 'c -> [>xhform] elt, 'c -> uri (*, 'c -> [>xhimg] elt, 'c -> [>xhlink] elt, 'c -> [>xhscript] elt*)) parameters
val _useragent :
  ('a, 'b, 'c, 'da, 'dform, 'duri (* 'dimg, 'dlink, 'dscript *)) parameters -> 
    (string -> 'a, 'b, 'c,  'da, 'dform, 'duri (* 'dimg, 'dlink, 'dscript *)) parameters
val _ip :
  ('a, 'b, 'c, 'da, 'dform, 'duri (* 'dimg, 'dlink, 'dscript *)) parameters -> 
    (string -> 'a, 'b, 'c, 'da, 'dform, 'duri (* 'dimg, 'dlink, 'dscript *)) parameters
val _current_url :
  ('a, 'b, 'c,  'da, 'dform, 'duri (* 'dimg, 'dlink, 'dscript *)) parameters -> 
    (string list -> 'a, 'b, 'c,  'da, 'dform, 'duri (* 'dimg, 'dlink, 'dscript *)) parameters
val _url_suffix :
    ('a, 'b, 'c,  'da, 'dform, 'duri (* 'dimg, 'dlink, 'dscript *)) parameters ->
      (string -> 'a, 'b, 'c, 
	string -> 'da, string -> 'dform, string -> 'duri (*, string -> 'dimg, string -> 'dlink, string -> 'dscript*)) parameters
val _http_params :
  ('a, 'b, 'c, 'da, 'dform, 'duri (* 'dimg, 'dlink, 'dscript *)) parameters ->
  (http_params -> 'a, 'b, 'c, 'da, 'dform, 'duri (* 'dimg, 'dlink, 'dscript *)) parameters
val ( ** ) :
    ('a, 'b, 'c -> 'd, 'e -> xha elt, 'e -> xhform elt, 'e -> uri (*,  'e -> xhimg elt,
      'e -> xhlink elt, 'e -> xhscript elt *)) parameters ->
      ('b, 'g, 'd -> 'h,  'i -> 'ja, 'i -> 'jform, 'i -> 'juri(*, 'i -> 'jimg, 'i -> 'jlink, 'i -> 'jscript*)) parameters ->
	('a, 'g, 'c -> 'h, 'e -> 'i -> 'ja, 'e -> 'i -> 'jform, 'e -> 'i -> 'juri (*, 'e -> 'i -> 'jimg, 'e -> 'i -> 'jlink, 'e -> 'i -> 'jscript *)) parameters


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
      'ca, 'cform, 'curi (* 'cimg, 'clink, 'cscript *)) parameters ->
    ('b, xhformcontl, 'ca,'cform, 'curi (*'cimg,'clink,'cscript *), 'a, 
      page, page, public_url internal_url) url

val new_external_url :
  name:url_activator ->
  params:('a, page, 'b -> xhformcontl, 
    'ca, 'cform, 'curi (* 'cimg, 'clink, 'cscript *)) parameters ->
  ('b, xhformcontl, 'ca,'cform, 'curi (*'cimg,'clink,'cscript *), 'a, page, page, external_url) url

val new_state_url :
  fallback:('b, xhformcontl, 'ca,'cform, 'curi (*'cimg,'clink,'cscript *), 'a, page, page, public_url internal_url) url 
  -> ('b, xhformcontl, 'ca,'cform, 'curi (*'cimg,'clink,'cscript *), 'a, page, page, state_url internal_url) url

val register_url :
  url:('a, xhformcontl, 'ca,'cform, 'curi (*'cimg,'clink,'cscript *), 'c, 'd, 'e, 'f internal_url) url -> page:'c -> unit

val register_session_url :
    url:('a, xhformcontl, 'ca,'cform, 'curi (*'cimg,'clink,'cscript *), 'c, 'd, 'e, 'f internal_url) url -> page:'c -> unit

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
    'ca, 'cform, 'curi (* 'cimg, 'clink, 'cscript *)) parameters ->
  page:'a -> ('b, xhformcontl, 'ca,'cform, 'curi (*'cimg,'clink,'cscript *), 'a, 
	      page, page, public_url internal_url) url

val register_new_session_url :
    fallback:('a, xhformcontl, 'ca,'cform, 'curi (*'cimg,'clink,'cscript *), 'c, page, page, public_url internal_url) url ->
    page:'c -> ('a, xhformcontl, 'ca,'cform, 'curi (*'cimg,'clink,'cscript *), 'c, page, page, state_url internal_url) url

val new_post_url :
  fallback:('a, 'b, 'ca,'cform, 'curi (*'cimg,'clink,'cscript *), 'd, 'e, 'f, public_url internal_url) url ->
  post_params:('h, 'd, 'j -> xhformcontl, 'da, 'dform, 'duri (* 'dimg, 'dlink, 'dscript *)) parameters ->
  ('a, 'j, 'ca,'cform, 'curi (*'cimg,'clink,'cscript *), 'd, 'h, 'd, public_url internal_url) url

val new_external_post_url :
  name:url_activator ->
  params:('a, page, 'b -> xhformcontl, 
    'ca, 'cform, 'curi (*'cimg, 'clink, 'cscript *)) parameters ->
  post_params:('h, 'i, 'j -> xhformcontl, 
    'ka, 'kform, 'kuri (* 'kimg, 'klink, 'kscript *)) parameters ->
  ('b, 'j, 'ca,'cform, 'curi (*'cimg,'clink,'cscript *), 'a, 'h, 'i, external_url) url

val new_post_state_url :
  fallback:('a, 'b, 'ca,'cform, 'curi (*'cimg,'clink,'cscript *), 'd, 'e, 'f, public_url internal_url) url ->
  post_params:('g, 'h, 'i -> xhformcontl, 
    'da, 'dform, 'duri (* 'dimg, 'dlink, 'dscript *)) parameters ->
  ('a, 'i, 'ca,'cform, 'curi (*'cimg,'clink,'cscript *), 'd, 'g, 'h, state_url internal_url) url

val register_post_url :
    url:('a, 'b -> 'c, 'ca,'cform, 'curi (*'cimg,'clink,'cscript *), 'e, 'f, 'e, 'g internal_url) url -> page:'f -> unit

val register_post_session_url :
    url:('a, 'b -> 'c, 'ca,'cform, 'curi (*'cimg,'clink,'cscript *), 'e, 'f, 'e, 'g internal_url) url -> page:'f -> unit

val register_new_post_url :
    fallback:('a, 'b, 'ca,'cform, 'curi (*'cimg,'clink,'cscript *), 'd, 'e, 'f, public_url internal_url) url ->
    post_params:('h, 'd, ('i -> 'j) -> xhformcontl, 
      'ka, 'kform, 'kuri (* 'kimg, 'klink, 'kscript *)) parameters ->
    page:'h -> ('a, 'i -> 'j, 'ca,'cform, 'curi (*'cimg,'clink,'cscript *), 'd, 'h, 'd, public_url internal_url) url

val register_new_post_session_url :
    fallback:('a, 'b, 'ca,'cform, 'curi (*'cimg,'clink,'cscript *), 'd, 'e, 'f, public_url internal_url) url ->
    post_params:('g, 'd, ('h -> 'i) -> xhformcontl, 
      'da, 'dform, 'duri (* 'dimg, 'dlink, 'dscript *)) parameters ->
    page:'g -> ('a, 'h -> 'i, 'ca,'cform, 'curi (*'cimg,'clink,'cscript *), 'd, 'g, 'd, state_url internal_url) url


(* actions (new 10/05) *)
type ('a,'b) actionurl

val new_actionurl :
  params:('a, unit, 'b -> xhformcontl, 
    'da, 'dform, 'duri (* 'dimg, 'dlink, 'dscript *)) parameters -> ('b, 'a) actionurl

val register_actionurl : actionurl:('a, 'b) actionurl -> action:'b -> unit

val register_session_actionurl :
  actionurl:('a, 'b) actionurl -> action:'b -> unit


val register_new_actionurl :
  params:('a, unit, 'b -> xhformcontl, 
    'da, 'dform, 'duri (* 'dimg, 'dlink, 'dscript *)) parameters ->
  action:'a 
  -> ('b, 'a) actionurl

val register_new_session_actionurl :
  params:('a, unit, 'b -> xhformcontl, 
    'da, 'dform, 'duri (* 'dimg, 'dlink, 'dscript *)) parameters ->
  action:'a 
  -> ('b, 'a) actionurl

(** static pages (new 10/05) *)
val register_new_static_directory :
    name:url_string ->
    location:string -> 
  (xhformcontl, xhformcontl, 
   string -> [>xha] elt, string -> [>xhform] elt, string -> uri (*, string -> [>xhimg] elt, string -> [>xhlink] elt, string -> [>xhscript] elt*), 
   page, page, page, 
   public_url internal_url) url

val register_new_session_static_directory :
    name:url_string ->
    location:string -> 
  (xhformcontl, xhformcontl, 
   string -> [>xha] elt, string -> [>xhform] elt, string -> uri (*, string -> [>xhimg] elt, string -> [>xhlink] elt, string -> [>xhscript] elt*), 
   page, page, page, 
   public_url internal_url) url

(** to close a session: *)
val close_session : unit -> unit

(** Functions to create web pages: *)

val a : ?a:([< xhaattrib > `Href ] attrib list) ->
  ('a, xhformcontl, 'ca,'cform, 'curi (*'cimg,'clink,'cscript *), 'd, 'e, 'f, 'g) url ->
    url_string -> 
      xhacont elt list -> 'ca

val css_link : ?a:([< xhlinkattrib > `Href `Rel `Type ] attrib list) ->
  uri -> [> xhlink ] elt

val js_script : ?a:([< xhscriptattrib > `Src ] attrib list) ->
  uri -> [> xhscript ] elt

(*
val css_link : ?a:([< xhlinkattrib > `Href `Rel `Type ] attrib list) ->
  ('a, xhformcontl, 'ca,'cform, 'curi (*'cimg,'clink,'cscript *), 'd, 'e, 'f, 'g) url -> url_string -> 'clink

val script : ?a:([< xhscriptattrib > `Src ] attrib list) ->
  ('a, xhformcontl, 'ca,'cform, 'curi (*'cimg,'clink,'cscript *), 'd, 'e, 'f, 'g) url -> url_string -> 'cscript
*)

val make_uri :
    ('a, xhformcontl,'ca,'cform,'curi(*'cimg,'clink,'cscript*),'d,'e,'f,'g) url
  -> url_string -> 'curi



(** Link a registrated URL with the function that takes the url and
    names of the parameters, and creates a form for these parameters
*)
val form_get : ?a:([< xhformattrib > `Method ] attrib list) ->
  ('a, xhformcontl, 'ca,'cform, 'curi (*'cimg,'clink,'cscript *), 'd, 'e, 'f, 'g) url -> 
    url_string -> 'a -> [> xhform ] elt
val form_post : ?a:([< xhformattrib > `Method ] attrib list) ->
  ('a, 'b, 'ca,'cform, 'curi (*'cimg,'clink,'cscript *), 'd, 'e, 'f, 'g) url
    -> url_string -> 'b -> 'cform

(*
val img : ?a:([< xhimgattrib ] attrib list) ->
  alt:string ->
  ('a, xhformcontl, 'ca,'cform, 'curi (*'cimg,'clink,'cscript *), 'd, 'e, 'f, 'g) url -> url_string -> 'cimg
*)

val int_input : ?a:([< xhinputattrib > `Input_Type `Name ] attrib list ) -> 
  int name -> [> xhinput ] elt
val hidden_int_input : 
    ?a:([< xhinputattrib > `Input_Type `Name `Value ] attrib list ) -> 
      int name -> int -> [> xhinput ] elt
val string_input : 
    ?a:([< xhinputattrib > `Input_Type `Name ] attrib list ) -> string name -> 
      [> xhinput ] elt
val password_input : 
    ?a:([< xhinputattrib > `Input_Type `Name ] attrib list ) -> string name -> 
      [> xhinput ] elt
val checkbox_input :
    ?a:([< xhinputattrib > `Input_Type `Name ] attrib list ) -> 
  bool name -> [> xhinput ] elt
val radio_input : ?a:([< xhinputattrib > `Input_Type `Name ] attrib list ) -> 
  bool name -> [> xhinput ] elt
val textarea : ?a:([< xhtextareaattrib > `Name ] attrib list ) -> 
  string name -> rows:number -> cols:number -> [ `PCDATA ] XHTML.M.elt ->
    [> xhtextarea ] elt
val submit_input : ?a:([< xhinputattrib > `Input_Type `Value ] attrib list ) -> 
  string -> [> xhinput ] elt

val action_link : ?a:([< xhaattrib > `Href ] attrib list) ->
  ?reload:bool ->
    (xhformcontl, unit -> unit) actionurl -> 
      http_params -> 
	xhacont elt list -> 
	  [> xhform] elt

val action_form : ?a:([< xhformattrib > `Method ] attrib list) ->
    ?reload:bool ->
      ?classe:string list ->
	?id:string ->
	    ('a, 'b) actionurl ->
	      http_params -> 
		'a ->
		  [> xhform] elt

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
