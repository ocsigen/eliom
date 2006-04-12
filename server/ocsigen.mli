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


(** Ocsigen.ml defines the functions you need to interact with Ocsigen:
   - To create the "typed URLs" and associate them to a path (directories/name)
   - To specify the types and names of the parameters of this URL
   - To register the functions that generate pages for each of these URLs
   - To create links or forms etc.
 *)

open XHTML.M
open Xhtmltypes

(** {2 Types} *)

type page = xhtml elt
(** Type of web pages. It is the result type for function generating xhtml *)

type form_content_l = form_content elt list
(** Type of formulars *)

(*
type public_url
(** Empty type used as parameter for [internal_url] to tell that it is a public url *)

type state_url
(** Empty type used as parameter for [internal_url] to tell that it is a "state" url *)

type 'a internal_url
(** Empty type used as parameter for [url] to tell that it is an internal URL (one from your web site) *)

type external_url
(** Empty type used as parameter for [url] to tell that it is an external URL (for ex to do a link towards another web site) *)
*)

type url_kind = [`Internal_Url of [`Public_Url | `State_Url] | `External_Url]
(** Kind of URL *)

type (-'a, -'b, +'c, +'e, +'u (*, +'f, +'g*), -'h, -'i, +'j, +'k) url
(** Typed URLs. You don't need to understand the meaning of all parameters. The last one is subset of url_kind. And some of them have for example the shape [int -> string -> string -> ...] for a page that takes an [int] and two [string]. *)

type url_path = string list
(** This type is used to represent URL paths; For example the path [coucou/ciao] is represented by the list [\["coucou";"ciao"\]] *)

type current_url
(** This type is used to represent the current URL paths. It is used to create relative links from the current page. *)

val string_list_of_current_url : current_url -> url_path
(** Converts a [current_url] to an [url_path] *)

val reconstruct_url_path : url_path -> string
(** Reconstructs the string from a [url_path] *)

val remove_slash : url_path -> url_path
(** remove the first [/] in a path *)

(** {2 Types of pages parameters} *)

(** Here are some examples of how to specify the types and names of pages parameters:
 - [no_get_param] for a page without parameter. Call a "register" function with a parameter of type {{:#TYPEpage}[page]}
 - [unit] for a page without parameter, but that may have an effect. Call a "register" function with a parameter of type [unit ->] {{:#TYPEpage}[page]}
 - [(int "myvalue")] for a page that takes one parameter, of type [int], called [myvalue]. (You must register a function of type [int ->] {{:#TYPEpage}[page]}).
 - [((int "myvalue") ** (string "mystring"))] for a page that takes two parameters, one of type [int] called [myvalue], and one of type [string] called [mystring]. (You must register a function of type [int -> string ->] {{:#TYPEpage}[page]}).
 - [ip no_get_param] for a page that takes no parameter but when you want to have access to the IP address of the client inside the function that generates the page. That function has type  [string ->] {{:#TYPEpage}[page]}.
 - [current_url no_get_param] for a page that takes no parameter but when you want to have access to the current URL inside the function that generates the page. That function has type  {{:#TYPEcurrent_url}[current_url]}[ -> ]{{:#TYPEpage}[page]}. It is needed when you want to make links or forms towards internal url.
 - [current_url (ip (string "hello"))] for a page that takes one string parameter, when you want to have access to the current URL inside the function that generates the page. That function has type {{:#TYPEcurrent_url}[current_url]}[ -> string -> string -> ]{{:#TYPEpage}[page]}.


Remember that:
 - page parameters are always at the end (after other parameters like IP or user-agent)
 - In page parameters: POST parameters are before GET parameters when you have both.
 *)

type http_params = {url_suffix: string;
		    full_url: string;
		    current_url: current_url;
		    useragent: string;
		    ip: Unix.inet_addr;
		    get_params: (string * string) list;
		    post_params: (string * string) list}
(** Type of http parameters *)

type 'a name
(** Type for names of page parameters *)

type ('a,'b,'c,'da, 'dform, 'duri (* 'dimg, 'dlink, 'dscript *)) parameters
(** Type for parameters of a web page *)

type ('a,'b,'ca,'cform,'curi) server_parameters
(** Type for server parameters *)


val no_get_param : 
    ('a, 'a, 'b -> 'b, [>a] elt, [>form] elt, uri (*, [>img] elt, [>link] elt, [>script] elt*)) parameters
(** Used for pages that don't have any parameters (static pages) *)

val unit : (unit -> 'a, 'a, 'b -> 'b, [>a] elt, [>form] elt, uri (*, [>img] elt, [>link] elt, [>script] elt *)) parameters
(** used for pages that don't have any parameters but may have side-effects *)

val int :
    string ->
    (int -> 'a, 'a, (int name -> 'b) -> 'b, 
      int -> [>a] elt, int -> [>form] elt, int -> uri (*, int -> [>img] elt, int -> [>link] elt, int -> [>script] elt *)) parameters
(** [int s] tells that the page take an integer as parameter, labeled [s] *)

val string :
  string ->
  (string -> 'a, 'a, (string name -> 'b) -> 'b, 
     string -> [>a] elt, string -> [>form] elt, string -> uri (*, string -> [>img] elt, string -> [>link] elt, string -> [>script] elt *)) parameters
(** [string s] tells that the page take a string as parameter, labeled [s] *)

val user_type :
  (string -> 'c) -> ('c -> string) -> string ->
  ('c -> 'a, 'a, ('c name -> 'b) -> 'b, 
    'c -> [>a] elt, 'c -> [>form] elt, 'c -> uri (*, 'c -> [>img] elt, 'c -> [>link] elt, 'c -> [>script] elt*)) parameters
(** Allows to use whatever type you want for a parameter of the page.
   [user_type s_to_t t_to_s s] tells that the page take a parameter, labeled [s], and that the server will have to use [s_to_t] and [t_to_s] to make the conversion from and to string.
 *)

val useragent :
    ('a,'b,'ca,'cform,'curi) server_parameters -> 
      (string -> 'a,'b,'ca,'cform,'curi) server_parameters
(** Tells that one of the parameters of the function that will generate the page is the user-agent of the browser. *)

val ip :
    ('a,'b,'ca,'cform,'curi) server_parameters -> 
      (string -> 'a,'b,'ca,'cform,'curi) server_parameters
(** Tells that one of the parameters of the function that will generate the page is the IP address of the client. *)

val current_url :
    ('a,'b,'ca,'cform,'curi) server_parameters -> 
      (current_url -> 'a, 'b,'ca,'cform,'curi) server_parameters
(** Tells that one of the parameters of the function that will generate the page is the URL of the current page. *)

val url_suffix :
    ('a, 'b, 'c, 'd, 'e) server_parameters ->
    (string -> 'a, 'b,
     ((string option -> 'f) -> 'g) -> (string option -> 'f) -> string -> 'g,
     ((string option -> 'h) -> 'i) -> (string option -> 'h) -> string -> 'i,
     'd)
    server_parameters
(** Tells that one of the parameters of the function that will generate the page is the suffix of the URL of the current page. (see {{:#VALregister_new_url}[register_new_url]}) *)

val http_params :
    ('a,'b,'ca,'cform,'curi) server_parameters -> 
      (http_params -> 'a, 'b,'ca,'cform,'curi) server_parameters
(** Tells that one of the parameters of the function that will generate the page is the set of HTTP parameters (see the type {{:#TYPEhttp_params}[http_params]}). *)

val ( ** ) :
    ('a, 'b, 'c -> 'd, 'e -> a elt, 'e -> form elt, 'e -> uri (*,  'e -> img elt,
      'e -> link elt, 'e -> script elt *)) parameters ->
      ('b, 'g, 'd -> 'h,  'i -> 'ja, 'i -> 'jform, 'i -> 'juri(*, 'i -> 'jimg, 'i -> 'jlink, 'i -> 'jscript*)) parameters ->
	('a, 'g, 'c -> 'h, 'e -> 'i -> 'ja, 'e -> 'i -> 'jform, 'e -> 'i -> 'juri (*, 'e -> 'i -> 'jimg, 'e -> 'i -> 'jlink, 'e -> 'i -> 'jscript *)) parameters
(** This is a combinator to allow the page to take several parameters (see examples above) *)

val ( *** ) : ('b -> 'c) -> ('a -> 'b) -> ('a -> 'c)
(** This is a combinator to allow the page to take several server parameters (see examples above) *)

val no_server_param : 'a -> 'a

(** {2 Pages registration} *)

val new_url :
    path:url_path ->
    ?prefix:bool ->
    server_params:(('a, page, 'c -> 'c, 'd -> 'd, 'e -> 'e) server_parameters ->
                   ('f, page,
                    ((string option -> a XHTML.M.elt) -> 'g) ->
                    (string option -> a XHTML.M.elt) -> 'h,
                    ((string option -> form XHTML.M.elt) -> 'i) ->
                    (string option -> form XHTML.M.elt) -> 'j,
                    ((string option -> XHTML.M.uri) -> 'k) ->
                    (string option -> XHTML.M.uri) -> 'l)
                   server_parameters) ->
    get_params:('a, page, 'm -> form_content_l, 'g, 'i, 'k) parameters ->
    unit ->
    ('m, form_content_l, 'h, 'j, 'l, 'f, page, page,
     [ `Internal_Url of [ `Public_Url ] ])
    url
(** [new_url ~path:p ~get_params:pa ()] creates an {{:#TYPEurl}[url]} associated to the {{:#TYPEurl_path}[url_path]} [p] and that takes the parameters [pa]. 

If you specify [~prefix:true], your URL will match all requests from client beginning by [path]. You can have acces the the suffix of the URL using {{:VAL_url_suffix}[url_suffix]}. For example [new_url ["mysite";"mywiki"] ~prefix:true (url_suffix no_get_param)] will match all the URL of the shape [http://myserver/mysite/mywiki/thesuffix]*)

val new_external_url :
    path:url_path ->
    ?prefix:bool ->
    server_params:(('a, 'b, 'c -> 'c, 'd -> 'd, 'e -> 'e) server_parameters ->
                   ('f, page,
                    ((string option -> Xhtmltypes.a XHTML.M.elt) -> 'g) ->
                    (string option -> Xhtmltypes.a XHTML.M.elt) -> 'h,
                    ((string option -> Xhtmltypes.form XHTML.M.elt) -> 'i) ->
                    (string option -> Xhtmltypes.form XHTML.M.elt) -> 'j,
                    ((string option -> XHTML.M.uri) -> 'k) ->
                    (string option -> XHTML.M.uri) -> 'l)
                   server_parameters) ->
    get_params:('a, 'b, 'm -> form_content_l, 'g, 'i, 'k) parameters ->
    unit ->
    ('m, form_content_l, 'h, 'j, 'l, 'f, page, page, [ `External_Url ]) url


(** Creates an URL for an external web site *)

val new_state_url :
    fallback:('b, form_content_l, 'ca,'cform, 'curi, 'a, page, page, [`Internal_Url of [`Public_Url]]) url 
  -> ('b, form_content_l, 'ca,'cform, 'curi, 'a, page, page, [`Internal_Url of [`State_Url]]) url
(** Creates another version of an already existing URL, where you can register another treatment. The two versions are automatically distinguished thanks to an extra parameter. It allows to have several links towards the same page, that will behave differently. See the tutorial for more informations.*)

val register_url :
    url:('a, form_content_l, 'ca,'cform, 'curi, 'c, 'd, 'e, [`Internal_Url of 'f]) url -> 'c -> unit
(** Register an url in the global table of the server 
   with the associated generation function.
   [register_url url t f] will associate the url [url] to the function [f].*)

(*   f is usually a function that takes any number of parameters of
   any types and that creates a page.
   t is a function that will translate f to a function from http_params
   to page. t can be written using unit int (++) etc.
 *)

	
val register_url_for_session :
    url:('a, form_content_l, 'ca,'cform, 'curi, 'c, 'd, 'e, [`Internal_Url of 'f]) url -> 'c -> unit
(** Registers an url and the associated function in the session table.
   If the same client does a request to this url, this function will be
   used instead of the one from the global table.

   Warning:
   - All URL must be registered in the global table during initialisation,
   but never after,
   - You (obviously) can't register an url in a session table when no session is active
 *)


(*  Create a new URL and register it in the server with the associated action.
   [register_new_url url t f] will associate the url [url] to the function [f].
   [f] is usually a function that takes any number of parameters of
   any types and that creates a page.
   [t] is a function that will translate f to a function from http_params
   to page. [t] can be written using [unit int (++)] etc.
*)
val register_new_url :
  path:url_path ->
  ?prefix:bool -> 
    server_params:(('a, page, 'c -> 'c, 'd -> 'd, 'e -> 'e) server_parameters ->
                   ('f, page,
                    ((string option -> a XHTML.M.elt) -> 'g) ->
                    (string option -> a XHTML.M.elt) -> 'h,
                    ((string option -> form XHTML.M.elt) -> 'i) ->
                    (string option -> form XHTML.M.elt) -> 'j,
                    ((string option -> XHTML.M.uri) -> 'k) ->
                    (string option -> XHTML.M.uri) -> 'l)
                   server_parameters) ->
    get_params:('a, page, 'm -> form_content_l, 'g, 'i, 'k) parameters ->
    'f ->
    ('m, form_content_l, 'h, 'j, 'l, 'f, page, page,
     [ `Internal_Url of [ `Public_Url ] ])
    url
(** Same as [new_url] followed by [register_url] *)

val register_new_state_url :
    fallback:('a, form_content_l, 'b, 'c, 'd, 'e, page, page,
              [`Internal_Url of [`Public_Url]])
    url ->
      'e ->
	('a, form_content_l, 'b, 'c, 'd, 'e, page, page, [`Internal_Url of [`State_Url]]) url
(** Same as [new_state_url] followed by [register_url] *)

val register_new_state_url_for_session :
    fallback:('a, form_content_l, 'ca,'cform, 'curi, 'c, page, page, [`Internal_Url of [`Public_Url]]) url ->
    'c -> ('a, form_content_l, 'ca,'cform, 'curi, 'c, page, page, [`Internal_Url of [`State_Url]]) url
(** Same as [new_state_url] followed by [register_url_for_session] *)

val new_post_url :
  fallback:('a, 'b, 'ca,'cform, 'curi, 'd, 'e, 'f, [`Internal_Url of [`Public_Url]]) url ->
  post_params:('h, 'd, 'j -> form_content_l, 'da, 'dform, 'duri (* 'dimg, 'dlink, 'dscript *)) parameters ->
  ('a, 'j, 'ca,'cform, 'curi, 'd, 'h, 'd, [`Internal_Url of [`Public_Url]]) url
(** Creates an URL that takes POST parameters. 
   [fallback] is the same URL without POST parameters.
   You can create an URL with POST parameters if the same URL does not exist
   without POST parameters. Thus, the user can't bookmark a page that does not
   exist.
 *)

val new_external_post_url :
  path:url_path ->
  ?prefix:bool -> 
  server_params:(('a, 'b, 'c -> 'c, 'd -> 'd, 'e -> 'e) server_parameters ->
                   ('f, page,
                    ((string option -> Xhtmltypes.a XHTML.M.elt) -> 'g) ->
                    (string option -> Xhtmltypes.a XHTML.M.elt) -> 'h,
                    ((string option -> Xhtmltypes.form XHTML.M.elt) -> 'i) ->
                    (string option -> Xhtmltypes.form XHTML.M.elt) -> 'j,
                    ((string option -> XHTML.M.uri) -> 'k) ->
                    (string option -> XHTML.M.uri) -> 'l)
                   server_parameters) ->
  get_params:('a, 'b, 'm -> form_content_l, 'g, 'i, 'k) parameters ->
  post_params:('n, 'o, 'p -> form_content_l, 'q, 'r, 's) parameters ->
  unit -> ('m, 'p, 'h, 'j, 'l, 'f, 'n, 'o, [ `External_Url ]) url
(** Creates an external URL with POST parameters *)

val new_post_state_url :
  fallback:('a, 'b, 'ca,'cform, 'curi, 'd, 'e, 'f, [`Internal_Url of [`Public_Url]]) url ->
  post_params:('g, 'h, 'i -> form_content_l, 
    'da, 'dform, 'duri (* 'dimg, 'dlink, 'dscript *)) parameters ->
  ('a, 'i, 'ca,'cform, 'curi, 'd, 'g, 'h, [`Internal_Url of [`State_Url]]) url
(** Creates a state URL with POST parameters *)

val register_post_url :
    url:('a, 'b -> 'c, 'ca,'cform, 'curi, 'e, 'f, 'e, [`Internal_Url of 'g]) url -> 'f -> unit
(** Registers an URL with POST parameters in the global table *)

val register_post_url_for_session :
    url:('a, 'b -> 'c, 'ca,'cform, 'curi, 'e, 'f, 'e, [`Internal_Url of 'g]) url -> 'f -> unit
(** Registers an URL with POST parameters in the session table *)

val register_new_post_url :
    fallback:('a, 'b, 'ca,'cform, 'curi, 'd, 'e, 'f, [`Internal_Url of [`Public_Url]]) url ->
    post_params:('h, 'd, ('i -> 'j) -> form_content_l, 
      'ka, 'kform, 'kuri (* 'kimg, 'klink, 'kscript *)) parameters ->
    'h -> ('a, 'i -> 'j, 'ca,'cform, 'curi, 'd, 'h, 'd, [`Internal_Url of [`Public_Url]]) url
(** Same as [new_post_url] followed by [register_post_url] *)

val register_new_post_state_url :
    fallback:('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, [`Internal_Url of [`Public_Url]]) url ->
      post_params:('i, 'f, ('j -> 'k) -> form_content_l, 'l, 'm, 'n) parameters ->
	'i -> ('a, 'j -> 'k, 'c, 'd, 'e, 'f, 'i, 'f, [`Internal_Url of [`State_Url]]) url
(** Same as [new_post_state_url] followed by [register_post_url] *)

val register_new_post_state_url_for_session :
    fallback:('a, 'b, 'ca,'cform, 'curi, 'd, 'e, 'f, [`Internal_Url of [`Public_Url]]) url ->
    post_params:('g, 'd, ('h -> 'i) -> form_content_l, 
      'da, 'dform, 'duri (* 'dimg, 'dlink, 'dscript *)) parameters ->
    'g -> ('a, 'h -> 'i, 'ca,'cform, 'curi, 'd, 'g, 'd, [`Internal_Url of [`State_Url]]) url
(** Same as [new_post_state_url] followed by [register_post_url_for_session] *)

val static_dir :
    (form_content_l, form_content_l, string -> [> Xhtmltypes.a ] XHTML.M.elt,
     string -> [> Xhtmltypes.form ] XHTML.M.elt, string -> XHTML.M.uri,
     page, 'a, 'a, 'b)
    url
(** The URL that correponds to the directory where static pages are.
   This directory is chosen in the config file (ocsigen.conf).
   This URL takes the name of the static file as a parameter.
 *)

val close_session : unit -> unit
(** Close the session *)


(** {2 Registering actions} *)

(* actions (new 10/05) *)
type ('a,'b) actionurl
(** Type of actions. Actions are like URLs but they do not generate any page.
   When an action is called, the function associated is launched and
   current page is (possibly) reloaded.
 *)

val new_actionurl :
    server_params:(('a, unit, 'b, 'c, 'd, 'e) parameters ->
      ('f, unit, 'g -> form_content_l, 'h, 'i, 'j) parameters) ->
    get_params:('a, unit, 'b, 'c, 'd, 'e) parameters -> ('g, 'f) actionurl
(** Creates an action *)

val register_actionurl : actionurl:('a, 'b) actionurl -> action:'b -> unit
(** Register an action in the global table *)

val register_actionurl_for_session :
  actionurl:('a, 'b) actionurl -> action:'b -> unit
(** Register an action in the session table *)

val register_new_actionurl :
    server_params:(('a, unit, 'b, 'c, 'd, 'e) parameters ->
                   ('f, unit, 'g -> form_content_l, 'h, 'i, 'j) parameters) ->
    get_params:('a, unit, 'b, 'c, 'd, 'e) parameters ->
    action:'f -> ('g, 'f) actionurl
(** Same as [new_actionurl] followed by [register_actionurl] *)

val register_new_actionurl_for_session :
    server_params:(('a, unit, 'b, 'c, 'd, 'e) parameters ->
                   ('f, unit, 'g -> form_content_l, 'h, 'i, 'j) parameters) ->
    get_params:('a, unit, 'b, 'c, 'd, 'e) parameters ->
    action:'f -> ('g, 'f) actionurl
(** Same as [new_actionurl] followed by [register_actionurl_for_session] *)


(** {2 Creating links, forms, etc.} *)

val a : ?a:([< a_attrib > `Href ] attrib list) ->
  ('a, form_content_l, 'ca,'cform, 'curi, 'd, 'e, 'f, 'g) url ->
    current_url -> 
      a_content elt list -> 'ca
(** [a url current cont] creates a link from [current] to [url]. The text of
   the link is [cont]. For example [cont] may be something like
   [\[pcdata "click here"\]]. To know the current URL (for [current]),
   use {{:#VAL_current_url}_current_url}.

   If  the URL  is  waiting for  parameters,  you give  them as  extra
 parameters to [a], for example [a url current cont 42 "hello"]

 The [~a] optional parameter is used for extra attributes 
   (see the module XHTML.M) *)

val css_link : ?a:([< link_attrib > `Href `Rel `Type ] attrib list) ->
  uri -> [> link ] elt
(** Creates a [<link>] tag for a Cascading StyleSheet (CSS).
 *)

val js_script : ?a:([< script_attrib > `Src ] attrib list) ->
  uri -> [> script ] elt
(** Creates a [<script>] tag to add a javascript file *)

(*
val css_link : ?a:([< link_attrib > `Href `Rel `Type ] attrib list) ->
  ('a, form_content_l, 'ca,'cform, 'curi, 'd, 'e, 'f, 'g) url -> current_url -> 'clink

val script : ?a:([< script_attrib > `Src ] attrib list) ->
  ('a, form_content_l, 'ca,'cform, 'curi, 'd, 'e, 'f, 'g) url -> current_url -> 'cscript
*)

val make_uri :
    ('a, form_content_l,'ca,'cform,'curi(*'cimg,'clink,'cscript*),'d,'e,'f,'g) url
  -> current_url -> 'curi
(** Create the text of the URL. Like the [a] function, it may take
   extra parameters. *)


(** Link a registrated URL with the function that takes the url and
    names of the parameters, and creates a form for these parameters
*)
val get_form : ?a:([< form_attrib > `Method ] attrib list) ->
  ('a, form_content_l, 'ca,'cform, 'curi, 'd, 'e, 'f, 'g) url -> 
    current_url -> 'a -> [> form ] elt
(** [get_form url current formgen] creates a GET form from [current] to [url]. 
   The content of
   the form is generated by the function [formgen], that takes the names
   of page parameters as parameters. *)

val post_form : ?a:([< form_attrib > `Method ] attrib list) ->
  ('a, 'b, 'ca,'cform, 'curi, 'd, 'e, 'f, 'g) url
    -> current_url -> 'b -> 'cform
(** [post_form url current formgen] creates a POST form from [current] to [url]. 
*)

(*
val img : ?a:([< img_attrib ] attrib list) ->
  alt:string ->
  ('a, form_content_l, 'ca,'cform, 'curi, 'd, 'e, 'f, 'g) url -> current_url -> 'cimg
*)

val int_input : ?a:([< input_attrib > `Input_Type `Name ] attrib list ) -> 
  int name -> [> input ] elt
(** Creates an [<input>] tag for an integer *)

val hidden_int_input : 
    ?a:([< input_attrib > `Input_Type `Name `Value ] attrib list ) -> 
      int name -> int -> [> input ] elt
(** Creates an hidden [<input>] tag for an integer *)

val string_input : 
    ?a:([< input_attrib > `Input_Type `Name ] attrib list ) -> string name -> 
      [> input ] elt
(** Creates an [<input>] tag for a string *)

val password_input : 
    ?a:([< input_attrib > `Input_Type `Name ] attrib list ) -> string name -> 
      [> input ] elt
(** Creates an [<password>] tag *)

val checkbox_input :
    ?a:([< input_attrib > `Input_Type `Name ] attrib list ) -> 
  string name -> [> input ] elt
(** Creates a checkbox [<input>] tag *)

val radio_input : ?a:([< input_attrib > `Input_Type `Name ] attrib list ) -> 
  string name -> [> input ] elt
(** Creates a radio [<input>] tag *)

val textarea : ?a:([< textarea_attrib > `Name ] attrib list ) -> 
  string name -> rows:number -> cols:number -> [ `PCDATA ] XHTML.M.elt ->
    [> textarea ] elt
(** Creates a [<textarea>] tag *)

val submit_input : ?a:([< input_attrib > `Input_Type `Value ] attrib list ) -> 
  string -> [> input ] elt
(** Creates a submit [<input>] tag *)


val action_a : ?a:([< a_attrib > `Href ] attrib list) ->
  ?reload:bool ->
    (form_content_l, unit -> unit) actionurl -> 
      http_params -> 
	a_content elt list -> 
	  [> form] elt
(** Creates a link that will perform an action (see {{:#TYPEactionurl}[actionurl]}).
   If [~reload:false] is specified, the current page will not be reloaded.
 *)

val action_form : ?a:([< form_attrib > `Method ] attrib list) ->
    ?reload:bool ->
      ('a, 'b) actionurl ->
	http_params -> 
	  'a ->
	    [> form] elt
(** Creates a form that will perform an action (see {{:#TYPEactionurl}[actionurl]}).
   If [~reload:false] is specified, the current page will not be reloaded.
 *)

(**/**) (* Internal functions *)

(** return a page from an url and parameters *)
val get_page :
  url_path * string * string * int option * (string * string) list *
  (string * string) list * string -> 
  Unix.sockaddr -> string option -> string option * page * string

val make_action :
  string -> (string * string) list ->
  url_path * string * string * int option * (string * string) list *
  (string * string) list * string -> 
  Unix.sockaddr -> string option -> string option * unit * string


val state_param_name : string
val action_prefix : string
val action_name : string
val action_reload : string

(** Profiling *)
val number_of_sessions : unit -> int
val get_number_of_connected : unit -> int

val get_number_of_connected : unit -> int
(** Server internal functions: *)
(** loads a module in the server *)
val load_ocsigen_module : dir:url_path -> cmo:string -> unit
val incr_connected : unit -> unit
val decr_connected : unit -> unit


exception Ocsigen_url_created_outside_site_loading
exception Ocsigen_duplicate_registering of string
exception Ocsigen_page_erasing of string
exception Ocsigen_there_are_unregistered_url of string
exception Ocsigen_register_for_session_outside_session
exception Ocsigen_error_while_loading of string
exception Ocsigen_Is_a_directory
exception Ocsigen_404
val end_initialisation : unit -> unit

