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

type xhformcontl = xhformcont elt list
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
 - [_noparam] for a page without parameter. Call a "register" function with a parameter of type {{:#TYPEpage}[page]}
 - [_unit] for a page without parameter, but that may have an effect. Call a "register" function with a parameter of type [unit ->] {{:#TYPEpage}[page]}
 - [(_int "myvalue")] for a page that takes one parameter, of type [int], called [myvalue]. (You must register a function of type [int ->] {{:#TYPEpage}[page]}).
 - [((_int "myvalue") ** (_string "mystring"))] for a page that takes two parameters, one of type [int] called [myvalue], and one of type [string] called [mystring]. (You must register a function of type [int -> string ->] {{:#TYPEpage}[page]}).
 - [_ip _noparam] for a page that takes no parameter but when you want to have access to the IP address of the client inside the function that generates the page. That function has type  [string ->] {{:#TYPEpage}[page]}.
 - [_current_url _noparam] for a page that takes no parameter but when you want to have access to the current URL inside the function that generates the page. That function has type  {{:#TYPEcurrent_url}[current_url]}[ -> ]{{:#TYPEpage}[page]}. It is needed when you want to make links or forms towards internal url.
 - [_current_url (_ip (_string "hello"))] for a page that takes one string parameter, when you want to have access to the current URL inside the function that generates the page. That function has type {{:#TYPEcurrent_url}[current_url]}[ -> string -> string -> ]{{:#TYPEpage}[page]}.


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


val _noparam : 
    ('a, 'a, 'b -> 'b, [>xha] elt, [>xhform] elt, uri (*, [>xhimg] elt, [>xhlink] elt, [>xhscript] elt*)) parameters
(** Used for pages that don't have any parameters (static pages) *)

val _unit : (unit -> 'a, 'a, 'b -> 'b, [>xha] elt, [>xhform] elt, uri (*, [>xhimg] elt, [>xhlink] elt, [>xhscript] elt *)) parameters
(** used for pages that don't have any parameters but may have side-effects *)

val _int :
    string ->
    (int -> 'a, 'a, (int name -> 'b) -> 'b, 
      int -> [>xha] elt, int -> [>xhform] elt, int -> uri (*, int -> [>xhimg] elt, int -> [>xhlink] elt, int -> [>xhscript] elt *)) parameters
(** [_int s] tells that the page take an integer as parameter, labeled [s] *)

val _string :
  string ->
  (string -> 'a, 'a, (string name -> 'b) -> 'b, 
     string -> [>xha] elt, string -> [>xhform] elt, string -> uri (*, string -> [>xhimg] elt, string -> [>xhlink] elt, string -> [>xhscript] elt *)) parameters
(** [_string s] tells that the page take a string as parameter, labeled [s] *)

val _user_type :
  (string -> 'c) -> ('c -> string) -> string ->
  ('c -> 'a, 'a, ('c name -> 'b) -> 'b, 
    'c -> [>xha] elt, 'c -> [>xhform] elt, 'c -> uri (*, 'c -> [>xhimg] elt, 'c -> [>xhlink] elt, 'c -> [>xhscript] elt*)) parameters
(** Allows to use whatever type you want for a parameter of the page.
   [_user_type s_to_t t_to_s s] tells that the page take a parameter, labeled [s], and that the server will have to use [s_to_t] and [t_to_s] to make the conversion from and to string.
 *)

val _useragent :
  ('a, 'b, 'c, 'da, 'dform, 'duri (* 'dimg, 'dlink, 'dscript *)) parameters -> 
    (string -> 'a, 'b, 'c,  'da, 'dform, 'duri (* 'dimg, 'dlink, 'dscript *)) parameters
(** Tells that one of the parameters of the function that will generate the page is the user-agent of the browser. *)

val _ip :
  ('a, 'b, 'c, 'da, 'dform, 'duri (* 'dimg, 'dlink, 'dscript *)) parameters -> 
    (string -> 'a, 'b, 'c, 'da, 'dform, 'duri (* 'dimg, 'dlink, 'dscript *)) parameters
(** Tells that one of the parameters of the function that will generate the page is the IP address of the client. *)

val _current_url :
  ('a, 'b, 'c,  'da, 'dform, 'duri (* 'dimg, 'dlink, 'dscript *)) parameters -> 
    (current_url -> 'a, 'b, 'c,  'da, 'dform, 'duri (* 'dimg, 'dlink, 'dscript *)) parameters
(** Tells that one of the parameters of the function that will generate the page is the URL of the current page. *)

val _url_suffix :
    ('a, 'b, 'c,  'da, 'dform, 'duri (* 'dimg, 'dlink, 'dscript *)) parameters ->
      (string -> 'a, 'b, 'c, 
	string -> 'da, string -> 'dform, string -> 'duri (*, string -> 'dimg, string -> 'dlink, string -> 'dscript*)) parameters
(** Tells that one of the parameters of the function that will generate the page is the suffix of the URL of the current page. (see {{:#VALregister_new_url}[register_new_url]}) *)

val _http_params :
  ('a, 'b, 'c, 'da, 'dform, 'duri (* 'dimg, 'dlink, 'dscript *)) parameters ->
  (http_params -> 'a, 'b, 'c, 'da, 'dform, 'duri (* 'dimg, 'dlink, 'dscript *)) parameters
(** Tells that one of the parameters of the function that will generate the page is the set of HTTP parameters (see the type {{:#TYPEhttp_params}[http_params]}). *)

val ( ** ) :
    ('a, 'b, 'c -> 'd, 'e -> xha elt, 'e -> xhform elt, 'e -> uri (*,  'e -> xhimg elt,
      'e -> xhlink elt, 'e -> xhscript elt *)) parameters ->
      ('b, 'g, 'd -> 'h,  'i -> 'ja, 'i -> 'jform, 'i -> 'juri(*, 'i -> 'jimg, 'i -> 'jlink, 'i -> 'jscript*)) parameters ->
	('a, 'g, 'c -> 'h, 'e -> 'i -> 'ja, 'e -> 'i -> 'jform, 'e -> 'i -> 'juri (*, 'e -> 'i -> 'jimg, 'e -> 'i -> 'jlink, 'e -> 'i -> 'jscript *)) parameters
(** This is a combinator to allow the page to take several parameters (see examples above) *)

(** {2 Pages registration} *)

val new_url :
    path:url_path ->
      ?prefix:bool ->
	params:('a, page, 'b -> xhformcontl, 
	  'ca, 'cform, 'curi (* 'cimg, 'clink, 'cscript *)) parameters ->
	    unit ->
	      ('b, xhformcontl, 'ca,'cform, 'curi (*'cimg,'clink,'cscript *), 'a, 
	       page, page, [`Internal_Url of [`Public_Url]]) url
(** [new_url ~path:p ~params:pa ()] creates an {{:#TYPEurl}[url]} associated to the {{:#TYPEurl_path}[url_path]} [p] and that takes the parameters [pa]. 

If you specify [~prefix:true], your URL will match all requests from client beginning by [path]. You can have acces the the suffix of the URL using {{:VAL_url_suffix}[_url_suffix]}. For example [new_url ["mysite";"mywiki"] ~prefix:true (_url_suffix _noparam)] will match all the URL of the shape [http://myserver/mysite/mywiki/thesuffix]*)

val new_external_url :
  path:url_path ->
    ?prefix:bool ->
      params:('a, page, 'b -> xhformcontl, 
	'ca, 'cform, 'curi (* 'cimg, 'clink, 'cscript *)) parameters ->
	  unit -> 
	    ('b, xhformcontl, 'ca,'cform, 'curi (*'cimg,'clink,'cscript *), 'a, page, page, [`External_Url]) url
(** Creates an URL for an external web site *)

val new_state_url :
    fallback:('b, xhformcontl, 'ca,'cform, 'curi (*'cimg,'clink,'cscript *), 'a, page, page, [`Internal_Url of [`Public_Url]]) url 
  -> ('b, xhformcontl, 'ca,'cform, 'curi (*'cimg,'clink,'cscript *), 'a, page, page, [`Internal_Url of [`State_Url]]) url
(** Creates another version of an already existing URL, where you can register another treatment. The two versions are automatically distinguished thanks to an extra parameter. It allows to have several links towards the same page, that will behave differently. See the tutorial for more informations.*)

val register_url :
    url:('a, xhformcontl, 'ca,'cform, 'curi (*'cimg,'clink,'cscript *), 'c, 'd, 'e, [`Internal_Url of 'f]) url -> 'c -> unit
(** Register an url in the global table of the server 
   with the associated generation function.
   [register_url url t f] will associate the url [url] to the function [f].*)

(*   f is usually a function that takes any number of parameters of
   any types and that creates a page.
   t is a function that will translate f to a function from http_params
   to page. t can be written using _unit _int (++) etc.
 *)

	
val register_url_for_session :
    url:('a, xhformcontl, 'ca,'cform, 'curi (*'cimg,'clink,'cscript *), 'c, 'd, 'e, [`Internal_Url of 'f]) url -> 'c -> unit
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
   to page. [t] can be written using [_unit _int (++)] etc.
*)
val register_new_url :
  path:url_path ->
  ?prefix:bool -> 
  params:('a, page, 'b -> xhformcontl, 
    'ca, 'cform, 'curi (* 'cimg, 'clink, 'cscript *)) parameters ->
  'a -> ('b, xhformcontl, 'ca,'cform, 'curi (*'cimg,'clink,'cscript *), 'a, 
	      page, page, [`Internal_Url of [`Public_Url]]) url
(** Same as [new_url] followed by [register_url] *)

val register_new_state_url :
    fallback:('a, xhformcontl, 'b, 'c, 'd, 'e, page, page,
              [`Internal_Url of [`Public_Url]])
    url ->
      'e ->
	('a, xhformcontl, 'b, 'c, 'd, 'e, page, page, [`Internal_Url of [`State_Url]]) url
(** Same as [new_state_url] followed by [register_url] *)

val register_new_state_url_for_session :
    fallback:('a, xhformcontl, 'ca,'cform, 'curi (*'cimg,'clink,'cscript *), 'c, page, page, [`Internal_Url of [`Public_Url]]) url ->
    'c -> ('a, xhformcontl, 'ca,'cform, 'curi (*'cimg,'clink,'cscript *), 'c, page, page, [`Internal_Url of [`State_Url]]) url
(** Same as [new_state_url] followed by [register_url_for_session] *)

val new_post_url :
  fallback:('a, 'b, 'ca,'cform, 'curi (*'cimg,'clink,'cscript *), 'd, 'e, 'f, [`Internal_Url of [`Public_Url]]) url ->
  post_params:('h, 'd, 'j -> xhformcontl, 'da, 'dform, 'duri (* 'dimg, 'dlink, 'dscript *)) parameters ->
  ('a, 'j, 'ca,'cform, 'curi (*'cimg,'clink,'cscript *), 'd, 'h, 'd, [`Internal_Url of [`Public_Url]]) url
(** Creates an URL that takes POST parameters. 
   [fallback] is the same URL without POST parameters.
   You can create an URL with POST parameters if the same URL does not exist
   without POST parameters. Thus, the user can't bookmark a page that does not
   exist.
 *)

val new_external_post_url :
  path:url_path ->
  ?prefix:bool -> 
  params:('a, page, 'b -> xhformcontl, 
    'ca, 'cform, 'curi (*'cimg, 'clink, 'cscript *)) parameters ->
  post_params:('h, 'i, 'j -> xhformcontl, 
    'ka, 'kform, 'kuri (* 'kimg, 'klink, 'kscript *)) parameters ->
  unit -> 
  ('b, 'j, 'ca,'cform, 'curi (*'cimg,'clink,'cscript *), 'a, 'h, 'i, [`External_Url]) url
(** Creates an external URL with POST parameters *)

val new_post_state_url :
  fallback:('a, 'b, 'ca,'cform, 'curi (*'cimg,'clink,'cscript *), 'd, 'e, 'f, [`Internal_Url of [`Public_Url]]) url ->
  post_params:('g, 'h, 'i -> xhformcontl, 
    'da, 'dform, 'duri (* 'dimg, 'dlink, 'dscript *)) parameters ->
  ('a, 'i, 'ca,'cform, 'curi (*'cimg,'clink,'cscript *), 'd, 'g, 'h, [`Internal_Url of [`State_Url]]) url
(** Creates a state URL with POST parameters *)

val register_post_url :
    url:('a, 'b -> 'c, 'ca,'cform, 'curi (*'cimg,'clink,'cscript *), 'e, 'f, 'e, [`Internal_Url of 'g]) url -> 'f -> unit
(** Registers an URL with POST parameters in the global table *)

val register_post_url_for_session :
    url:('a, 'b -> 'c, 'ca,'cform, 'curi (*'cimg,'clink,'cscript *), 'e, 'f, 'e, [`Internal_Url of 'g]) url -> 'f -> unit
(** Registers an URL with POST parameters in the session table *)

val register_new_post_url :
    fallback:('a, 'b, 'ca,'cform, 'curi (*'cimg,'clink,'cscript *), 'd, 'e, 'f, [`Internal_Url of [`Public_Url]]) url ->
    post_params:('h, 'd, ('i -> 'j) -> xhformcontl, 
      'ka, 'kform, 'kuri (* 'kimg, 'klink, 'kscript *)) parameters ->
    'h -> ('a, 'i -> 'j, 'ca,'cform, 'curi (*'cimg,'clink,'cscript *), 'd, 'h, 'd, [`Internal_Url of [`Public_Url]]) url
(** Same as [new_post_url] followed by [register_post_url] *)

val register_new_post_state_url :
    fallback:('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, [`Internal_Url of [`Public_Url]]) url ->
      post_params:('i, 'f, ('j -> 'k) -> xhformcontl, 'l, 'm, 'n) parameters ->
	'i -> ('a, 'j -> 'k, 'c, 'd, 'e, 'f, 'i, 'f, [`Internal_Url of [`State_Url]]) url
(** Same as [new_post_state_url] followed by [register_post_url] *)

val register_new_post_state_url_for_session :
    fallback:('a, 'b, 'ca,'cform, 'curi (*'cimg,'clink,'cscript *), 'd, 'e, 'f, [`Internal_Url of [`Public_Url]]) url ->
    post_params:('g, 'd, ('h -> 'i) -> xhformcontl, 
      'da, 'dform, 'duri (* 'dimg, 'dlink, 'dscript *)) parameters ->
    'g -> ('a, 'h -> 'i, 'ca,'cform, 'curi (*'cimg,'clink,'cscript *), 'd, 'g, 'd, [`Internal_Url of [`State_Url]]) url
(** Same as [new_post_state_url] followed by [register_post_url_for_session] *)

val static_dir :
    (xhformcontl, xhformcontl, string -> [> Xhtmltypes.xha ] XHTML.M.elt,
     string -> [> Xhtmltypes.xhform ] XHTML.M.elt, string -> XHTML.M.uri,
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
  params:('a, unit, 'b -> xhformcontl, 
    'da, 'dform, 'duri (* 'dimg, 'dlink, 'dscript *)) parameters -> ('b, 'a) actionurl
(** Creates an action *)

val register_actionurl : actionurl:('a, 'b) actionurl -> action:'b -> unit
(** Register an action in the global table *)

val register_actionurl_for_session :
  actionurl:('a, 'b) actionurl -> action:'b -> unit
(** Register an action in the session table *)

val register_new_actionurl :
  params:('a, unit, 'b -> xhformcontl, 
    'da, 'dform, 'duri (* 'dimg, 'dlink, 'dscript *)) parameters ->
  action:'a 
  -> ('b, 'a) actionurl
(** Same as [new_actionurl] followed by [register_actionurl] *)

val register_new_actionurl_for_session :
  params:('a, unit, 'b -> xhformcontl, 
    'da, 'dform, 'duri (* 'dimg, 'dlink, 'dscript *)) parameters ->
  action:'a 
  -> ('b, 'a) actionurl
(** Same as [new_actionurl] followed by [register_actionurl_for_session] *)


(** {2 Creating links, forms, etc.} *)

val a : ?a:([< xhaattrib > `Href ] attrib list) ->
  ('a, xhformcontl, 'ca,'cform, 'curi (*'cimg,'clink,'cscript *), 'd, 'e, 'f, 'g) url ->
    current_url -> 
      xhacont elt list -> 'ca
(** [a url current cont] creates a link from [current] to [url]. The text of
   the link is [cont]. For example [cont] may be something like
   [\[pcdata "click here"\]]. To know the current URL (for [current]),
   use {{:#VAL_current_url}_current_url}.

   If  the URL  is  waiting for  parameters,  you give  them as  extra
 parameters to [a], for example [a url current cont 42 "hello"]

 The [~a] optional parameter is used for extra attributes 
   (see the module XHTML.M) *)

val css_link : ?a:([< xhlinkattrib > `Href `Rel `Type ] attrib list) ->
  uri -> [> xhlink ] elt
(** Creates a [<link>] tag for a Cascading StyleSheet (CSS).
 *)

val js_script : ?a:([< xhscriptattrib > `Src ] attrib list) ->
  uri -> [> xhscript ] elt
(** Creates a [<script>] tag to add a javascript file *)

(*
val css_link : ?a:([< xhlinkattrib > `Href `Rel `Type ] attrib list) ->
  ('a, xhformcontl, 'ca,'cform, 'curi (*'cimg,'clink,'cscript *), 'd, 'e, 'f, 'g) url -> current_url -> 'clink

val script : ?a:([< xhscriptattrib > `Src ] attrib list) ->
  ('a, xhformcontl, 'ca,'cform, 'curi (*'cimg,'clink,'cscript *), 'd, 'e, 'f, 'g) url -> current_url -> 'cscript
*)

val make_uri :
    ('a, xhformcontl,'ca,'cform,'curi(*'cimg,'clink,'cscript*),'d,'e,'f,'g) url
  -> current_url -> 'curi
(** Create the text of the URL. Like the [a] function, it may take
   extra parameters. *)


(** Link a registrated URL with the function that takes the url and
    names of the parameters, and creates a form for these parameters
*)
val get_form : ?a:([< xhformattrib > `Method ] attrib list) ->
  ('a, xhformcontl, 'ca,'cform, 'curi (*'cimg,'clink,'cscript *), 'd, 'e, 'f, 'g) url -> 
    current_url -> 'a -> [> xhform ] elt
(** [get_form url current formgen] creates a GET form from [current] to [url]. 
   The content of
   the form is generated by the function [formgen], that takes the names
   of page parameters as parameters. *)

val post_form : ?a:([< xhformattrib > `Method ] attrib list) ->
  ('a, 'b, 'ca,'cform, 'curi (*'cimg,'clink,'cscript *), 'd, 'e, 'f, 'g) url
    -> current_url -> 'b -> 'cform
(** [post_form url current formgen] creates a POST form from [current] to [url]. 
*)

(*
val img : ?a:([< xhimgattrib ] attrib list) ->
  alt:string ->
  ('a, xhformcontl, 'ca,'cform, 'curi (*'cimg,'clink,'cscript *), 'd, 'e, 'f, 'g) url -> current_url -> 'cimg
*)

val int_input : ?a:([< xhinputattrib > `Input_Type `Name ] attrib list ) -> 
  int name -> [> xhinput ] elt
(** Creates an [<input>] tag for an integer *)

val hidden_int_input : 
    ?a:([< xhinputattrib > `Input_Type `Name `Value ] attrib list ) -> 
      int name -> int -> [> xhinput ] elt
(** Creates an hidden [<input>] tag for an integer *)

val string_input : 
    ?a:([< xhinputattrib > `Input_Type `Name ] attrib list ) -> string name -> 
      [> xhinput ] elt
(** Creates an [<input>] tag for a string *)

val password_input : 
    ?a:([< xhinputattrib > `Input_Type `Name ] attrib list ) -> string name -> 
      [> xhinput ] elt
(** Creates an [<password>] tag *)

val checkbox_input :
    ?a:([< xhinputattrib > `Input_Type `Name ] attrib list ) -> 
  bool name -> [> xhinput ] elt
(** Creates a checkbox [<input>] tag *)

val radio_input : ?a:([< xhinputattrib > `Input_Type `Name ] attrib list ) -> 
  bool name -> [> xhinput ] elt
(** Creates a radio [<input>] tag *)

val textarea : ?a:([< xhtextareaattrib > `Name ] attrib list ) -> 
  string name -> rows:number -> cols:number -> [ `PCDATA ] XHTML.M.elt ->
    [> xhtextarea ] elt
(** Creates a [<textarea>] tag *)

val submit_input : ?a:([< xhinputattrib > `Input_Type `Value ] attrib list ) -> 
  string -> [> xhinput ] elt
(** Creates a submit [<input>] tag *)


val action_a : ?a:([< xhaattrib > `Href ] attrib list) ->
  ?reload:bool ->
    (xhformcontl, unit -> unit) actionurl -> 
      http_params -> 
	xhacont elt list -> 
	  [> xhform] elt
(** Creates a link that will perform an action (see {{:#TYPEactionurl}[actionurl]}).
   If [~reload:false] is specified, the current page will not be reloaded.
 *)

val action_form : ?a:([< xhformattrib > `Method ] attrib list) ->
    ?reload:bool ->
      ('a, 'b) actionurl ->
	http_params -> 
	  'a ->
	    [> xhform] elt
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


exception Ocsigen_url_created_after_init
exception Ocsigen_duplicate_registering of string
exception Ocsigen_page_erasing of string
exception Ocsigen_there_are_unregistered_url of string
exception Ocsigen_register_for_session_outside_session
exception Ocsigen_error_while_loading of string
exception Ocsigen_Is_a_directory
exception Ocsigen_404
val end_initialisation : unit -> unit
