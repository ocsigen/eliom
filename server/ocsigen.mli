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

type url_kind = [`Internal_Url of [`Public_Url | `State_Url] | `External_Url]
(** Kind of URL *)

type ('get,'post,'kind,'tipo,'getnames,'postnames) url
(** Typed URLs. The ['kind] parameter is subset of url_kind. ['get] 
and ['post] are the type of GET and POST parameters. *)

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
 - [unit] for a page without parameter. }
 - [(int "myvalue")] for a page that takes one parameter, of type [int], called [myvalue]. (You must register a function of type [int ->] {{:#TYPEpage}[page]}).
 - [(int "myvalue" ** string "mystring")] for a page that takes two parameters, one of type [int] called [myvalue], and one of type [string] called [mystring]. (The function you will register has a parameter of type [(int * string)]).
 - [list "l" (int "myvalue" ** string "mystring")] for a page that takes a list of pairs. (The function you will register has a parameter of type [(int * string) list]).

 *)

type server_params = {full_url: string;
		      current_url: current_url;
		      user_agent: string;
		      ip: Unix.inet_addr;
		      get_params: (string * string) list;
		      post_params: (string * string) list}
(** Type of server parameters *)

type ('a, 'b) binsum = Inj1 of 'a | Inj2 of 'b
(** Binary sums *)

type 'a name
(** Type for names of page parameters *)

type ('a, 'b, 'c) params_type
(** Type for parameters of a web page *)

type 'a listnames = {
  it :
    'b.
      ('a -> 'b -> form_content_l) ->
      'b list -> form_content_l -> form_content_l;
}
(** Type of the iterator used to construct forms from lists *)

val int : string -> (int, [ `WithoutSuffix ], int name) params_type
(** [int s] tells that the page take an integer as parameter, labeled [s] *)

val string :
  string -> (string, [ `WithoutSuffix ], string name) params_type
(** [string s] tells that the page take a string as parameter, labeled [s] *)

val unit : (unit, [ `WithoutSuffix ], unit name) params_type
(** used for pages that don't have any parameters *)

val user_type :
  (string -> 'a) ->
  ('a -> string) -> string -> ('a, [ `WithoutSuffix ], 'a name) params_type
(** Allows to use whatever type you want for a parameter of the page.
   [user_type s_to_t t_to_s s] tells that the page take a parameter, labeled [s], and that the server will have to use [s_to_t] and [t_to_s] to make the conversion from and to string.
 *)

val sum :
  ('a, [ `WithoutSuffix ], 'b) params_type ->
  ('a, [ `WithoutSuffix ], 'b) params_type ->
  (('a, 'a) binsum, [ `WithoutSuffix ], 'b * 'b) params_type
val prod :
  ('a, [ `WithoutSuffix ], 'b) params_type ->
  ('c, [ `WithoutSuffix ], 'd) params_type ->
  ('a * 'c, [ `WithoutSuffix ], 'b * 'd) params_type
(** See [**] above *)

val option :
  ('a, [ `WithoutSuffix ], 'b) params_type ->
  ('a option, [ `WithoutSuffix ], 'b) params_type
(** Use this if you want a parameter to be optional *)

val list :
  string ->
  ('a, [ `WithoutSuffix ], 'b) params_type ->
  ('a list, [ `WithoutSuffix ], 'b listnames) params_type
(** The page takes a list of parameters. 
   The first parameter of this function is the name of the list. *)

val ( ** ) :
  ('a, [ `WithoutSuffix ], 'b) params_type ->
  ('c, [ `WithoutSuffix ], 'd) params_type ->
  ('a * 'c, [ `WithoutSuffix ], 'b * 'd) params_type
(** This is a combinator to allow the page to take several parameters (see examples above) Warning: it is a binary operator. Pages cannot take tuples but only pairs. *)

val suffix_only : (string, [ `WithSuffix ], string name) params_type
(** Tells that the only parameter of the function that will generate the page is the suffix of the URL of the current page. (see {{:#VALregister_new_url}[register_new_url]}) *)

val suffix :
  ('a, [ `WithoutSuffix ], 'b) params_type ->
  (string * 'a, [ `WithSuffix ], string name * 'b) params_type
(** Tells that the function that will generate the page takes a pair whose first element is the suffix of the URL of the current page. (see {{:#VALregister_new_url}[register_new_url]}). e.g. [suffix (int "i" ** string "s")] *)


(** {2 Pages registration} *)

val new_url :
  path:url_path ->
  ?prefix:bool ->
  get_params:('a, [< `WithSuffix | `WithoutSuffix ] as 'b, 'c) params_type ->
  unit ->
  ('a, unit, [ `Internal_Url of [ `Public_Url ] ], 'b, 'c, unit name) url
(** [new_url ~path:p ~get_params:pa ()] creates an {{:#TYPEurl}[url]} associated to the {{:#TYPEurl_path}[url_path]} [p] and that takes the parameters [pa]. 

If you specify [~prefix:true], your URL will match all requests from client beginning by [path]. You can have access to the suffix of the URL using {{:VALsuffix}[suffix]} or {{:VALsuffix_only}[suffix_only]}. For example [new_url ["mysite";"mywiki"] ~prefix:true suffix_only] will match all the URL of the shape [http://myserver/mysite/mywiki/thesuffix]*)

val new_external_url :
  path:url_path ->
  ?prefix:bool ->
  get_params:('a, [< `WithSuffix | `WithoutSuffix ] as 'b, 'c) params_type ->
  post_params:('d, [ `WithoutSuffix ], 'e) params_type ->
  unit -> ('a, 'd, [ `External_Url ], 'b, 'c, 'e) url
(** Creates an URL for an external web site *)

val new_state_url :
  fallback:('a, unit, [ `Internal_Url of [ `Public_Url ] ], 'b, 'c, 'd) url ->
  ('a, unit, [ `Internal_Url of [ `State_Url ] ], 'b, 'c, 'd) url
(** Creates another version of an already existing URL, where you can register another treatment. The two versions are automatically distinguished thanks to an extra parameter. It allows to have several links towards the same page, that will behave differently. See the tutorial for more informations.*)

val register_url :
  url:('a, 'b, [ `Internal_Url of 'c ], [< `WithSuffix | `WithoutSuffix ],
       'd, 'e)
      url ->
  (server_params -> 'a -> 'b -> page) -> unit
(** Register an url in the global table of the server 
   with the associated generation function.
   [register_url url t f] will associate the url [url] to the function [f].
   [f] is the function that creates a page. 
   It takes three parameters. The first one has type [server_params]
   and allows to have acces to informations about the request.
   The second and third ones are respectively GET and POST parameters.
   For example if [t] is (int "s"), then ['a] is int.
 *)

val register_url_for_session :
  url:('a, 'b, [ `Internal_Url of 'c ], [< `WithSuffix | `WithoutSuffix ],
       'd, 'e)
      url ->
  (server_params -> 'a -> 'b -> page) -> unit
(** Registers an url and the associated function in the session table.
   If the same client does a request to this url, this function will be
   used instead of the one from the global table.

   Warning:
   - All URL must be registered in the global table during initialisation,
   but never after,
   - You (obviously) can't register an url in a session table 
   when no session is active
 *)


val register_new_url :
  path:url_path ->
  ?prefix:bool ->
  get_params:('a, [< `WithSuffix | `WithoutSuffix ] as 'b, 'c) params_type ->
  (server_params -> 'a -> unit -> page) ->
  ('a, unit, [ `Internal_Url of [ `Public_Url ] ], 'b, 'c, unit name) url
(** Same as [new_url] followed by [register_url] *)

val register_new_state_url :
  fallback:('a, unit, [ `Internal_Url of [ `Public_Url ] ],
            [< `WithSuffix | `WithoutSuffix ] as 'b, 'c, 'd)
           url ->
  (server_params -> 'a -> unit -> page) ->
  ('a, unit, [ `Internal_Url of [ `State_Url ] ], 'b, 'c, 'd) url
(** Same as [new_state_url] followed by [register_url] *)

val register_new_state_url_for_session :
  fallback:('a, unit, [ `Internal_Url of [ `Public_Url ] ],
            [< `WithSuffix | `WithoutSuffix ] as 'b, 'c, 'd)
           url ->
  (server_params -> 'a -> unit -> page) ->
  ('a, unit, [ `Internal_Url of [ `State_Url ] ], 'b, 'c, 'd) url
(** Same as [new_state_url] followed by [register_url_for_session] *)

val new_post_url :
  fallback:('a, unit, [ `Internal_Url of [ `Public_Url ] ], 'b, 'c,
            unit name)
           url ->
  post_params:('d, [ `WithoutSuffix ], 'e) params_type ->
  ('a, 'd, [ `Internal_Url of [ `Public_Url ] ], 'b, 'c, 'e) url
(** Creates an URL that takes POST parameters. 
   [fallback] is the same URL without POST parameters.
   You can create an URL with POST parameters if the same URL does not exist
   without POST parameters. Thus, the user can't bookmark a page that does not
   exist.
 *)

val new_post_state_url :
  fallback:('a, 'b, [ `Internal_Url of [ `Public_Url ] ], 'c, 'd, 'e) url ->
  post_params:('f, [ `WithoutSuffix ], 'g) params_type ->
  ('a, 'f, [ `Internal_Url of [ `State_Url ] ], 'c, 'd, 'g) url
(** Creates a state URL with POST parameters *)

val register_new_post_url :
  fallback:('a, unit, [ `Internal_Url of [ `Public_Url ] ],
            [< `WithSuffix | `WithoutSuffix ] as 'b, 'c, unit name)
           url ->
  post_params:('d, [ `WithoutSuffix ], 'e) params_type ->
  (server_params -> 'a -> 'd -> page) ->
  ('a, 'd, [ `Internal_Url of [ `Public_Url ] ], 'b, 'c, 'e) url
(** Same as [new_post_url] followed by [register_post_url] *)

val register_new_post_state_url :
  fallback:('a, 'b, [ `Internal_Url of [ `Public_Url ] ],
            [< `WithSuffix | `WithoutSuffix ] as 'c, 'd, 'e)
           url ->
  post_params:('f, [ `WithoutSuffix ], 'g) params_type ->
  (server_params -> 'a -> 'f -> page) ->
  ('a, 'f, [ `Internal_Url of [ `State_Url ] ], 'c, 'd, 'g) url
(** Same as [new_post_state_url] followed by [register_post_url] *)

val register_new_post_state_url_for_session :
  fallback:('a, 'b, [ `Internal_Url of [ `Public_Url ] ],
            [< `WithSuffix | `WithoutSuffix ] as 'c, 'd, 'e)
           url ->
  post_params:('f, [ `WithoutSuffix ], 'g) params_type ->
  (server_params -> 'a -> 'f -> page) ->
  ('a, 'f, [ `Internal_Url of [ `State_Url ] ], 'c, 'd, 'g) url
(** Same as [new_post_state_url] followed by [register_post_url_for_session] *)

val static_dir :
  (string, unit, [ `Internal_Url of [ `Public_Url ] ], [ `WithSuffix ],
   string name, unit name)
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
  post_params:('a, [ `WithoutSuffix ], 'b) params_type -> ('a, 'b) actionurl
(** Creates an action *)

val register_actionurl :
  actionurl:('a, 'b) actionurl ->
  action:(server_params -> 'a -> unit) -> unit
(** Register an action in the global table *)

val register_actionurl_for_session :
  actionurl:('a, 'b) actionurl ->
  action:(server_params -> 'a -> unit) -> unit
(** Register an action in the session table *)

val register_new_actionurl :
  params:('a, [ `WithoutSuffix ], 'b) params_type ->
  action:(server_params -> 'a -> unit) -> ('a, 'b) actionurl
(** Same as [new_actionurl] followed by [register_actionurl] *)

val register_new_actionurl_for_session :
  params:('a, [ `WithoutSuffix ], 'b) params_type ->
  action:(server_params -> 'a -> unit) -> ('a, 'b) actionurl
(** Same as [new_actionurl] followed by [register_actionurl_for_session] *)


(** {2 Creating links, forms, etc.} *)

val a : ?a:([< a_attrib > `Href ] attrib list) ->
  ('get, unit, 'b, [< `WithSuffix | `WithoutSuffix ], 'c, unit name) url ->
    current_url -> 
      a_content elt list -> 'get -> [>a] XHTML.M.elt
(** [a url current cont ()] creates a link from [current] to [url]. The text of
   the link is [cont]. For example [cont] may be something like
   [\[pcdata "click here"\]]. To know the current URL (for [current]),
   use {{:#VAL_current_url}_current_url}.

   The last  parameter is for GET parameters.
   For example [a url current cont (42,"hello")]

 The [~a] optional parameter is used for extra attributes 
   (see the module XHTML.M) *)

val css_link : ?a:([< link_attrib > `Href `Rel `Type ] attrib list) ->
  uri -> [> link ] elt
(** Creates a [<link>] tag for a Cascading StyleSheet (CSS). *)

val js_script : ?a:([< script_attrib > `Src ] attrib list) ->
  uri -> [> script ] elt
(** Creates a [<script>] tag to add a javascript file *)

val make_uri :
  ('a, unit, 'b, [< `WithSuffix | `WithoutSuffix ], 'c, 'd) url ->
  current_url -> 'a -> uri
(** Create the text of the URL. Like the [a] function, it may take
   extra parameters. *)


(** Link a registrated URL with the function that takes the url and
    names of the parameters, and creates a form for these parameters
*)
val get_form : ?a:([< form_attrib > `Method ] attrib list) ->
  ('get, unit, 'c, 'd, 'getnames, unit name) url ->
  current_url -> ('getnames -> form_content_l) -> [>form] elt
(** [get_form url current formgen] creates a GET form from [current] to [url]. 
   The content of
   the form is generated by the function [formgen], that takes the names
   of page parameters as parameters. *)

val post_form : ?a:([< form_attrib > `Method ] attrib list) ->
  ('get, 'post, 'c, [< `WithSuffix | `WithoutSuffix ], 'getnames, 'postnames) url ->
  current_url ->
  ('postnames -> form_content_l) -> 'get -> [>form] elt
(** [post_form url current formgen] creates a POST form from [current] 
   to [url]. The last parameter is for GET parameters (as in the function [a]).
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
    ('a,'b) actionurl -> 
      server_params -> 
	a_content elt list -> 
	  [> form] elt
(** Creates a link that will perform an action (see {{:#TYPEactionurl}[actionurl]}).
   If [~reload:false] is specified, the current page will not be reloaded.
 *)

val action_form : ?a:([< form_attrib > `Method ] attrib list) ->
    ?reload:bool ->
      ('a, 'b) actionurl ->
	server_params -> 
	  ('b -> form_content_l) ->
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

