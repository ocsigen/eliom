(* Ocsigen
 * http://www.ocsigen.org
 * Module eliom.mli
 * Copyright (C) 2007 Vincent Balat
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception; 
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)


(** Eliom.ml defines the functions you need to interact with the
   Eliommod module:
   - To create the "typed services" and associate them to a path (directories/name)
   - To specify the types and names of the parameters of this service
   - To register the functions that generate pages for each of these services
   - To create links or forms etc.
 *)

open XHTML.M
open Xhtmltypes
open Extensions


exception Eliom_Link_too_old
exception Eliom_Session_expired
exception Eliom_Wrong_parameter
exception Eliom_Typing_Error of (string * exn) list


(** Allows extensions of the configuration file for your modules *)
val get_config : unit -> Simplexmlparser.xml list
(** Put your options between <eliom ...> and </eliom> *)

(** This function may be used for services that can not be interrupted
  (no cooperation point for threads). It is defined by
  [let sync f sp g p = Lwt.return (f sp g p)]
 *)
val sync : ('a -> 'b -> 'c -> 'd) -> ('a -> 'b -> 'c -> 'd Lwt.t)

(** {2 Types} *)

type suff = [ `WithSuffix | `WithoutSuffix ]

type servcoserv = [ `Service | `Coservice ]
type getpost = [ `Get | `Post ]
      (* `Post means that there is at least one post param
         (possibly only the state post param).
         `Get is for all the other cases.
       *)
type attached_service_kind = 
    [ `Internal of servcoserv * getpost
    | `External]

type get_attached_service_kind = 
    [ `Internal of servcoserv * [ `Get ]
    | `External ]

type post_attached_service_kind = 
    [ `Internal of servcoserv * [ `Post ]
    | `External ]

type internal = 
    [ `Internal of servcoserv * getpost ]

type registrable = [ `Registrable | `Unregistrable ]
(** You can call register function only on registrable services *)
(* Registrable means not pre-applied *)

type +'a a_s
      
type +'a na_s

type service_kind =
    [ `Attached of attached_service_kind a_s
  | `Nonattached of getpost na_s ]

type get_service_kind =
    [ `Attached of get_attached_service_kind a_s
  | `Nonattached of [ `Get ] na_s ]

type post_service_kind =
    [ `Attached of post_attached_service_kind a_s
  | `Nonattached of [ `Post ] na_s ]

type internal_service_kind =
    [ `Attached of internal a_s
  | `Nonattached of getpost na_s ]

type attached =
    [ `Attached of attached_service_kind a_s ]

type nonattached =
    [ `Nonattached of getpost na_s ]

type ('get,'post,+'kind,+'tipo,+'getnames,+'postnames,+'registr) service
(** Typed services. The ['kind] parameter is a subset of service_kind. ['get] 
   and ['post] are the type of GET and POST parameters. *)


type url_path = string list

val string_of_url_path : url_path -> string
(** This type is used to represent URL paths; For example the path [coucou/ciao] is represented by the list [\["coucou";"ciao"\]] *)

(** {2 Server parameters} *)

type server_params
(** Type of server parameters *)

val get_user_agent : server_params -> string
val get_full_url : server_params -> string
val get_ip : server_params -> string
val get_inet_addr : server_params -> Unix.inet_addr

(** All GET parameters in the URL *)
val get_all_get_params : server_params -> (string * string) list

(** Only GET parameters concerning that page *)
val get_get_params : server_params -> (string * string) list

(** All POST parameters in the request *)
val get_all_post_params : server_params -> (string * string) list

(** Only POST parameters concerning that page *)
val get_post_params : server_params -> (string * string) list Lwt.t

val get_current_path_string : server_params -> string
val get_current_path : server_params -> url_path
val get_hostname : server_params -> string option
val get_port : server_params -> int
val get_other_get_params : server_params -> (string * string) list
val get_suffix : server_params -> url_path
val get_exn : server_params -> exn list
val get_config_file_charset : server_params -> string option
val get_cookies : server_params -> (string * string) list

val set_user_timeout : server_params -> float option -> unit
val unset_user_timeout : server_params -> unit
val get_user_timeout : server_params -> float option

val set_user_persistent_timeout : server_params -> float option -> unit
val unset_user_persistent_timeout : server_params -> unit
val get_user_persistent_timeout : server_params -> float option

(** Setting and getting cookie expiration date for the session. None means the cookie will expire when the browser is closed. *)
val set_user_expdate : server_params -> float option -> unit
val get_user_expdate : server_params -> float option

val set_user_persistent_expdate : server_params -> float option -> unit
val get_user_persistent_expdate : server_params -> float option

(** Setting and getting timeout for the session (server side). 
    The session will be closed after this amount of time of inactivity 
    from the user. 
    If you use these function after the initialisation phase,
    you must give the [~sp] parameter.
*)
val set_global_timeout : ?sp:server_params -> float option -> unit
val get_global_timeout : ?sp:server_params -> unit -> float option
val get_default_timeout : unit -> float option
val set_global_persistent_timeout : ?sp:server_params -> float option -> unit
val get_global_persistent_timeout : ?sp:server_params -> unit -> float option
val get_default_persistent_timeout : unit -> float option

(** Use your own error pages (404, or any exception during page generation) *)
val set_exn_handler : 
    ?sp:server_params ->
      (server_params -> exn -> Eliommod.result_to_send Lwt.t) -> unit

(** Type of files *)
val get_tmp_filename : file_info -> string
val get_filesize : file_info -> int64
val get_original_filename : file_info -> string

(** {2 Types of pages parameters} *)

(** Here are some examples of how to specify the types and names of pages parameters:
   - [unit] for a page without parameter.
   - [(int "myvalue")] for a page that takes one parameter, of type [int], called [myvalue]. (You must register a function of type [int ->] {{:#TYPEpage}[page]}).
   - [(int "myvalue" ** string "mystring")] for a page that takes two parameters, one of type [int] called [myvalue], and one of type [string] called [mystring]. (The function you will register has a parameter of type [(int * string)]).
   - [list "l" (int "myvalue" ** string "mystring")] for a page that takes a list of pairs. (The function you will register has a parameter of type [(int * string) list]).

 *)

type ('a, 'b) binsum = Inj1 of 'a | Inj2 of 'b
(** Binary sums *)

type 'a param_name
(** Type for names of page parameters *)

type ('a, 'b, 'c) params_type
(** Type for parameters of a web page *)

type 'an listnames =
    {it:'el 'a. ('an -> 'el -> 'a list) -> 'el list -> 'a list -> 'a list}
(** Type of the iterator used to construct forms from lists *)

type 'a setoneopt = [ `Set of 'a | `One of 'a | `Opt of 'a ]
type 'a oneopt = [ `One of 'a | `Opt of 'a ]
type 'a setone = [ `Set of 'a | `One of 'a ]

val int : string -> (int, [ `WithoutSuffix ], [ `One of int ] param_name) params_type
(** [int s] tells that the page takes an integer as parameter, labeled [s] *)

val float : string -> (float, [ `WithoutSuffix ], [ `One of float ] param_name) params_type
(** [float s] tells that the page takes a floating point number as parameter, labeled [s] *)

val string :
    string -> (string, [ `WithoutSuffix ], [ `One of string ] param_name) params_type
(** [string s] tells that the page takes a string as parameter, labeled [s] *)

val bool :
    string -> (bool, [ `WithoutSuffix ], [ `One of bool ] param_name) params_type
(** [bool s] tells that the page takes a boolean as parameter, labeled [s]
   (to use for example with boolean checkboxes) *)

val file :
    string -> (file_info, [ `WithoutSuffix ], 
               [ `One of file_info ] param_name) params_type
(** [file s] tells that the page takes a file as parameter, labeled [s] *)

val radio_answer :
    string -> (string option, [ `WithoutSuffix ], 
               [ `Opt of string ] param_name) params_type
(** [radio_answer s] tells that the page takes the result of a click on
   a radio button as parameter. *)

val unit : (unit, [ `WithoutSuffix ], unit) params_type
(** used for pages that don't have any parameters *)

val user_type :
    (string -> 'a) ->
      ('a -> string) -> string -> 
        ('a, [ `WithoutSuffix ], [ `One of 'a ] param_name) params_type
(** Allows to use whatever type you want for a parameter of the page.
   [user_type s_to_t t_to_s s] tells that the page take a parameter, labeled [s], and that the server will have to use [s_to_t] and [t_to_s] to make the conversion from and to string.
 *)

(** [coordinates] is for the data sent by an [<input type="image" ...>]. *)
type coordinates =
    {abscissa: int;
     ordinate: int}

val coordinates :
    string ->
      (coordinates, [`WithoutSuffix], 
       [ `One of coordinates ] param_name) params_type

val string_coordinates :
    string ->
      (string * coordinates, [`WithoutSuffix], 
       [ `One of (string * coordinates) ] param_name) params_type
(** It is possible to send a value together with the coordinates
   ([<input type="image" value="..." ...>]) (Here a string) *)

val int_coordinates :
    string ->
      (int * coordinates, [`WithoutSuffix], 
       [ `One of (int * coordinates) ] param_name) params_type
(** Same for an integer value *)
        
val float_coordinates :
    string ->
      (float * coordinates, [`WithoutSuffix], 
       [ `One of (float * coordinates) ] param_name) params_type
(** Same for a float value *)
        
val user_type_coordinates :
    (string -> 'a) -> ('a -> string) -> string ->
      ('a * coordinates, [`WithoutSuffix], 
       [ `One of ('a * coordinates) ] param_name) params_type
(** Same for a value of your own type *)


val sum :
    ('a, [ `WithoutSuffix ], 'b) params_type ->
      ('a, [ `WithoutSuffix ], 'b) params_type ->
        (('a, 'a) binsum, [ `WithoutSuffix ], 'b * 'b) params_type
val prod :
    ('a, [ `WithoutSuffix ], 'b) params_type ->
      ('c, [ `WithoutSuffix | `Endsuffix ], 'd) params_type ->
        ('a * 'c, [ `WithoutSuffix ], 'b * 'd) params_type
(** See [**] above *)

val opt :
    ('a, [ `WithoutSuffix ], [ `One of 'b ] param_name) params_type ->
      ('a option, [ `WithoutSuffix ], [ `Opt of 'b ] param_name) params_type
(** Use this if you want a parameter to be optional *)

val any :
      ((string * string) list, [ `WithoutSuffix ], unit) params_type
(** Use this if you want to take any parameters *)

val set :
    (string ->
      ('a, [ `WithoutSuffix ], [ `One of 'b ] param_name) params_type) ->
        string ->
          ('a list, [ `WithoutSuffix ], [ `Set of 'b ] param_name) params_type
(** Use this if you want to take a set of parameters all with the same name *)

val list :
    string ->
      ('a, [ `WithoutSuffix ], 'b) params_type ->
        ('a list, [ `WithoutSuffix ], 'b listnames) params_type
(** The page takes a list of parameters. 
   The first parameter of this function is the name of the list. *)

val ( ** ) :
    ('a, [ `WithoutSuffix ], 'b) params_type ->
      ('c, [< `WithoutSuffix | `Endsuffix ], 'd) params_type ->
        ('a * 'c, [ `WithoutSuffix ], 'b * 'd) params_type
(** This is a combinator to allow the page to take several parameters (see examples above) Warning: it is a binary operator. Pages cannot take tuples but only pairs. *)

val regexp :
    Netstring_pcre.regexp -> string -> string ->
      (string, [ `WithoutSuffix ], 
       [` One of string ] param_name) params_type
(** [regexp r d s] tells that the page takes a string that matches [r]
   as parameter, labeled [s], and that will be rewritten in d *)

val suffix : 
    ('s, [< `WithoutSuffix | `Endsuffix ], 'sn) params_type ->
      ('s, [ `WithSuffix ], 'sn) params_type
(** Tells that the only parameter of the function that will generate the page is the suffix of the URL of the current page. (see {{:#VALregister_new_service}[register_new_service]}) *)

val all_suffix :
    string ->
      (string list, [`Endsuffix], [` One of string list ] param_name) params_type
(** Takes all the suffix, as long as possible, as a string list *)

val all_suffix_string :
    string -> (string, [`Endsuffix], [` One of string ] param_name) params_type
(** Takes all the suffix, as long as possible, as a string *)

val all_suffix_user :
    (string -> 'a) ->
      ('a -> string) -> string -> 
        ('a, [ `Endsuffix ], [` One of 'a ] param_name) params_type
(** Takes all the suffix, as long as possible, 
   with a type specified by the user *)

val all_suffix_regexp :
    Netstring_pcre.regexp -> string -> string ->
      (string, [ `Endsuffix ], [` One of string ] param_name) params_type
(** Takes all the suffix, as long as possible, matching the regexp *)

val suffix_prod :
    ('s,[<`WithoutSuffix|`Endsuffix],'sn) params_type ->
      ('a,[`WithoutSuffix], 'an) params_type ->
        (('s * 'a), [`WithSuffix], 'sn * 'an) params_type 
(** Tells that the function that will generate the page takes a pair whose first element is the suffix of the URL of the current page. (see {{:#VALregister_new_service}[register_new_service]}). e.g. [suffix (int "i" ** string "s")] *)







(***** Static dir and actions do not depend on the type of pages ******)

(** {2 Misc} *)


val static_dir :
    server_params -> 
      (string list, unit, [> `Attached of 
        [> `Internal of [> `Service ] * [> `Get] ] a_s ],
       [ `WithSuffix ],
       [ `One of string ] param_name, unit, [> `Unregistrable ])
        service
(** The service that correponds to the directory where static pages are.
   This directory is chosen in the config file (ocsigen.conf).
   This service takes the name of the static file as a parameter.
 *)

    

(** {2 Definitions of entry points (services/URLs)} *)


val new_service :
    ?sp: server_params ->
    url:url_path ->
        get_params:('get, [< suff ] as 'tipo,'gn)
          params_type ->
            unit ->
              ('get,unit,
               [> `Attached of 
                 [> `Internal of [> `Service ] * [>`Get] ] a_s ],
               'tipo,'gn, 
               unit, [> `Registrable ]) service
(** [new_service ~url:p ~get_params:pa ()] creates an {{:#TYPEservice}[service]} associated to the {{:#TYPEurl_path}[url_path]} [p] and that takes the parameters [pa]. 
   
   If you specify [~suffix:true], your service will match all requests from client beginning by [path]. You can have access to the suffix of the URL using {{:VALsuffix}[suffix]} or {{:VALsuffix_only}[suffix_only]}. For example [new_service ["mysite";"mywiki"] ~suffix:true suffix_only] will match all the URL of the shape [http://myserver/mysite/mywiki/thesuffix]

   If you want to create dynamically a new service during session,
   you must add the [~sp] parameter (current server parameters)
*)
	      
val new_external_service :
    url:url_path ->
        get_params:('get, [< suff ] as 'tipo, 'gn) params_type ->
          post_params:('post, [ `WithoutSuffix ], 'pn) params_type ->
            unit -> 
              ('get, 'post, [> `Attached of [> `External ] a_s ], 'tipo, 
               'gn, 'pn, [> `Unregistrable ]) service
(** Creates an service for an external web site *)
		
val new_coservice :
    ?max_use:int ->
    ?timeout:float ->
    fallback: 
    (unit, unit, [ `Attached of [ `Internal of [ `Service ] * [`Get]] a_s ],
     [`WithoutSuffix] as 'tipo,
     unit, unit, [< registrable ]) service ->
       get_params: 
         ('get,[`WithoutSuffix],'gn) params_type ->
           unit ->
             ('get,unit,[> `Attached of 
               [> `Internal of [> `Coservice] * [> `Get]] a_s ],
              'tipo, 'gn, unit, 
              [> `Registrable ]) service
(** Creates another version of an already existing service, where you can register another treatment. The two versions are automatically distinguished thanks to an extra parameter. It allows to have several links towards the same page, that will behave differently. See the tutorial for more informations.*)

val new_coservice' :
    ?max_use:int ->
    ?timeout:float ->
    get_params: 
    ('get,[`WithoutSuffix],'gn) params_type ->
      unit ->
        ('get, unit, [> `Nonattached of [> `Get] na_s ],
         [`WithoutSuffix], 'gn, unit, [> `Registrable ]) service
(** Creates a non-attached coservice. Links towards such services will not change the URL, just add extra parameters. *)
        
val new_post_service :
    ?sp: server_params ->
    fallback: ('get, unit, 
               [`Attached of [`Internal of 
                 ([ `Service | `Coservice ] as 'kind) * [`Get]] a_s ],
               [< suff] as 'tipo, 'gn, unit, 
               [< `Registrable ]) service ->
                 post_params: ('post,[`WithoutSuffix],'pn) params_type ->
                   unit ->
                     ('get, 'post, [> `Attached of 
                       [> `Internal of 'kind * [> `Post]] a_s ],
                      'tipo, 'gn, 'pn, [> `Registrable ]) service
(** Creates an service that takes POST parameters. 
   [fallback] is the same service without POST parameters.
   You can create an service with POST parameters if the same service does not exist
   without POST parameters. Thus, the user can't bookmark a page that does not
   exist.
 *)
(* fallback must be registrable! (= not preapplied) *)
	  
val new_post_coservice :
    ?max_use:int ->
    ?timeout:float ->
    fallback: ('get, unit, [ `Attached of 
      [`Internal of [<`Service | `Coservice] * [`Get]] a_s ],
               [< suff ] as 'tipo,
               'gn, unit, [< `Registrable ]) service ->
                 post_params: ('post,[`WithoutSuffix],'pn) params_type ->
                   unit ->
                     ('get, 'post, 
                      [> `Attached of 
                        [> `Internal of [> `Coservice] * [> `Post]] a_s ],
                      'tipo, 'gn, 'pn, [> `Registrable ]) service
(** Creates a coservice with POST parameters *)

val new_post_coservice' :
    ?max_use:int ->
    ?timeout:float ->
    post_params: ('post,[`WithoutSuffix],'pn) params_type ->
      unit ->
        (unit, 'post, 
         [> `Nonattached of [> `Post] na_s ],
         [ `WithoutSuffix ], unit, 'pn, [> `Registrable ]) service
(** Creates a non attached coservice with POST parameters *)

(*
val new_get_post_coservice' :
    ?max_use:int ->
    ?timeout:float ->
   fallback: ('get, unit, [`Nonattached of [`Get] na_s ],
   [< suff ] as 'tipo,
   'gn, unit, [< `Registrable ]) service ->
   post_params: ('post,[`WithoutSuffix],'pn) params_type ->
   unit ->
   ('get, 'post, 
   [> `Nonattached of [> `Post] na_s ],
   'tipo,'gn,'pn, [> `Registrable ]) service
(* * Creates a non-attached coservice with GET and POST parameters. The fallback is a non-attached coservice with GET parameters. *)
*)

(** Preapplied services are created from a service and its GET parameters.
   It is not possible to register something on an preapplied service.
   Preapplied services may be used in links or as fallbacks for coservices *)
val preapply :
    ('a, 'b, [> `Attached of 'd a_s ] as 'c,
     [< suff ], 'e, 'f, 'g)
    service ->
      'a -> 
        (unit, 'b, 'c, 
         [ `WithoutSuffix ], unit, 'f, [ `Unregistrable ]) service
 

val make_string_uri :
    ('get, unit, [< get_service_kind ],
     [< suff ], 'gn, unit, 
     [< registrable ]) service ->
       server_params -> 'get -> string



(** {2 Creating modules to register services for one type of pages} *)

module type REGCREATE = 
  sig
    type page

    val send : 
        ?cookies:cookieslist -> 
          ?charset:string ->
            ?code:int ->
              server_params -> page -> Eliommod.result_to_send

  end


module type FORMCREATE = 
  sig
    type form_content_elt
    type form_content_elt_list
    type form_elt
    type a_content_elt
    type a_content_elt_list
    type a_elt
    type a_elt_list
    type div_content_elt
    type div_content_elt_list
    type uri
    type link_elt
    type script_elt
    type textarea_elt
    type input_elt
    type pcdata_elt
    type select_elt
    type select_content_elt
    type select_content_elt_list
    type option_elt
    type option_elt_list
    type button_elt
    type button_content_elt
    type button_content_elt_list

    type a_attrib_t
    type form_attrib_t
    type input_attrib_t
    type textarea_attrib_t
    type select_attrib_t
    type link_attrib_t
    type script_attrib_t
    type optgroup_attrib_t
    type option_attrib_t
    type button_attrib_t


    type input_type_t
    type button_type_t


        

    val hidden : input_type_t
    val checkbox : input_type_t
    val radio : input_type_t
    val submit : input_type_t
    val file : input_type_t
    val image : input_type_t

    val buttonsubmit : button_type_t

    val empty_seq : form_content_elt_list
    val cons_form : 
        form_content_elt -> form_content_elt_list -> form_content_elt_list 
    val map_option :
        ('a -> option_elt) -> 'a list -> 
          option_elt_list
    val map_optgroup :
        ('a -> select_content_elt) -> 'a -> 'a list -> 
          (select_content_elt * select_content_elt_list)
    val select_content_of_option : option_elt -> select_content_elt

    val make_pcdata : string -> pcdata_elt
    val make_a : ?a:a_attrib_t -> href:string -> a_content_elt_list -> a_elt
    val make_get_form : ?a:form_attrib_t -> 
      action:string -> 
        form_content_elt -> form_content_elt_list -> form_elt
    val make_post_form : ?a:form_attrib_t ->
      action:string -> ?id:string -> ?inline:bool -> 
        form_content_elt -> form_content_elt_list -> form_elt
    val make_hidden_field : input_elt -> form_content_elt
    val remove_first : 
        form_content_elt_list -> form_content_elt * form_content_elt_list
    val make_input : ?a:input_attrib_t -> ?checked:bool ->
      typ:input_type_t -> ?name:string -> ?src:uri ->
        ?value:string -> unit -> input_elt
    val make_button : ?a:button_attrib_t -> button_type:button_type_t ->
      ?name:string -> ?value:string ->
        button_content_elt_list -> button_elt
    val make_textarea : 
        ?a:textarea_attrib_t -> 
          name:string -> ?value:pcdata_elt -> rows:int -> cols:int ->
            unit -> textarea_elt
    val make_select :
        ?a:select_attrib_t ->
          multiple:bool ->
            name:string ->
              select_content_elt ->
                select_content_elt_list ->
                  select_elt
    val make_option : 
        ?a:option_attrib_t ->
          selected:bool ->
            ?value:string ->
              pcdata_elt ->
                option_elt
    val make_optgroup :
        ?a:optgroup_attrib_t ->
          label:string ->
            option_elt ->
              option_elt_list ->
                select_content_elt
    val uri_of_string : string -> uri


    val make_css_link : ?a:link_attrib_t -> uri -> link_elt

    val make_js_script : ?a:script_attrib_t -> uri -> script_elt


  end

module type ELIOMFORMSIG =
  sig




    type form_content_elt
    type form_content_elt_list
    type form_elt
    type a_content_elt
    type a_content_elt_list
    type a_elt
    type a_elt_list
    type div_content_elt
    type div_content_elt_list
    type uri
    type link_elt
    type script_elt
    type textarea_elt
    type input_elt
    type pcdata_elt
    type select_elt
    type select_content_elt
    type select_content_elt_list
    type button_elt
    type button_content_elt
    type button_content_elt_list
          
    type a_attrib_t
    type form_attrib_t
    type input_attrib_t
    type textarea_attrib_t
    type select_attrib_t
    type link_attrib_t
    type script_attrib_t
    type optgroup_attrib_t
    type option_attrib_t
    type button_attrib_t

    type input_type_t
    type button_type_t

    val a :
        ?a:a_attrib_t ->
          ('get, unit, [< get_service_kind ], 
           [< suff ], 'gn, 'pn,
           [< registrable ]) service ->
            server_params -> a_content_elt_list -> 'get -> a_elt
    val get_form :
        ?a:form_attrib_t ->
          ('get, unit, [< get_service_kind ],
           [<suff ], 'gn, 'pn, 
           [< registrable ]) service ->
             server_params ->
              ('gn -> form_content_elt_list) -> form_elt
    val post_form :
        ?a:form_attrib_t ->
          ('get, 'post, [< post_service_kind ],
           [< suff ], 'gn, 'pn, 
           [< registrable ]) service ->
            server_params ->
              ('pn -> form_content_elt_list) -> 'get -> form_elt
    val make_uri :
        ('get, unit, [< get_service_kind ],
         [< suff ], 'gn, 'pn, 
         [< registrable ]) service ->
          server_params -> 'get -> uri

    val js_script :
        ?a:script_attrib_t -> uri -> script_elt
    val css_link : ?a:link_attrib_t -> uri -> link_elt


    val int_input :
        ?a:input_attrib_t -> input_type:input_type_t ->
          ?name:[< int setoneopt ] param_name ->
            ?value:int -> unit -> input_elt
    val float_input :
        ?a:input_attrib_t -> input_type:input_type_t ->
          ?name:[< float setoneopt ] param_name ->
            ?value:float -> unit -> input_elt
    val string_input :
        ?a:input_attrib_t -> input_type:input_type_t ->
           ?name:[< string setoneopt ] param_name -> 
             ?value:string -> unit -> input_elt
    val user_type_input :
        ?a:input_attrib_t -> input_type:input_type_t ->
          ?name:[< 'a setoneopt ] param_name -> 
            ?value:'a -> ('a -> string) -> input_elt
    val any_input :
        ?a:input_attrib_t -> input_type:input_type_t ->
          ?name:string -> ?value:string -> unit -> input_elt

    val file_input :
        ?a:input_attrib_t -> 
          name:[< file_info setoneopt ] param_name -> 
            unit -> input_elt

    val image_input :
        ?a:input_attrib_t -> 
          name:[< coordinates oneopt ] param_name -> 
          ?src:uri -> unit -> input_elt

    val int_image_input :
        ?a:input_attrib_t -> 
          name:[< (int * coordinates) oneopt ] param_name -> value:int -> 
            ?src:uri -> unit -> input_elt
    val float_image_input :
        ?a:input_attrib_t -> 
          name:[< (float * coordinates) oneopt ] param_name -> value:float -> 
            ?src:uri -> unit -> input_elt
    val string_image_input :
        ?a:input_attrib_t -> 
          name:[< (string * coordinates) oneopt ] param_name -> value:string -> 
            ?src:uri -> unit -> input_elt
    val user_type_image_input :
        ?a:input_attrib_t -> 
          name:[< ('a * coordinates) oneopt ] param_name -> value:'a -> 
            ?src:uri -> ('a -> string) -> input_elt
    val any_image_input :
        ?a:input_attrib_t -> 
          name:string -> value:string -> ?src:uri -> unit -> input_elt


    val bool_checkbox :
        ?a:input_attrib_t -> ?checked:bool -> 
          name:[ `One of bool ] param_name -> unit -> input_elt

    val int_checkbox :
        ?a:input_attrib_t -> ?checked:bool -> 
          name:[ `Set of int ] param_name -> value:int -> unit -> input_elt

    val float_checkbox :
        ?a:input_attrib_t -> ?checked:bool -> 
          name:[ `Set of float ] param_name -> value:float -> unit -> input_elt

    val string_checkbox :
        ?a:input_attrib_t -> ?checked:bool -> 
          name:[ `Set of string ] param_name -> value:string -> 
            unit -> input_elt

    val user_type_checkbox :
        ?a:input_attrib_t -> ?checked:bool -> 
          name:[ `Set of 'a ] param_name -> value:'a -> 
            ('a -> string) -> input_elt

    val any_checkbox :
        ?a:input_attrib_t -> ?checked:bool -> 
          name:string -> value:string -> unit -> input_elt


    val string_radio :
        ?a:input_attrib_t -> ?checked:bool -> 
          name:[ `Opt of string ] param_name -> 
            value:string -> unit -> input_elt
    val int_radio :
        ?a:input_attrib_t -> ?checked:bool -> 
           name:[ `Opt of int ] option param_name -> 
             value:int -> unit -> input_elt
    val float_radio :
        ?a:input_attrib_t -> ?checked:bool -> 
           name:[ `Opt of float ] param_name -> 
             value:float -> unit -> input_elt
    val user_type_radio :
        ?a:input_attrib_t -> ?checked:bool ->
           name:[ `Opt of 'a ] param_name -> 
             value:'a -> ('a -> string) -> input_elt
    val any_radio :
        ?a:input_attrib_t -> ?checked:bool -> 
          name:string -> value:string -> unit -> input_elt


    val string_button : 
        ?a:button_attrib_t -> 
          name:[< string setone ] param_name -> value:string -> 
            button_content_elt_list -> button_elt

    val int_button : 
        ?a:button_attrib_t ->
          name:[< int setone ] param_name -> value:int -> 
            button_content_elt_list -> button_elt

    val float_button : 
        ?a:button_attrib_t ->
          name:[< float setone ] param_name -> value:float -> 
            button_content_elt_list -> button_elt

    val user_type_button : 
        ?a:button_attrib_t ->
          name:[< 'a setone ] param_name -> value:'a -> ('a -> string) ->
            button_content_elt_list -> button_elt

    val any_button :
        ?a:button_attrib_t ->
          button_type:button_type_t ->
            name:string -> value:string -> 
              button_content_elt_list -> button_elt

    val button : 
        ?a:button_attrib_t ->
          button_type:button_type_t ->
            button_content_elt_list -> button_elt




    val textarea :
        ?a:textarea_attrib_t ->
          name:[< string setoneopt ] param_name -> ?value:pcdata_elt -> 
            rows:int -> cols:int -> unit -> textarea_elt

    type 'a soption =
        option_attrib_t
          * 'a (* Content (or value if the following is present) *)
          * pcdata_elt option (* if content different from value *)
          * bool (* selected *)

    type 'a select_opt = 
      | Optgroup of 
          optgroup_attrib_t
            * string (* label *)
            * 'a soption
            * 'a soption list
      | Option of 'a soption
            
    val any_select :
        ?a:select_attrib_t ->
          name:string ->
            string select_opt ->
              string select_opt list ->
                select_elt

    val int_select :
        ?a:select_attrib_t ->
          name:[< `Opt of int ] param_name ->
            int select_opt ->
              int select_opt list ->
                select_elt

    val float_select :
        ?a:select_attrib_t ->
          name:[< `Opt of float ] param_name ->
            float select_opt ->
              float select_opt list ->
                select_elt

    val string_select :
        ?a:select_attrib_t ->
          name:[< `Opt of string ] param_name ->
            string select_opt ->
              string select_opt list ->
                select_elt

    val user_type_select :
        ?a:select_attrib_t ->
          name:[< `Opt of 'a ] param_name ->
            'a select_opt ->
              'a select_opt list ->
                ('a -> string) ->
                  select_elt

    val any_multiple_select :
        ?a:select_attrib_t ->
          name:string ->
            string select_opt ->
              string select_opt list ->
                select_elt

    val int_multiple_select :
        ?a:select_attrib_t ->
          name:[< `Set of int ] param_name ->
            int select_opt ->
              int select_opt list ->
                select_elt

    val float_multiple_select :
        ?a:select_attrib_t ->
          name:[< `Set of float ] param_name ->
            float select_opt ->
              float select_opt list ->
                select_elt

    val string_multiple_select :
        ?a:select_attrib_t ->
          name:[< `Set of string ] param_name ->
            string select_opt ->
              string select_opt list ->
                select_elt

    val user_type_multiple_select :
        ?a:select_attrib_t ->
          name:[< `Set of 'a ] param_name ->
            'a select_opt ->
              'a select_opt list ->
                ('a -> string) ->
                  select_elt


  end





module type ELIOMREGSIG1 =
  sig



    type page

    val send : 
        ?cookies:cookieslist -> 
          ?charset:string ->
            ?code: int ->
              server_params -> page -> Eliommod.result_to_send

    val register :
        ?sp: server_params ->
        service:('get, 'post,
                 [< internal_service_kind ],
                 [< suff ], 'gn, 'pn, [ `Registrable ]) service ->
        ?error_handler:(server_params ->
                               (string * exn) list -> page Lwt.t) ->
        (server_params -> 'get -> 'post -> page Lwt.t) ->
          unit
(** Register an service in the global table of the server 
   with the associated generation function.
   [register service t f] will associate the service [service] to the function [f].
   [f] is the function that creates a page. 
   It takes three parameters. The first one has type [server_params]
   and allows to have acces to informations about the request.
   The second and third ones are respectively GET and POST parameters.
   For example if [t] is (int "s"), then ['a] is int.

   If you want to register a service in the global table after initialization,
   you must add the [~sp] parameter (current server parameters).
    Warning: registering after initialization is not encouraged for coservices
    without timeout, as such services will be available only until the end
    of the server process!
    If you use that for main services, you will dynamically create new URLs!
    This may be dangerous as they will disappear if you stop the server.
    Be very careful to re-create these URLs when you relaunch the server,
    otherwise, some external links or bookmarks will be broken!

 *)


    val register_for_session :
        server_params ->
          service:('get, 'post, [< internal_service_kind ],
                   [< suff ], 'gn, 'pn, [ `Registrable ]) service ->
              ?error_handler:(server_params -> (string * exn) list -> 
                page Lwt.t) ->
                  (server_params -> 'get -> 'post -> page Lwt.t) -> unit
(** Registers an service and the associated function in the session table.
   If the same client does a request to this service, this function will be
   used instead of the one from the global table.

   Warning:
   - All service must be registered in the global table during initialisation,
   but never after,
   - You (obviously) can't register an service in a session table 
   when no session is active
 *)


    val register_new_service :
        ?sp: server_params ->
        url:url_path ->
            get_params:('get, [< suff ] as 'tipo, 'gn)
              params_type ->
                ?error_handler:(server_params -> (string * exn) list -> 
                  page Lwt.t) ->
                    (server_params -> 'get -> unit -> page Lwt.t) ->
                      ('get, unit, 
                       [> `Attached of 
                         [> `Internal of [> `Service ] * [> `Get] ] a_s ],
                       'tipo, 'gn, unit, 
                       [> `Registrable ]) service
(** Same as [new_service] followed by [register] *)
                      
    val register_new_coservice :
        ?sp: server_params ->
        ?max_use:int ->
        ?timeout:float ->
        fallback:(unit, unit, 
                  [ `Attached of [ `Internal of [ `Service ] * [`Get]] a_s ],
                   [ `WithoutSuffix ] as 'tipo, 
                   unit, unit, [< registrable ])
        service ->
          get_params: 
            ('get, [`WithoutSuffix], 'gn) params_type ->
              ?error_handler:(server_params -> 
                (string * exn) list -> page Lwt.t) ->
                  (server_params -> 'get -> unit -> page Lwt.t) ->
                    ('get, unit, 
                     [> `Attached of 
                       [> `Internal of [> `Coservice ] * [> `Get]] a_s ], 
                     'tipo, 'gn, unit, 
                     [> `Registrable ])
                      service
(** Same as [new_coservice] followed by [register] *)

    val register_new_coservice' :
      ?sp: server_params ->
      ?max_use:int ->
      ?timeout:float ->
        get_params: 
        ('get, [`WithoutSuffix] as 'tipo, 'gn) params_type ->
          ?error_handler:(server_params -> 
            (string * exn) list -> page Lwt.t) ->
              (server_params -> 'get -> unit -> page Lwt.t) ->
                ('get, unit, 
                 [> `Nonattached of [> `Get] na_s ],
                 'tipo, 'gn, unit, [> `Registrable ])
                  service
(** Same as [new_coservice'] followed by [register] *)

    val register_new_coservice_for_session :
        server_params ->
        ?max_use:int ->
        ?timeout:float ->
          fallback:(unit, unit, 
                    [ `Attached of [ `Internal of [ `Service ] * [`Get]] a_s ],
                    [ `WithoutSuffix ] as 'tipo, 
                    unit, unit, [< registrable ])
            service ->
              get_params: 
                ('get, [`WithoutSuffix] as 'tipo, 'gn) params_type ->
                  ?error_handler:(server_params -> (string * exn) list -> 
                    page Lwt.t) ->
                      (server_params -> 'get -> unit -> page Lwt.t) ->
                        ('get, unit, 
                         [> `Attached of 
                           [> `Internal of [> `Coservice ] * [> `Get] ] a_s ], 
                         'tipo, 'gn, unit, 
                         [> `Registrable ])
                          service
(** Same as [new_coservice] followed by [register_for_session] *)

    val register_new_coservice_for_session' :
        server_params ->
        ?max_use:int ->
        ?timeout:float ->
          get_params: 
            ('get, [`WithoutSuffix] as 'tipo, 'gn) params_type ->
              ?error_handler:(server_params -> (string * exn) list -> 
                page Lwt.t) ->
                  (server_params -> 'get -> unit -> page Lwt.t) ->
                    ('get, unit, [> `Nonattached of [> `Get] na_s ], 
                     'tipo, 'gn, unit, 
                     [> `Registrable ])
                      service
(** Same as [new_coservice'] followed by [register_for_session] *)

    val register_new_post_service :
        ?sp: server_params ->
        fallback:('get, unit, 
                  [ `Attached of [ `Internal of 
                    ([ `Service | `Coservice ] as 'kind) * [`Get] ] a_s ],
                  [< suff ] as 'tipo, 'gn,
                  unit, [< `Registrable ])
        service ->
          post_params:('post, [ `WithoutSuffix ], 'pn) params_type ->
            ?error_handler:(server_params -> (string * exn) list -> 
              page Lwt.t) ->
                (server_params -> 'get -> 'post -> page Lwt.t) ->
                  ('get, 'post, [> `Attached of
                    [> `Internal of 'kind * [> `Post] ] a_s ], 
                   'tipo, 'gn, 'pn, [> `Registrable ])
                    service
(** Same as [new_post_service] followed by [register] *)

    val register_new_post_coservice :
        ?sp: server_params ->
        ?max_use:int ->
        ?timeout:float ->
        fallback:('get, unit , 
                  [ `Attached of 
                    [ `Internal of [< `Service | `Coservice ] * [`Get] ] a_s ],
                   [< suff ] as 'tipo, 
                   'gn, unit, [< `Registrable ])
        service ->
          post_params:('post, [ `WithoutSuffix ], 'pn) params_type ->
            ?error_handler:(server_params -> (string * exn) list -> 
              page Lwt.t) ->
                (server_params -> 'get -> 'post -> page Lwt.t) ->
                  ('get, 'post, 
                   [> `Attached of 
                     [> `Internal of [> `Coservice ] * [> `Post] ] a_s ], 
                     'tipo, 'gn, 'pn, [> `Registrable ])
                    service
(** Same as [new_post_coservice] followed by [register] *)

    val register_new_post_coservice' :
        ?sp: server_params ->
        ?max_use:int ->
        ?timeout:float ->
        post_params:('post, [ `WithoutSuffix ], 'pn) params_type ->
          ?error_handler:(server_params -> (string * exn) list -> 
            page Lwt.t) ->
              (server_params -> unit -> 'post -> page Lwt.t) ->
                (unit, 'post, [> `Nonattached of [> `Post] na_s ], 
                 [ `WithoutSuffix ], unit, 'pn,
                 [> `Registrable ])
                  service
(** Same as [new_post_coservice'] followed by [register] *)

(*
    val register_new_get_post_coservice' :
        ?sp: server_params ->
        ?max_use:int ->
        ?timeout:float ->
        fallback:('get, unit , 
                  [ `Nonattached of [`Get] na_s ],
                   [< suff ] as 'tipo, 
                   'gn, unit, [< `Registrable ])
        service ->
          post_params:('post, [ `WithoutSuffix ], 'pn) params_type ->
            ?error_handler:(server_params -> (string * exn) list -> 
              page Lwt.t) ->
                (server_params -> 'get -> 'post -> page Lwt.t) ->
                  ('get, 'post, [> `Nonattached of [> `Post] na_s ], 
                   [> 'tipo], 'gn, 'pn, [> `Registrable ])
                    service
(* * Same as [new_get_post_coservice'] followed by [register] *)
*)

    val register_new_post_coservice_for_session :
        server_params ->
        ?max_use:int ->
        ?timeout:float ->
          fallback:('get, unit, 
                    [< `Attached of [< `Internal of
                      [< `Service | `Coservice ] * [`Get] ] a_s ],
                    [< suff ] as 'tipo, 
                    'gn, unit, [< `Registrable ])
            service ->
              post_params:('post, [ `WithoutSuffix ], 'pn) params_type ->
                ?error_handler:(server_params -> 
                  (string * exn) list -> page Lwt.t) ->
                    (server_params -> 'get -> 'post -> page Lwt.t) ->
                      ('get, 'post, 
                       [> `Attached of 
                         [> `Internal of [> `Coservice ] * [> `Post]] a_s ], 
                       'tipo, 'gn, 'pn, [> `Registrable ])
                        service
(** Same as [new_post_coservice] followed by [register_for_session] *)

    val register_new_post_coservice_for_session' :
        server_params ->
        ?max_use:int ->
        ?timeout:float ->
          post_params:('post, [ `WithoutSuffix ], 'pn) params_type ->
            ?error_handler:(server_params -> 
              (string * exn) list -> page Lwt.t) ->
                (server_params -> unit -> 'post -> page Lwt.t) ->
                  (unit, 'post, [> `Nonattached of [> `Post] na_s ], 
                   [ `WithoutSuffix ], unit, 'pn, 
                   [> `Registrable ])
                    service
(** Same as [new_post_coservice'] followed by [register_for_session] *)

(*
    val register_new_get_post_coservice_for_session' :
        server_params ->
        ?max_use:int ->
        ?timeout:float ->
          fallback:('get, unit, [ `Nonattached of [`Get] na_s ],
                    [< suff ] as 'tipo, 
                    'gn, unit, [< `Registrable ])
            service ->
              post_params:('post, [ `WithoutSuffix ], 'pn) params_type ->
                ?error_handler:(server_params -> 
                  (string * exn) list -> page Lwt.t) ->
                    (server_params -> 'get -> 'post -> page Lwt.t) ->
                      ('get, 'post, [> `NonAttached of [> `Post] na_s ], 
                       'tipo, 'gn, 'pn, [> `Registrable ])
                        service
(* * Same as [new_get_post_coservice] followed by [register_for_session] *)
*)







  end


module type ELIOMREGSIG =
  sig
    include ELIOMREGSIG1
    module Cookies : ELIOMREGSIG1 
    with type page = page * cookieslist
  end

module type ELIOMSIG = sig
  include ELIOMREGSIG
  include ELIOMFORMSIG
end



module MakeRegister : functor (Pages: REGCREATE) -> ELIOMREGSIG with 
type page = Pages.page

module MakeForms : functor (Pages: FORMCREATE) -> ELIOMFORMSIG with 
type form_content_elt = Pages.form_content_elt
and type form_content_elt_list = Pages.form_content_elt_list
and type form_elt = Pages.form_elt
and type a_content_elt = Pages.a_content_elt
and type a_content_elt_list = Pages.a_content_elt_list
and type a_elt = Pages.a_elt
and type a_elt_list = Pages.a_elt_list
and type div_content_elt = Pages.div_content_elt
and type div_content_elt_list = Pages.div_content_elt_list
and type uri = Pages.uri
and type link_elt = Pages.link_elt
and type script_elt = Pages.script_elt
and type textarea_elt = Pages.textarea_elt
and type select_elt = Pages.select_elt
and type input_elt = Pages.input_elt
and type pcdata_elt = Pages.pcdata_elt
and type a_attrib_t = Pages.a_attrib_t
and type form_attrib_t = Pages.form_attrib_t
and type input_attrib_t = Pages.input_attrib_t
and type textarea_attrib_t = Pages.textarea_attrib_t
and type select_attrib_t = Pages.select_attrib_t
and type link_attrib_t = Pages.link_attrib_t
and type script_attrib_t = Pages.script_attrib_t
and type input_type_t = Pages.input_type_t

(** {2 Module for registering typed Xhtml pages} *)

(** {3 Creating links, forms, etc.} *)

module type XHTMLFORMSSIG = sig



  open XHTML.M
  open Xhtmltypes

  val a :
      ?a:a_attrib attrib list ->
        ('get, unit, [< get_service_kind ], 
         [< suff ], 'gn, 'pn,
         [< registrable ]) service ->
           server_params -> a_content elt list -> 'get -> [> a] XHTML.M.elt
(** [a service sp cont ()] creates a link from [current] to [service]. 
   The text of
   the link is [cont]. For example [cont] may be something like
   [\[pcdata "click here"\]]. 

   The last  parameter is for GET parameters.
   For example [a service sp cont (42,"hello")]

   The [~a] optional parameter is used for extra attributes 
   (see the module XHTML.M) *)

  val css_link : ?a:(link_attrib attrib list) ->
    uri -> [> link ] elt
(** Creates a [<link>] tag for a Cascading StyleSheet (CSS). *)

  val js_script : ?a:(script_attrib attrib list) ->
    uri -> [> script ] elt
(** Creates a [<script>] tag to add a javascript file *)

    val make_uri :
        ('get, unit, [< get_service_kind ],
         [< suff ], 'gn, 'pn, 
         [< registrable ]) service ->
          server_params -> 'get -> uri
(** Create the text of the service. Like the [a] function, it may take
   extra parameters. *)


    val get_form :
        ?a:form_attrib attrib list ->
          ('get, unit, [< get_service_kind ],
           [<suff ], 'gn, 'pn, 
           [< registrable ]) service ->
             server_params ->
              ('gn -> form_content elt list) -> [>form] elt
(** [get_form service current formgen] creates a GET form from [current] to [service]. 
   The content of
   the form is generated by the function [formgen], that takes the names
   of page parameters as parameters. *)

    val post_form :
        ?a:form_attrib attrib list ->
          ('get, 'post, [< post_service_kind ],
           [< suff ], 'gn, 'pn, 
           [< registrable ]) service ->
            server_params ->
              ('pn -> form_content elt list) -> 'get -> [>form] elt
(** [post_form service current formgen] creates a POST form from [current] 
   to [service]. The last parameter is for GET parameters (as in the function [a]).
 *)

  type basic_input_type =
      [
    | `Hidden
    | `Password
    | `Submit
    | `Text ]

  val int_input :
      ?a:input_attrib attrib list -> input_type:[< basic_input_type ] ->
        ?name:[< int setoneopt ] param_name -> 
          ?value:int -> unit -> [> input ] elt
(** Creates an [<input>] tag for an integer *)

  val float_input :
      ?a:input_attrib attrib list -> input_type:[< basic_input_type ] ->
        ?name:[< float setoneopt ] param_name -> 
          ?value:float -> unit -> [> input ] elt
(** Creates an [<input>] tag for a float *)

  val string_input : 
      ?a:input_attrib attrib list -> input_type:[< basic_input_type ] ->
        ?name:[< string setoneopt ] param_name -> 
          ?value:string -> unit -> [> input ] elt
(** Creates an [<input>] tag for a string *)

  val user_type_input : 
      ?a:input_attrib attrib list -> input_type:[< basic_input_type ] ->
        ?name:[< 'a setoneopt ] param_name -> 
          ?value:'a -> ('a -> string) -> [> input ] elt
(** Creates an [<input>] tag for a user type *)

  val any_input :
      ?a:input_attrib attrib list -> 
        input_type:[< basic_input_type | `Reset | `Button ] ->
        ?name:string -> ?value:string -> unit -> [> input ] elt
(** Creates an untyped [<input>] tag (low level) *)

  val file_input :
      ?a:input_attrib attrib list -> 
        name:[< file_info setoneopt ] param_name -> 
          unit -> [> input ] elt
(** Creates an [<input>] tag for sending a file *)

  val image_input :
      ?a:input_attrib attrib list -> 
        name:[< coordinates oneopt ] param_name -> 
          ?src:uri -> unit -> [> input ] elt
(** Creates an [<input type="image" name="...">] tag that sends the coordinates 
   you clicked on *)
            
  val int_image_input :
      ?a:input_attrib attrib list -> 
        name:[< (int * coordinates) oneopt ] param_name -> value:int -> 
          ?src:uri -> unit -> [> input ] elt
(** Creates an [<input type="image" name="..." value="...">] tag that sends
   the coordinates you clicked on and a value of type int *)

  val float_image_input :
      ?a:input_attrib attrib list -> 
        name:[< (float * coordinates) oneopt ] param_name -> value:float -> 
          ?src:uri -> unit -> [> input ] elt
(** Creates an [<input type="image" name="..." value="...">] tag that sends
    the coordinates you clicked on and a value of type float *)

  val string_image_input :
      ?a:input_attrib attrib list -> 
        name:[< (string * coordinates) oneopt ] param_name -> value:string -> 
          ?src:uri -> unit -> [> input ] elt
(** Creates an [<input type="image" name="..." value="...">] tag that sends
   the coordinates you clicked on and a value of type string *)

  val user_type_image_input :
      ?a:input_attrib attrib list -> 
        name:[< ('a * coordinates) oneopt ] param_name -> value:'a -> 
          ?src:uri -> ('a -> string) -> [> input ] elt
(** Creates an [<input type="image" name="..." value="...">] tag that sends
   the coordinates you clicked on and a value of user defined type *)

  val any_image_input :
      ?a:input_attrib attrib list -> 
        name:string -> value:string -> ?src:uri -> unit -> [> input ] elt
(** Creates an [<input type="image" name="..." value="...">] tag that sends
   the coordinates you clicked on and an untyped value *)

  val bool_checkbox :
      ?a:input_attrib attrib list -> ?checked:bool -> 
        name:[ `One of bool ] param_name -> unit -> [> input ] elt
(** Creates a checkbox [<input>] tag that will have a boolean value.
   The service must declare a [bool] parameter.
 *)

    val int_checkbox :
        ?a:input_attrib attrib list -> ?checked:bool -> 
          name:[ `Set of int ] param_name -> value:int -> 
            unit -> [> input ] elt
(** Creates a checkbox [<input>] tag that will have an int value.
   Thus you can do several checkboxes with the same name 
   (and different values). 
   The service must declare a parameter of type [set].
 *)

    val float_checkbox :
        ?a:input_attrib attrib list -> ?checked:bool -> 
          name:[ `Set of float ] param_name -> value:float -> 
            unit -> [> input ] elt
(** Creates a checkbox [<input>] tag that will have a float value.
   Thus you can do several checkboxes with the same name 
   (and different values). 
   The service must declare a parameter of type [set].
 *)


    val string_checkbox :
        ?a:input_attrib attrib list -> ?checked:bool -> 
          name:[ `Set of string ] param_name -> value:string -> 
            unit -> [> input ] elt
(** Creates a checkbox [<input>] tag that will have a string value.
   Thus you can do several checkboxes with the same name 
   (and different values). 
   The service must declare a parameter of type [set].
 *)


    val user_type_checkbox :
        ?a:input_attrib attrib list -> ?checked:bool -> 
          name:[ `Set of 'a ] param_name -> value:'a -> 
            ('a -> string) -> [> input ] elt
(** Creates a checkbox [<input>] tag that will have a "user type" value.
   Thus you can do several checkboxes with the same name 
   (and different values). 
   The service must declare a parameter of type [set].
 *)


    val any_checkbox :
        ?a:input_attrib attrib list -> ?checked:bool -> 
          name:string -> value:string -> unit -> [> input ] elt
(** Creates a checkbox [<input>] tag with untyped content.
   Thus you can do several checkboxes with the same name 
   (and different values). 
   The service must declare a parameter of type [any].
 *)




  val string_radio : ?a:(input_attrib attrib list ) -> ?checked:bool -> 
    name:[ `Opt of string ] param_name -> value:string -> unit -> [> input ] elt
(** Creates a radio [<input>] tag with string content *)

  val int_radio : ?a:(input_attrib attrib list ) -> ?checked:bool -> 
     name:[ `Opt of int ] param_name -> value:int -> unit -> [> input ] elt
(** Creates a radio [<input>] tag with int content *)

  val float_radio : ?a:(input_attrib attrib list ) -> ?checked:bool -> 
     name:[ `Opt of float ] param_name -> value:float -> unit -> [> input ] elt
(** Creates a radio [<input>] tag with float content *)

  val user_type_radio : ?a:(input_attrib attrib list ) -> ?checked:bool ->
    name:[ `Opt of 'a ] param_name -> value:'a -> ('a -> string) -> [> input ] elt
(** Creates a radio [<input>] tag with user_type content *)

  val any_radio : ?a:(input_attrib attrib list ) -> ?checked:bool -> 
    name:string -> value:string -> unit -> [> input ] elt
(** Creates a radio [<input>] tag with untyped string content (low level) *)


  type button_type =
      [ `Button | `Reset | `Submit ]

  val string_button : ?a:(button_attrib attrib list ) -> 
    name:[< string setone ] param_name -> value:string -> 
      button_content elt list -> [> button ] elt
(** Creates a submit [<button>] tag with string content *)
  val int_button : ?a:(button_attrib attrib list ) ->
    name:[< int setone ] param_name -> value:int -> 
      button_content elt list -> [> button ] elt
(** Creates a submit [<button>] tag with int content *)
  val float_button : ?a:(button_attrib attrib list ) ->
    name:[< float setone ] param_name -> value:float -> 
      button_content elt list -> [> button ] elt
(** Creates a submit [<button>] tag with float content *)
  val user_type_button : ?a:(button_attrib attrib list ) ->
    name:[< 'a setone ] param_name -> value:'a -> ('a -> string) ->
      button_content elt list -> [> button ] elt
(** Creates a submit [<button>] tag with user_type content *)
  val any_button : ?a:(button_attrib attrib list ) ->
    button_type:[< button_type ] ->
      name:string -> value:string -> 
        button_content elt list -> [> button ] elt
(** Creates a [<button>] tag with untyped string content (low level) *)

  val button : ?a:(button_attrib attrib list ) ->
    button_type:[< button_type ] ->
      button_content elt list -> [> button ] elt
(** Creates a [<button>] tag with no value. No value is sent. *)



  val textarea : 
      ?a:textarea_attrib attrib list ->
        name:[< string setoneopt ] param_name -> 
          ?value:Xhtmltypes.pcdata XHTML.M.elt -> 
            rows:int -> cols:int -> 
              unit -> [> textarea ] elt
(** Creates a [<textarea>] tag *)

  type 'a soption =
      Xhtmltypes.option_attrib XHTML.M.attrib list
        * 'a (* Value to send *)
        * pcdata elt option (* Text to display (if different from the latter) *)
        * bool (* selected *)
        
  type 'a select_opt = 
    | Optgroup of 
        [ common | `Disabled ] XHTML.M.attrib list
          * string (* label *)
          * 'a soption
          * 'a soption list
    | Option of 'a soption
          
  (** The type for [<select>] options and groups of options.
     The field of type 'a in [soption] is the value that will be sent 
     by the form. If the [string option] is not present it is also the
     value displayed.
   *)

  val int_select :
      ?a:select_attrib attrib list ->
        name:[< `Opt of int ] param_name ->
          int select_opt ->
            int select_opt list ->
              select elt
(** Creates a [<select>] tag for int values. *)

  val float_select :
      ?a:select_attrib attrib list ->
        name:[< `Opt of float ] param_name ->
          float select_opt ->
            float select_opt list ->
              select elt
(** Creates a [<select>] tag for float values. *)

  val string_select :
      ?a:select_attrib attrib list ->
        name:[< `Opt of string ] param_name ->
          string select_opt ->
            string select_opt list ->
              select elt
(** Creates a [<select>] tag for string values. *)

  val user_type_select :
      ?a:select_attrib attrib list ->
        name:[< `Opt of 'a ] param_name ->
          'a select_opt ->
            'a select_opt list ->
              ('a -> string) ->
                select elt
(** Creates a [<select>] tag for user type values. *)

  val any_select :
      ?a:select_attrib attrib list ->
        name:string ->
          string select_opt ->
            string select_opt list ->
              select elt
(** Creates a [<select>] tag for any (untyped) value. *)


  val int_multiple_select :
      ?a:select_attrib attrib list ->
        name:[< `Set of int ] param_name ->
          int select_opt ->
            int select_opt list ->
              select elt
(** Creates a [<select>] tag for int values. *)

  val float_multiple_select :
      ?a:select_attrib attrib list ->
        name:[< `Set of float ] param_name ->
          float select_opt ->
            float select_opt list ->
              select elt
(** Creates a [<select>] tag for float values. *)

  val string_multiple_select :
      ?a:select_attrib attrib list ->
        name:[< `Set of string ] param_name ->
          string select_opt ->
            string select_opt list ->
              select elt
(** Creates a [<select>] tag for string values. *)

  val user_type_multiple_select :
      ?a:select_attrib attrib list ->
        name:[< `Set of 'a ] param_name ->
          'a select_opt ->
            'a select_opt list ->
              ('a -> string) ->
                select elt
(** Creates a [<select>] tag for user type values. *)

  val any_multiple_select :
      ?a:select_attrib attrib list ->
        name:string ->
          string select_opt ->
            string select_opt list ->
              select elt
(** Creates a [<select>] tag for any (untyped) value. *)


end




module Xhtml : sig

  include ELIOMREGSIG with type page = xhtml elt
  include XHTMLFORMSSIG

end


module Blocks : sig

  include ELIOMREGSIG with type page = body_content elt list
  include XHTMLFORMSSIG

end


module SubXhtml : functor (T : sig type content end) ->
  sig
    
    include ELIOMREGSIG with type page = T.content elt list
    include XHTMLFORMSSIG
    
  end




(** {2 Modules to register other types of pages} *)

module HtmlText : ELIOMSIG with 
type page = string
and type form_content_elt = string
and type form_content_elt_list = string
and type form_elt = string 
and type a_content_elt = string 
and type a_content_elt_list = string 
and type a_elt = string 
and type a_elt_list = string 
and type div_content_elt = string 
and type div_content_elt_list = string 
and type uri = string 
and type link_elt = string 
and type script_elt = string 
and type textarea_elt = string 
and type select_elt = string 
and type input_elt = string 
and type pcdata_elt = string 
and type a_attrib_t = string 
and type form_attrib_t = string 
and type input_attrib_t = string 
and type textarea_attrib_t = string 
and type select_attrib_t = string 
and type link_attrib_t = string 
and type script_attrib_t = string 
and type input_type_t = string 

module CssText : ELIOMREGSIG with type page = string

module Text : ELIOMREGSIG with type page = string * string
(** The first string is the content, the second is the content type,
 for example "text/html" *)

module Actions : ELIOMREGSIG with 
  type page = exn list

module Unit : ELIOMREGSIG with 
  type page = unit

module Redirections : ELIOMREGSIG with 
  type page = string

module Files : ELIOMREGSIG with 
  type page = string

module Any : ELIOMREGSIG with 
  type page = Eliommod.result_to_send

(*****************************************************************************)
(** {2 Session data in memory} *)

type 'a table

val create_table : ?sp:server_params -> unit -> 'a table
(** Create a table in memory where you can store your session data
    After initialization phase, you must give the [~sp] parameter *)

val get_session_data : 
    'a table -> server_params -> 'a option

val set_session_data : 
    'a table -> server_params -> 'a -> unit

val remove_session_data : 
    'a table -> server_params -> unit

(*****************************************************************************)
(** {2 Persistent sessions} *)

type 'a persistent_table

val create_persistent_table : string -> 'a persistent_table

val get_persistent_data : 
    'a persistent_table -> server_params -> 'a option Lwt.t

val set_persistent_data : 
    'a persistent_table -> server_params -> 'a -> unit Lwt.t

val remove_persistent_data : 
    'a persistent_table -> server_params -> unit Lwt.t

(** Close the persistent session (destroying all persistent data) *)
val close_persistent_session : server_params -> unit Lwt.t

(** Close Eliom's volatile session *)
val close_volatile_session : server_params -> unit

(** Close noth sessions *)
val close_session : server_params -> unit Lwt.t

(**/**)
(*****************************************************************************)
val number_of_sessions : server_params -> int

val number_of_tables : unit -> int

val number_of_table_elements : unit -> int list

val number_of_persistent_sessions : unit -> int Lwt.t

val number_of_persistent_tables : unit -> int

val number_of_persistent_table_elements : unit -> (string * int) list Lwt.t
(* Because of Dbm implementation, the result may be less thann the expected
   result in some case (with a version of ocsipersist based on Dbm) *)

