(* Copyright Vincent Balat 2005 *)
(* Do not redistribute *)

open Http_frame
open Http_com
open Xhtmlpp

(** Type of answers from modules (web pages) *)
type page = Xhtmlpp.xhtml

(** Type of formulars *)
type form = Xhtmlpp.xhtmlcont
type formorlink = Xhtmlpp.xhtmlcont
type insideform = Xhtmlpp.insideform
type insideforml = insideform list

(** type of URL, without parameter *)
type url_string = string list

(** Url matches the exact string
    Url_Prefix matches any string with this prefix
   (for ex to make the string "wiki/" activate all pages like "wiki/toto")
 *)
type url_activator = Url of url_string | Url_Prefix of url_string


(** Type of http parameters *)
type http_param = {url_suffix: string;
		   useragent: string;
		   ip: Unix.inet_addr;
		   get_params: (string * string) list;
		   post_params: (string * string) list}


(** state is a parameter to differenciate
    several instances of the same URL.
	(for internal use)
 *)
type internal_state = int

let new_state =
  let c : internal_state ref = ref (-1) in
  fun () -> c := !c + 1 ; Some !c

(* À revoir !!!!!!!!!!!!!!!!!!!!!!!!! Faire des cookies plus compliqués *)
let new_cookie =
  let c = ref (-1) in
  fun () -> c := !c + 1 ; string_of_int !c

exception Omlet_Typing_Error
exception Omlet_Wrong_parameter

type postorget = Get | Post

let find_param name pog http_param =
  try 
    match pog with
	Post -> List.assoc name http_param.post_params
      | Get -> List.assoc name http_param.get_params
  with Not_found -> raise Omlet_Wrong_parameter

let id x = x

(** Type of names in a formular *)
type 'a name = string

let write_param name value = name^"="^value

(** Constructors for parameters *)
(** The type of conversion_function is ('a -> httpparam -> page)
    where 'a has the shape 'b -> 'c ->... -> page
    The string list is the list of required parameters names
*)
type ('a, 'b, 'c, 'd) parameters =
    {param_names: string list;
     write_parameters: ((string option -> formorlink) -> 'd);
     (* corresponds to the last argument in ('a,'b,'c) url
	'd is for example int -> int -> string, 
	and the first function is the function to apply after construction
     *)
     give_form_parameters: 'c;
     (* 'c is exactly the same as 'a or 'b in ('a,'b,'c) url type,
	usually something like (int name -> int name -> form) -> form
	(but we may have something else instead of form when constructing
	the value)
     *)
     conversion_function: postorget -> 'a -> http_param -> 'b 
       (* for dynamic typing,
	  for ex (int -> int -> page) -> http_param -> page 
	  (but 'b can be more complicated, for ex in register_post_url)
       *)}

let _noparam = 
    {param_names=[];
     write_parameters=(fun f -> f None);
     give_form_parameters = id;
     conversion_function= (fun pog f _ -> f)}

let _string name = 
    {param_names=[name];
     write_parameters=(fun f s -> f (Some (write_param name s)));
     give_form_parameters=(fun h -> h (name : string name));
     conversion_function= (fun pog f httpparam -> 
			     f (find_param name pog httpparam))}

let _user_type (mytype_of_string : string -> 'a) string_of_mytype name = 
    {param_names=[name];
     write_parameters=
	(fun f i -> f (Some (write_param name (string_of_mytype i))));
     give_form_parameters=(fun h -> h (name : 'a name));
     conversion_function=(fun pog f httpparam -> 
			    let p =
			      let pa = (find_param name pog httpparam) in
			      try mytype_of_string pa
			      with _ -> raise Omlet_Typing_Error
			    in f p)}
(* 
let _int name = 
    {param_names=[name];
     write_parameters=
	(fun f i -> f (Some (write_param name (string_of_int i))));
     give_form_parameters=(fun h -> h (name : int name));
     conversion_function=(fun f httpparam -> 
			    let p = 
                              let pa = (find_param name httpparam) in
			      try int_of_string pa
			      with _ -> raise Omlet_Typing_Error
*)

let _int name = _user_type int_of_string string_of_int name

let _unit = 
    {param_names=[];
     write_parameters= (fun f -> f None);
     give_form_parameters=id;
     conversion_function=(fun pog f httpparam -> f ())}


(* Dans une ancienne version on pouvait mettre des paramètres optionnels.
   mais la sémantique n'est pas claire.
   maintenant il faut enregistrer plusieurs URL différentes.
let _option p =
  (* By typing, _option can take only one parameter.
     So opt_param_names is empty and param_names contains at most one element
  *)
    {param_names=[];
     opt_param_names=p.param_names;
     write_parameters=(fun f s -> match s with 
       None -> f None
     | Some v -> p.write_parameters f v);
     give_form_parameters=p.give_form_parameters;
     conversion_function=
	(fun f httpparam -> 
	   try
	     p.conversion_function (fun x -> f (Some x)) httpparam
	   with Omlet_Wrong_parameter -> f None)}
*)

let _useragent p = 
    {param_names=p.param_names;
     write_parameters=p.write_parameters;
     give_form_parameters=p.give_form_parameters;
     conversion_function=
     (fun pog f httpparam -> 
	p.conversion_function pog (f (httpparam.useragent)) httpparam)
    }

let _ip p = 
    {param_names=p.param_names;
     write_parameters=p.write_parameters;
     give_form_parameters=p.give_form_parameters;
     conversion_function=
     (fun pog f httpparam -> 
	let ip = Unix.string_of_inet_addr httpparam.ip in
        p.conversion_function pog (f ip) httpparam)
    }

let _url_suffix p =
    {param_names=p.param_names;
     write_parameters=
	(fun f suffix -> p.write_parameters
	   (function None -> f (Some ("/"^suffix))
	      | Some v -> f (Some ("/"^suffix^"?"^v))));
     give_form_parameters=p.give_form_parameters;
     conversion_function=
     (fun pog f httpparam -> p.conversion_function pog (f (httpparam.url_suffix)) httpparam)
    }

let ( ** ) p1 p2 =
    {param_names=(p1.param_names@p2.param_names);
     (* p1.param_names has length 0 or 1 so @ is not expensive *)
     write_parameters=
	(fun s v1 v2 -> 
	   (p2.write_parameters 
	      (function 
		   Some ss -> 
		     (p1.write_parameters 
			(function Some s' -> s (Some (s'^"&"^ss))
			   | None -> assert false (*** ???????? ****)) v1)
		 | None -> (p1.write_parameters (fun s' -> s s') v1))
	      v2));
     give_form_parameters=
	(fun h -> p2.give_form_parameters (p1.give_form_parameters h));
     conversion_function=
	(fun pog f httpparam -> 
	   (p2.conversion_function pog
	      (p1.conversion_function pog f httpparam) httpparam))}
    

(* The current working directory *)
let current_dir : url_string ref = ref []

let remove_slash = function
    [] -> []
  | ""::l -> l
  | a::l -> a::l

let absolute_change_dir dir = current_dir := remove_slash dir

let change_empty_list = function
    [] -> [""] (* It is not possible to register an empty URL *)
  | l -> l

let change_empty_activator = function
    Url l -> Url (change_empty_list l)
  | Url_Prefix l -> Url_Prefix (change_empty_list l)

(** We associate to an URL a function http_param -> page *)
module type DIRECTORYTREE =
  sig
    type tree
    type page_table_key =
	{get_names: string list;
	 post_names: string list;
	 state: internal_state option}
    val empty_tree : unit -> tree
    val is_empty_table : tree -> bool
    val add_url : tree ->  url_activator -> 
      page_table_key * (http_param -> page) -> unit
    val find_url :
      tree ->
      url_string -> (string * string) list -> (string * string) list ->
      internal_state option -> ((http_param -> page) * url_string) 
      * url_string option
  end

module Directorytree : DIRECTORYTREE = struct
  (* Each node contains either a list of nodes (case directory)
     or a table of "answers" (functions that will generate the page) *)
  
  type page_table_key =
      {get_names: string list;
       post_names: string list;
       state: internal_state option}
       (* action: http_param -> page *)

  module Page_Table = Hashtbl.Make(struct 
				     type t = page_table_key 
				     let equal = (=)
				     let hash = Hashtbl.hash
				   end)
    (* pas sûr que ce soit bien d'utiliser des tables de hashage ici
       mais ça m'évite d'implémenter une table avec remplacement moi-même *)

  type table = Vide | Table of ((http_param -> page) * url_string) Page_Table.t

  let empty_table () = Vide

  let add_table t (key,elt) = 
    match t with
	Vide -> let t = (Page_Table.create 5) in
	  Page_Table.add t key elt; Table t
      | Table t -> Page_Table.add t key elt; Table t

  let find_table t k = 
    match t with
	Vide -> raise Not_found
      | Table t -> Page_Table.find t k

  type dircontent = Realdir of ((string * dir) list)
		    | Dirprefix of table ref
  and dir = Dir of (table ref * dircontent ref)
  type tree = dir
      
  let empty_tree () = Dir (ref (empty_table ()), ref (Realdir []))
  let is_empty_table (Dir (r1,r2)) = (!r1 = Vide && !r2 = Realdir [])
    
  let add_url tree url_act (page_table_key, action) =
    let rec search_table (Dir (table, dircontent_ref)) =
      let aux a l =
	(match !dircontent_ref with
             Realdir str_dir_list_ref ->
               (try
                  search_table (List.assoc a str_dir_list_ref) l
		with
                    Not_found ->
                      let new_dir = empty_tree () in
			dircontent_ref :=
                          Realdir ((a, new_dir)::str_dir_list_ref);
			search_table new_dir l)
           | Dirprefix t -> if l = [] then table,dircontent_ref
             else let new_dir = empty_tree () in
               dircontent_ref := Realdir [(a, new_dir)];
               search_table new_dir l)
      in function
            [] -> table,dircontent_ref
	| [""] ->  print_endline "j'enregistre un page par défaut du répertoire" ; aux "" []
	| ""::l -> print_endline "j'enrSLASH" ; search_table (Dir (table, dircontent_ref)) l
	| a::l -> print_endline ("j'enregistre dans "^a) ; aux a l
    in
    let content = ({get_names = List.sort compare page_table_key.get_names;
		    post_names = List.sort compare page_table_key.post_names;
		    state = page_table_key.state},
		   (action, !current_dir)) in
    let current_tree = print_endline "--------"; Dir (search_table tree !current_dir) in
      match url_act with 
	  Url u2 ->
	    let table,dircontentref = 
	      (search_table current_tree (change_empty_list u2)) in
              table := add_table !table content
	| Url_Prefix u2 ->
            let table,dircontentref = 
	      (search_table current_tree (change_empty_list u2)) in
	      dircontentref := 
		Dirprefix (ref (add_table (empty_table ()) content))
	 
  let find_url dir string_list get_param_list post_param_list state_option =
    let rec search_table (Dir (tableref, dircontent_ref)) =
      let aux a l =
	(match !dircontent_ref with
             Realdir str_dir_list_ref ->
               search_table (List.assoc a str_dir_list_ref) l
           | Dirprefix tr -> !tr, (Some (a::l)))
      in function
            [] -> !tableref, None
	| [""] -> print_endline "Je cherche la page par défaut"; aux "" []
	| ""::l -> print_endline "SLASH"; search_table (Dir (tableref, dircontent_ref)) l
	| a::l -> print_endline ("Je cherche dans "^a); aux a l
    in
    let get_names  = List.sort compare (fst (List.split  get_param_list))
    and post_names = List.sort compare (fst (List.split post_param_list)) in
    let table, suffix = (search_table dir (change_empty_list string_list)) in
      ((find_table 
	  table
	  {get_names = get_names;
	   post_names = post_names;
	   state = state_option}),
       suffix)
end

open Directorytree

type url_table = tree

let global_tree =  (empty_tree ())
let session_tree = ref (empty_tree ())
let new_session_table = empty_tree
let is_empty_table = is_empty_table


(* The table of tables for each session. Keys are (hostname,cookie) *)
module Cookies = Hashtbl.Make(struct 
				type t = Unix.inet_addr * string
				let equal = (=)
				let hash = Hashtbl.hash
			      end)

let cookie_table = Cookies.create 100

(** Typed URLs *)
type public_url

type state_url

type 'a internal_url

type external_url


(* Comment rendre cette structure moins lourde ? *)
type ('a,'b,'c,'d,'e,'f,'g) url = 
    {url: url_activator; (* name of the URL without parameters *)
     url_state: internal_state option;
       (* 'g is just a type information: it can be only 
	  public_url internal_url or session_url internal_url
	  or external_url, so that we can't use session urls as fallbacks for
	  other session urls. If it is a session url, it contains a value
	  (internal state) that will allow to differenciate between
	  url that have the same name.
	*)
     get_param_names: string list;
     post_param_names: string list;
     create_get_form: 'a -> insideforml; 
       (* 'a is for example int name -> string name -> insideform *)
     create_post_form: 'b -> insideforml;  
                   (* idem, but for POST. If no post param 'b is insideform *)
     create_get_url: (string -> formorlink) -> 'c;
         (* 'c is for example int -> int -> string, function that will create
	    the name of the url, for ex the string "hello?int1=3&int2=4"
	    Then the function will be applied to this string to create a link
	    or a formular to this url.
	  *)
     get_conversion_function: 'd -> http_param -> page;
         (* for dynamic typing, (as in parameters)
	    for ex int -> int -> page -> http_param -> page 
	    (but 'b can be more complicated, for ex in register_post_url)
	    We need this here in order to be able to re-register the same
	    being sure we use exactly the same parameter's names and types.
	    If we could have had parameters names in the type it wouldn't
	    have been necessary...
	  *)
     post_conversion_function: 'e -> http_param -> 'f;
    }
      
(* En fait cette fonction est dans Neturl
let rec cut_url s = 
  try
    let length = String.length s in
      if length = 0 then []
      else
	let pos_slash = String.index s '/' in
	  if pos_slash = 0 
	  then cut_url (String.sub s 1 (length-1))
	  else 
	    let prefix = String.sub s 0 pos_slash in
	      (*  if length > (pos_slash+1)
		  then *)
	      prefix::(cut_url (String.sub s (pos_slash+1) (length - pos_slash - 1)))
		(* else [prefix] *)
  with _ -> [s]
*)

let rec reconstruct_url_string = function
    [] -> "/"
  | [a] -> a
  | a::l -> a^"/"^(reconstruct_url_string l)

let reconstruct_url_string_option = function
    None -> ""
  | Some l -> reconstruct_url_string l

(** Create an url *)
let new_url_aux
    ~(name : url_activator)
    ~params
    : ('a, insideforml, 'b, 'c, page, page, 'popo) url =
(* ici faire une vérification "duplicate parameter" ? SÉCURITÉ !! *) 
  let name = change_empty_activator name in
  {url = name;
   url_state = None;
   get_param_names = params.param_names;
   post_param_names = [];
   create_get_form = params.give_form_parameters;
   create_post_form = id;
   create_get_url = (match name with
			 Url s -> 
			   let ss = (reconstruct_url_string s) in
			     (fun f -> params.write_parameters 
				(function Some v -> f (ss^"?"^v)
				   | None -> f ss))
		       | Url_Prefix s -> 
			   let ss = (reconstruct_url_string s) in
			     (fun f -> params.write_parameters
				(function Some v -> f (ss^v)
				   | None -> f ss)));
   get_conversion_function = params.conversion_function Get;
   post_conversion_function = (fun a http_param -> a)
  }

let new_url
    ~(name : url_activator)
    ~params
    : ('a, insideforml, 'b, 'c, page, page, public_url internal_url) url =
  new_url_aux name params

let new_state_url
   ~(fallback : ('a, insideforml, 'b, 'c, page, page, public_url internal_url)url)
    : ('a, insideforml, 'b, 'c, page, page, state_url internal_url) url =
  {fallback with url_state = new_state ()}

let new_external_url
    ~(name : url_activator)
    ~params
    : ('a, insideforml, 'b, 'c, page, page, external_url) url =
  new_url_aux name params

let register_url_aux
    tree
    state
    ~(url : ('a,insideforml,'c,'d,'e,'f,'g) url)
    ~action =
(* ici faire une vérification "duplicate url" et REMPLACER si elle existe *)
  add_url tree url.url
    ({get_names = url.get_param_names;
      post_names = []; (* url.post_param_names; *)
      state = state},
     (url.get_conversion_function action))

let register_url 
    ~(url : ('a,insideforml,'c,'d,'e,'f,'g internal_url) url)
    ~action =
  register_url_aux global_tree (url.url_state) url action

(* WARNING: if we create a new URL without registering it,
   we can have a link towards a page that does not exist!!! :-(
   The only way I see to avoid this is to impose a syntax extension
   like "let rec" for url...
 *)

let register_session_url
    ~(url : ('a,insideforml,'c,'d,'e,'f,'g internal_url) url)
    ~action =
  register_url_aux !session_tree url.url_state url action

let register_new_url 
    ~name
    ~params
    ~action 
    : ('a,insideforml,'b, 'c, page, page, public_url internal_url) url =
  let u = (new_url name params) in
  register_url u action;
  u

let register_new_session_url
   ~(fallback : ('a, insideforml, 'b, 'c, page, page, public_url internal_url)url)
   ~action 
   : ('a,insideforml,'b, 'c, page, page, state_url internal_url) url =
  let u = (new_state_url fallback) in
    register_session_url u action;
    u


(** Register an url with post parameters in the server *)
let new_post_url_aux
    ~fallback
    ~post_params
    : ('a,'b,'c,'d,'e,'f, 'popo) url = 
(* ici faire une vérification "duplicate parameter" ? SÉCURITÉ !! *) 
  {url = fallback.url;
   url_state = None;
   get_param_names = fallback.get_param_names;
   post_param_names = post_params.param_names;
   create_get_form = fallback.create_get_form;
   create_post_form = post_params.give_form_parameters;
   create_get_url = fallback.create_get_url;
   get_conversion_function = fallback.get_conversion_function;
   post_conversion_function = post_params.conversion_function Post
  }

let new_post_url
    ~fallback
    ~post_params
    : ('a,'b,'c,'d,'e,'f, public_url internal_url) url = 
  new_post_url_aux fallback post_params

let new_external_post_url
    ~(name : url_activator)
    ~params
    ~post_params
    : ('a,'b,'c,'d,'e,'f, external_url) url = 
  new_post_url_aux (new_url_aux name params) post_params

let new_post_state_url
    ~(fallback : ('a,'b,'c,'d,'e,'f, public_url internal_url) url)
    ~post_params 
    : ('aa,'bb,'cc,'dd,'ee,'ff, state_url internal_url) url = 
  {fallback with 
   url_state = new_state ();
   post_param_names = post_params.param_names;
   create_post_form = post_params.give_form_parameters;
   post_conversion_function = post_params.conversion_function Post
  }

let register_post_url_aux
    tree
    state
    ~(url : ('a,'b->'bb,'c,'d,'e,'f,'g) url)
    ~action =
(* ici faire une vérification "duplicate url" et REMPLACER si elle existe *)
  add_url tree url.url
    ({get_names = url.get_param_names;
      post_names = url.post_param_names;
      state = state},
     (fun http_param ->
	(url.get_conversion_function
	   (url.post_conversion_function action http_param) 
	   http_param)))
    (* Je n'arrive pas à mettre les params2 avant params pour des raisons
       de typage... *)

let register_post_url 
    ~(url : ('a,'b->'bb,'c,'d,'e,'f,'g internal_url) url)
    ~action =
  register_post_url_aux global_tree (url.url_state) url action

let register_post_session_url
    ~(url : ('a,'b->'bb,'c,'d,'e,'f,'g internal_url) url)
    ~action =
  register_post_url_aux !session_tree url.url_state url action

let register_new_post_url 
    ~fallback
    ~post_params
    ~action
    : ('a,'b,'c,'d,'e,'f, public_url internal_url) url =
  let u = new_post_url ~fallback:fallback ~post_params:post_params in
  register_post_url u action;
  u

let register_new_post_session_url
    ~(fallback : ('a,'b,'c,'d,'e,'f, public_url internal_url) url)
    ~post_params 
    ~action
    : ('aa,'bb,'cc,'dd,'ee,'ff, state_url internal_url) url =
  let u = new_post_state_url ~fallback:fallback ~post_params:post_params in
  register_post_session_url u action;
  u



(** Close a session *)
let close_session () = session_tree := empty_tree ()

let make_http_params url_suffix get_params post_params useragent ip = 
  {url_suffix = (reconstruct_url_string_option url_suffix);
   useragent=useragent;
   ip=ip;
   get_params = get_params;
   post_params = post_params}

let state_param_name = "__state__"

(** Functions to construct web pages: *)

let link name (url : ('a, insideforml,'c,'d,'e,'f,'g) url) = 
  match url.url_state with
      None -> url.create_get_url
	(fun v -> << <a href=$v$>$str:name$</a> >>)
      | Some i -> url.create_get_url
	(fun v -> 
	   let vstateparam = (v^"?"^state_param_name^"="^(string_of_int i)) in
	     << <a href=$vstateparam$>$str:name$</a> >>)
(* Pas vraiment de moyen simple pour passer un paramètre POST dans un lien...
	   let stateparam = string_of_int i in
	   let link = "submit()" in
	     << <form method="post" action=$v$> 
	           <input type="hidden" name=$state_param_name$ 
			      value=$stateparam$/>
	           <input type="submit" style="background:none; border:none; cursor:pointer; color:red; text-align: left; line-height: 1" value=$name$/>
	        </form> >>) *)



let form_get (url : ('a,insideforml,'c,'d,'e,'f,'g) url) (f : 'a) =
  let urlname = (match url.url with Url_Prefix s | Url s -> 
		   reconstruct_url_string s) in
  let state_param =
    (match  url.url_state with
	 None -> `PCData ""
       | Some i -> 
	   let i' = string_of_int i in
	  << <input type="hidden" name=$state_param_name$ value=$i'$/> >>)
  in
  let inside = url.create_get_form f in
(* `Form ([(`Method, "get"); (`Action, urlname)], state_param::inside) *)
  << <form method="get" action=$urlname$>
	$state_param$
        $list:inside$
     </form> >>


let form_post (url : ('a,'b,'c,'d,'e,'f,'g) url) (f : 'b) = 
  let state_param =
    (match  url.url_state with
	 None -> `PCData ""
       | Some i -> 
	   let i' = string_of_int i in
	  << <input type="hidden" name=$state_param_name$ value=$i'$/> >>)
  in
  url.create_get_url
    (fun v -> 
       let inside = url.create_post_form f in
(*	 `Form ([(`Method, "post"); (`Action, v)], state_param::inside)) *)
	 << <form method="post" action=$v$>
	   $state_param$
	   $list:inside$
	   </form> >>)


let int_box (name : int name) = 
  << <input type="text" name=$name$/> >>

let string_box (name : string name) = 
  << <input type="text" name=$name$/> >>

let button (name : string) = 
  << <input type="submit" value=$name$/> >>


(** return a page from an url and parameters *)
let localhost = Unix.inet_addr_of_string "127.0.0.1"

let get_page 
    (url, internal_state, get_params, post_params, useragent)
    sockaddr cookie = 
  let ip = match sockaddr with
      Unix.ADDR_INET (ip,port) -> ip
    | _ -> localhost
  in
  let save_current_dir = !current_dir in
  let answer =
    let (tree, new_session) = 
      (match cookie with
	   None -> (new_session_table (), true)
	 | Some c -> try (Cookies.find cookie_table (ip,c), false)
	   with Not_found -> (new_session_table (), true))
    in
      session_tree := tree;
      try
	let ((action, working_dir), url_suffix) =
	  try (* D'abord recherche dans la table de session *)
	    print_endline "--- recherche dans la table de session :";
	    (find_url 
	       !session_tree
	       url
	       get_params
	       post_params
	       internal_state)
	  with Not_found -> try (* ensuite dans la table globale *)
	    print_endline "--- recherche dans la table globale :";
	    (find_url 
	       global_tree
	       url
	       get_params
	       post_params
	       internal_state)
	  with Not_found -> (* si pas trouvé avec, on essaie sans l'état *)
	    match internal_state with
		None -> raise Not_found
	      | _ -> try (* d'abord la table de session *)
		  print_endline "--- recherche dans la table de session, sans état :";
		  (find_url 
		     !session_tree
		     url
		     get_params
		     post_params
		     None)
		with Not_found -> (* ensuite dans la table globale *)
	    print_endline "--- recherche dans la table globale, sans état :";
		  (find_url 
		     global_tree
		     url
		     get_params
		     post_params
		     None)
	in 
	let page = 
	  absolute_change_dir working_dir;
	  action (make_http_params 
		    url_suffix get_params post_params useragent ip) in
	let cookie2 = 
	  if is_empty_table !session_tree
	  then ((if not new_session 
		 then match cookie with
		     Some c -> print_endline "J'efface la table de session <---------------------------------------------------"; Cookies.remove cookie_table (ip,c)
		   | None -> ());None)
	  else (if new_session 
		then let c = new_cookie () in
		  (Cookies.add cookie_table (ip,c) !session_tree;
		   Some c)
		else cookie)
	in (cookie2, page)
      with 
	  Omlet_Typing_Error -> (cookie, Error_pages.page_error_param_type)
	| Omlet_Wrong_parameter -> (cookie, Error_pages.page_bad_param)
  in current_dir := save_current_dir; answer

(** Module loading *)
exception Aaaaa_error_while_loading of string
	
let load_aaaaa_module ~dir ~cmo =
  let save_current_dir = !current_dir in
  try
    absolute_change_dir dir;
    Dynlink.loadfile cmo;
    current_dir := save_current_dir
  with Dynlink.Error e -> 
    current_dir := save_current_dir;
    raise (Aaaaa_error_while_loading (cmo^" ("^(Dynlink.error_message e)^")"))
    | _ -> 
	current_dir := save_current_dir;
	raise (Aaaaa_error_while_loading cmo)
