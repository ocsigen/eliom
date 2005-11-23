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

open Http_frame
open Http_com
open Xhtmlpp

(** Type of answers from modules (web pages) *)
type page = Xhtmlpp.xhtml

(** Type of formulars *)
type xhformcontl = xhformcont list

(** type of URL, without parameter *)
type url_string = string list

(** Url matches the exact string
    Url_Prefix matches any string with this prefix
   (for ex to make the string "wiki/" activate all pages like "wiki/toto")
 *)
type url_activator = Url of url_string | Url_Prefix of url_string


(** Type of http parameters *)
type http_params = {url_suffix: string;
		    full_url: string;
		    current_url: string list;
		    useragent: string;
		    ip: Unix.inet_addr;
		    get_params: (string * string) list;
		    post_params: (string * string) list}


(** state is a parameter to differenciate
    several instances of the same URL.
	(for internal use)
 *)
type internal_state = int

let counter = let c = ref 0 in fun () -> c := !c + 1 ; !c

let new_state =
  let c : internal_state ref = ref (-1) in
  fun () -> c := !c + 1 ; Some !c

(* À revoir !!!!!!!!!!!!!!!!!!!!!!!!! Faire des cookies plus compliqués *)
let new_cookie =
  let c = ref (-1) in
  fun () -> c := !c + 1 ; string_of_int !c

exception Ocsigen_Typing_Error of string
exception Ocsigen_Wrong_parameter
exception Ocsigen_404

type postorget = Get | Post

let find_param name pog http_params =
  try 
    match pog with
	Post -> List.assoc name http_params.post_params
      | Get -> List.assoc name http_params.get_params
  with Not_found -> raise Ocsigen_Wrong_parameter

let id x = x

(** Type of names in a formular *)
type 'a name = string

let write_param name value = name^"="^value

(** Constructors for parameters *)
(** The type of conversion_function is ('a -> httpparam -> page)
    where 'a has the shape 'b -> 'c ->... -> page
    The string list is the list of required parameters names
*)
type ('a, 'b, 'c, 'dalink, 'dform, 'dheadlink, 'dscript) parameters =
    {param_names: string list;
     write_parameters_alink: (string option -> xhalink) -> 'dalink;
     (* corresponds to the 3rd argument in ('a,'b,'calink,...) url
	'dalink is for example int -> int -> 'alink, 
	and the first function is the function to apply after construction
     *)
     write_parameters_form: (string option -> xhform) -> 'dform;
     write_parameters_headlink: (string option -> xhheadlink) -> 'dheadlink;
     write_parameters_script: (string option -> xhscript) -> 'dscript;
     give_form_parameters: 'c;
     (* 'c is exactly the same as 'a or 'b in ('a,'b,'c) url type,
	usually something like (int name -> int name -> xhform) -> xhform
	(but we may have something else instead of form when constructing
	the value)
     *)
     conversion_function: postorget -> 'a -> http_params -> 'b 
       (* for dynamic typing,
	  for ex (int -> int -> page) -> http_params -> page 
	  (but 'b can be more complicated, for ex in register_post_url)
       *)}

let _noparam = 
  let write_parameters = (fun f -> f None) in
  {param_names=[];
   write_parameters_alink = 
   (write_parameters :> (string option -> xhalink) -> [>xhalink]);
   write_parameters_form = 
   (write_parameters :> (string option -> xhform) -> [>xhform]);
   write_parameters_headlink = 
   (write_parameters :> (string option -> xhheadlink) -> [>xhheadlink]);
   write_parameters_script = 
   (write_parameters :> (string option -> xhscript) -> [>xhscript]);
   give_form_parameters = id;
   conversion_function= (fun pog f _ -> f)}

let _string name = 
  let write_parameters = (fun f s -> f (Some (write_param name s))) in
  {param_names=[name];
   write_parameters_alink = 
   (write_parameters :> (string option -> xhalink) -> string -> [>xhalink]);
   write_parameters_form = 
   (write_parameters :> (string option -> xhform) -> string -> [>xhform]);
   write_parameters_headlink = 
   (write_parameters :> (string option -> xhheadlink) -> string -> [>xhheadlink]);
   write_parameters_script = 
   (write_parameters :> (string option -> xhscript) -> string -> [>xhscript]);
   give_form_parameters=(fun h -> h (name : string name));
   conversion_function= (fun pog f httpparam -> 
     f (find_param name pog httpparam))}

let _user_type (mytype_of_string : string -> 'a) string_of_mytype name = 
  let write_parameters =
    (fun f i -> f (Some (write_param name (string_of_mytype i)))) in
  {param_names=[name];
   write_parameters_alink = 
   (write_parameters :> (string option -> xhalink) -> 'a -> [>xhalink]);
   write_parameters_form = 
   (write_parameters :> (string option -> xhform) -> 'a -> [>xhform]);
   write_parameters_headlink = 
   (write_parameters :> (string option -> xhheadlink) -> 'a -> [>xhheadlink]);
   write_parameters_script = 
   (write_parameters :> (string option -> xhscript) -> 'a -> [>xhscript]);
   give_form_parameters=(fun h -> h (name : 'a name));
   conversion_function=(fun pog f httpparam -> 
     let p =
       let pa = (find_param name pog httpparam) in
       try mytype_of_string pa
       with _ -> raise (Ocsigen_Typing_Error name)
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
			      with _ -> raise (Ocsigen_Typing_Error name)
*)

let _int name = _user_type int_of_string string_of_int name

let _unit = 
  let write_parameters= (fun f -> f None) in
  {param_names=[];
   write_parameters_alink = 
   (write_parameters :> (string option -> xhalink) -> [>xhalink]);
   write_parameters_form = 
   (write_parameters :> (string option -> xhform) -> [>xhform]);
   write_parameters_headlink = 
   (write_parameters :> (string option -> xhheadlink) -> [>xhheadlink]);
   write_parameters_script = 
   (write_parameters :> (string option -> xhscript) -> [>xhscript]);
   give_form_parameters=id;
   conversion_function=(fun pog f httpparam -> f ())}
    

(* Dans une ancienne version on pouvait mettre des paramètres optionnels.
   mais la sémantique n'est pas claire.
   maintenant il faut enregistrer plusieurs URL différentes.
let _option p =
  (* By typing, _option can take only one parameter.
     So opt_param_names is empty and param_names contains at most one element
  *)
   let write_parameters=(fun f s -> match s with 
   None -> f None
   | Some v -> p.write_parameters f v) in
   {param_names=[];
   opt_param_names=p.param_names;
   write_parameters_alink = 
   (write_parameters :> (string option -> xhalink) -> [>xhalink]);
   write_parameters_form = 
   (write_parameters :> (string option -> xhform) -> [>xhform]);
   write_parameters_headlink = 
   (write_parameters :> (string option -> xhheadlink) -> [>xhheadlink]);
   write_parameters_script = 
   (write_parameters :> (string option -> xhscript) -> [>xhscript]);
   give_form_parameters=p.give_form_parameters;
   conversion_function=
   (fun f httpparam -> 
   try
   p.conversion_function (fun x -> f (Some x)) httpparam
   with Ocsigen_Wrong_parameter -> f None)}
 *)

let _useragent p = 
    {param_names=p.param_names;
     write_parameters_alink=p.write_parameters_alink;
     write_parameters_form=p.write_parameters_form;
     write_parameters_headlink=p.write_parameters_headlink;
     write_parameters_script=p.write_parameters_script;
     give_form_parameters=p.give_form_parameters;
     conversion_function=
     (fun pog f httpparam -> 
	p.conversion_function pog (f httpparam.useragent) httpparam)
    }

let _ip p = 
    {param_names=p.param_names;
     write_parameters_alink=p.write_parameters_alink;
     write_parameters_form=p.write_parameters_form;
     write_parameters_headlink=p.write_parameters_headlink;
     write_parameters_script=p.write_parameters_script;
     give_form_parameters=p.give_form_parameters;
     conversion_function=
     (fun pog f httpparam -> 
	let ip = Unix.string_of_inet_addr httpparam.ip in
        p.conversion_function pog (f ip) httpparam)
    }

let _current_url p = 
    {param_names=p.param_names;
     write_parameters_alink=p.write_parameters_alink;
     write_parameters_form=p.write_parameters_form;
     write_parameters_headlink=p.write_parameters_headlink;
     write_parameters_script=p.write_parameters_script;
     give_form_parameters=p.give_form_parameters;
     conversion_function=
     (fun pog f httpparam -> 
        p.conversion_function pog (f httpparam.current_url) httpparam)
    }

let _url_suffix p =
  let write_parameters wp =
    (fun f suffix -> wp
	(function None -> f (Some ("/"^suffix))
	  | Some v -> f (Some ("/"^suffix^"?"^v)))) in
  {param_names=p.param_names;
   write_parameters_alink = write_parameters p.write_parameters_alink;
   write_parameters_form = write_parameters p.write_parameters_form;
   write_parameters_headlink = write_parameters p.write_parameters_headlink;
   write_parameters_script = write_parameters p.write_parameters_script;
   give_form_parameters=p.give_form_parameters;
   conversion_function=
   (fun pog f httpparam -> p.conversion_function pog (f (httpparam.url_suffix)) httpparam)
 }

let _http_params p = 
    {param_names=p.param_names;
     write_parameters_alink=p.write_parameters_alink;
     write_parameters_form=p.write_parameters_form;
     write_parameters_headlink=p.write_parameters_headlink;
     write_parameters_script=p.write_parameters_script;
     give_form_parameters=p.give_form_parameters;
     conversion_function=
     (fun pog f httpparam -> 
        p.conversion_function pog (f httpparam) httpparam)
    }

let ( ** ) p1 p2 =
  let write_parameters p1wp p2wp =
    (fun s v1 v2 -> 
      (p2wp
	 (function 
	     Some ss -> 
	       (p1wp
		  (function Some s' -> s (Some (s'^"&"^ss))
		    | None -> assert false (*** ???????? ****)) v1)
	   | None -> (p1wp (fun s' -> s s') v1))
	 v2)) in
  {param_names=(p1.param_names@p2.param_names);
   (* p1.param_names has length 0 or 1 so @ is not expensive *)
   write_parameters_alink = 
   write_parameters p1.write_parameters_alink p2.write_parameters_alink;
   write_parameters_form = 
   write_parameters p1.write_parameters_form p2.write_parameters_form;
   write_parameters_headlink = 
   write_parameters p1.write_parameters_headlink p2.write_parameters_headlink;
   write_parameters_script = 
   write_parameters p1.write_parameters_script p2.write_parameters_script;
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
    [] -> ""
  | [a] -> a
  | a::l -> a^"/"^(reconstruct_url_string l)

let reconstruct_absolute_url_string current_url = reconstruct_url_string

let reconstruct_url_string_option = function
    None -> ""
  | Some l -> reconstruct_url_string l

let reconstruct_relative_url_string current_url u =
  let rec drop cururl desturl = match cururl, desturl with
    | a::l, [b] -> l, desturl
    | [a], m -> [], m
    | a::l, b::m when a = b -> drop l m
    | a::l, m -> l, m
    | [], m -> [], m
  in let rec makedotdot = function
    | [] -> ""
(*    | [a] -> "" *)
    | _::l -> "../"^(makedotdot l)
  in let aremonter, aaller = drop current_url (""::(!current_dir)@u)
  in (makedotdot aremonter)^(reconstruct_url_string aaller)



exception Static_File of string

(** We associate to an URL a function http_params -> page *)
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
      page_table_key * (http_params -> page) -> unit
    val add_action : 
      tree -> string -> string list -> (http_params -> unit) -> unit
    val add_static_dir : 
      tree -> name:string list -> location:string -> unit
    val find_url :
      tree ->
      url_string -> (string * string) list -> (string * string) list ->
      internal_state option -> ((http_params -> page) * url_string) 
      * url_string option
    val find_action :
      tree -> string -> (string * string) list -> 
      ((http_params -> unit) * url_string)
  end

module Directorytree : DIRECTORYTREE = struct
  (* Each node contains either a list of nodes (case directory)
     or a table of "answers" (functions that will generate the page) *)

  type page_table_key =
      {get_names: string list;
       post_names: string list;
       state: internal_state option}
       (* action: http_params -> page *)

  module Page_Table = Map.Make(struct type t = page_table_key 
				      let compare = compare end)

  module Action_Table = Map.Make(struct type t = string
					let compare = compare end)

  type table = 
      Vide
    | Table of ((http_params -> page) * url_string) Page_Table.t
	(* Here, the url_string is the working directory.
	   That is, the directory in which we are when we register
	   dynamically the pages.
	   Each time we load a page, we change to this directory
	   (in case the page registers new pages).
	*)

  type action_table = 
      AVide 
    | ATable of (string list * (http_params -> unit) *
		   url_string) Action_Table.t

  let empty_table () = Vide
  let empty_action_table () = AVide

  let add_table t (key,elt) = 
    match t with
	Vide -> Table (Page_Table.add key elt Page_Table.empty)
      | Table t -> Table (Page_Table.add key elt t)

  let find_table t k = 
    match t with
	Vide -> raise Not_found
      | Table t -> Page_Table.find k t

  let add_action_table at (key,elt) = 
    match at with
	AVide -> ATable (Action_Table.add key elt Action_Table.empty)
      | ATable t -> ATable (Action_Table.add key elt t)

  let find_action_table at k = 
    match at with
	AVide -> raise Not_found
      | ATable t -> Messages.warning "table pas vide - recherche";Action_Table.find k t

  type dircontent = Realdir of ((string * dir) list)
		    | Dirprefix of table ref
		    | StaticDir of string
  and dir = Dir of (table ref * dircontent ref)
  type tree = dir * (action_table ref)
      
  let empty_dir () = (Dir (ref (empty_table ()), ref (Realdir [])))

  let empty_tree () = ((empty_dir ()), ref (empty_action_table ()))

  let is_empty_table ((Dir (r1,r2)),at) = 
    (!r1 = Vide 
	&& !r2 = Realdir []
	&& !at = AVide)

  let add_action (_,actiontableref) name paramslist action =
    actiontableref :=
      add_action_table !actiontableref 
	(name,((List.sort compare paramslist),action,!current_dir))

  let find_action (_,atr) name paramslist =
    let paramsattendus,action,working_dir = find_action_table !atr name in
    let pl = List.sort compare (fst (List.split paramslist)) in
      if pl = paramsattendus 
      then action, working_dir
      else raise Not_found
    
  let rec search_table (Dir (table, dircontent_ref)) =
    let aux a l =
      (match !dircontent_ref with
           Realdir str_dir_list_ref ->
             (try
                search_table (List.assoc a str_dir_list_ref) l
	      with
                  Not_found ->
                    let new_dir = empty_dir () in
		      dircontent_ref :=
                        Realdir ((a, new_dir)::str_dir_list_ref);
		      search_table new_dir l)
         | Dirprefix _
	 | StaticDir _ -> if l = [] then table,dircontent_ref
           else let new_dir = empty_dir () in
             dircontent_ref := Realdir [(a, new_dir)];
             search_table new_dir l)
    in function
          [] -> table,dircontent_ref
      | ""::l -> search_table (Dir (table, dircontent_ref)) l
      | a::l -> aux a l

  let add_url (tree,_) url_act (page_table_key, action) =
    let content = ({get_names = List.sort compare page_table_key.get_names;
		    post_names = List.sort compare page_table_key.post_names;
		    state = page_table_key.state},
		   (action, !current_dir)) in
    let current_tree = Dir (search_table tree !current_dir) in
      match url_act with 
	  Url u2 ->
	    let table,dircontentref = 
	      (search_table current_tree (change_empty_list u2)) in
              table := add_table !table content
	| Url_Prefix u2 ->
            let _,dircontentref = 
	      (search_table current_tree (change_empty_list u2)) in
	    match !dircontentref with
	      Dirprefix tr -> tr := (add_table !tr content)
	    | _ ->
	      dircontentref := 
		Dirprefix (ref (add_table (empty_table ()) content))
	 
  let add_static_dir (tree,_) ~name ~location =
    let current_tree = Dir (search_table tree !current_dir) in
    let _,dircontentref = 
      (search_table current_tree (change_empty_list name)) in
      dircontentref := StaticDir location


  let find_url 
      (dir,_) string_list get_param_list post_param_list state_option =
    let rec search_table (Dir (tableref, dircontent_ref)) =
      let aux a l =
	(match !dircontent_ref with
             Realdir str_dir_list_ref ->
               search_table (List.assoc a str_dir_list_ref) l
           | Dirprefix tr -> !tr, (Some (a::l))
	   | StaticDir location -> 
	       raise (Static_File (reconstruct_url_string (location::a::l))))
      in function
            [] -> !tableref, None
	| ""::l -> search_table (Dir (tableref, dircontent_ref)) l
	| a::l -> aux a l
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
type ('a,'b,'calink,'cform,'cheadlink,'cscript,'d,'e,'f,'g) url = 
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
     create_get_form: 'a -> xhformcontl; 
       (* 'a is for example int name -> string name -> xhformcont *)
     create_post_form: 'b -> xhformcontl;  
                   (* idem, but for POST. If no post param 'b is xhformcont *)
     create_alink_url: string list -> (string -> xhalink) -> 'calink;
         (* 'c is for example
	    int -> int -> xhalink, 
	    function that will create
	    the name of the url, for ex the string "hello?int1=3&int2=4"
	    Then the function will be applied to this string to create a link
	    or a formular to this url.
	  *)
     create_form_url: string list -> (string -> xhform) -> 'cform;
     create_headlink_url: string list -> (string -> xhheadlink) -> 'cheadlink;
     create_script_url: string list -> (string -> xhscript) -> 'cscript;
     get_conversion_function: 'd -> http_params -> page;
         (* for dynamic typing, (as in parameters)
	    for ex int -> int -> page -> http_params -> page 
	    (but 'b can be more complicated, for ex in register_post_url)
	    We need this here in order to be able to re-register the same
	    being sure we use exactly the same parameter's names and types.
	    If we could have had parameters names in the type it wouldn't
	    have been necessary...
	  *)
     post_conversion_function: 'e -> http_params -> 'f;
    }
      


(** Create an url *)
let new_url_aux_aux
    ~(name : url_activator)
    ~params
    reconstruct_url_function
    : ('a, xhformcontl, 'calink,'cform,'cheadlink,'cscript, 'c, page, page, 'popo) url =
(* ici faire une vérification "duplicate parameter" ? SÉCURITÉ !! *) 
  let name = change_empty_activator name in
  let create_get_url write_param = 
    (match name with
      Url s -> 
	(fun current_url f -> 
	  let ss = 
	    (reconstruct_url_function current_url s) in
	  write_param
	    (function Some v -> f (ss^"?"^v)
	      | None -> f ss))
    | Url_Prefix s -> 
	(fun current_url f -> 			   
	  let ss = 
	    (reconstruct_url_function current_url s) in
	  write_param
	    (function Some v -> f (ss^v)
	      | None -> f ss)))
  in
  {url = name;
   url_state = None;
   get_param_names = params.param_names;
   post_param_names = [];
   create_get_form = params.give_form_parameters;
   create_post_form = id;
   create_alink_url = create_get_url params.write_parameters_alink;
   create_form_url = create_get_url params.write_parameters_form;
   create_headlink_url = create_get_url params.write_parameters_headlink;
   create_script_url = create_get_url params.write_parameters_script;
   get_conversion_function = params.conversion_function Get;
   post_conversion_function = (fun a http_params -> a)
  }

let new_url_aux
    ~(name : url_activator)
    ~params
    : ('a, xhformcontl, 'calink,'cform,'cheadlink,'cscript, 'c, page, page, 'popo internal_url) url =
  new_url_aux_aux ~name ~params reconstruct_relative_url_string

let new_external_url_aux
    ~(name : url_activator)
    ~params
    : ('a, xhformcontl, 'calink,'cform,'cheadlink,'cscript, 'c, page, page, external_url) url =
  new_url_aux_aux ~name ~params reconstruct_absolute_url_string

let new_url
    ~(name : url_activator)
    ~params
    : ('a, xhformcontl, 'calink,'cform,'cheadlink,'cscript, 'c, page, page, public_url internal_url) url =
  new_url_aux name params

let new_state_url
   ~(fallback : ('a, xhformcontl, 'calink,'cform,'cheadlink,'cscript, 'c, page, page, public_url internal_url)url)
    : ('a, xhformcontl, 'calink,'cform,'cheadlink,'cscript, 'c, page, page, state_url internal_url) url =
  {fallback with url_state = new_state ()}

let new_external_url
    ~(name : url_activator)
    ~params
    : ('a, xhformcontl, 'calink,'cform,'cheadlink,'cscript, 'c, page, page, external_url) url =
  new_external_url_aux name params

let register_url_aux
    tree
    state
    ~(url : ('a,xhformcontl,'calink,'cform,'cheadlink,'cscript,'d,'e,'f,'g) url)
    ~page =
(* ici faire une vérification "duplicate url" et REMPLACER si elle existe *)
  add_url tree url.url
    ({get_names = url.get_param_names;
      post_names = []; (* url.post_param_names; *)
      state = state},
     (url.get_conversion_function page))

let register_url 
    ~(url : ('a,xhformcontl,'calink,'cform,'cheadlink,'cscript,'d,'e,'f,'g internal_url) url)
    ~page =
  register_url_aux global_tree (url.url_state) url page

(* WARNING: if we create a new URL without registering it,
   we can have a link towards a page that does not exist!!! :-(
   The only way I see to avoid this is to impose a syntax extension
   like "let rec" for url...
 *)

let register_session_url
    ~(url : ('a,xhformcontl,'calink,'cform,'cheadlink,'cscript,'d,'e,'f,'g internal_url) url)
    ~page =
  register_url_aux !session_tree url.url_state url page

let register_new_url 
    ~name
    ~params
    ~page
    : ('a,xhformcontl,'calink,'cform,'cheadlink,'cscript, 'c, page, page, public_url internal_url) url =
  let u = (new_url name params) in
  register_url u page;
  u

let register_new_session_url
   ~(fallback : ('a, xhformcontl, 'calink,'cform,'cheadlink,'cscript, 'c, page, page, public_url internal_url)url)
   ~page
   : ('a,xhformcontl,'calink,'cform,'cheadlink,'cscript, 'c, page, page, state_url internal_url) url =
  let u = (new_state_url fallback) in
    register_session_url u page;
    u


(** Register an url with post parameters in the server *)
let new_post_url_aux
    ~fallback
    ~post_params
    : ('a,'b,'calink,'cform,'cheadlink,'cscript,'d,'e,'f, 'popo) url = 
(* ici faire une vérification "duplicate parameter" ? SÉCURITÉ !! *) 
  {url = fallback.url;
   url_state = None;
   get_param_names = fallback.get_param_names;
   post_param_names = post_params.param_names;
   create_get_form = fallback.create_get_form;
   create_post_form = post_params.give_form_parameters;
   create_alink_url = fallback.create_alink_url;
   create_form_url = fallback.create_form_url;
   create_headlink_url = fallback.create_headlink_url;
   create_script_url = fallback.create_script_url;
   get_conversion_function = fallback.get_conversion_function;
   post_conversion_function = post_params.conversion_function Post
  }

let new_post_url
    ~fallback
    ~post_params
    : ('a,'b,'calink,'cform,'cheadlink,'cscript,'d,'e,'f, public_url internal_url) url = 
  new_post_url_aux fallback post_params

let new_external_post_url
    ~(name : url_activator)
    ~params
    ~post_params
    : ('a,'b,'calink,'cform,'cheadlink,'cscript,'d,'e,'f, external_url) url = 
  new_post_url_aux (new_url_aux name params) post_params

let new_post_state_url
    ~(fallback : ('a,'b,'calink,'cform,'cheadlink,'cscript,'d,'e,'f, public_url internal_url) url)
    ~post_params 
    : ('aa,'bb,'ccalink,'ccform,'ccheadlink,'ccscript,'dd,'ee,'ff, state_url internal_url) url = 
  {fallback with 
   url_state = new_state ();
   post_param_names = post_params.param_names;
   create_post_form = post_params.give_form_parameters;
   post_conversion_function = post_params.conversion_function Post
  }

let register_post_url_aux
    tree
    state
    ~(url : ('a,'b->'bb,'calink,'cform,'cheadlink,'cscript,'d,'e,'f,'g) url)
    ~page =
(* ici faire une vérification "duplicate url" et REMPLACER si elle existe *)
  add_url tree url.url
    ({get_names = url.get_param_names;
      post_names = url.post_param_names;
      state = state},
     (fun http_params ->
	(url.get_conversion_function
	   (url.post_conversion_function page http_params) 
	   http_params)))
    (* Je n'arrive pas à mettre les params2 avant params pour des raisons
       de typage... *)

let register_post_url 
    ~(url : ('a,'b->'bb,'calink,'cform,'cheadlink,'cscript,'d,'e,'f,'g internal_url) url)
    ~page =
  register_post_url_aux global_tree (url.url_state) url page

let register_post_session_url
    ~(url : ('a,'b->'bb,'calink,'cform,'cheadlink,'cscript,'d,'e,'f,'g internal_url) url)
    ~page =
  register_post_url_aux !session_tree url.url_state url page

let register_new_post_url 
    ~fallback
    ~post_params
    ~page
    : ('a,'b,'calink,'cform,'cheadlink,'cscript,'d,'e,'f, public_url internal_url) url =
  let u = new_post_url ~fallback:fallback ~post_params:post_params in
  register_post_url u page;
  u

let register_new_post_session_url
    ~(fallback : ('a,'b,'calink,'cform,'cheadlink,'cscript,'d,'e,'f, public_url internal_url) url)
    ~post_params 
    ~page
    : ('aa,'bb,'ccalink,'ccform,'ccheadlink,'ccscript,'dd,'ee,'ff, state_url internal_url) url =
  let u = new_post_state_url ~fallback:fallback ~post_params:post_params in
  register_post_session_url u page;
  u


(** actions (new 10/05) *)
type ('a,'b) actionurl =
    {action_name: string;
     action_param_names: string list;
     create_action_form: 'a -> xhformcontl;
     action_conversion_function : 'b -> http_params -> unit}

let action_prefix = "__ocsaction__"
let action_name = "name"
let action_reload = "reload"

let new_action_name () = string_of_int (counter ())

let new_actionurl ~(params: ('a, unit, 'calink,'cform,'cheadlink,'cscript, 'd) parameters) =
  {
    action_name = new_action_name ();
    action_param_names = params.param_names;
    create_action_form = params.give_form_parameters;
    action_conversion_function = params.conversion_function Post
  }

let register_actionurl_aux tree ~actionurl ~action =
  add_action tree 
    actionurl.action_name
    actionurl.action_param_names
    (fun h -> actionurl.action_conversion_function action h)

let register_actionurl ~actionurl ~action =
  register_actionurl_aux global_tree actionurl action

let register_new_actionurl ~params ~action = 
  let a = new_actionurl params in
    register_actionurl a action;
    a

let register_session_actionurl ~actionurl ~action =
  register_actionurl_aux !session_tree actionurl action


let register_new_session_actionurl ~params ~action =
  let a = new_actionurl params in
    register_session_actionurl a action;
    a


(** static pages (new 10/05) *)
let register_new_static_directory_aux
    tree
    ~(name : url_string)
    ~(location : string)
    : ('a, xhformcontl, 'calink,'cform,'cheadlink,'cscript, 'c, page, page, 'd internal_url) url =
  add_static_dir tree name location;
  let create_get_url = 
    (fun current_url f -> 			   
      let ss = 
	(reconstruct_relative_url_string current_url name) in
      (fun suffix -> f (ss^"/"^suffix))
    )
  in
  {url = Url_Prefix name;
   url_state = None;
   get_param_names = [];
   post_param_names = [];
   create_get_form = id;
   create_post_form = id;
   create_alink_url = 
   (create_get_url :> string list -> (string -> xhalink) -> string -> [>xhalink]);
   create_form_url = 
   (create_get_url :> string list -> (string -> xhform) -> string -> [>xhform]);
   create_headlink_url = 
   (create_get_url :> string list -> (string -> xhheadlink) -> string -> [>xhheadlink]);
   create_script_url = 
   (create_get_url :> string list -> (string -> xhscript) -> string -> [>xhscript]);
   get_conversion_function = (fun a http_params -> a);
   post_conversion_function = (fun a http_params -> a)
  }

let register_new_static_directory
    ~(name : url_string)
    ~(location : string) :
    (xhformcontl, xhformcontl, 
     'calink,'cform,'cheadlink,'cscript,
     page, page, page, 
     public_url internal_url) url =
  register_new_static_directory_aux global_tree name location

let register_new_session_static_directory
    ~(name : url_string)
    ~(location : string) :
    (xhformcontl, xhformcontl, 
     'calink,'cform,'cheadlink,'cscript, 
     page, page, page, 
     public_url internal_url) url =
  register_new_static_directory_aux !session_tree name location


(** Close a session *)
let close_session () = session_tree := empty_tree ()

let make_http_params 
    url fullurl url_suffix get_params post_params useragent ip = 
  {url_suffix = (reconstruct_url_string_option url_suffix);
   full_url= fullurl;
   useragent=useragent;
   current_url=url;
   ip=ip;
   get_params = get_params;
   post_params = post_params}

let state_param_name = "__ocsetat__"

(** Functions to construct web pages: *)
let make_attrs ?size ?maxlength ?classe ?id ?title ?accesskey
    ?(disabled=false) ?(readonly=false) ?(checked=false) () =
  let rec make_class = function
      [] -> ""
    | [a] -> a
    | a::l -> a^" "^(make_class l)
  in
  let attrs = match size with
    Some s -> [`Size,(string_of_int s)] 
  | None -> [] in
  let attrs = match maxlength with
    Some s -> (`Maxlength,(string_of_int s))::attrs
  | None -> attrs in
  let attrs = match classe with
    Some s -> (`Class, (make_class s))::attrs
  | None -> attrs in
  let attrs = match id with
    Some s -> (`Id, s)::attrs
  | None -> attrs in
  let attrs = match title with
    Some s -> (`Title, s)::attrs
  | None -> attrs in
  let attrs = match accesskey with
    Some s -> (`Accesskey, s)::attrs
  | None -> attrs in
  let attrs = if disabled then (`Disabled,"disabled")::attrs else attrs in
  let attrs = if readonly then (`Readonly,"readonly")::attrs else attrs in
  let attrs = if checked then (`Checked,"checked")::attrs else attrs in
  attrs


(* à enlever !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
let create_url_string 
    current_url (url : ('a, xhformcontl,'calink,'cform,'cheadlink,'cscript,'d,'e,'f,'g) url) =
  match url.url_state with
      None -> url.create_get_url current_url
    | Some i -> url.create_get_url current_url
	(fun v -> 
	   let stateparam = string_of_int i in
	   let formname="hiddenform"^(string_of_int (counter ())) in
	   let href="javascript:document."^formname^".submit ()" in
	     << <a href=$href$>$str:name$<form name=$formname$ method="post" action=$v$ style="display:none">
	       <input type="hidden" name=$state_param_name$
			  value=$stateparam$/>
			  </form></a> >>) 
*)

let link (name :string) current_url
    (url : ('a, xhformcontl,'calink,'cform,'cheadlink,'cscript,'d,'e,'f,'g) url) =
  match url.url_state with
      None -> url.create_alink_url current_url
	(fun v -> << <a href=$v$>$str:name$</a> >>)
    | Some i -> url.create_alink_url current_url
	  (fun v -> 
   let vstateparam = (v^"?"^state_param_name^"="^(string_of_int i)) in
   << <a href=$vstateparam$>$str:name$</a> >>)

(* avec un formulaire caché (ça marche mais ce n'est pas du xhtml valide
	   let stateparam = string_of_int i in
	   let formname="hiddenform"^(string_of_int (counter ())) in
	   let href="javascript:document."^formname^".submit ()" in
	     << <a href=$href$>$str:name$<form name=$formname$ method="post" action=$v$ style="display:none">
	       <input type="hidden" name=$state_param_name$
			  value=$stateparam$/>
			  </form></a> >>) *)

(* 	   let stateparam = string_of_int i in
	     << <form name="hiddenform" method="post" action=$v$>
	           <input type="hidden" name=$state_param_name$
			      value=$stateparam$/>
                   <a href="javascript:document.hiddenform.submit ()">$str:name$</a>
	        </form> >>)

À VOIR ! IMPORTANT ! :

Pour les form get on peut faire pareil, du style :
<input type="button"
onClick="document.form1.submit();document.form2.submit()">
(problème : on n'a pas accès au bouton)

*)

let css_link current_url (url : ('a, xhformcontl,'calink,'cform,'cheadlink,'cscript,'d,'e,'f,'g) url) =
  url.create_headlink_url current_url
    (fun v -> << <link href=$v$ type="text/css" rel="stylesheet"/> >>)

let js_link current_url (url : ('a, xhformcontl,'calink,'cform,'cheadlink,'cscript,'d,'e,'f,'g) url) =
  url.create_script_url current_url
    (fun v -> << <script type="text/javascript" src=$v$> <!-- --> </script> >>)


(*	   let stateparam = string_of_int i in
	     << <span class="link" onclick="document.hiddenform.submit ()"><form name="hiddenform" method="post" action=$v$>
	           <input type="hidden" name=$state_param_name$
			      value=$stateparam$/>
	        </form>$str:name$</span> >>) *)
(* Pas vraiment de moyen simple pour passer un paramètre POST dans un lien...
	   let stateparam = string_of_int i in
	   let link = "submit()" in
	     << <form method="post" action=$v$> 
	           <input type="hidden" name=$state_param_name$
			      value=$stateparam$/>
	           <input type="submit" style="background:none; border:none; cursor:pointer; color:red; text-align: left; line-height: 1" value=$name$/>
	        </form> >>) *)



(*
let form_get current_url (url : ('a,xhformcontl,'c,'d,'e,'f,'g) url) (f : 'a) =
  let urlname = (match url.url with Url_Prefix s | Url s -> 
		   reconstruct_relative_url_string current_url s) in
  let inside = url.create_get_form f in
    (match  url.url_state with
	 None ->   << <form method="get" action=$urlname$>
           $list:inside$
	   </form> >>
       | Some i -> 
	   let i' = string_of_int i in
	   let formname="hiddenform"^(string_of_int (counter ())) in
	   let onsubmit="document."^formname^".submit();" in
	     << <form method="get" action=$urlname$ onsubmit=$onsubmit$>
	           <form name=$formname$ method="post" action=$urlname$ 
		           style="display:none">
                     <input type="hidden" name=$state_param_name$ value=$i'$/>
		   </form>
                   $list:inside$
                </form> >>)

ou alors faire un form POST et du javascript qui va mettre 
la chaîne ?blbla=truc&etc
*)


(*	   let i' = string_of_int i in
	   let formname="hiddenform"^(string_of_int (counter ())) in
	   let onsubmit="document."^formname^".submit();window.open('"^urlname^"','indexWindow',''); return true;" in
	     << <form method="get" action=$urlname$ onsubmit=$onsubmit$>
	           <form name=$formname$ method="post" action=$urlname$ 
		           style="display:none">
                     <input type="hidden" name=$state_param_name$ value=$i'$/>
		   </form>
                   $list:inside$
                </form> >>)
*)


let form_get current_url (url : ('a,xhformcontl,'calink,'cform,'cheadlink,'cscript,'d,'e,'f,'g) url) (f : 'a) =
  let urlname = (match url.url with Url_Prefix s | Url s -> 
		   reconstruct_relative_url_string current_url s) in
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
       <p style="display:none">
	$state_param$
       </p>
        $list:inside$
     </form> >>


let form_post current_url (url : ('a,'b,'calink,'cform,'cheadlink,'cscript,'d,'e,'f,'g) url) (f : 'b) = 
  let state_param =
    (match  url.url_state with
	 None -> `PCData ""
       | Some i -> 
	   let i' = string_of_int i in
	   << <input type="hidden" name=$state_param_name$ value=$i'$/> >>)
  in
  url.create_form_url current_url
    (fun v -> 
       let inside = url.create_post_form f in
(*	 `Form ([(`Method, "post"); (`Action, v)], state_param::inside)) *)
	 << <form method="post" action=$v$>
	   <p style="display:none">$state_param$</p>
	   $list:inside$
	   </form> >>)

(* actions : *)
let action_link ?(reload=true) name h actionurl =
  let formname="hiddenform"^(string_of_int (counter ())) in
  let href="javascript:document."^formname^".submit ()" in
  let action_param_name = action_prefix^action_name in
  let action_param = (actionurl.action_name) in
  let reload_name = action_prefix^action_reload in
  let reload_param = 
    if reload 
    then <:xmllist< <input type="hidden" name=$reload_name$ value=$reload_name$/> >> 
    else [] in
  let v = h.full_url in
    << <form name=$formname$ method="post" action=$v$ >
      <p><a href=$href$>$str:name$</a>
      <input type="hidden" name=$action_param_name$ value=$action_param$/>
      $list:reload_param$
      </p>
      </form> >>

let action_form
    ?(reload=true) ?classe ?id h (actionurl : ('a,'b) actionurl) (f : 'a) = 
  let action_param_name = action_prefix^action_name in
  let action_param = (actionurl.action_name) in
  let reload_name = action_prefix^action_reload in
  let action_line =
    << <input type="hidden" name=$action_param_name$ value=$action_param$/> >>
  in
  let v = h.full_url in
  let inside = actionurl.create_action_form f in
  let inside_reload = 
    if reload 
    then << <p><input type="hidden" name=$reload_name$ value=$reload_name$/></p> >> 
		       :: inside
    else inside in
  let attrs = make_attrs ?id ?classe () in
    << <form method="post" action=$v$ $list:attrs$>
         <p>$action_line$</p>
         $list:inside_reload$
       </form> >>


let gen_input ?size ?maxlength ?classe ?id ?title ?accesskey ?disabled ?readonly name = 
  let attrs = 
    make_attrs ?size ?maxlength ?classe ?id ?title ?accesskey ?disabled ?readonly () in
  << <input type="text" name=$name$ $list:attrs$ /> >>

let password_input ?size ?maxlength ?classe ?id ?title ?accesskey ?disabled ?readonly (name : string name) = 
  let attrs = 
    make_attrs ?size ?maxlength ?classe ?id ?title ?accesskey ?disabled ?readonly () in
  << <input type="password" name=$name$ $list:attrs$ /> >>

let int_input ?size ?maxlength ?classe ?id ?title ?accesskey ?disabled ?readonly (name : int name) = 
  gen_input ?size ?maxlength ?classe ?id ?title ?accesskey ?disabled ?readonly name

let hidden_int_input (name : int name) v = 
  let vv = string_of_int v in
  << <input type="hidden" name=$name$ value=$vv$/> >>

let string_input ?size ?maxlength ?classe ?id ?title ?accesskey ?disabled ?readonly (name : string name) = 
  gen_input ?size ?maxlength ?classe ?id ?title ?accesskey name

let submit_input ?classe ?id ?title ?accesskey ?disabled ?readonly (name : string) = 
  let attrs = make_attrs ?classe ?id ?title ?accesskey ?disabled ?readonly () in
  << <input type="submit" value=$name$ $list:attrs$ /> >>

let reset_input ?classe ?id ?title ?accesskey ?disabled ?readonly (name : string) = 
  let attrs = make_attrs ?classe ?id ?title ?accesskey ?disabled ?readonly () in
  << <input type="reset" value=$name$ $list:attrs$ /> >>

let checkbox_input ?classe ?id ?title ?accesskey ?disabled ?readonly ?checked
    (name : bool name) =
  let attrs = make_attrs ?classe ?id ?title ?accesskey ?disabled ?readonly ?checked () in
  << <input type="checkbox" value=$name$ $list:attrs$ /> >>

let radio_input ?classe ?id ?title ?accesskey ?disabled ?readonly ?checked
    (name : bool name) =
  let attrs = make_attrs ?classe ?id ?title ?accesskey ?disabled ?readonly ?checked () in
  << <input type="radio" value=$name$ $list:attrs$ /> >>

let textarea ?classe ?id ?title ?accesskey ?disabled ?readonly ~rows ~cols ?(dir:[`Rtl|`Ltr] option) (name : string name) = 
  let attrs = 
    make_attrs ?classe ?id ?title ?accesskey ?disabled ?readonly () in
  let attrs = 
    (`Rows, (string_of_int rows))::(`Cols, (string_of_int cols))::attrs in
  let attrs = match dir with
    Some `Rtl -> (`Dir, "rtl")::attrs
  | Some `Ltr -> (`Dir, "ltr")::attrs
  | None -> attrs in
  << <textarea name=$name$ $list:attrs$ /> >>

(*
let select ?classe ?id ?title ?disabled ?size ?(dir:[`Rtl|`Ltr] option)
    (name : string name) = 
  let attrs = 
    make_attrs ?classe ?id ?title ?disabled ?size () in
  let attrs = match dir with
    Some `Rtl -> (`Dir, "rtl")::attrs
  | Some `Ltr -> (`Dir, "ltr")::attrs
  | None -> attrs in
  let l =
  << <select name=$name$ $list:attrs$ >$l$</select>>>

let mutliple_select ?classe ?id ?title ?disabled ?size 
    ?(dir:[`Rtl|`Ltr] option) (name : string name) = 
  let attrs = 
    make_attrs ?classe ?id ?title ?disabled ?size () in
  let attrs = match dir with
    Some `Rtl -> (`Dir, "rtl")::attrs
  | Some `Ltr -> (`Dir, "ltr")::attrs
  | None -> attrs in
  let l =
  << <select name=$name$ multiple="multiple" $list:attrs$ >$l$</select>>>
*)

(** return a page from an url and parameters *)
let localhost = Unix.inet_addr_of_string "127.0.0.1"

let execute find 
    (url, fullurl, get_params, post_params, useragent)
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
      let ((action, working_dir), url_suffix) = find ()
      in 
      let page = 
	Messages.warning "Page found";
	absolute_change_dir working_dir;
	action 
	  (make_http_params
	     url fullurl url_suffix get_params post_params useragent ip) in
      let cookie2 = 
	if is_empty_table !session_tree
	then ((if not new_session 
	       then match cookie with
		   Some c -> Cookies.remove cookie_table (ip,c)
		 | None -> ());None)
	else (if new_session 
	      then let c = new_cookie () in
		(Cookies.add cookie_table (ip,c) !session_tree;
		 Some c)
	      else cookie)
      in (cookie2, page, ("/"^(reconstruct_url_string working_dir)))
  in current_dir := save_current_dir; 	
    answer


let get_page 
    (url, fullurl, internal_state, get_params, post_params, useragent)
    sockaddr cookie = 
  let find () =
    try (* D'abord recherche dans la table de session *)
      print_endline ("--- recherche "^(reconstruct_url_string url)^" dans la table de session :");
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
	  None -> raise Ocsigen_404
	| _ -> try (* d'abord la table de session *)
	    print_endline "--- recherche dans la table de session, sans état :";
	    (find_url 
	       !session_tree
	       url
	       get_params
	       post_params
	       None)
	  with Not_found -> (* ensuite dans la table globale *)
	    try 
	      print_endline "--- recherche dans la table globale, sans état :";
	      (find_url 
		 global_tree
		 url
		 get_params
		 post_params
		 None)
	    with Not_found -> raise Ocsigen_404
  in try 
    execute 
      find 
      (url, fullurl, get_params, post_params, useragent)
      sockaddr cookie
    with 
	Ocsigen_Typing_Error n -> 
	  (cookie, (Error_pages.page_error_param_type n), "/")
      | Ocsigen_Wrong_parameter -> (cookie, Error_pages.page_bad_param, "/")


let make_action action_name action_params 
    (url, fullurl, _, _, _, useragent) sockaddr cookie =
  let find () =
    (try 
       find_action !session_tree action_name action_params
     with Not_found ->
       (find_action global_tree action_name action_params)),None
  in try 
      execute 
	find
	(url,fullurl,[],action_params,useragent)
	sockaddr cookie
    with 
	Ocsigen_Typing_Error _ -> (cookie, (),"/")
      | Ocsigen_Wrong_parameter -> (cookie, (),"/")


(** Module loading *)
exception Ocsigen_error_while_loading of string
	
let load_ocsigen_module ~dir ~cmo =
  let save_current_dir = !current_dir in
  try
    absolute_change_dir dir;
    Dynlink.loadfile cmo;
    current_dir := save_current_dir
  with Dynlink.Error e -> 
    current_dir := save_current_dir;
    raise (Ocsigen_error_while_loading (cmo^" ("^(Dynlink.error_message e)^")"))
    | e -> 
	current_dir := save_current_dir;
	raise e (*Ocsigen_error_while_loading cmo*)


let number_of_sessions () = Cookies.length cookie_table
