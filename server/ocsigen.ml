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
open XHTML.M
open Xhtmltypes

(** Type of answers from modules (web pages) *)
type page = xhtml elt

(** Type of formulars *)
type xhformcontl = xhformcont elt list

(** type of URL, without parameter *)
type url_path = string list

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
exception Ocsigen_Is_a_directory
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
type ('a, 'b, 'c, 'da, 'dform, 'duri (*,'dimg, 'dlink, 'dscript *)) parameters =
    {param_names: string list;
     write_parameters_a: (string option -> xha elt) -> 'da;
     (* corresponds to the 3rd argument in ('a,'b,'ca,...) url
	'da is for example int -> int -> 'a, 
	and the first function is the function to apply after construction
     *)
     write_parameters_form: (string option -> xhform elt) -> 'dform;
(*     write_parameters_img: (string option -> xhimg elt) -> 'dimg;
     write_parameters_link: (string option -> xhlink elt) -> 'dlink;
     write_parameters_script: (string option -> xhscript elt) -> 'dscript; *)
     write_parameters_uri: (string option -> uri) -> 'duri;
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

let _noparam : ('a, 'a, 'b -> 'b, [>xha] elt, [>xhform] elt, uri (*, [>xhimg] elt, [>xhlink] elt, [>xhscript] elt*)) parameters = 
  let write_parameters = (fun f -> f None) in
  {param_names=[];
   write_parameters_a = 
   (write_parameters :> (string option -> xha elt) -> [>xha] elt);
   write_parameters_form = 
   (write_parameters :> (string option -> xhform elt) -> [>xhform] elt);
(*   write_parameters_img = 
   (write_parameters :> (string option -> xhimg elt) -> [>xhimg] elt);
   write_parameters_link = 
   (write_parameters :> (string option -> xhlink elt) -> [>xhlink] elt);
   write_parameters_script = 
   (write_parameters :> (string option -> xhscript elt) -> [>xhscript] elt); *)
   write_parameters_uri = 
   (write_parameters :> (string option -> uri) -> uri);
   give_form_parameters = id;
   conversion_function= (fun pog f _ -> f)}

let _string name = 
  let write_parameters = (fun f s -> f (Some (write_param name s))) in
  {param_names=[name];
   write_parameters_a = 
   (write_parameters :> (string option -> xha elt) -> string -> [>xha] elt);
   write_parameters_form = 
   (write_parameters :> (string option -> xhform elt) -> string -> [>xhform] elt);
(*   write_parameters_img = 
   (write_parameters :> (string option -> xhimg elt) -> string -> [>xhimg] elt);
   write_parameters_link = 
   (write_parameters :> (string option -> xhlink elt) -> string -> [>xhlink] elt);
   write_parameters_script = 
   (write_parameters :> (string option -> xhscript elt) -> string -> [>xhscript] elt); *)
   write_parameters_uri = 
   (write_parameters :> (string option -> uri) -> string -> uri);
   give_form_parameters=(fun h -> h (name : string name));
   conversion_function= (fun pog f httpparam -> 
     f (find_param name pog httpparam))}

let _user_type (mytype_of_string : string -> 'a) string_of_mytype name = 
  let write_parameters =
    (fun f i -> f (Some (write_param name (string_of_mytype i)))) in
  {param_names=[name];
   write_parameters_a = 
   (write_parameters :> (string option -> xha elt) -> 'a -> [>xha] elt);
   write_parameters_form = 
   (write_parameters :> (string option -> xhform elt) -> 'a -> [>xhform] elt);
(*   write_parameters_img = 
   (write_parameters :> (string option -> xhimg elt) -> 'a -> [>xhimg] elt);
   write_parameters_link = 
   (write_parameters :> (string option -> xhlink elt) -> 'a -> [>xhlink] elt);
   write_parameters_script = 
   (write_parameters :> (string option -> xhscript elt) -> 'a -> [>xhscript] elt);*)
   write_parameters_uri = 
   (write_parameters :> (string option -> uri) -> 'a -> uri);
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
   write_parameters_a = 
   (write_parameters :> (string option -> xha elt) -> [>xha] elt);
   write_parameters_form = 
   (write_parameters :> (string option -> xhform elt) -> [>xhform] elt);
(*   write_parameters_img = 
   (write_parameters :> (string option -> xhimg elt) -> [>xhimg] elt);
   write_parameters_link = 
   (write_parameters :> (string option -> xhlink elt) -> [>xhlink] elt);
   write_parameters_script = 
   (write_parameters :> (string option -> xhscript elt) -> [>xhscript] elt); *)
   write_parameters_uri = 
   (write_parameters :> (string option -> uri) -> uri);
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
   write_parameters_a = 
   (write_parameters :> (string option -> xha elt) -> [>xha] elt);
   write_parameters_form = 
   (write_parameters :> (string option -> xhform elt) -> [>xhform] elt);
(*   write_parameters_img = 
   (write_parameters :> (string option -> xhimg elt) -> [>xhimg] elt);
   write_parameters_link = 
   (write_parameters :> (string option -> xhlink elt) -> [>xhlink] elt);
   write_parameters_script = 
   (write_parameters :> (string option -> xhscript elt) -> [>xhscript] elt); *)
   write_parameters_uri = 
   (write_parameters :> (string option -> uri) -> uri);
   give_form_parameters=p.give_form_parameters;
   conversion_function=
   (fun f httpparam -> 
   try
   p.conversion_function (fun x -> f (Some x)) httpparam
   with Ocsigen_Wrong_parameter -> f None)}
 *)

let _useragent p = 
    {param_names=p.param_names;
     write_parameters_a=p.write_parameters_a;
     write_parameters_form=p.write_parameters_form;
(*     write_parameters_img=p.write_parameters_img;
     write_parameters_link=p.write_parameters_link;
     write_parameters_script=p.write_parameters_script; *)
     write_parameters_uri=p.write_parameters_uri;
     give_form_parameters=p.give_form_parameters;
     conversion_function=
     (fun pog f httpparam -> 
	p.conversion_function pog (f httpparam.useragent) httpparam)
    }

let _ip p = 
    {param_names=p.param_names;
     write_parameters_a=p.write_parameters_a;
     write_parameters_form=p.write_parameters_form;
(*     write_parameters_img=p.write_parameters_img;
     write_parameters_link=p.write_parameters_link;
     write_parameters_script=p.write_parameters_script; *)
     write_parameters_uri=p.write_parameters_uri;
     give_form_parameters=p.give_form_parameters;
     conversion_function=
     (fun pog f httpparam -> 
	let ip = Unix.string_of_inet_addr httpparam.ip in
        p.conversion_function pog (f ip) httpparam)
    }

let _current_url p = 
    {param_names=p.param_names;
     write_parameters_a=p.write_parameters_a;
     write_parameters_form=p.write_parameters_form;
(*     write_parameters_img=p.write_parameters_img;
     write_parameters_link=p.write_parameters_link;
     write_parameters_script=p.write_parameters_script;*)
     write_parameters_uri=p.write_parameters_uri;
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
   write_parameters_a = write_parameters p.write_parameters_a;
   write_parameters_form = write_parameters p.write_parameters_form;
(*   write_parameters_img = write_parameters p.write_parameters_img;
   write_parameters_link = write_parameters p.write_parameters_link;
   write_parameters_script = write_parameters p.write_parameters_script; *)
   write_parameters_uri = write_parameters p.write_parameters_uri;
   give_form_parameters=p.give_form_parameters;
   conversion_function=
   (fun pog f httpparam -> p.conversion_function pog (f (httpparam.url_suffix)) httpparam)
 }

let _http_params p = 
    {param_names=p.param_names;
     write_parameters_a=p.write_parameters_a;
     write_parameters_form=p.write_parameters_form;
(*     write_parameters_img=p.write_parameters_img;
     write_parameters_link=p.write_parameters_link;
     write_parameters_script=p.write_parameters_script;*)
     write_parameters_uri=p.write_parameters_uri;
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
   write_parameters_a = 
   write_parameters p1.write_parameters_a p2.write_parameters_a;
   write_parameters_form = 
   write_parameters p1.write_parameters_form p2.write_parameters_form;
(*   write_parameters_img = 
   write_parameters p1.write_parameters_img p2.write_parameters_img;
   write_parameters_link = 
   write_parameters p1.write_parameters_link p2.write_parameters_link;
   write_parameters_script = 
   write_parameters p1.write_parameters_script p2.write_parameters_script;*)
   write_parameters_uri = 
   write_parameters p1.write_parameters_uri p2.write_parameters_uri;
   give_form_parameters=
   (fun h -> p2.give_form_parameters (p1.give_form_parameters h));
   conversion_function=
   (fun pog f httpparam -> 
     (p2.conversion_function pog
	(p1.conversion_function pog f httpparam) httpparam))}
    

(* The current working directory *)
let current_dir : url_path ref = ref []

let defaultpagename = "index"

let remove_slash = function
    [] -> []
  | ""::l -> l
  | a::l -> a::l

let absolute_change_dir dir = current_dir := remove_slash dir

let change_empty_list = function
    [] -> [""] (* It is not possible to register an empty URL *)
  | l -> l


(* En fait cette fonction est dans Neturl (split_path)
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

let rec reconstruct_url_path = function
    [] -> ""
  | [a] -> a
  | a::l -> a^"/"^(reconstruct_url_path l)

let reconstruct_absolute_url_path current_url = reconstruct_url_path

let reconstruct_relative_url_path current_url u =
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
  in let aremonter, aaller = drop current_url (""::u)
  in let s = (makedotdot aremonter)^(reconstruct_url_path aaller) in
  if s = "" then defaultpagename else s



(** We associate to an URL a function http_params -> page *)
module type DIRECTORYTREE =
  sig
    type tables
    type page_table_key =
	{get_names: string list;
	 post_names: string list;
	 prefix:bool;
	 state: internal_state option}
    val empty_tables : unit -> tables
    val are_empty_tables : tables -> bool
    val add_url : tables -> url_path -> 
      page_table_key * (http_params -> page) -> unit
    val add_action : 
      tables -> string -> string list -> (http_params -> unit) -> unit
    val find_url :
      tables ->
      url_path -> (string * string) list -> (string * string) list ->
      internal_state option -> ((http_params -> page) * url_path)
      * url_path
    val find_action :
      tables -> string -> (string * string) list -> 
      ((http_params -> unit) * url_path)
  end

module Directorytree : DIRECTORYTREE = struct
  (* Each node contains either a list of nodes (case directory)
     or a table of "answers" (functions that will generate the page) *)

  type page_table_key =
      {get_names: string list;
       post_names: string list;
       prefix:bool;
       state: internal_state option}
       (* action: http_params -> page *)

  module Page_Table = Map.Make(struct type t = page_table_key 
				      let compare = compare end)

  module String_Table = Map.Make(struct type t = string
					let compare = compare end)

  type page_table = ((http_params -> page) * url_path) Page_Table.t
	(* Here, the url_path is the working directory.
	   That is, the directory in which we are when we register
	   dynamically the pages.
	   Each time we load a page, we change to this directory
	   (in case the page registers new pages).
	*)

  type action_table = 
      AVide 
    | ATable of (string list * (http_params -> unit) *
		   url_path) String_Table.t

  type dircontent = 
      Vide
    | Table of direlt ref String_Table.t

  and direlt = 
      Dir of dircontent ref
    | File of page_table ref

  type tables = dircontent ref * action_table ref

  let empty_page_table () = Page_Table.empty
  let empty_action_table () = AVide
  let empty_dircontent () = Vide

  let add_page_table t (key,elt) = Page_Table.add key elt t

  let find_page_table t k = Page_Table.find k t

  let add_dircontent dc (key,elt) =
    match dc with
      Vide -> Table (String_Table.add key elt String_Table.empty)
    | Table t -> Table (String_Table.add key elt t)

  let find_dircontent dc k =
    match dc with
      Vide -> raise Not_found
    | Table t -> String_Table.find k t

  let add_action_table at (key,elt) = 
    match at with
	AVide -> ATable (String_Table.add key elt String_Table.empty)
      | ATable t -> ATable (String_Table.add key elt t)

  let find_action_table at k = 
    match at with
	AVide -> raise Not_found
      | ATable t -> String_Table.find k t

  let empty_tables () = 
    (ref (empty_dircontent ()), ref (empty_action_table ()))

  let are_empty_tables (dcr,atr) = 
    (!dcr = Vide && !atr = AVide)

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

  let add_url (dircontentref,_) url_act (page_table_key, action) =
    let aux search dircontentref a l =
      try 
	let direltref = find_dircontent !dircontentref a in
	match !direltref with
	  Dir dcr -> search dcr l
	| File ptr ->
	    Messages.warning ("Ocsigen page registering: Page "^
				 a^" has been replaced by a directory");
	    let newdcr = ref (empty_dircontent ()) in
	    (direltref := Dir newdcr;
	     search newdcr l)
      with
	Not_found -> 
	  let newdcr = ref (empty_dircontent ()) in
	  (dircontentref := 
	    add_dircontent !dircontentref (a, ref (Dir newdcr));
	   search newdcr l)
    in 
    let rec search_page_table_ref dircontentref = function
          [] | [""] -> search_page_table_ref dircontentref [defaultpagename]
	| [a] -> 
	    (try 
	      let direltref = find_dircontent !dircontentref a in
	      (match !direltref with
		Dir _ ->
		  Messages.warning ("Ocsigen page registering: Directory "^
				       a^" has been replaced by a page");
		  let newpagetableref = ref (empty_page_table ()) in
		  (direltref := File newpagetableref;
		   newpagetableref)
	      | File ptr -> ptr)
	    with
	      Not_found ->
		let newpagetableref = ref (empty_page_table ()) in
		(dircontentref := 
		  add_dircontent !dircontentref (a,ref (File newpagetableref));
		 newpagetableref))
	| ""::l -> search_page_table_ref dircontentref l
	| a::l -> aux search_page_table_ref dircontentref a l
    (* and search_dircontentref dircontentref = function
          [] -> dircontentref
	| ""::l -> search_dircontentref dircontentref l
	| a::l -> aux search_dircontentref a l *)
    in
    let content = ({get_names = List.sort compare page_table_key.get_names;
		    post_names = List.sort compare page_table_key.post_names;
		    prefix = page_table_key.prefix;
		    state = page_table_key.state},
		   (action, !current_dir)) in
    (* let current_dircontentref = 
      search_dircontentref dircontentref !current_dir in *)
    let page_table_ref = 
      search_page_table_ref (*current_*)dircontentref url_act in
    page_table_ref := add_page_table !page_table_ref content

	 
  let find_url 
      (dircontentref,_) 
      string_list get_param_list post_param_list state_option =
    let rec search_page_table dircontent =
      let aux a l =
	(match !(find_dircontent dircontent a) with
	  Dir dircontentref2 -> search_page_table !dircontentref2 l
	| File page_table_ref -> !page_table_ref, l)
      in function
          [] -> raise Ocsigen_Is_a_directory
        | [""] -> aux defaultpagename []
	| ""::l -> search_page_table dircontent l
	| a::l -> aux a l
    in
    let get_names  = List.sort compare (fst (List.split  get_param_list))
    and post_names = List.sort compare (fst (List.split post_param_list)) in
    let page_table, suffix = 
      (search_page_table !dircontentref (change_empty_list string_list)) in
    ((find_page_table 
	page_table
	{get_names = get_names;
	 post_names = post_names;
	 prefix = (suffix <> []);
	 state = state_option}),
     suffix)
end

open Directorytree

type url_table = tables

let global_tables =  (empty_tables ())
let session_tables = ref (empty_tables ())
let new_session_tables = empty_tables
let are_empty_tables = are_empty_tables


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
type ('a,'b,'ca,'cform,'curi(*,'cimg,'clink,'cscript*),'d,'e,'f,'g) url = 
    {url: url_path; (* name of the URL without parameters *)
     url_prefix: bool;
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
     create_a_url: string list -> (string -> xha elt) -> 'ca;
         (* 'c is for example
	    int -> int -> xha, 
	    function that will create
	    the name of the url, for ex the string "hello?int1=3&int2=4"
	    Then the function will be applied to this string to create a link
	    or a formular to this url.
	  *)
     create_form_url: string list -> (string -> xhform elt) -> 'cform;
(*     create_img_url: string list -> (string -> xhimg elt) -> 'cimg;
     create_link_url: string list -> (string -> xhlink elt) -> 'clink;
     create_script_url: string list -> (string -> xhscript elt) -> 'cscript; *)
     create_uri: string list -> (string -> uri) -> 'curi;
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
    ~(path : url_path)
    ~prefix
    ~params
    reconstruct_url_function
    : ('a, xhformcontl, 'ca,'cform,'curi(*'cimg,'clink,'cscript*), 'c, page, page, 'popo) url =
(* ici faire une vérification "duplicate parameter" ? SÉCURITÉ !! *) 
  let create_get_url write_param = 
    (if prefix then
      (fun current_url f -> 			   
	let ss = 
	  (reconstruct_url_function current_url path) in
	write_param
	  (function Some v -> f (ss^v)
	    | None -> f ss))
    else
      (fun current_url f -> 
	let ss = 
	  (reconstruct_url_function current_url path) in
	write_param
	  (function Some v -> f (ss^"?"^v)
	    | None -> f ss)))
  in
  {url = path;
   url_prefix = prefix;
   url_state = None;
   get_param_names = params.param_names;
   post_param_names = [];
   create_get_form = params.give_form_parameters;
   create_post_form = id;
   create_a_url = create_get_url params.write_parameters_a;
   create_form_url = create_get_url params.write_parameters_form;
(*   create_img_url = create_get_url params.write_parameters_img;
   create_link_url = create_get_url params.write_parameters_link;
   create_script_url = create_get_url params.write_parameters_script;*)
   create_uri = create_get_url params.write_parameters_uri;
   get_conversion_function = params.conversion_function Get;
   post_conversion_function = (fun a http_params -> a)
  }

let new_url_aux
    ~(path : url_path)
    ~prefix
    ~params
    : ('a, xhformcontl, 'ca,'cform,'curi(*'cimg,'clink,'cscript*), 'c, page, page, 'popo internal_url) url =
  new_url_aux_aux
    ~path:(!current_dir@(change_empty_list path))
    ~prefix
    ~params reconstruct_relative_url_path

let new_external_url_aux
    ~(path : url_path)
    ~prefix
    ~params
    : ('a, xhformcontl, 'ca,'cform,'curi(*'cimg,'clink,'cscript*), 'c, page, page, external_url) url =
  new_url_aux_aux ~path ~prefix ~params reconstruct_absolute_url_path

let new_url
    ~(path : url_path)
    ?(prefix=false)
    ~params ()
    : ('a, xhformcontl, 'ca,'cform,'curi(*'cimg,'clink,'cscript*), 'c, page, page, public_url internal_url) url =
  new_url_aux ~path ~prefix ~params

let new_state_url
   ~(fallback : ('a, xhformcontl, 'ca,'cform,'curi(*'cimg,'clink,'cscript*), 'c, page, page, public_url internal_url)url)
    : ('a, xhformcontl, 'ca,'cform,'curi(*'cimg,'clink,'cscript*), 'c, page, page, state_url internal_url) url =
  {fallback with url_state = new_state ()}

let new_external_url
    ~(path : url_path)
    ?(prefix=false)
    ~params ()
    : ('a, xhformcontl, 'ca,'cform,'curi(*'cimg,'clink,'cscript*), 'c, page, page, external_url) url =
  new_external_url_aux ~path ~prefix ~params

let register_url_aux
    tables
    state
    ~(url : ('a,xhformcontl,'ca,'cform,'curi(*'cimg,'clink,'cscript*),'d,'e,'f,'g) url)
    page =
(* ici faire une vérification "duplicate url" et REMPLACER si elle existe *)
  add_url tables url.url
    ({get_names = url.get_param_names;
      post_names = []; (* url.post_param_names; *)
      prefix = url.url_prefix;
      state = state},
     (url.get_conversion_function page))

let register_url 
    ~(url : ('a,xhformcontl,'ca,'cform,'curi(*'cimg,'clink,'cscript*),'d,'e,'f,'g internal_url) url)
    page =
  register_url_aux global_tables (url.url_state) url page

(* WARNING: if we create a new URL without registering it,
   we can have a link towards a page that does not exist!!! :-(
   The only way I see to avoid this is to impose a syntax extension
   like "let rec" for url...
 *)

let register_session_url
    ~(url : ('a,xhformcontl,'ca,'cform,'curi(*'cimg,'clink,'cscript*),'d,'e,'f,'g internal_url) url)
    page =
  register_url_aux !session_tables url.url_state url page

let register_new_url 
    ~path
    ?(prefix=false)
    ~params
    page
    : ('a,xhformcontl,'ca,'cform,'curi(*'cimg,'clink,'cscript*), 'c, page, page, public_url internal_url) url =
  let u = new_url ~prefix ~path ~params () in
  register_url u page;
  u

let register_new_session_url
   ~(fallback : ('a, xhformcontl, 'ca,'cform,'curi(*'cimg,'clink,'cscript*), 'c, page, page, public_url internal_url)url)
   page
   : ('a,xhformcontl,'ca,'cform,'curi(*'cimg,'clink,'cscript*), 'c, page, page, state_url internal_url) url =
  let u = (new_state_url fallback) in
    register_session_url u page;
    u


(** Register an url with post parameters in the server *)
let new_post_url_aux
    ~fallback
    ~post_params
    : ('a,'b,'ca,'cform,'curi(*'cimg,'clink,'cscript*),'d,'e,'f, 'popo) url = 
(* ici faire une vérification "duplicate parameter" ? SÉCURITÉ !! *) 
  {url = fallback.url;
   url_prefix = fallback.url_prefix;
   url_state = None;
   get_param_names = fallback.get_param_names;
   post_param_names = post_params.param_names;
   create_get_form = fallback.create_get_form;
   create_post_form = post_params.give_form_parameters;
   create_a_url = fallback.create_a_url;
   create_form_url = fallback.create_form_url;
(*   create_img_url = fallback.create_img_url;
   create_link_url = fallback.create_link_url;
   create_script_url = fallback.create_script_url; *)
   create_uri = fallback.create_uri;
   get_conversion_function = fallback.get_conversion_function;
   post_conversion_function = post_params.conversion_function Post
  }

let new_post_url
    ~fallback
    ~post_params
    : ('a,'b,'ca,'cform,'curi(*'cimg,'clink,'cscript*),'d,'e,'f, public_url internal_url) url = 
  new_post_url_aux fallback post_params

let new_external_post_url
    ~(path : url_path)
    ?(prefix=false)
    ~params
    ~post_params ()
    : ('a,'b,'ca,'cform,'curi(*'cimg,'clink,'cscript*),'d,'e,'f, external_url) url = 
  new_post_url_aux (new_url_aux path prefix params) post_params

let new_post_state_url
    ~(fallback : ('a,'b,'ca,'cform,'curi(*'cimg,'clink,'cscript*),'d,'e,'f, public_url internal_url) url)
    ~post_params 
    : ('aa,'bb,'cca,'ccform,'ccuri(*,'ccimg,'cclink,'ccscript*),'dd,'ee,'ff, state_url internal_url) url = 
  {fallback with 
   url_state = new_state ();
   post_param_names = post_params.param_names;
   create_post_form = post_params.give_form_parameters;
   post_conversion_function = post_params.conversion_function Post
  }

let register_post_url_aux
    tables
    state
    ~(url : ('a,'b->'bb,'ca,'cform,'curi(*'cimg,'clink,'cscript*),'d,'e,'f,'g) url)
    page =
(* ici faire une vérification "duplicate url" et REMPLACER si elle existe *)
  add_url tables url.url
    ({get_names = url.get_param_names;
      post_names = url.post_param_names;
      prefix = url.url_prefix;
      state = state},
     (fun http_params ->
	(url.get_conversion_function
	   (url.post_conversion_function page http_params) 
	   http_params)))
    (* Je n'arrive pas à mettre les params2 avant params pour des raisons
       de typage... *)

let register_post_url 
    ~(url : ('a,'b->'bb,'ca,'cform,'curi(*'cimg,'clink,'cscript*),'d,'e,'f,'g internal_url) url)
    page =
  register_post_url_aux global_tables (url.url_state) url page

let register_post_session_url
    ~(url : ('a,'b->'bb,'ca,'cform,'curi(*'cimg,'clink,'cscript*),'d,'e,'f,'g internal_url) url)
    page =
  register_post_url_aux !session_tables url.url_state url page

let register_new_post_url 
    ~fallback
    ~post_params
    page
    : ('a,'b,'ca,'cform,'curi(*'cimg,'clink,'cscript*),'d,'e,'f, public_url internal_url) url =
  let u = new_post_url ~fallback:fallback ~post_params:post_params in
  register_post_url u page;
  u

let register_new_post_session_url
    ~(fallback : ('a,'b,'ca,'cform,'curi(*'cimg,'clink,'cscript*),'d,'e,'f, public_url internal_url) url)
    ~post_params 
    page
    : ('aa,'bb,'cca,'ccform,'ccuri(*,'ccimg,'cclink,'ccscript*),'dd,'ee,'ff, state_url internal_url) url =
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

let new_actionurl ~(params: ('a, unit, 'ca,'cform,'curi(*'cimg,'clink,'cscript*), 'd) parameters) =
  {
    action_name = new_action_name ();
    action_param_names = params.param_names;
    create_action_form = params.give_form_parameters;
    action_conversion_function = params.conversion_function Post
  }

let register_actionurl_aux tables ~actionurl ~action =
  add_action tables 
    actionurl.action_name
    actionurl.action_param_names
    (fun h -> actionurl.action_conversion_function action h)

let register_actionurl ~actionurl ~action =
  register_actionurl_aux global_tables actionurl action

let register_new_actionurl ~params ~action = 
  let a = new_actionurl params in
    register_actionurl a action;
    a

let register_session_actionurl ~actionurl ~action =
  register_actionurl_aux !session_tables actionurl action


let register_new_session_actionurl ~params ~action =
  let a = new_actionurl params in
    register_session_actionurl a action;
    a

let static_dir =
  let create_get_url = (fun current_url f -> (fun suffix -> f suffix))
  in
  {url = [""];
   url_state = None;
   url_prefix = true;
   get_param_names = [];
   post_param_names = [];
   create_get_form = id;
   create_post_form = id;
   create_a_url = 
   (create_get_url :> string list -> (string -> xha elt) -> string -> [>xha] elt);
   create_form_url = 
   (create_get_url :> string list -> (string -> xhform elt) -> string -> [>xhform] elt);
(*   create_img_url = 
   (create_get_url :> string list -> (string -> xhimg elt) -> string -> [>xhimg] elt);
   create_link_url = 
   (create_get_url :> string list -> (string -> xhlink elt) -> string -> [>xhlink] elt);
   create_script_url = 
   (create_get_url :> string list -> (string -> xhscript elt) -> string -> [>xhscript] elt); *)
   create_uri = 
   (create_get_url :> string list -> (string -> uri) -> string -> uri);
   get_conversion_function = (fun a http_params -> a);
   post_conversion_function = (fun a http_params -> a)
  }

(** static pages (new 10/05) (removed 13/12/05) *)
(*
let register_new_static_directory_aux
    tables
    ~(path : url_path)
    ~(location : string)
    : ('a, xhformcontl, 'ca,'cform,'curi(*'cimg,'clink,'cscript*), 'c, page, page, 'd internal_url) url =
  add_static_dir tables path location;
  let create_get_url = 
    (fun current_url f -> 			   
      let ss = 
	(reconstruct_relative_url_path current_url path) in
      (fun suffix -> f (ss^"/"^suffix))
    )
  in
  {url = Url_Prefix path;
   url_state = None;
   get_param_names = [];
   post_param_names = [];
   create_get_form = id;
   create_post_form = id;
   create_a_url = 
   (create_get_url :> string list -> (string -> xha elt) -> string -> [>xha] elt);
   create_form_url = 
   (create_get_url :> string list -> (string -> xhform elt) -> string -> [>xhform] elt);
(*   create_img_url = 
   (create_get_url :> string list -> (string -> xhimg elt) -> string -> [>xhimg] elt);
   create_link_url = 
   (create_get_url :> string list -> (string -> xhlink elt) -> string -> [>xhlink] elt);
   create_script_url = 
   (create_get_url :> string list -> (string -> xhscript elt) -> string -> [>xhscript] elt); *)
   create_uri = 
   (create_get_url :> string list -> (string -> uri) -> string -> uri);
   get_conversion_function = (fun a http_params -> a);
   post_conversion_function = (fun a http_params -> a)
  }

let register_new_static_directory
    ~(path : url_path)
    ~(location : string) :
    (xhformcontl, xhformcontl, 
     'ca,'cform,'curi(*'cimg,'clink,'cscript*),
     page, page, page, 
     public_url internal_url) url =
  register_new_static_directory_aux global_tables path location

let register_new_session_static_directory
    ~(path : url_path)
    ~(location : string) :
    (xhformcontl, xhformcontl, 
     'ca,'cform,'curi(*'cimg,'clink,'cscript*), 
     page, page, page, 
     public_url internal_url) url =
  register_new_static_directory_aux !session_tables path location
*)

(** Close a session *)
let close_session () = session_tables := empty_tables ()

let make_http_params 
    url fullurl url_suffix get_params post_params useragent ip = 
  {url_suffix = (reconstruct_url_path url_suffix);
   full_url= fullurl;
   useragent=useragent;
   current_url=url;
   ip=ip;
   get_params = get_params;
   post_params = post_params}

let state_param_name = "__ocsetat__"






(** Functions to construct web pages: *)
let make_attrs ?size ?maxlength ?classe ?id ?title ?accesskey ?alt
    ?(disabled=false) ?(readonly=false) ?(checked=false) () =
  let rec make_class = function
      [] -> ""
    | [a] -> a
    | a::l -> a^" "^(make_class l)
  in
  let attrs = match size with
    Some s -> [XML.string_attrib "size" (string_of_int s)] 
  | None -> [] in
  let attrs = match maxlength with
    Some s -> (XML.string_attrib "maxlength" (string_of_int s))::attrs
  | None -> attrs in
  let attrs = match classe with
    Some s -> (XML.string_attrib "class"  (make_class s))::attrs
  | None -> attrs in
  let attrs = match id with
    Some s -> (XML.string_attrib "id" s)::attrs
  | None -> attrs in
  let attrs = match alt with
    Some s -> (XML.string_attrib "alt" s)::attrs
  | None -> attrs in
  let attrs = match title with
    Some s -> (XML.string_attrib "title" s)::attrs
  | None -> attrs in
  let attrs = match accesskey with
    Some s -> (XML.string_attrib "accesskey" s)::attrs
  | None -> attrs in
  let attrs = if disabled then (XML.string_attrib "disabled" "disabled")::attrs else attrs in
  let attrs = if readonly then (XML.string_attrib "readonly" "readonly")::attrs else attrs in
  let attrs = if checked then (XML.string_attrib "checked" "checked")::attrs else attrs in
  attrs
    
let make_a ?(a=[]) l = XHTML.M.a ~a:a l



(* à enlever !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   let create_url_path 
   current_url (url : ('a, xhformcontl,'ca,'cform,'curi(*'cimg,'clink,'cscript*),'d,'e,'f,'g) url) =
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

  let a ?(a=[])
      (url : ('a, xhformcontl,'ca,'cform,'curi(*'cimg,'clink,'cscript*),'d,'e,'f,'g) url) current_url content =
    match url.url_state with
      None -> url.create_a_url current_url
	  (fun v -> make_a ~a:((a_href (make_uri_from_string v))::a) content)
    | Some i -> url.create_a_url current_url
	  (fun v -> 
	    let vstateparam = (v^"?"^state_param_name^"="^(string_of_int i)) in
	    make_a ~a:((a_href (make_uri_from_string vstateparam))::a) content)

let a_ = a
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

let css_link ?(a=[]) uri =
      link ~a:((a_href uri)::
	       (a_type "text/css")::(a_rel [`Stylesheet])::a) ()

let js_script ?(a=[]) uri =
  script ~a:((a_src uri)::a) ~contenttype:"text/javascript" (pcdata "")

(*
let css_link ?(a=[]) (url : ('a, xhformcontl,'ca,'cform,'curi(*'cimg,'clink,'cscript*),'d,'e,'f,'g) url) current_url =
  url.create_link_url current_url
    (fun v -> 
      link ~a:((a_href (make_uri_from_string  v))::(a_type "text/css")::(a_rel [`Stylesheet])::a) ())

let script ?(a=[]) (url : ('a, xhformcontl,'ca,'cform,'curi(*'cimg,'clink,'cscript*),'d,'e,'f,'g) url) current_url =
  url.create_script_url current_url
    (fun v -> 
      script ~a:((a_src v)::a) ~contenttype:"text/javascript" (pcdata ""))
*)

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
   let form_get (url : ('a,xhformcontl,'c,'d,'e,'f,'g) url) current_url (f : 'a) =
   let urlname = (match url.url with Url_Prefix s | Url s -> 
   reconstruct_relative_url_path current_url s) in
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


let form_get ?(a=[])
    (url : ('a,xhformcontl,'ca,'cform,'curi(*'cimg,'clink,'cscript*),'d,'e,'f,'g) url) current_url (f : 'a) =
  let urlname = reconstruct_relative_url_path current_url url.url in
  let state_param =
    (match  url.url_state with
      None -> []
    | Some i -> 
	let i' = string_of_int i in
	[<< <input type="hidden" name=$state_param_name$ value=$i'$/> >>])
  in
  let inside = url.create_get_form f in
(* `Form ([(`Method, "get"); (`Action, urlname)], state_param::inside) *)
  form ~a:((a_method `Get)::a) ~action:(make_uri_from_string urlname)
    << <p style="display:none">
      $list:state_param$
      </p> >>
    inside


    let form_post ?(a=[])
	(url : ('a,'b,'ca,'cform,'curi(*'cimg,'clink,'cscript*),'d,'e,'f,'g) url) current_url (f : 'b) = 
      let state_param =
	(match  url.url_state with
	  None -> []
	| Some i -> 
	    let i' = string_of_int i in
	    <:xmllist< <input type="hidden" name=$state_param_name$ value=$i'$/> >>)
      in
      url.create_form_url current_url
	(fun v -> 
	  let inside = url.create_post_form f in
(*	 `Form ([(`Method, "post"); (`Action, v)], state_param::inside)) *)
	  form ~a:((a_method `Post)::a) ~action:(make_uri_from_string v)
	    << <p style="display:none">
          	  $list:state_param$
	       </p> >>
	  inside)

let make_uri 
    (url : ('a, xhformcontl,'ca,'cform,'curi(*'cimg,'clink,'cscript*),'d,'e,'f,'g) url) current_url =
  match url.url_state with
    None -> url.create_uri current_url make_uri_from_string
  | Some i -> url.create_uri current_url
	(fun v -> make_uri_from_string 
	    (v^"?"^state_param_name^"="^(string_of_int i)))


(*
let img ?a ~alt
    (url : ('a, xhformcontl,'ca,'cform,'curi(*'cimg,'clink,'cscript*),'d,'e,'f,'g) url) current_url =
  match url.url_state with
    None -> url.create_img_url current_url
	(fun v -> img ~src:v ~alt ?a ())
  | Some i -> url.create_img_url current_url
	(fun v -> 
	  let vstateparam = (v^"?"^state_param_name^"="^(string_of_int i)) in
	  img ~src:vstateparam ~alt ?a ())
*)

(* actions : *)
let action_link ?(a=[]) ?(reload=true) actionurl h content =
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
    <p>$XHTML.M.a ~a:((a_href (make_uri_from_string href))::a) content$
    <input type="hidden" name=$action_param_name$ value=$action_param$/>
	$list:reload_param$
	</p>
	</form> >>

    let action_form ?(a=[])
	?(reload=true) ?classe ?id (actionurl : ('a,'b) actionurl) h (f : 'a) = 
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
	then <:xmllist< <p><input type="hidden" name=$reload_name$ value=$reload_name$/></p>$list:inside$ >>
	else inside in
      let attrs = make_attrs ?id ?classe () in
	  form ~a:((a_method `Get)::a) ~action:(make_uri_from_string v)
	    << <p>$action_line$</p> >>
            inside_reload






let gen_input ?(a=[]) name = 
  input ~a:((a_name name)::(a_input_type `Text)::a) ()

let password_input ?(a=[]) (name : string name) = 
  input ~a:((a_name name)::(a_input_type `Password)::a) ()

let int_input ?a (name : int name) = gen_input ?a name
let string_input ?a (name : string name) = gen_input ?a name

let hidden_int_input ?(a=[]) (name : int name) v = 
  let vv = string_of_int v in
  input ~a:((a_name name)::(a_input_type `Hidden)::(a_value vv)::a) ()

let checkbox_input ?(a=[]) (name : bool name) =
  input ~a:((a_name name)::(a_input_type `Checkbox)::a) ()

let radio_input ?(a=[]) (name : bool name) =
  input ~a:((a_name name)::(a_input_type `Radio)::a) ()

let textarea ?(a=[]) (name : string name) =
  textarea ~a:((a_name name)::a)

let submit_input ?(a=[]) s =
  input ~a:((a_input_type `Submit)::(a_value s)::a) ()









(** return a page from an url and parameters *)
let localhost = Unix.inet_addr_of_string "127.0.0.1"

let execute find 
    (url, path, params, get_params, post_params, useragent)
    sockaddr cookie = 
  let ip = match sockaddr with
      Unix.ADDR_INET (ip,port) -> ip
    | _ -> localhost
  in
  let save_current_dir = !current_dir in
  let answer =
    let (tables, new_session) = 
      (match cookie with
	   None -> (new_session_tables (), true)
	 | Some c -> try (Cookies.find cookie_table (ip,c), false)
	   with Not_found -> (new_session_tables (), true))
    in
      session_tables := tables;
      let ((action, working_dir), url_suffix) = find ()
      in 
      let fullurl = path^params in
      let page = 
	Messages.debug "Page found";
	absolute_change_dir working_dir;
	action 
	  (make_http_params
	     url fullurl url_suffix get_params post_params useragent ip) in
      let cookie2 = 
	if are_empty_tables !session_tables
	then ((if not new_session 
	       then match cookie with
		   Some c -> Cookies.remove cookie_table (ip,c)
		 | None -> ());None)
	else (if new_session 
	      then let c = new_cookie () in
		(Cookies.add cookie_table (ip,c) !session_tables;
		 Some c)
	      else cookie)
      in (cookie2, page, ("/"^(reconstruct_url_path working_dir)))
  in current_dir := save_current_dir; 	
    answer


let get_page 
    (url, path, params, internal_state, get_params, post_params, useragent)
    sockaddr cookie = 
  let find () =
    try (* D'abord recherche dans la table de session *)
      Messages.debug ("--- recherche "^(reconstruct_url_path url)^" dans la table de session :");
      (find_url 
	 !session_tables
	 url
	 get_params
	 post_params
	 internal_state)
    with Not_found -> try (* ensuite dans la table globale *)
      Messages.debug "--- recherche dans la table globale :";
      (find_url 
	 global_tables
	 url
	 get_params
	 post_params
	 internal_state)
    with Not_found -> (* si pas trouvé avec, on essaie sans l'état *)
      match internal_state with
	  None -> raise Ocsigen_404
	| _ -> try (* d'abord la table de session *)
	    Messages.debug "--- recherche dans la table de session, sans état :";
	    (find_url 
	       !session_tables
	       url
	       get_params
	       post_params
	       None)
	  with Not_found -> (* ensuite dans la table globale *)
	    try 
	      Messages.debug "--- recherche dans la table globale, sans état :";
	      (find_url 
		 global_tables
		 url
		 get_params
		 post_params
		 None)
	    with Not_found -> raise Ocsigen_404
  in try 
    execute 
      find 
      (url, path, params, get_params, post_params, useragent)
      sockaddr cookie
    with 
	Ocsigen_Typing_Error n -> 
	  (cookie, (Error_pages.page_error_param_type n), "/")
      | Ocsigen_Wrong_parameter -> (cookie, Error_pages.page_bad_param, "/")


let make_action action_name action_params 
    (url, path, params, _, _, _, useragent) sockaddr cookie =
  let find () =
    (try 
       find_action !session_tables action_name action_params
     with Not_found ->
       (find_action global_tables action_name action_params)),[]
  in try 
      execute 
	find
	(url,path,params,[],action_params,useragent)
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
