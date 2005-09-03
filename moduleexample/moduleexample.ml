(* Copyright Vincent Balat 2005 *)
(* Do not redistribute without permission *)

open Omlet

(* ------------------------------------------------------------------ *)
(* To create a web page without parameter: *)
let plop = 
  register_new_url 
    ~name:(Url ["plop"]) 
    ~params:_noparam 
    ~action:<< <html><head></head><body> plop </body></html> >>

(* Pages can have side effects: *)
let plip = 
  let next =
    let c = ref 0 in
      (fun () -> c := !c + 1; !c)
  in
  register_new_url 
    ~name:(Url ["plip"]) 
    ~params:_unit
    ~action:(fun () -> 
	       let str = string_of_int (next ()) in 
	       << <html> $str:str$ </html> >>)

(* As usual in OCaml, you can forget labels when the application is total: *)
let plop2 = 
  register_new_url 
    (Url ["plop";"plip"])  (* the url plop/plip *)
    _noparam 
    << <html> plop2 </html> >>

let plop3 = 
  register_new_url 
    (Url ["plop";""])  (* the url plop/ is NOT equivalent to plop *)
    _noparam 
    << <html> plop3 </html> >>
(* plop3 will replace plop in the table *)


(* ------------------------------------------------------------------ *)
let repeteparams entier chaine chaine2 = 
  let i = string_of_int entier in
<<
  <html>
    Vous m'avez envoyé : 
    <b>$str:i$</b> et 
    <b>$str:chaine$</b> et 
    <b>$str:chaine2$</b>
  </html>
>>

(* you can register twice the same url, with different parameters names *)
let plop_params = register_new_url 
  (Url ["plop"])
  ((_int "entier") ** (_string "chaine") ** (_string "chaine2"))
  repeteparams
(* If you register twice exactly the same URL, the first one will 
   disappear! *)


(* ------------------------------------------------------------------ *)
(* A web page without parameter which has access to the user-agent 
   and which answers to any url of the form uaprefix/*
*)
let uaprefix = 
  register_new_url 
    (Url_Prefix ["uaprefix"]) 
    (_useragent (_ip (_url_suffix _noparam)))
    (fun ua ip suff -> 
	 <<
	   <html>
	     The suffix of the url is <b>$str:suff$</b>
             and your user-agent is <b>$str:ua$</b>
             and your IP is <b>$str:ip$</b>
           </html>
         >>)

let iprefix = 
  register_new_url (Url_Prefix ["iprefix"]) (_url_suffix (_int "i"))
    (fun suff i -> 
       let i' = string_of_int i in
<<
  <html>
  The suffix of the url is <b>$str:suff$</b> and i is equal to <b>$str:i'$</b>
  </html>
>>)


(* ------------------------------------------------------------------ *)
(* You can use your own types *)
type mysum = A | B
let mysum_of_string = function
    "A" -> A
  | "B" -> B
  | _ -> raise (Failure "mysum_of_string")
let string_of_mysum = function
    A -> "A"
  | B -> "B"

let mytype = register_new_url 
  (Url ["mytype"])
  (_user_type mysum_of_string string_of_mysum "valeur")
  (fun x -> let v = string_of_mysum x in
<<
  <html>
  $str:v$
  </html>
>>)



(* ------------------------------------------------------------------ *)
(* To create a link to a registered url, use the link function: *)

let links = register_new_url (Url ["links"]) _noparam 
  (let l = link "plop" plop in
   let ll = link "plop2" plop2 in
   let lll = link "uaprefix" uaprefix "suf" in
   let llll = link "plop_params" plop_params 45 "hello" "plpl" in
   let lllll = link "wikipedia" 
     (new_external_url
	(Url_Prefix ["http://fr.wikipedia.org";"wiki"])
	(_url_suffix _noparam)) "Ocaml" 
   in
     << <html> $l$ <br/> $ll$ <br/> $lll$ <br/> $llll$ <br/> $lllll$</html> >>)



(* Note new_external_url to create a link on external url *)
(* If you want to put a link towards a page that is not already defined,
   you can break register_new_url into new_url and register_url.
   Do not forget to register the url!!!
 *)

let linkrec = new_url (Url ["linkrec"]) _noparam

let _ = register_url linkrec 
  (let l = link "cliquez" linkrec in
     << <html> $l$ </html> >>)




(* ------------------------------------------------------------------ *)
(* To create a form, call the function form with the name of a registered
   url and a function that takes the names of the parameters and
   build the formular
*)
let create_form = 
     (fun entier chaine chaine2 ->
	let ib = int_box entier in
	let sb = string_box chaine in
	let sb2 = string_box chaine2 in
	let b = button "Cliquez" in
	  <:xmllist< Write an int: $ib$ <br/>
                     Write a string: $sb$ <br/>
                     Write a string: $sb2$ <br/>
	             $b$
	  >>)

let form = register_new_url (Url ["form"]) _noparam 
  (let f = form_get plop_params create_form in
  << <html> $f$ </html> >>)



(* ------------------------------------------------------------------ *)
(* By default parameters of a web page are in the URL (GET parameters).
   A web page may expect parameters from the http header (POST parameters)
   (that is parameters which are not in the URL)
   Use this if you don't want the user to be able to bookmark
   the URL with parameters, for example if you want to post some
   data which will change the state of the server (database, etc).
   When you register an URL with hidden parameters, you must register
   before an url (fallback) without these parameters (for example that will
   answer if the page is reloaded without the hidden parameters, or
   bookmarked).
 *)
let no_post_param_url = 
  register_new_url 
    ~name:(Url ["post"]) 
    ~params:_noparam 
    ~action:<< <html> Version of the page without POST parameters </html> >>
    
let my_url_with_post_params = register_new_post_url
    ~fallback:no_post_param_url
    ~post_params:(_string "value")
    ~action:(fun value -> 
	       << <html> $str:value$ </html> >>)

(* You can mix get and post parameters *)
let get_no_post_param_url = 
  register_new_url 
    ~name:(Url ["post2"]) 
    ~params:(_int "i")
    ~action:(fun i -> 
	       let i' = string_of_int i in
	       << <html> No POST parameter, i: <b>$str:i'$</b> </html> >>)
    
let my_url_with_get_and_post = register_new_post_url 
  ~fallback:get_no_post_param_url
  ~post_params:(_string "value")
  ~action:(fun value i -> 
	       let i' = string_of_int i in
	       << <html> Value: <b>$str:value$</b> <br/> 
		 i: <b>$str:i'$</b> </html> >>)


(* ------------------------------------------------------------------ *)
(* To create a POST form, use the form_post function,
   possibly applied to GET parameters (if any)
*)

let form2 = register_new_url (Url ["form2"]) _noparam 
  (let f = 
     (form_post my_url_with_post_params
	(fun chaine -> 
	   let sb = string_box chaine in
	     <:xmllist< Write a string: $sb$ >>)) in
     << <html> $f$ </html> >>)

let form3 = register_new_url (Url ["form3"]) _noparam 
  (let f = 
     (form_post my_url_with_get_and_post 
	(fun chaine -> 
	   let sb = string_box chaine in
	     <:xmllist< Write a string: $sb$ >>) 222) in
     << <html> $f$ </html> >>)

let form4 = register_new_url (Url ["form4"]) _noparam 
  (let f = 
     (form_post 
	(new_external_post_url 
	   ~name:(Url ["http://www.petitspois.com";"form"])
	   ~params:(_int "i")
	   ~post_params:(_string "chaine"))
	(fun chaine -> 
	   let sb = string_box chaine in
	     <:xmllist< Write a string: $sb$ >>) 222) in
     << <html> $f$ </html> >>)
       
(* note new_external_post_url *)


(* ------------------------------------------------------------------ *)
(* URL with state:
   You can define new urls that differs from public url only by a (hidden)
   parameter (internal state).
   To do that, use new_state_url and new_post_state_url.
   Actually the text of the URL need to be exactly the same as a public
   url, so that it doesn't fail if you bookmark the page.
   That's why new_state_url and new_post_state_url take a public
   URL as parameter (called fallback).
   The session URL will have the same GET parameters but may have
   different POST parameters.
   This session URL will be differenciated from the public one by an internal
   (hidden) parameter.

   This parameter is an hidden post
   parameter if possible... (but in the case of a GET form, it is not 
   possible :-( and for a link it is difficult to do...).

   ** I am not sure whether we should allow them in global table or not
   or only in session tables because of the problem of GET parameters ** 

   Solutions (?) : 
    - interdire les url avec état en dehors des sessions ?
    - trouver un moyen d'utiliser la redirection pour changer l'url au vol ?
    - ... ?
 *)

let ustate = new_url (Url ["state"]) _unit

let ustate2 = new_state_url ustate

let _ = 
  let c = ref 0 in
  let page () = 
    let i = string_of_int !c in
    let l = link "reload" ustate in
    let l2 = link "incr i" ustate2 in
    let l3 = form_post ustate2 
      (<:xmllist< <input type="submit" value="incr i" /> >>) in
    << <html> i vaut $str:i$ <br/> $l$ <br/> $l2$ <br/> $l3$ </html> >>
  in
    register_url ustate page;
    register_url ustate2 (fun () -> c := !c + 1; page ())


(* ------------------------------------------------------------------ *)
(* You can replace some public url by an url valid only for one session.
   To do that, use the optional parameter url_table of the make_page function.
   It contains a set of url you want to register only for this session.
   To create this value, use register_session_url or
   register_post_session_url
   (and session_table () for the empty table)

   Use this for example if you want two versions of each page,
   one public, one for connected users

   To close a session, use close_session ()
*)
let public_session_without_post_params = 
  new_url 
    ~name:(Url ["session"]) 
    ~params:_unit

let public_session_with_post_params = 
  new_post_url 
    ~fallback:public_session_without_post_params
    ~post_params:(_string "login")

let accueil () = 
  let f = form_post public_session_with_post_params
    (fun login -> 
       let sb = string_box login in
	 <:xmllist< login: $sb$ >>) in
    << <html> $f$ </html> >>

let _ = register_url
  ~url:public_session_without_post_params
  ~action:accueil

let rec launch_session login =
  let close = register_new_session_url (* See later *)
    ~fallback:public_session_without_post_params 
    ~action:(fun () -> close_session (); accueil ())
  in
  let new_main_page () =
    let l1 = link "plop" plop
    and l2 = link "plop2" plop2
    and l3 = link "links" links
    and l4 = link "close session" close in
    << <html>
         Bienvenue $str:login$ ! <br/>
         $l1$ <br/>
         $l2$ <br/>
         $l3$ <br/>
         $l4$ <br/>
      </html> >>
  in
    register_session_url 
      ~url:public_session_without_post_params 
                               (* url is any public url already registered *)
      ~action:new_main_page;
    register_session_url 
      plop (* any public url already registered *)
      << <html> Plop $str:login$ ! </html> >>;
    register_session_url 
      plop2
      << <html> Plop2 $str:login$ ! </html> >>;
    new_main_page
      
let _ =
  register_post_url
    ~url:public_session_with_post_params
    ~action:launch_session



(* ------------------------------------------------------------------ *)
(* You can register url with states in session tables.
   Use this if you want a link or a form which depends precisely on an
   instance of the web page, for example to buy something on an internet shop.
*)
let shop_without_post_params =
  new_url
    ~name:(Url ["shop"])
    ~params:_unit

let shop_with_post_params =
  new_post_url
    ~fallback:shop_without_post_params
    ~post_params:(_string "article")

let write_shop shop =
  (form_post shop 
     (fun article -> 
	let sb = string_box article in
	  <:xmllist< What do you want to buy? $sb$ >>))

let shop_public_main_page () =
  let f = write_shop shop_with_post_params in
    << <html> $f$ </html> >>

let _ = 
  register_url shop_without_post_params shop_public_main_page
    

let write_shopping_basket shopping_basket =
  let rec aux = function
      [] -> [<< <br/> >>]
    | a::l -> let fol = aux l in <:xmllist< $str:a$ <br/> $list:fol$ >>
  in
  let ffol = aux shopping_basket in
    <:xmllist< Your shopping basket: <br/> $list:ffol$ >>

let rec page_for_shopping_basket shopping_basket =
  let local_shop_with_post_params = 
    new_post_state_url
      ~fallback:shop_without_post_params
      ~post_params:(_string "article")
  and local_pay = new_state_url ~fallback:shop_without_post_params
  in
    register_post_session_url
      ~url:local_shop_with_post_params
      ~action:(fun article () -> 
		 page_for_shopping_basket (article::shopping_basket));
    register_session_url
      local_pay
      (fun () ->
	 let f = write_shopping_basket shopping_basket in
	   << <html> You are going to pay: $list:f$ </html> >>);
    let sb = `Div ([], (write_shopping_basket shopping_basket)) in
    let sh = write_shop local_shop_with_post_params in
    let lp = link "pay" local_pay in
      << <html> $sb$ $sh$ $lp$ </html> >>

let _ = register_post_url
  ~url:shop_with_post_params
  ~action:(fun article () -> page_for_shopping_basket [article])


(* Main page for this example *)
let _ = register_new_url (Url []) _noparam 
  (let l1 = link "plop" plop in
   let l2 = link "plip" plip in
   let l3 = link "plop/plip" plop2 in
   let l4 = link "plop3" plop3 in
   let l5 = link "plop avec params" plop_params 45 "hello" "plpl" in
   let l6 = link "uaprefix" uaprefix "suf" in
   let l7 = link "iprefix" iprefix "popo" 333 in
   let l8 = link "mytype" mytype A in
   let l9 = link "links" links in
   let l10 = link "linkrec" linkrec in
   let l11 = link "form" form in
   let l12 = link "post sans post_params" no_post_param_url in
   let l13 = link "form2" form2 in
   let l14 = link "form3" form3 in
   let l15 = link "form4" form4 in
   let l16 = link "state" ustate in
   let l17 = link "session" public_session_without_post_params in
   let l18 = link "shop" shop_without_post_params in
     << <html> 
       Une page simple : $l1$ <br/>
       Une page avec un compteur : $l2$ <br/> 
       Une page simple dans un répertoire : $l3$ <br/>
       Page par défaut d'un répertoire : $l4$ (plop/ n'est pas équivalent à plop)<br/>
       Une page avec paramètres GET : $l5$ (que se passe-t-il si le premier paramètre n'est pas un entier ?)<br/> 
       Une page avec URL "préfixe" qui récupère l'IP et l'user-agent : $l6$ <br/> 
       Une page URL "préfixe" avec des paramètres GET : $l7$ <br/> 
       Une page qui récupère un paramètre d'un type utilisateur : $l8$ <br/> 
       Une page avec des liens : $l9$ <br/> 
       Une page avec un lien vers elle-même : $l10$ <br/> 
       Une page avec un formulaire GET qui pointe vers post_params : $l11$ <br/> 
       Un formulaire POST qui pointe vers la page "post" : $l13$ <br/> 
       La page "post" quand elle ne reçoit pas de paramètres POST : $l12$ <br/> 
       Un formulaire POST qui pointe vers une URL avec paramètres GET : $l14$ <br/> 
       Un formulaire POST vers une page externe : $l15$ <br/> 
       URL avec état : $l16$ (problème des paramètres GET bookmarkés...) <br/> 
       Une session basée sur les cookies : $l17$ <br/> 
       Une session avec "url de sessions" : $l18$ 
     </html> >>)


