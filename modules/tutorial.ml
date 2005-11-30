(* Copyright Vincent Balat 2005 *)

open Ocsigen

(* ------------------------------------------------------------------ *)
(* To create a web page without parameter: *)
let plop = 
  register_new_url 
    ~name:(Url ["plop"]) 
    ~params:_noparam 
    ~page:<< <html>
               <head><title></title></head>
               <body><h1>plop</h1></body>
             </html> >>

(* Pages can have side effects: *)
let plip = 
  let next =
    let c = ref 0 in
      (fun () -> c := !c + 1; !c)
  in
  register_new_url 
    ~name:(Url ["plip"]) 
    ~params:_unit
    ~page:(fun () -> 
	       let str = string_of_int (next ()) in 
	       << <html><body><p>$str:str$</p></body></html> >>)

(* As usual in OCaml, you can forget labels when the application is total: *)
let plop2 = 
  register_new_url 
    (Url ["plop";"plip"])  (* the url plop/plip *)
    _noparam 
    << <html> <body> <h1>plop2</h1> </body> </html> >>

let oups = 
  register_new_url 
    (Url ["oups"])
    _noparam 
    << <html><body><h1>La première</h1></body></html> >>

let oups2 = 
  register_new_url 
    (Url ["oups";""])  (* the url plop/ is equivalent to plop *)
    _noparam 
    << <html><body><p> oups/ is equivalent to oups.
      Here there was both but the first one has been replaced by the 
      second one.
       </p></body>
       </html> >>
(* oups2 will replace oups in the table *)


(* ------------------------------------------------------------------ *)
let repeteparams entier chaine chaine2 = 
  let i = string_of_int entier in
<<
  <html>
    <body>
    <p>
    Vous m'avez envoyé : 
    <strong>$str:i$</strong> et 
    <strong>$str:chaine$</strong> et 
    <strong>$str:chaine2$</strong>
    </p>
    </body>
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
    (_useragent (_ip (_url_suffix (_string "s"))))
    (fun ua ip suff s -> 
	 <<
	   <html>
            <body>
             <p>
	     The suffix of the url is <strong>$str:suff$</strong>
             and your user-agent is <strong>$str:ua$</strong>
             and your IP is <strong>$str:ip$</strong>
             and s is <strong>$str:s$</strong>
             </p>
            </body>
           </html>
         >>)

let iprefix = 
  register_new_url (Url_Prefix ["iprefix"]) (_url_suffix (_int "i"))
    (fun suff i -> 
       let i' = string_of_int i in
<<
  <html>
    <body>
    <p>
  The suffix of the url is <strong>$str:suff$</strong> and i is equal to <strong>$str:i'$</strong>
    </p></body>
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
    <body>
    <p>
  $str:v$
    </p></body>
  </html>
>>)



(* ------------------------------------------------------------------ *)
(* To create a link to a registered url, use the a function: *)

open Xhtmlpp

let links = register_new_url (Url ["plop";"links"]) (_current_url _noparam)
  (fun current_url ->
     (let l = a <:xmllist< plop >> current_url plop in
      let ll = a <:xmllist< plop2 >> current_url plop2 in
      let lll = a <:xmllist< uaprefix >> current_url uaprefix "suf" "toto" in
      let llll = 
	a <:xmllist< plop_params >> current_url plop_params 45 "hello" "plpl" in
      let lllll  = a <:xmllist< wikipedia >> current_url
	  (new_external_url
	     (Url_Prefix ["http://fr.wikipedia.org";"wiki"])
	     (_url_suffix _noparam)) "Ocaml"
      in
	<< <html><body><p> $l$ <br/> $ll$ <br/> $lll$ <br/> $llll$ <br/> $lllll$</p></body></html> >>))
(* Note that to create a link we need to know the current url, because:
   - the link from toto/titi to toto/tata is "tata" and not "toto/tata"
   - "toto/titi/" is equivalent to "toto/titi", but the link for the
   first one is "../tata" and the second "tata".
*)



(* Note new_external_url to create a link on external url *)
(* If you want to put a link towards a page that is not already defined,
   you can break register_new_url into new_url and register_url.
   Do not forget to register the url!!!
 *)

let linkrec = new_url (Url ["linkrec"]) (_current_url _noparam)

let _ = register_url linkrec 
  (fun url -> 
     let l = a <:xmllist< cliquez >> url linkrec in
       << <html><body><p> $l$ </p></body></html> >>)





(* ------------------------------------------------------------------ *)
(* To create a form, call the function form with the name of a registered
   url and a function that takes the names of the parameters and
   build the formular
*)
let create_form = 
     (fun entier chaine chaine2 ->
	let ib = int_input entier in
	let sb = string_input chaine in
	let sb2 = string_input chaine2 in
	let b = submit_input "Cliquez" in
	  <:xmllist< <p>Write an int: $ib$ <br/>
                     Write a string: $sb$ <br/>
                     Write a string: $sb2$ <br/>
	             $b$</p>
	  >>)

let form = register_new_url (Url ["form"]) (_current_url _noparam)
  (fun current_url -> 
     let f = form_get current_url plop_params create_form in
     << <html><body> $f$ </body></html> >>)



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
    ~page:<< <html><body><p>Version of the page without POST parameters</p></body></html> >>
    
let my_url_with_post_params = register_new_post_url
    ~fallback:no_post_param_url
    ~post_params:(_string "value")
    ~page:(fun value -> 
	       << <html><body><p>$str:value$</p></body></html> >>)

(* You can mix get and post parameters *)
let get_no_post_param_url = 
  register_new_url 
    ~name:(Url ["post2"]) 
    ~params:(_int "i")
    ~page:(fun i -> 
	       let i' = string_of_int i in
	       << <html><body><p>No POST parameter, i: <strong>$str:i'$</strong></p></body></html> >>)
    
let my_url_with_get_and_post = register_new_post_url 
  ~fallback:get_no_post_param_url
  ~post_params:(_string "value")
  ~page:(fun value i -> 
	       let i' = string_of_int i in
	       << <html><body><p>Value: <strong>$str:value$</strong> <br/> 
		 i: <strong>$str:i'$</strong>
	       </p></body></html> >>)


(* ------------------------------------------------------------------ *)
(* To create a POST form, use the form_post function,
   possibly applied to GET parameters (if any)
*)

let form2 = register_new_url (Url ["form2"]) (_current_url _noparam)
  (fun current_url -> 
     let f  = 
       (form_post current_url my_url_with_post_params
	  (fun chaine -> 
	     let sb = string_input chaine in
	       <:xmllist< <p> Write a string: $sb$ </p> >>)) in
       << <html><body>$f$</body></html> >>)

let form3 = register_new_url (Url ["form3"]) (_current_url _noparam)
  (fun current_url ->
     let f  = 
       (form_post current_url my_url_with_get_and_post 
	  (fun chaine -> 
	     let sb = string_input chaine in
	       <:xmllist< <p> Write a string: $sb$ </p> >>) 222) in
       << <html><body>$f$</body></html> >>)

let form4 = register_new_url (Url ["form4"]) (_current_url _noparam) 
  (fun current_url ->
     let f  = 
       (form_post current_url
	  (new_external_post_url 
	     ~name:(Url ["http://www.petitspois.com";"form"])
	     ~params:(_int "i")
	     ~post_params:(_string "chaine"))
	  (fun chaine -> 
	     let sb = string_input chaine in
	       <:xmllist< <p> Write a string: $sb$ </p> >>) 222) in
       << <html><body>$f$</body></html> >>)
       
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

let ustate = new_url (Url ["state"]) (_current_url _noparam)

let ustate2 = new_state_url ustate

let _ = 
  let c = ref 0 in
  let page url = 
    let i = string_of_int !c in
    let l = a <:xmllist< reload >> url ustate in
    let l2 = a <:xmllist< incr i >> url ustate2 in
    let si1 = submit_input "incr i (post)" in
    let si2 = submit_input "incr i (get)" in
    let l3 = form_post url ustate2 [<< <p> $si1$ </p> >>] in
    let l4 = form_get url ustate2 [<< <p> $si2$ </p> >>] in
    << <html><body><p> i vaut $str:i$ <br/> $l$ <br/> $l2$ </p> $l3$ $l4$ </body></html> >>
  in
    register_url ustate page;
    register_url ustate2 (fun url -> c := !c + 1; page url)


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
    ~params:(_current_url _noparam)

let public_session_with_post_params = 
  new_post_url 
    ~fallback:public_session_without_post_params
    ~post_params:(_string "login")


let accueil url = 
  let f  = form_post url public_session_with_post_params
    (fun login -> 
       let sb = string_input login in
	 <:xmllist< <p> login: $sb$ </p> >>) in
    << <html><body>$f$</body></html> >>

let _ = register_url
  ~url:public_session_without_post_params
  ~page:accueil

let rec launch_session login =
  let close = register_new_session_url (* See later *)
    ~fallback:public_session_without_post_params 
    ~page:(fun url -> close_session (); accueil url)
  in
  let new_main_page url =
    let l1 = a <:xmllist< plop >> url plop
    and l2 = a <:xmllist< plop2 >> url plop2
    and l3 = a <:xmllist< links >> url links
    and l4 = a <:xmllist< close session >> url close in
    << <html>
    <body><p>
         Bienvenue $str:login$ ! <br/>
         $l1$ <br/>
         $l2$ <br/>
         $l3$ <br/>
         $l4$ <br/>
    </p></body>
      </html> >>
  in
    register_session_url 
      ~url:public_session_without_post_params 
                               (* url is any public url already registered *)
      ~page:new_main_page;
    register_session_url 
      plop (* any public url already registered *)
      << <html><body><p> Plop $str:login$ ! </p></body></html> >>;
    register_session_url 
      plop2
      << <html><body><p> Plop2 $str:login$ ! </p></body></html> >>;
    new_main_page
      
let _ =
  register_post_url
    ~url:public_session_with_post_params
    ~page:launch_session



(* ------------------------------------------------------------------ *)
(* You can register url with states in session tables.
   Use this if you want a link or a form which depends precisely on an
   instance of the web page, for example to buy something on an internet shop.
   UPDATE: Actually it is not a good example, because what we want in a shop
   is the same shoping basket for all pages. 
   SEE queinnec example instead.
*)
let shop_without_post_params =
  new_url
    ~name:(Url ["shop"])
    ~params:(_current_url _noparam)

let shop_with_post_params =
  new_post_url
    ~fallback:shop_without_post_params
    ~post_params:(_string "article")

let write_shop shop url  =
  (form_post url shop 
     (fun article -> 
	let sb = string_input article in
	  <:xmllist< <p> What do you want to buy? $sb$ </p> >>))

let shop_public_main_page current_url =
  let f  = write_shop shop_with_post_params current_url in
    << <html><body>$f$</body></html> >>

let _ = 
  register_url shop_without_post_params shop_public_main_page
    

let write_shopping_basket shopping_basket =
  let rec aux = function
      [] -> [<< <br/> >>]
    | a::l -> let fol = aux l in <:xmllist< $str:a$ <br/> $list:fol$ >>
  in
  let ffol = aux shopping_basket in
    <:xmllist< Your shopping basket: <br/> $list:ffol$ >>

let rec page_for_shopping_basket url shopping_basket =
  let local_shop_with_post_params = 
    new_post_state_url
      ~fallback:shop_without_post_params
      ~post_params:(_string "article")
  and local_pay = new_state_url ~fallback:shop_without_post_params
  in
    register_post_session_url
      ~url:local_shop_with_post_params
      ~page:(fun article current_url -> 
		 page_for_shopping_basket 
		   current_url (article::shopping_basket));
    register_session_url
      local_pay
      (fun current_url ->
	 let f = write_shopping_basket shopping_basket in
	   << <html><body><p>You are going to pay: $list:f$ </p></body></html> >>);
    let sb' = write_shopping_basket shopping_basket in
    let sb = << <div>$list:sb'$</div> >> in
    let sh = write_shop local_shop_with_post_params url in
    let lp   = a <:xmllist< pay >> url local_pay in
      << <html><body> $sb$ $sh$ <p>$lp$</p></body></html> >>

let _ = register_post_url
  ~url:shop_with_post_params
  ~page:(fun article url -> page_for_shopping_basket url [article])


(* Queinnec example: *)

let queinnec = 
  new_url
    ~name:(Url ["queinnec"])
    ~params:(_current_url _noparam)

let queinnec_post = 
  new_post_url 
    ~fallback:queinnec
    ~post_params:(_int "i")

let _ = 
  let create_form is = 
    (fun entier ->
      let ib = int_input entier in
      let b = submit_input "Somme" in
      <:xmllist< <p> $str:is$ + $ib$ <br/>
      $b$ </p>
      >>)
  in
  register_post_url
    ~url:queinnec_post
    ~page:(fun i current_url ->
      let is = string_of_int i in
      let queinnec_result = register_new_post_session_url
	  ~fallback:queinnec_post
	  ~post_params:(_int "j")
	  ~page:(fun j current_url -> 
	    let js = string_of_int j in
	    let ijs = string_of_int (i+j) in
	    << <html> <body><p> $str:is$ + $str:js$ = $str:ijs$ </p></body></html> >>)
      in
      let f  = 
	form_post current_url queinnec_result (create_form is) in
      << <html><body>$f$</body></html> >>)
    
let _ =   
  let create_form = 
    (fun entier ->
      let ib = int_input entier in
      let b = submit_input "Envoyer" in
      <:xmllist< <p> Write a number: $ib$ <br/>
      $b$ </p>
      >>)
  in
  register_url
    queinnec
    (fun current_url ->
      let f  = form_post current_url queinnec_post create_form in
      << <html><body>$f$</body></html> >>)
      


(* Actions: *)
(* Actions are like url but they do not generate any page.
   For ex, when you have the same form (or link) on several pages 
   (for ex a connect form),
   instead of making a version with post params of all these pages,
   you can use actions.
   Create and register an action with new_actionurl, register_actionurl,
     register_new_actionurl, register_session_actionurl,
     register_new_session_actionurl
   Make a form or a link to an action with action_form or action_link.
   By default, they print the page again after having done the action.
   But you can give the optional boolean parameter reload to action_form
   or action link to prevent reloading the page.
*)
let action_session = 
  new_url ~name:(Url ["action"]) ~params:(_http_params _noparam)

let connect_action = new_actionurl ~params:(_string "login")

let accueil_action h = 
  let f = action_form h connect_action
    (fun login -> 
       let sb = string_input login in
	 <:xmllist< <p> login: $sb$ </p> >>) in
    << <html><body>$f$</body></html> >>

let _ = register_url
  ~url:action_session
  ~page:accueil_action

let rec launch_session login =
  let deconnect_action = register_new_actionurl _unit close_session in
  let deconnect_box h s = action_link s h deconnect_action in
  let new_main_page h =
    let l1  = a <:xmllist< plop >> h.current_url plop
    and l2  = a <:xmllist< plop2 >> h.current_url plop2
    and l3  = a <:xmllist< links >> h.current_url links
    and deconnect_link = deconnect_box h "Close session" in
    << <html>
       <body><p>
         Bienvenue $str:login$ ! <br/>
         $l1$ <br/>
         $l2$ <br/>
         $l3$ </p>
         $deconnect_link$
      </body>
      </html> >>
  in
    register_session_url ~url:action_session ~page:new_main_page;
    register_session_url plop << <html><body><p> Plop $str:login$ ! </p></body></html> >>;
    register_session_url plop2 << <html><body><p> Plop2 $str:login$ ! </p></body></html> >>
      
let _ = register_actionurl
    ~actionurl:connect_action
    ~action:(fun login -> launch_session login)



(* Static files: *)
let filedir = register_new_static_directory ["files"] "modules-files"
(* This url works like a "prefix url". The suffix is the file name *)


(* Main page for this example *)
let _ = register_new_url (Url []) (_current_url _noparam)
  (fun url ->
     let lcss = css_link url filedir "style.css" in
     let l1 = a <:xmllist< plop >> url plop in
     let l2 = a <:xmllist< plip >> url plip in
     let l3 = a <:xmllist< plop/plip >> url plop2 in
     let l4 = a <:xmllist< oups >> url oups in
     let l5 = a <:xmllist< plop avec params >> url plop_params 45 "hello" "krokodile" in
     let l6 = a <:xmllist< uaprefix >> url uaprefix "suf" "toto" in
     let l7 = a <:xmllist< iprefix >> url iprefix "popo" 333 in
     let l8 = a <:xmllist< mytype >> url mytype A in
     let l9 = a <:xmllist< links >> url links in
     let l10 = a <:xmllist< linkrec >> url linkrec in
     let l11 = a <:xmllist< form >> url form in
     let l12 = a <:xmllist< post sans post_params >> url no_post_param_url in
     let l13 = a <:xmllist< form2 >> url form2 in
     let l14 = a <:xmllist< form3 >> url form3 in
     let l15 = a <:xmllist< form4 >> url form4 in
     let l16 = a <:xmllist< state >> url ustate in
     let l17 = a <:xmllist< session >> url public_session_without_post_params in
     let l19 = a <:xmllist< actions >> url action_session in
     let l20 = a <:xmllist< shop >> url shop_without_post_params in
     let l18 = a <:xmllist< queinnec >> url queinnec in
     << 
       <html> 
       <!-- This is a comment! -->
       <head>
         $lcss$
	 <title>Ocsigen Tutorial</title>
       </head>
       <body>
       <h1>Ocsigen</h1>
       <h2>Examples</h2>
       <h3>Simple pages</h3>
       <p>
       Une page simple : $l1$ <br/>
       Une page avec un compteur : $l2$ <br/> 
       Une page simple dans un répertoire : $l3$ <br/>
       Page par défaut d'un répertoire : $l4$ (oups/ est équivalent à oups)</p>
       <h3>Parameters</h3>
       <p>
       Une page avec paramètres GET : $l5$ (que se passe-t-il si le premier paramètre n'est pas un entier ?)<br/> 
       Une page avec URL "préfixe" qui récupère l'IP et l'user-agent : $l6$ <br/> 
       Une page URL "préfixe" avec des paramètres GET : $l7$ <br/> 
       Une page qui récupère un paramètre d'un type utilisateur : $l8$ </p>
       <h3>Links and Formulars</h3>
       <p>
       Une page avec des liens : $l9$ <br/> 
       Une page avec un lien vers elle-même : $l10$ <br/>
       Une page avec un formulaire GET qui pointe vers la page plop avec params : $l11$ <br/> 
       Un formulaire POST qui pointe vers la page "post" : $l13$ <br/> 
       La page "post" quand elle ne reçoit pas de paramètres POST : $l12$ <br/> 
       Un formulaire POST qui pointe vers une URL avec paramètres GET : $l14$ <br/> 
       Un formulaire POST vers une page externe : $l15$ </p> 
       <h3>Sessions</h3>
       <p>
       URL avec état : $l16$ (problème des paramètres GET bookmarkés...) <br/> 
       Une session basée sur les cookies : $l17$ <br/> 
       Une session avec des actions : $l19$ <br/>
       Une session avec "url de sessions" : $l18$
       - (ancienne version : $l20$)
       </p>
       </body>
     </html> >>)


(* WARNING! As we don't use threads, 
   the following page will stop the server 5 seconds!!!
let _ = 
  register_new_url 
    ~name:(Url ["loooooooooong"]) 
    ~params:_unit
    ~page:(fun () -> 
               Unix.sleep 5;
	       << <html><body><p>Ok now, you can read the page.</p></body></html> >>)
*)
