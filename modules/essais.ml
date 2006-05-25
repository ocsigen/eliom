open XHTML.M
open Ocsigen

let def = new_url 
    ~path:["essai";"essai"]  
    ~get_params:unit
    ()

let post = 
  new_post_url
    ~fallback:def
    ~post_params:(string "group" ** 
		    (bool "macase" ** 
		       (option (string "madeuxiemecase")
			  ** option (string "monradio"))))

let create_form (group,(case,(case2,radio))) =
    [p [select ~a:[a_name group]
          (XHTML.M.option (pcdata "choi1")) 
	  [XHTML.M.option (pcdata "choi2")];
	checkbox_input case;
	input ~a:[a_name case2; a_value "Bip"; a_input_type `Checkbox] ();
	input ~a:[a_name radio; a_value "premier"; a_input_type `Radio] ();
	input ~a:[a_name radio; a_value "deuxieme"; a_input_type `Radio] ();
        submit_input "Envoyer"
     ]]

let genere_form current_url = post_form post current_url create_form ()

let _ = register_url def
              (fun sp () () ->
                       (html
                           (head (title (pcdata "")) 
			      [css_link (make_uri static_dir sp.current_url "style.css")])
                           (body [genere_form sp.current_url])))

let fonction sp () (group,(case,(case2, radio_opt))) = 
  (html
     (head (title (pcdata "")) 
	[css_link (make_uri static_dir sp.current_url "style.css")])
     (body [h1 [pcdata group; br (); 
		pcdata (if case
 		        then "Case cochée"
		        else "Case pas cochée");
		br ();
		pcdata (match case2 with
 		  None -> "Deuxième case pas cochée"
		| Some v -> ("Deuxième case cochée : "^v));
		br ();
		(match radio_opt with
		  None -> pcdata "Pas de bouton radio enfoncé"
		| Some nom -> pcdata ("Bouton radio : "^nom))]; 
	    genere_form sp.current_url]))

let _ = register_url post fonction


(*********)
(* lists *)
let coucou_list = register_new_service 
    ~url:["coucou"]
    ~get_params:(list "a" (string "nom" ** (option (int "entier") ** bool "chkbx")))
  (fun _ l () ->  (* l est une liste de (nom * (string option)) *)
    let ll = 
      List.map (fun (nom, (entieropt, chkbxopt)) -> 
    << <tr>
	 <td>
         <strong>$str:nom$</strong></td>
	<td>$str:(match entieropt with
	  None -> "pas de valeur"
	| Some entier -> string_of_int entier)$</td>
	 <td>
         $if chkbxopt
	  then pcdata "coché"
          else pcdata "pas coché"$
	 </td>
       </tr> >>) l in
    << <html>
         <head><title></title></head>
         <body>
          <p>You sent:</p>
          <table>
           $list:ll$
          </table>
         </body>
       </html> >>)

let create_listform f = 
  match 
  (* f.it est un itérateur dans le style List.map *)
  (* Pour chaque valeur de la liste ["durand"; etc.] 
     il applique la fonction.
     nomname, chainename et chkbxname sont les noms des champs du formulaires ;
     nom est successivement "durand" "dupont", etc.
  *)
   f.it (fun (nomname, (intname, chkbxname)) nom ->
    <:xmllist< <tr>
      <td>Write the value for $str:nom$:
	$input ~a:[(a_name nomname);(a_input_type `Hidden);(a_value nom)] ()$
	<!-- Un champ caché pour envoyer le nom -->
      </td>
      <td>$int_input intname$</td>
      <td>$checkbox_input chkbxname$</td>
      </tr> >>)
    ["durand";"dupont";"dupond";"durateau"]
      []
  with 
    [] -> [] (* liste vide, on ne crée pas de tableau ? *)
  | a::l -> 
      [(table a l); p [submit_input "Click"]]


let listform = register_new_service ["listform"] unit
  (fun sp () () -> 
     let f = get_form coucou_list sp.current_url create_listform in
     << <html>
          <head><title></title></head>
          <body> $f$ </body>
        </html> >>)

(*

let plop1 = 
  register_new_url 
    ~path:["plop1"]
    ~server_params:(fun x -> x)
    ~get_params:_noparam 
    << <html>
         <head><title></title></head>
         <body><h1>Coucou</h1></body>
       </html> >>


let u1 = 
  register_new_url 
    ~path:["u1"]
    ~params:_unit
    (fun () ->
    << <html>
         <head><title></title></head>
         <body><h1>Coucou</h1></body>
       </html> >>)


let create_form = 
  (
    <:xmllist< <p>$submit_input "Click"$</p>
    >>)

let u2 = register_new_url ["u2"] (_current_url _noparam)
  (fun current_url -> 
     let f = get_form u1 current_url create_form in
     << <html>
          <head><title></title></head>
          <body> $f$ </body>
        </html> >>)

let u3 = 
  register_new_post_url 
    ~fallback: u1
    ~post_params:(_int "i")
    (fun i () ->
    << <html>
         <head><title></title></head>
         <body><h1>Coucou</h1></body>
       </html> >>)

let u4 = register_new_url ["u4"] (_current_url _noparam)
  (fun current_url -> 
     let f = post_form u3 current_url create_form in
     << <html>
          <head><title></title></head>
          <body> $f$ </body>
        </html> >>)

*)
(*
let plop1 = 
  register_new_url 
    ~path:["plop1"]
    ~params:_noparam 
    << <html>
         <head><title></title></head>
         <body><h1>Coucou</h1></body>
       </html> >>

let aa = new_url 
    ~path:["aa"]
    ~params:(_current_url _noparam)
    ()

let bb = new_post_url
    ~fallback:aa
    ~post_params:(_string "s")


let f n = [p [string_input n]]

let _ = register_url aa
    (fun cu -> 
    << <html>
         <head><title></title></head>
         <body><h1>Coucou</h1>
           $post_form bb cu f$
         </body>
       </html> >>)

let _ = register_post_url bb
    (fun s cu -> 
    << <html>
         <head><title></title></head>
         <body><h1>Coucou</h1><p>$a aa cu [pcdata "clic"]$</p></body>
       </html> >>)

let cc = new_post_url
    ~fallback:plop1
    ~post_params:(_current_url (_string "s"))

let _ = register_post_url cc
    (fun cu s -> 
    << <html>
         <head><title></title></head>
         <body><h1>Coucou</h1><p>$a aa cu [pcdata "clic"]$</p></body>
       </html> >>)

let _ = register_url aa
    (fun cu -> 
    << <html>
         <head><title></title></head>
         <body><h1>Coucou</h1>
           $post_form cc cu f$
         </body>
       </html> >>)


let create_form = 
  (fun entier chaine chaine2 ->
    <:xmllist< <p>Write an int: $int_input entier$ <br/>
    Write a string: $string_input chaine$ <br/>
    Write a string: $string_input chaine2$ <br/>
    $submit_input "Click"$</p>
    >>)

let form = register_new_url ["form"] (_current_url _noparam)
  (fun current_url -> 
     let f = get_form plop_params current_url create_form in
     << <html>
          <head><title></title></head>
          <body> $f$ </body>
        </html> >>)

(* radio checkboxes *)

let foboo = register_new_url 
    ~path:["fo"]
    ~params:((_int "i") ** (_string "radio") ** (_string "chk2") ** (_string "chk2"))
    (fun i radio chk1 chk2 -> 
       << <html>
            <head><title></title></head>
            <body>
              <p>i vaut $str:string_of_int i$<br/>
                 radio vaut $str:radio$<br/>
                 chk1 vaut $str:chk1$<br/>
                 chk2 vaut $str:chk2$
              </p>
            </body>
          </html> >>)

let cf = 
  (fun entier ra chk1 chk2 ->
    <:xmllist< <p>Write an int: $int_input entier$ <br/>
    $radio_input ~a:[a_value "aa"; a_checked `Checked] ra$ AA<br/>
    $radio_input ~a:[a_value "bb"] ra$ BB<br/>
    $checkbox_input ~a:[a_value "case1"] chk1$ CASE 1<br/>
    $checkbox_input ~a:[a_value "case2"] chk2$ CASE 2<br/>
    $submit_input "envoyer"$
    </p>
    >>)

let fo = register_new_url ["fo"] (_current_url _noparam)
  (fun current_url -> 
     let f = get_form foboo current_url cf in
     << <html>
          <head><title></title></head>
          <body> $f$ </body>
        </html> >>)


*)
