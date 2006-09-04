open XHTML.M
open Ocsigen
open Ocsigen.Xhtml
open Lwt

let looong = 
  register_new_service 
    ~url:["looong"]
    ~get_params:unit
    (fun sp () () -> 
      Lwt_unix.sleep 10.0 >>= (fun _ ->
	failwith "lkljk";
	return
        (html
	  (head (title (pcdata "")) [])
	  (body [h1 [pcdata "Ok now, you can read the page."]]))))



(************************)

let no_post_param_service =
  register_new_service
   ~url:["post"]
     ~get_params:unit
      (fun _ () () -> return
        (html         
          (head (title (pcdata "")) [])
          (body [h1 [pcdata     
	    "Version of the page without POST parameters"]])))
    
let my_service_with_post_params = register_new_post_service
   ~fallback:no_post_param_service
   ~post_params:(string "name" ** string "file")
	 (fun _ () (name,file) ->
	 let to_display = 
	 let full = ("/tmp/"^file) in
	(* if (Unix.stat full).Unix.st_size > 1024 
	 then begin Sys.remove full; "File too large" end
	 else begin *)
	 let fd_in = open_in ("/tmp/"^file) in
	 try
	 let line = input_line fd_in in close_in fd_in; line (*end*)
	 with End_of_file -> "vide"
	 in
	 return
	    (html
		(head (title (pcdata name)) [])
		(body [h1 [pcdata to_display]])))


let form2 = register_new_service ["form2"] unit
  (fun sp () () ->
    let f =
     (post_form ~a:[(XHTML.M.a_enctype "multipart/form-data")] my_service_with_post_params sp
     (*post_form my_service_with_post_params sp        *)
	(fun (chaine,file) ->
          [p [pcdata "Write a string: ";
              string_input chaine;
	      br ();
	      file_input file]]) ()) in  return
         (html
           (head (title (pcdata "form")) [])
           (body [f])))
									 
(*********************************)
let get_param_service =
  register_new_service
   ~url:["get"]
   ~get_params:(string "name" ** string "file")
    (fun _ (name,file) () ->
	 let to_display = 
	 let full = ("/tmp/"^file) in
	(* if (Unix.stat full).Unix.st_size > 1024 
	 then begin Sys.remove full; "File too large" end
	 else begin *)
	 let fd_in = open_in ("/tmp/"^file) in
	 try
	 let line = input_line fd_in in close_in fd_in; line (*end*)
	 with End_of_file -> "vide"
	 in
	 return
	    (html
		(head (title (pcdata name)) [])
		(body [h1 [pcdata to_display]])))


let form3 = register_new_service ["form3"] unit
  (fun sp () () ->
    let f =
     (get_form ~a:[(XHTML.M.a_enctype "multipart/form-data")] get_param_service sp
     (*post_form my_service_with_post_params sp        *)
	(fun (chaine,file) ->
          [p [pcdata "Write a string: ";
              string_input chaine;
	      br ();
	      file_input file]])) in  return
         (html
           (head (title (pcdata "form")) [])
           (body [f])))
									 
(*********************************)

let def = new_service
    ~url:["essai";"essai"]  
    ~get_params:unit
    ()

let post = 
  new_post_service
    ~fallback:def
    ~post_params:((*string "group" ** *)
		    (bool "macase" (** (bool "madeuxiemecase"
		       (* opt (string "madeuxiemecase" *)
			  ** (radio_answer "monradio")*)))

let create_form ((*groupname,*)(casename(*,(casename2,radioname)*))) =
    [p [(*select ~a:[a_name groupname]
          (option (pcdata "choi1")) 
	  [option (pcdata "choi2")];*)
	bool_checkbox casename;
(*	bool_checkbox casename2;
(*	input ~a:[a_name casename2; a_value "Bip"; a_input_type `Checkbox] ();*)
	string_radio radioname "premier";
	string_radio radioname "deuxième";*)
        submit_input "Envoyer"
     ]]

let genere_form sp = post_form post sp create_form ()

let _ = register_service def
              (fun sp () () -> Lwt.return
                       (html
                           (head (title (pcdata "")) 
			      [css_link (make_uri (static_dir sp) sp "style.css")])
                           (body [genere_form sp])))

let fonction sp () ((* group,*)(case(*,(case2, radio_opt*))) = Lwt.return
  (html
     (head (title (pcdata "")) 
	[css_link (make_uri (static_dir sp) sp "style.css")])
     (body [h1 [(*pcdata group; br (); *)
		pcdata (if case
 		        then "Case cochée"
		        else "Case pas cochée");
(*		br ();
		pcdata (match case2 with
 		  None -> "Deuxième case pas cochée"
		| Some v -> ("Deuxième case cochée : "^v)); *)
		br ();
		(* match radio_opt with
		  None -> pcdata "Pas de bouton radio enfoncé"
		| Some nom -> pcdata ("Bouton radio : "^nom)*)]; 
	    genere_form sp]))

let _ = register_service post fonction


(*********)
(* lists *)
let coucou_list = register_new_service 
    ~url:["cl"]
    ~get_params:(list "a" (string "nom" ** (opt (int "entier") ** bool "chkbx")))
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
       </tr> >>) l in Lwt.return
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
	$hidden_string_input nomname nom$
	<!-- Un champ caché pour envoyer le nom -->
      </td>
      <td>$int_input intname$</td>
      <td>$bool_checkbox chkbxname$</td>
      </tr> >>)
    ["durand";"dupont";"dupond";"durateau"]
      []
  with 
    [] -> [] (* liste vide, on ne crée pas de tableau ? *)
  | a::l -> 
      [(table a l); p [submit_input "Click"]]


let listform = register_new_service ["lf"] unit
  (fun sp () () -> 
     let f = get_form coucou_list sp create_listform in Lwt.return
     << <html>
          <head><title></title></head>
          <body> $f$ </body>
        </html> >>)

(*****)
let coucou_list = register_new_service
    ~url:["coucoul"]
    ~get_params:((list "a" (bool "chkbx")) ** string "login")
  (fun _ (l,login) () ->     (* l est une liste de (nom * (string option))
*)
    let ll =
      List.map (fun (chkbxopt) ->
                  << <tr>
                      <td>
                       <strong>teste</strong></td>
                      <td>
                       $if chkbxopt 
                        then pcdata "pas coché"
                        else pcdata "coché" $
                      </td>
                     </tr> >>) l in Lwt.return
    << <html>
         <head><title></title></head>
         <body>
          <p>You sent:</p>
          <table>
           $list:ll$
          </table>
      </body>
       </html> >>)

let create_listform (l,login) (f,loginname) =
  (* f.it est un itérateur comme List.map *)
  (* Pour chaque valeur de la liste ["durand"; etc.]
      il applique la fonction.
      nomname et chkbxname sont les noms des champs du formulaires
      nom est successivement "durand" "dupont", etc.
  *)
  f.it (fun (chkbxname) nom ->
      <:xmllist< <p>Write the value for $str:nom$
      $bool_checkbox ~checked:true chkbxname$
      </p> >>)l

  <:xmllist< <p>
    $hidden_string_input loginname login$ (*le paramètre string login *)
    $submit_input "Click"$</p> >>

let listform = register_new_service ["listform"] unit
  (fun sp () () ->
     let l = ["durand";"dupont";"dupond";"durateau"]
     and login = "sam" in
     let f = get_form coucou_list sp (create_listform (l,login))
in Lwt.return
     << <html>
          <head><title></title></head>
          <body> $f$ </body>
        </html> >>)


(******* essai register_for_session *************)

let public_session_without_post_params = 
  new_service ~url:["session"] ~get_params:unit ()

let public_session_with_post_params = 
  new_post_service 
    ~fallback:public_session_without_post_params
    ~post_params:(string "login")

let accueil sp () () = 
  let f = post_form public_session_with_post_params sp
    (fun login -> 
	 [p [pcdata "login: ";
	     string_input login]]) () in Lwt.return
  (html
     (head (title (pcdata "")) [])
     (body [f]))


let _ = register_service
  ~service:public_session_without_post_params
  accueil

let rec launch_session sp () login =
  let close = register_new_auxiliary_service_for_session
    sp
    ~fallback:public_session_without_post_params 
    (fun sp () () -> close_session sp; accueil sp () ())
  in
  let new_main_page sp () () =
    Messages.debug "plo";
    raise Not_found;
    Lwt.return (html
       (head (title (pcdata "")) [])
       (body [p [pcdata "Welcome ";
		 pcdata login; 
		 pcdata "!"; br ();
		 a close sp [pcdata "close session"] ()]]))
  in
  register_service_for_session 
    sp
    ~service:public_session_without_post_params 
    (* service is any public service already registered *)
    new_main_page;
  Messages.debug "registration done";
  new_main_page sp () ()
    
let _ =
  register_service
    ~service:public_session_with_post_params
    launch_session

