open XHTML.M
open Ocsigen
open Ocsigenduce.Xhtml
open Lwt
open Xhtml1_strict

let s =
  register_new_service 
    ~url:[""]
    ~get_params:unit
    (fun sp () () -> 
      return
        ({{ <html>
             [<head> [<title> ""]
              <body> [<h1> "This page has been type checked by OcamlDuce"]] }} : {{ html }}))

let create_form = 
  (fun (number_name,(number2_name,string_name)) ->
    {{ [ <p> [ 'Write an int: '
             {{ int_input number_name }}
             'Write another int: '
             {{ int_input number2_name }}
             'Write a string: '
             {{ string_input string_name }}
             {{ submit_input "Click" }} ] ] }} )

let form = register_new_service ["form"] unit
  (fun sp () () -> 
     let f = get_form Tutorial.coucou_params sp create_form in 
     return
        {{ <html>
             [<head> [<title> ""]
              <body> [ f ] ]}})

let links = register_new_service ["links"] unit
 (fun sp () () -> return
   {{ <html>
      [ <head> [<title> ""]
	<body> 
	[<p>
          [{{ a s sp {{ "first page" }} () }}
	   <br> []
	   {{ a form sp {{ "form" }} () }}
	   <br> []
(*           {{ a s sp {{ "hello" }} () }}
	   <br> []
           {{ a coucou_params sp 
             {{ "coucou_params" }} (42,(22,"ciao")) }}
	   <br> [] *)
           {{ a
             (new_external_service
		~url:["http://fr.wikipedia.org";"wiki"]
		~prefix:true
		~get_params:suffix_only
		~post_params:unit ())
             sp
             {{ "ocaml on wikipedia" }}
             "OCaml" }}]]] }})
