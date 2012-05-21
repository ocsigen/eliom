(* Ocsigen
 * Copyright (C) 2005 Vincent Balat
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

open Eliom_compatibility_2_1

open Eliom_service
open Eliom_parameter
open Eliom_state
open Eliom_duce.Xhtml
open Lwt
open Xhtml_types_duce

let s =
  register_service
    ~path:[""]
    ~get_params:unit
    (fun () () ->
      return
        ({{ <html xmlns="http://www.w3.org/1999/xhtml">
             [<head> [<title> ""]
              <body> [<h1> "This page has been type checked by OcamlDuce"
                     ]] }} : {{ html }}))

let create_form =
  (fun (number_name,(number2_name,string_name)) ->
    {{ [ <p> [ 'Write an int: '
             {{ int_input ~input_type:{{ "text" }} ~name:number_name () }}
             'Write another int: '
             {{ int_input ~input_type:{{ "text" }} ~name:number2_name () }}
             'Write a string: '
             {{ string_input ~input_type:{{ "text" }} ~name:string_name () }}
             {{ string_input ~input_type:{{ "submit" }} ~value:"Click" ()}} ] ] }} )

let form = register_service ["form"] unit
  (fun () () ->
     let f = get_form Eliom_testsuite1.coucou_params create_form in
     return
        {{ <html xmlns="http://www.w3.org/1999/xhtml">
             [<head> [<title> ""]
              <body> [ f ] ]}})

let links = register_service ["links"] unit
 (fun () () -> return
   {{ <html xmlns="http://www.w3.org/1999/xhtml">
      [ <head> [<title> ""]
        <body>
        [<p>
          [{{ a s {{ "first page" }} () }}
           <br> []
           {{ a form {{ "form" }} () }}
           <br> []
(*           {{ a s {{ "hello" }} () }}
           <br> []
           {{ a coucou_params
             {{ "coucou_params" }} (42,(22,"ciao")) }}
           <br> [] *)
           {{ a
             (external_service
                ~prefix:"http://fr.wikipedia.org"
                ~path:["wiki"]
                ~get_params:(suffix (string "a"))
                ())
             {{ "ocaml on wikipedia" }}
             "OCaml" }}]]] }})




let main = service ~path:["radio"] ~get_params:unit ()
let form =
  post_service ~fallback:main ~post_params:(radio string "test") ()

let gen_form = fun x ->
        {{ [<p>[
                {: string_radio ~checked:false ~name:x ~value:"Blerp" () :}
                'Blerp'
                {: string_radio ~checked:false ~name:x ~value:"Gnarf" () :}
                'Gnarf'
                {: string_input ~input_type:{{ "submit" }} ~value:"OK" () :}
                ]] }}

let _ =
        register ~service:main
        (fun () () ->
                return {{ <html xmlns="http://www.w3.org/1999/xhtml">[
                        <head>[<title>"Main"]
                        <body>[{: post_form form gen_form () :}]
                ] }}
        );
        register ~service:form
        (fun () x ->
                return {{ <html xmlns="http://www.w3.org/1999/xhtml">[
                                <head>[<title>"Form"]
                                <body>[<p>{: match x with None -> "Geen" | Some y -> y :}]
                        ] }})



let blocks =
  Eliom_duce.Block.register_service
    ~path:["blocks"]
    ~get_params:unit
    (fun () () ->
      return
        ({: [ <h1> "This page has been type checked by OcamlDuce"] :} ))


