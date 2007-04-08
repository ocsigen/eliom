(* Ocsigen
 * http://www.ocsigen.org
 * Module eliomexamples.ml
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


(* Other examples for Eliom, and various tests *)

open Tutoeliom
open XHTML.M
open Eliom.Xhtml
open Eliom
open Lwt

(* menu with preapplied services *)

let preappl = preapply coucou_params (3,(4,"cinq"))
let preappl2 = preapply uasuffix (["plop";"plip";"plup"],"aaa")

let mymenu current sp =
  Eliomboxes.menu ~classe:["menuprincipal"]
    (coucou, <:xmllist< coucou >>)
    [
     (preappl, <:xmllist< params >>);
     (preappl2, <:xmllist< params and suffix >>);
   ] current sp

let _ = 
  register_new_service 
    ~url:["menu"]
    ~get_params:unit
    (fun sp () () -> 
      return 
        (html
          (head (title (pcdata "")) [])
          (body [h1 [pcdata "Hallo"];
               mymenu coucou sp ])))

(* GET Non-attached coservice *)
let nonatt = new_coservice' ~get_params:(string "e") ()

(* GET coservice with preapplied fallback *)
(* + Non-attached coservice on a pre-applied coservice *)
(* + Non-attached coservice on a non-attached coservice *)
let f sp s =
  (html
     (head (title (pcdata "")) [])
     (body [h1 [pcdata s];
            p [a nonatt sp [pcdata "clic"] "nonon"];
            get_form nonatt sp 
              (fun string_name ->
                [p [pcdata "Non attached coservice: ";
                    string_input string_name;
                    submit_input "Click"]])
          ]))

let getco = register_new_coservice
    ~fallback:preappl
    ~get_params:(int "i" ** string "s")
    (fun sp (i,s) () -> return (f sp s))

let _ = register nonatt (fun sp s () -> return (f sp s))

let _ = 
  register_new_service 
    ~url:["getco"]
    ~get_params:unit
    (fun sp () () -> 
      return 
        (html
          (head (title (pcdata "")) [])
          (body [p [a getco sp [pcdata "clic"] (22,"eee") ];
                 get_form getco sp 
                   (fun (number_name,string_name) ->
                     [p [pcdata "Write an int: ";
                         int_input number_name;
                         pcdata "Write a string: ";
                         string_input string_name;
                         submit_input "Click"]])
               ])))


(* POST service with preapplied fallback are not possible: *)
(*
let my_service_with_post_params = 
  register_new_post_service
    ~fallback:preappl
    ~post_params:(string "value")
    (fun _ () value ->  return
      (html
         (head (title (pcdata "")) [])
         (body [h1 [pcdata value]])))
*)

(* GET coservice with coservice fallback: not possible *)
(*
let preappl3 = preapply getco (777,"ooo")

let getco2 = 
  register_new_coservice
    ~fallback:preappl3
    ~get_params:(int "i2" ** string "s2")
    (fun sp (i,s) () -> 
      return 
        (html
          (head (title (pcdata "")) [])
          (body [h1 [pcdata s]])))

*)


(* POST service with coservice fallback *)
let my_service_with_post_params = 
  register_new_post_service
    ~fallback:getco
    ~post_params:(string "value")
    (fun _ (i,s) value ->  return
      (html
         (head (title (pcdata "")) [])
         (body [h1 [pcdata (s^" "^value)]])))

let form2 = register_new_service ["postco"] unit
  (fun sp () () -> 
     let f =
       (post_form my_service_with_post_params sp
          (fun chaine -> 
            [p [pcdata "Write a string: ";
                string_input chaine]]) (222,"ooo")) in
     return
       (html
         (head (title (pcdata "form")) [])
         (body [f])))


(* Many cookies *)
let cookiename = "c"

let cookies = new_service ["c";""] unit ()

let _ = Cookies.register cookies
    (fun sp () () ->  return
      ((html
        (head (title (pcdata "")) [])
        (body [p 
                 (List.fold_left
                    (fun l (n,v) ->
                      (pcdata (n^"="^v))::
                      (br ())::l
                    )
                    [a cookies sp [pcdata "send other cookies"] ()]
                    (get_cookies sp))])),
       [(None, [((cookiename^"1"),(string_of_int (Random.int 100)));
               ((cookiename^"2"),(string_of_int (Random.int 100)));
               ((cookiename^"3"),(string_of_int (Random.int 100)))]);
        (Some ["c"], [((cookiename^"4"),(string_of_int (Random.int 100)));
                ((cookiename^"5"),(string_of_int (Random.int 100)))]);
        (Some [".."], [((cookiename^"6"),(string_of_int (Random.int 100)));
                ((cookiename^"7"),(string_of_int (Random.int 100)))]);
        (Some ["plop"], [((cookiename^"8"),(string_of_int (Random.int 100)));
                ((cookiename^"9"),(string_of_int (Random.int 100)));
                ((cookiename^"10"),(string_of_int (Random.int 100)));
                ((cookiename^"11"),(string_of_int (Random.int 100)));
                ((cookiename^"12"),(string_of_int (Random.int 100)))]);
      ]))


(* Cookies or not cookies with Any *)
let sendany = 
  Any.register_new_service 
    ~url:["sendany2"]
    ~get_params:(string "type")
   (fun sp s () -> 
     if s = "nocookie"
     then
       return
         (Xhtml.send
            sp
           (html
             (head (title (pcdata "")) [])
             (body [p [pcdata "This page does not set cookies"]])))
     else 
       return
         (Xhtml.Cookies.send
            sp
            ((html
                (head (title (pcdata "")) [])
                (body [p [pcdata "This page does set a cookie"]])),
             [None, [(("arf"),(string_of_int (Random.int 100)))]]))
   )


(* Send file *)
let _ = 
  register_new_service 
    ~url:["files";""]
    ~get_params:unit
    (fun _ () () -> 
      return 
        (html
          (head (title (pcdata "")) [])
          (body [h1 [pcdata "With a suffix, that page will send a file"]])))

let sendfile2 = 
  Files.register_new_service 
    ~url:["files";""]
    ~get_params:suffix_only
    (fun _ s () -> 
      let rec remove_dotdot = function
          [] -> []
        | ".."::l -> remove_dotdot l
        | a::l -> a::(remove_dotdot l)
      in
      return ("/var/www/ocsigen/"^(string_of_url_path (remove_dotdot s))))

let _ = 
  register_new_service 
    ~url:["files";"exception"]
    ~get_params:unit
    (fun _ () () -> 
      return 
        (html
          (head (title (pcdata "")) [])
          (body [h1 [pcdata "With another suffix, that page will send a file"]])))

