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
let preappl2 = preapply uasuffix (1999,01)

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


(* action on GET attached coservice *)
let v = ref 0

let getact = 
  new_service 
    ~url:["getact"]
    ~get_params:(int "p")
    ()

let act = Actions.register_new_coservice
    ~fallback:(preapply getact 22)
    ~get_params:(int "bip")
    (fun _ g p -> v := g; return [])

(* action on GET non-attached coservice on GET coservice page *)
let naact = Actions.register_new_coservice'
    ~get_params:(int "bop")
    (fun _ g p -> v := g; return [])

let naunit = Unit.register_new_coservice'
    ~get_params:(int "bap")
    (fun _ g p -> v := g; return ())

let _ =
  register
    getact
    (fun sp aa () -> 
      return
        (html
           (head (title (pcdata "getact")) [])
           (body [h1 [pcdata ("v = "^(string_of_int !v))];
                  p [pcdata ("p = "^(string_of_int aa))];
                  p [a getact sp [pcdata "link to myself"] 0;
                     br ();
                     a act sp [pcdata "an attached action to change v"] 
                       (Random.int 100);
                     br ();
                     a naact sp [pcdata "a non attached action to change v"] 
                       (100 + Random.int 100);
                     pcdata " (Actually if called after the previous one, v won't change. More precisely, it will change and turn back to the former value because the attached coservice is reloaded after action)";
                     br ();
                     a naunit sp [pcdata "a non attached \"Unit\" page to change v"] 
                       (200 + Random.int 100);
                     pcdata " (Reload after clicking here)"
                   ]])))




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
       let cookies =
         [Extensions.Set (Some [], Some (Unix.time () +. 30.), 
                          [((cookiename^"6"),(string_of_int (Random.int 100)));
                           ((cookiename^"7"),(string_of_int (Random.int 100)))]);
          Extensions.Set (Some ["plop"], None, 
                          [((cookiename^"8"),(string_of_int (Random.int 100)));
                           ((cookiename^"9"),(string_of_int (Random.int 100)));
                           ((cookiename^"10"),(string_of_int (Random.int 100)));
                           ((cookiename^"11"),(string_of_int (Random.int 100)));
                           ((cookiename^"12"),(string_of_int (Random.int 100)))]);
        ]
       in if List.mem_assoc (cookiename^"1") (get_cookies sp)
       then 
         (Extensions.Unset (None, 
                            [(cookiename^"1");(cookiename^"2")]))::cookies
       else 
         (Extensions.Set (None, None,
                          [((cookiename^"1"),(string_of_int (Random.int 100)));
                           ((cookiename^"2"),(string_of_int (Random.int 100)));
                           ((cookiename^"3"),(string_of_int (Random.int 100)))]))
         ::cookies
      ))


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
             [Extensions.Set (None, None, 
                              [(("arf"),(string_of_int (Random.int 100)))])]))
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
    ~get_params:(suffix (all_suffix "filename"))
    (fun _ s () -> 
      return ("/var/www/ocsigen/"^(string_of_url_path s)))

let _ = 
  register_new_service 
    ~url:["files";"exception"]
    ~get_params:unit
    (fun _ () () -> 
      return 
        (html
          (head (title (pcdata "")) [])
          (body [h1 [pcdata "With another suffix, that page will send a file"]])))


(* Complex suffixes *)
let suffix2 = 
  register_new_service 
    ~url:["suffix2";""]
    ~get_params:(suffix (string "suff1" ** int "ii" ** all_suffix "ee"))
    (fun sp (suf1,(ii,ee)) () ->  
      return
        (html
           (head (title (pcdata "")) [])
           (body
              [p [pcdata "The suffix of the url is ";
                  strong [pcdata (suf1^", "^(string_of_int ii)^", "^
                                  (string_of_url_path ee))]]])))

let suffix3 = 
  register_new_service 
    ~url:["suffix3";""]
    ~get_params:(suffix_prod (string "suff1" ** int "ii" ** all_suffix_user int_of_string string_of_int "ee") (string "a" ** int "b"))
    (fun sp ((suf1, (ii, ee)), (a, b)) () ->  
      return
        (html
           (head (title (pcdata "")) [])
           (body
              [p [pcdata "The parameters in the url are ";
                  strong [pcdata (suf1^", "^(string_of_int ii)^", "^
                                  (string_of_int ee)^", "^
                                  a^", "^(string_of_int b))]]])))

let create_suffixform2 (suf1,(ii,ee)) =
    <:xmllist< <p>Write a string: 
      $string_input suf1$ <br/>
      Write an int: $int_input ii$ <br/>
      Write a string: $user_type_input string_of_url_path ee$ <br/>
      $submit_input "Click"$</p> >>

let suffixform2 = register_new_service ["suffixform2"] unit
  (fun sp () () -> 
     let f = get_form suffix2 sp create_suffixform2 in
     return
       (html
          (head (title (pcdata "")) [])
          (body [h1 [pcdata "Hallo"];
                 f ])))

let create_suffixform3 ((suf1, (ii, ee)), (a, b)) =
    <:xmllist< <p>Write a string: 
      $string_input suf1$ <br/>
      Write an int: $int_input ii$ <br/>
      Write an int: $int_input ee$ <br/>
      Write a string: $string_input a$ <br/>
      Write an int: $int_input b$ <br/> 
      $submit_input "Click"$</p> >>

let suffixform3 = register_new_service ["suffixform3"] unit
  (fun sp () () -> 
     let f = get_form suffix3 sp create_suffixform3 in
     return
        (html
          (head (title (pcdata "")) [])
          (body [h1 [pcdata "Hallo"];
                 f ])))


(* Send file with regexp *)
let _ = 
  register_new_service 
    ~url:["files2";""]
    ~get_params:unit
    (fun _ () () -> 
      return 
        (html
          (head (title (pcdata "")) [])
          (body [h1 [pcdata "With a suffix, that page will send a file"]])))

let r = Netstring_pcre.regexp "~([^/]*)(.*)"

let sendfile2 = 
  Files.register_new_service 
    ~url:["files2";""]
(*    ~get_params:(regexp r "/home/$1/public_html$2" "filename") *)
    ~get_params:(regexp r "$$u($1)$2" "filename")
    (fun _ s () -> return s)

let sendfile2 = 
  Files.register_new_service 
    ~url:["files2";""]
    ~get_params:(suffix (all_suffix_regexp r "/home/$1/public_html$2" "filename"))
    (fun _ s () -> return s)

let create_suffixform4 n =
    <:xmllist< <p>Write the name of the file: 
      $string_input n$ 
      $submit_input "Click"$</p> >>

let suffixform4 = register_new_service ["suffixform4"] unit
  (fun sp () () -> 
     let f = get_form sendfile2 sp create_suffixform4 in
     return
        (html
          (head (title (pcdata "")) [])
          (body [h1 [pcdata "Hallo"];
                 f ])))


(* Advanced use of any *)
let any2 = register_new_service 
    ~url:["any2"]
    ~get_params:(int "i" ** any)
  (fun _ (i,l) () ->
    let ll = 
      List.map 
        (fun (a,s) -> << <strong>($str:a$, $str:s$)</strong> >>) l 
    in  
    return
  << <html>
       <head><title></title></head>
       <body>
       <p>
         You sent: 
         <span>$list:ll$</span>
         <br/>
         i = $str:(string_of_int i)$
       </p>
       </body>
     </html> >>)

(* the following will not work because s is staken in any.
   Is it normal?
   Could this be improved?
 *)
let any3 = register_new_service 
    ~url:["any3"]
    ~get_params:(int "i" ** any ** string "s")
  (fun _ (i,(l,s)) () ->
    let ll = 
      List.map 
        (fun (a,s) -> << <strong>($str:a$, $str:s$)</strong> >>) l 
    in  
    return
  << <html>
       <head><title></title></head>
       <body>
       <p>
         You sent: 
         <span>$list:ll$</span>
         <br/>
         i = $str:(string_of_int i)$
         <br/>
         s = $str:s$
       </p>
       </body>
     </html> >>)

let any4 = register_new_service 
    ~url:["any4"]
    ~get_params:(suffix any)
  (fun _ l () ->
    let ll = 
      List.map 
        (fun (a,s) -> << <strong>($str:a$, $str:s$)</strong> >>) l 
    in  
    return
  << <html>
       <head><title></title></head>
       <body>
       <p>
         You sent: 
         <span>$list:ll$</span>
       </p>
       </body>
     </html> >>)

let sufli = register_new_service 
    ~url:["sufli"]
    ~get_params:(suffix (list "l" (string "s")))
  (fun _ l () ->
    let ll = 
      List.map 
        (fun s -> << <strong>$str:s$</strong> >>) l 
    in  
    return
  << <html>
       <head><title></title></head>
       <body>
       <p>
         You sent: 
         <span>$list:ll$</span>
       </p>
       </body>
     </html> >>)

(* form to any2 *)
let any2form = register_new_service 
    ~url:["any2form"]
    ~get_params:unit
    (fun sp () () ->
      return
        (html
           (head (title (pcdata "")) [])
           (body [h1 [pcdata "Any Form"];
                  get_form any2 sp 
                    (fun (iname,grr) ->
                      [p [pcdata "Form to any2: ";
                          int_input iname;
                          any_input "plop";
                          any_input "plip";
                          any_input "plap";
                          submit_input "Click"]])
                ])))


(* main *)
let main = new_service ["ex"] unit ()

let _ = register main
  (fun sp () () -> return
     << 
       <html> 
       <!-- This is a comment! -->
       <head>
         $css_link (make_uri (static_dir sp) sp ["style.css"])$
         <title>Eliom Tutorial</title>
       </head>
       <body>
         
         <h1>$img ~alt:"Ocsigen" ~src:(make_uri (static_dir sp) sp ["ocsigen5.png"]) ()$</h1>

       <h2>Eliom examples, bis</h2>
       <p>
         any2 : $a any2 sp <:xmllist< any2 >> (3,[("Ciao","bel");
                                                  ("ragazzo","!")])$ <br/> 
         any3 : $a any3 sp <:xmllist< any3 >> (3,(["a","e"],"z"))$ <br/> 
<!--         sufli : $a sufli sp <:xmllist< sufli >> ["Ciao";"bel";"ragazzo";"!"]$ <br/> -->
       </p>
       </body>
     </html> >>)
