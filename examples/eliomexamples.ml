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
open Eliom_predefmod.Xhtmlcompact
open Eliom_predefmod
open Eliom_services
open Eliom_parameters
open Eliom_sessions
open Lwt



(* Preapplied service with suffix parameters *)

let presu_service =
  register_new_service 
    ~path: ["preappliedsuffix2"]
    ~get_params: (suffix (int "i"))
    (fun _ i () ->
      Lwt.return
        (html
           (head (title (pcdata "")) [])
           (body [p [ pcdata ("You sent: " ^ (string_of_int i))]])))


let creator_handler sp () () =
  let create_form () =
    [fieldset [string_input ~input_type:`Submit ~value:"Click" ()]] in
  let myservice = preapply presu_service 10 in
  let myform = get_form myservice sp create_form in
  Lwt.return
    (html
       (head (title (pcdata "")) [])
       (body   [
        p [pcdata "Form with preapplied parameter:"];
        myform;
        p [a myservice sp [pcdata "Link with preapplied parameter"] ()]
      ]))

let preappliedsuffix =
  register_new_service
    ~path: ["preappliedsuffix"]
    ~get_params: unit
    creator_handler
    

(* URL with ? or / in data or paths *)

let url_encoding = 
  register_new_service 
    ~path:["urlencoding"]
    ~get_params:(suffix_prod (all_suffix "s//\\à") any)
    (fun sp (suf, l) () -> 
      let ll = 
        List.map 
          (fun (a,s) -> << <strong>($str:a$, $str:s$) </strong> >>) l
      in  
      let sl = 
        List.map 
          (fun s -> << <strong>$str:s$ </strong> >>) suf
      in  
      return 
        (html
           (head (title (pcdata "")) [])
           (body [h1 [pcdata "Hallo"];
                  p sl;
                  p ll
                ])))


(* menu with preapplied services *)

let preappl = preapply coucou_params (3,(4,"cinq"))
let preappl2 = preapply uasuffix (1999,01)

let mymenu current sp =
  Eliom_tools.menu ~classe:["menuprincipal"]
    (coucou, <:xmllist< coucou >>)
    [
     (preappl, <:xmllist< params >>);
     (preappl2, <:xmllist< params and suffix >>);
   ] ~service:current ~sp

let preappmenu = 
  register_new_service 
    ~path:["menu"]
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
                    string_input ~input_type:`Text ~name:string_name ();
                    string_input ~input_type:`Submit ~value:"Click" ()]])
          ]))

let getco = register_new_coservice
    ~fallback:preappl
    ~get_params:(int "i" ** string "s")
    (fun sp (i,s) () -> return (f sp s))

let _ = register nonatt (fun sp s () -> return (f sp s))

let getcoex = 
  register_new_service 
    ~path:["getco"]
    ~get_params:unit
    (fun sp () () -> 
      return 
        (html
          (head (title (pcdata "")) [])
          (body [p [a getco sp [pcdata "clic"] (22,"eee") ];
                 get_form getco sp 
                   (fun (number_name,string_name) ->
                     [p [pcdata "Write an int: ";
                         int_input ~input_type:`Text ~name:number_name ();
                         pcdata "Write a string: ";
                         string_input ~input_type:`Text ~name:string_name ();
                         string_input  ~input_type:`Submit ~value:"Click" ()]])
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

let postcoex = register_new_service ["postco"] unit
  (fun sp () () -> 
     let f =
       (post_form my_service_with_post_params sp
          (fun chaine -> 
            [p [pcdata "Write a string: ";
                string_input ~input_type:`Text ~name:chaine ()]]) 
          (222,"ooo")) in
     return
       (html
         (head (title (pcdata "form")) [])
         (body [f])))


(* action on GET attached coservice *)
let v = ref 0

let getact = 
  new_service 
    ~path:["getact"]
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

let cookies = new_service ["c";""] (suffix (all_suffix_string "s")) ()

let _ = Cookies.register cookies
    (fun sp s () ->  return
      ((html
        (head (title (pcdata "")) [])
        (body [p 
                 (Http_frame.Cookievalues.fold
                    (fun n v l ->
                      (pcdata (n^"="^v))::
                      (br ())::l
                    )
                    (get_cookies sp)
                    [a cookies sp [pcdata "send other cookies"] ""; br ();
                     a cookies sp [pcdata "send other cookies and see the url /c/plop"] "plop"]
                    )])),
       let now = Unix.time () in
       let cookies =
         [Eliom_services.Set (Some [], Some (now +. 10.), 
                          (cookiename^"6"), (string_of_int (Random.int 100)));
          Eliom_services.Set (Some [], Some (now +. 10.), 
                          (cookiename^"7"), (string_of_int (Random.int 100)));
          Eliom_services.Set (Some ["c";"plop"], None, 
                          (cookiename^"8"), (string_of_int (Random.int 100)));
          Eliom_services.Set (Some ["c";"plop"], None, 
                          (cookiename^"9"), (string_of_int (Random.int 100)));
          Eliom_services.Set (Some ["c";"plop"], None, 
                          (cookiename^"10"), (string_of_int (Random.int 100)));
          Eliom_services.Set (Some ["c";"plop"], None, 
                          (cookiename^"11"), (string_of_int (Random.int 100)));
          Eliom_services.Set (Some ["c";"plop"], None, 
                          (cookiename^"12"), (string_of_int (Random.int 100)));
        ]
       in if Http_frame.Cookievalues.mem (cookiename^"1") (get_cookies sp)
       then 
         (Eliom_services.Unset (None, (cookiename^"1")))::
         (Eliom_services.Unset (None, (cookiename^"2")))::cookies
       else 
         (Eliom_services.Set (None, None, (cookiename^"1"),
                        (string_of_int (Random.int 100))))::
          (Eliom_services.Set (None, None, (cookiename^"2"),
                         (string_of_int (Random.int 100))))::
          (Eliom_services.Set (None, None, (cookiename^"3"),
                         (string_of_int (Random.int 100))))
          ::cookies
      ))


(* Cookies or not cookies with Any *)
let sendany = 
  Any.register_new_service 
    ~path:["sendany2"]
    ~get_params:(suffix (all_suffix_string "type"))
   (fun sp s () -> 
     if s = "nocookie"
     then
       Xhtml.send
         sp
         (html
            (head (title (pcdata "")) [])
            (body [p [pcdata "This page does not set cookies"]]))
     else 
       Xhtml.Cookies.send
         sp
         ((html
             (head (title (pcdata "")) [])
             (body [p [pcdata "This page does set a cookie"]])),
          [Eliom_services.Set (None, None, "arf", (string_of_int (Random.int 100)))])
   )


(* Send file *)
let sendfileex = 
  register_new_service 
    ~path:["files";""]
    ~get_params:unit
    (fun _ () () -> 
      return 
        (html
          (head (title (pcdata "")) [])
          (body [h1 [pcdata "With a suffix, that page will send a file"]])))

let sendfile2 = 
  Files.register_new_service 
    ~path:["files";""]
    ~get_params:(suffix (all_suffix "filename"))
    (fun _ s () -> 
      return ("/var/www/ocsigen/"^(Extensions.string_of_url_path s)))

let sendfileexception = 
  register_new_service 
    ~path:["files";"exception"]
    ~get_params:unit
    (fun _ () () -> 
      return 
        (html
          (head (title (pcdata "")) [])
          (body [h1 [pcdata "With another suffix, that page will send a file"]])))


(* Complex suffixes *)
let suffix2 = 
  register_new_service 
    ~path:["suffix2";""]
    ~get_params:(suffix (string "suff1" ** int "ii" ** all_suffix "ee"))
    (fun sp (suf1,(ii,ee)) () ->  
      return
        (html
           (head (title (pcdata "")) [])
           (body
              [p [pcdata "The suffix of the url is ";
                  strong [pcdata (suf1^", "^(string_of_int ii)^", "^
                                  (Extensions.string_of_url_path ee))]]])))

let suffix3 = 
  register_new_service 
    ~path:["suffix3";""]
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
      $string_input ~input_type:`Text ~name:suf1 ()$ <br/>
      Write an int: $int_input ~input_type:`Text ~name:ii ()$ <br/>
      Write a string: $user_type_input ~input_type:`Text ~name:ee 
                         Extensions.string_of_url_path$ <br/>
      $string_input ~input_type:`Submit ~value:"Click" ()$</p> >>

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
      $string_input ~input_type:`Text ~name:suf1 ()$ <br/>
      Write an int: $int_input ~input_type:`Text ~name:ii ()$ <br/>
      Write an int: $int_input ~input_type:`Text ~name:ee ()$ <br/>
      Write a string: $string_input ~input_type:`Text ~name:a ()$ <br/>
      Write an int: $int_input ~input_type:`Text ~name:b ()$ <br/> 
      $string_input ~input_type:`Submit ~value:"Click" ()$</p> >>

let suffixform3 = register_new_service ["suffixform3"] unit
  (fun sp () () -> 
     let f = get_form suffix3 sp create_suffixform3 in
     return
        (html
          (head (title (pcdata "")) [])
          (body [h1 [pcdata "Hallo"];
                 f ])))

let suffix5 = 
  register_new_service 
    ~path:["suffix5";""]
    ~get_params:(suffix (all_suffix "s"))
    (fun sp s () ->  
      return
        (html
           (head (title (pcdata "")) [])
           (body
              [p [pcdata "This is a page with suffix "; 
                  strong [pcdata (Ocsigen_lib.string_of_url_path s)]]])))

let nosuffix = 
  register_new_service 
    ~path:["suffix5";"notasuffix";""]
    ~get_params:unit
    (fun sp () () ->  
      return
        (html
           (head (title (pcdata "")) [])
           (body
              [p [pcdata "This is a page without suffix. Replace ";
                  code [pcdata "notasuffix"];
                  pcdata " in the URL by something else."
                ]])))



(* Send file with regexp *)
let sendfileregexp = 
  register_new_service 
    ~path:["files2";""]
    ~get_params:unit
    (fun _ () () -> 
      return 
        (html
          (head (title (pcdata "")) [])
          (body [h1 [pcdata "With a suffix, that page will send a file"]])))

let r = Netstring_pcre.regexp "~([^/]*)(.*)"

let sendfile2 = 
  Files.register_new_service 
    ~path:["files2";""]
(*    ~get_params:(regexp r "/home/$1/public_html$2" "filename") *)
    ~get_params:(regexp r "$$u($1)$2" "filename")
    (fun _ s () -> return s)

let sendfile2 = 
  Files.register_new_service 
    ~path:["files2";""]
    ~get_params:(suffix 
                   (all_suffix_regexp r "/home/$1/public_html$2" "filename"))
(*    ~get_params:(suffix (all_suffix_regexp r "$$u($1)$2" "filename")) *)
    (fun _ s () -> return s)

let create_suffixform4 n =
    <:xmllist< <p>Write the name of the file: 
      $string_input ~input_type:`Text ~name:n ()$ 
      $string_input ~input_type:`Submit ~value:"Click" ()$</p> >>

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
    ~path:["any2"]
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

(* the following will not work because s is taken in any. (not checked) *)
let any3 = register_new_service 
    ~path:["any3"]
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


(* any cannot be in suffix: (not checked) *)
let any4 = register_new_service 
    ~path:["any4"]
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


let any5 = register_new_service 
    ~path:["any5"]
    ~get_params:(suffix_prod (string "s") any)
  (fun _ (s, l) () ->
    let ll = 
      List.map 
        (fun (a,s) -> << <strong>($str:a$, $str:s$)</strong> >>) l 
    in  
    return
  << <html>
       <head><title></title></head>
       <body>
       <p>
         You sent <strong>$str:s$</strong> and : 
         <span>$list:ll$</span>
       </p>
       </body>
     </html> >>)

(* list cannot be in suffix: (not checked) *)
let sufli = register_new_service 
    ~path:["sufli"]
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
    ~path:["any2form"]
    ~get_params:unit
    (fun sp () () ->
      return
        (html
           (head (title (pcdata "")) [])
           (body [h1 [pcdata "Any Form"];
                  get_form any2 sp 
                    (fun (iname,grr) ->
                      [p [pcdata "Form to any2: ";
                          int_input ~input_type:`Text ~name:iname ();
                          raw_input ~input_type:`Text ~name:"plop" ();
                          raw_input ~input_type:`Text ~name:"plip" ();
                          raw_input ~input_type:`Text ~name:"plap" ();
                          string_input ~input_type:`Submit ~value:"Click" ()]])
                ])))


(* bool list *)

let boollist = register_new_service 
    ~path:["boollist"]
    ~get_params:(list "a" (bool "b"))
  (fun _ l () ->
    let ll = 
      List.map (fun b -> 
        (strong [pcdata (if b then "true" else "false")])) l in  
    return
      (html
         (head (title (pcdata "")) [])
         (body
            [p ((pcdata "You sent: ")::ll)]
         )))

let create_listform f = 
  (* Here, f.it is an iterator like List.map, 
     but it must be applied to a function taking 2 arguments 
     (and not 1 as in map), the first one being the name of the parameter.
     The last parameter of f.it is the code that must be appended at the 
     end of the list created
   *)
  let l =
    f.it (fun boolname v ->
      [tr (td [pcdata ("Write the value for "^v^": ")])
         [td [bool_checkbox ~name:boolname ()]]])
      ["one";"two";"three"]
      []
  in
  [table (List.hd l) (List.tl l);
   p [raw_input ~input_type:`Submit ~value:"Click" ()]]

let boollistform = register_new_service ["boolform"] unit
  (fun sp () () -> 
     let f = get_form boollist sp create_listform in return
        (html
          (head (title (pcdata "")) [])
          (body [f])))


(********)


(* any with POST *)
let any = register_new_post_service 
    ~fallback:coucou
    ~post_params:any
  (fun _ () l ->
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
         $list:ll$
       </p>
       </body>
     </html> >>)

(* form to any *)
let anypostform = register_new_service 
    ~path:["anypostform"]
    ~get_params:unit
    (fun sp () () ->
      return
        (html
           (head (title (pcdata "")) [])
           (body [h1 [pcdata "Any Form"];
                  post_form any sp 
                    (fun () ->
                      [p [pcdata "Empty form to any: ";
                          string_input ~input_type:`Submit ~value:"Click" ()]])
                    ()
                ])))

(**********)
(* upload *)

(* ce qui suit ne doit pas fonctionner. Mais il faudrait l'interdire *)
let get_param_service =
  register_new_service
   ~path:["uploadget"]
   ~get_params:(string "name" ** file "file")
    (fun _ (name,file) () ->
         let to_display = 
           let newname = "/tmp/fichier" in
           (try
             Unix.unlink newname;
           with _ -> ());
           Unix.link (get_tmp_filename file) newname;
           let fd_in = open_in newname in
           try
             let line = input_line fd_in in close_in fd_in; line (*end*)
           with End_of_file -> close_in fd_in; "vide"
         in
         return
            (html
                (head (title (pcdata name)) [])
                (body [h1 [pcdata to_display]])))


let uploadgetform = register_new_service ["uploadget"] unit
  (fun sp () () ->
    let f =
(* ARG        (post_form ~a:[(XHTML.M.a_enctype "multipart/form-data")] fichier2 sp *)
     (get_form ~a:[(XHTML.M.a_enctype "multipart/form-data")] ~service:get_param_service ~sp
     (*post_form my_service_with_post_params sp        *)
        (fun (str, file) ->
          [p [pcdata "Write a string: ";
              string_input ~input_type:`Text ~name:str ();
              br ();
              file_input ~name:file ()]])) in  return
         (html
           (head (title (pcdata "form")) [])
           (body [f])))


(*******)
(* Actions that raises an exception *)
let exn_act = Actions.register_new_coservice'
    ~get_params:unit
    (fun _ g p -> fail Not_found)

let exn_act_main = 
  register_new_service 
    ~path:["exnact"]
    ~get_params:unit
    (fun sp () () -> 
      return
        (html
           (head (title (pcdata "exnact")) [])
           (body [h1 [pcdata "Hello"];
                  p [a exn_act sp [pcdata "Do the action"] ()
                   ]])))


(* close sessions from outside *)
let close_from_outside = 
  register_new_service 
    ~path:["close_from_outside"]
    ~get_params:unit
    (fun sp () () -> 
      close_all_sessions ~session_name:"persistent_sessions" ~sp () >>= fun () ->
      close_all_sessions ~session_name:"action_example2" ~sp () >>= fun () ->
      return 
        (html
           (head (title (pcdata "")) [])
           (body [h1 [pcdata "all sessions called \"persistent_sessions\" and \"action_example2\" closed"];
                  p [a persist_session_example sp [pcdata "try"] ()]])))



(* setting timeouts *)
let set_timeout = 
register_new_service 
    ~path:["set_timeout"]
    ~get_params:(int "t" ** bool "recompute")
    (fun sp (t, recompute) () -> 
      set_global_persistent_data_session_timeout ~session_name:"persistent_sessions"
        ~recompute_expdates:recompute ~sp (Some (float_of_int t)) >>= fun () ->
      set_global_volatile_session_timeout ~session_name:"action_example2"
        ~recompute_expdates:recompute ~sp (Some (float_of_int t)) >>= fun () ->
      return 
        (html
           (head (title (pcdata "")) [])
           (body [h1 [pcdata "Setting timeout"];
                  p [
                  if recompute
                  then pcdata ("The timeout for sessions called \"persistent_sessions\" and \"action_example2\" has been set to "^(string_of_int t)^" seconds (all expiration dates updated).")
                  else pcdata ("From now, the timeout for sessions called \"persistent_sessions\" and \"action_example2\" will be "^(string_of_int t)^" seconds (expiration dates not updated)."); br ();
                  a persist_session_example sp [pcdata "Try"] ()]])))
    

let create_form = 
  (fun (number_name, boolname) ->
    [p [pcdata "New timeout: ";
        Eliom_predefmod.Xhtml.int_input ~input_type:`Text ~name:number_name ();
        br ();
        pcdata "Check the box if you want to recompute all timeouts: ";
        Eliom_predefmod.Xhtml.bool_checkbox ~name:boolname ();
        Eliom_predefmod.Xhtml.string_input ~input_type:`Submit ~value:"Submit" ()]])

let set_timeout_form = 
  register_new_service
    ["set_timeout"] 
    unit
    (fun sp () () -> 
      let f = Eliom_predefmod.Xhtml.get_form set_timeout sp create_form in 
      return
        (html
           (head (title (pcdata "")) [])
           (body [f])))



(******************************************************************)
let mainpage = register_new_service ["tests"] unit
 (fun sp () () -> 
   return
    (html
     (head (title (pcdata "Test")) 
        [css_link (make_uri (static_dir sp) sp ["style.css"]) ()])
     (body 
       [h1 [img ~alt:"Ocsigen" ~src:(make_uri (static_dir sp) sp ["ocsigen5.png"]) ()];
        h3 [pcdata "Eliom tests"];
        p
        [
         a coucou sp [pcdata "coucou"] (); br ();
         a getcoex sp [pcdata "GET coservice with preapplied fallback, etc"] (); br ();
         a postcoex sp [pcdata "POST service with coservice fallback"] (); br ();
         a preappliedsuffix sp [pcdata "Preapplied suffix"] (); br ();
         a getact sp [pcdata "action on GET attached coservice, etc"] 127; br ();
         a cookies sp [pcdata "Many cookies"] "le suffixe de l'URL"; br ();
         a sendany sp [pcdata "Cookie or not with Any"] "change this suffix to \"nocookie\""; br ();
         a sendfileex sp [pcdata "Send file"] (); br ();
         a sendfile2 sp [pcdata "Send file 2"] "style.css"; br ();
         a sendfileexception sp [pcdata "Do not send file"] (); br ();
         a sendfileregexp sp [pcdata "Send file with regexp"] (); br ();
         a suffixform2 sp [pcdata "Suffix 2"] (); br ();
         a suffixform3 sp [pcdata "Suffix 3"] (); br ();
         a suffixform4 sp [pcdata "Suffix 4"] (); br ();
         a nosuffix sp [pcdata "Page without suffix on the same URL of a page with suffix"] (); br ();
         a anypostform sp [pcdata "POST form to any parameters"] (); br ();
         a any2 sp [pcdata "int + any parameters"] 
           (3, [("Ciao","bel"); ("ragazzo","!")]); br ();
         a any3 sp [pcdata "any parameters broken (s after any)"] 
           (4, ([("Thierry","Richard");("Sébastien","Stéphane")], "s")); br ();
(* broken        a any4 sp [pcdata "Any in suffix"] [("bo","ba");("bi","bu")]; br (); *)
         a any5 sp [pcdata "Suffix + any parameters"] 
           ("ee", [("bo","ba");("bi","bu")]); br ();
         a uploadgetform sp [pcdata "Upload with GET"] (); br (); 
(* broken        a sufli sp [pcdata "List in suffix"] ["bo";"ba";"bi";"bu"]; br ();*)
         a boollistform sp [pcdata "Bool list"] (); br ();
         a preappmenu sp [pcdata "Menu with pre-applied services"] (); br ();
         a exn_act_main sp [pcdata "Actions that raises an exception"] (); br ();
         a close_from_outside sp [pcdata "Closing sessions from outside"] (); br ();
         a set_timeout_form sp [pcdata "Setting timeouts from outside sessions"] (); br ();
         a
           ~fragment:"a--   ---++&é/@"
           ~service:url_encoding ~sp 
           [pcdata "Urls with strange characters inside"] 
           (["l/l%l      &l=l+l)l@";"m\\m\"m";"nèn~n"],
            [("po?po&po~po/po+po", "lo\"l     o#lo'lo lo=lo&l      o/lo+lo"); 
            ("bo=mo@co:ro", "zo^zo%zo$zo:zo")]); br ();



       ]])))
