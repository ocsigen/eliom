open XHTML.M
open Eliom_predefmod.Xhtml
open Eliom_service


(* Messages database *)

(* For the example, I'm storing messages in memory.
   I should use a database instead.
   Here are some predefined messages: *)
let table = ref ["Welcome to Eliom's world.";
                 "Hello! This is the second message.";
                 "I am the third message of the forum."]

let display_message_list sp =
  let f m i =
    [pcdata m; 
     pcdata " ";
     a Services.msgpage sp [pcdata "read"] i]
  in
  match !table with
  | [] -> p [em [pcdata "No message"]]
  | m::l ->
      ul
        (li (f m 0))
        (snd
           (List.fold_right
              (fun m (i, l) -> 
                 (i+1, 
                  (li (f m i))::l))
              l 
              (0, [])))

let display_message n =
  try
    let m = List.nth !table n in
    p [pcdata m]
  with 
  | Failure _
  | Invalid_argument _ -> p [em [pcdata "no such message"]]

let register_message msg = table := !table@[msg]

let disconnect_box sp s = 
  post_form Services.disconnect_action sp 
    (fun _ -> [p [Eliom_predefmod.Xhtml.string_input
                    ~input_type:`Submit ~value:s ()]]) ()

let login_box sp = 
  post_form Services.connect_action sp
    (fun loginname ->
      [p 
         (let l = [pcdata "login: "; 
                   Eliom_predefmod.Xhtml.string_input
                     ~input_type:`Text ~name:loginname ()]
         in l)
     ])
    ()


let userbox ~sp =
  let sessdat = Eliom_sessions.get_volatile_data_session_group ~sp () in
  (match sessdat with
  | Eliom_sessions.Data name ->
      div
        [p [pcdata ("Hello "^name); br ()];
         disconnect_box sp "Close session"]
  | Eliom_sessions.Data_session_expired
  | Eliom_sessions.No_data -> login_box sp
  )

(*****)
let create_page sp mytitle mycontent =
  Lwt.return
    (html
       (head 
          (title (pcdata mytitle)) 
          [css_link (make_uri ~service:(static_dir sp) ~sp ["style.css"]) ()])
       (body ((h1 [pcdata mytitle])::(userbox sp)::mycontent)))


