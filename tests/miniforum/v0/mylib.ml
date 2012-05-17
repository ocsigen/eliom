open Eliom_predefmod.Xhtml
open XHTML.M
open Eliom_service

let create_page sp mytitle mycontent =
  Lwt.return
    << <html>
         <head><title>$str:mytitle$</title></head>
         <body><h1>$str:mytitle$</h1>$list:mycontent$</body>
       </html> >>

let create_page2 sp mytitle mycontent =
  Lwt.return
    (html
       (head (title (pcdata mytitle)) [])
       (body ((h1 [pcdata mytitle])::mycontent)))





(* Messages database *)

(* For the example, I'm storing messages in memory.
   I should use a database instead.
   Here are some predefined messages: *)
let table = ref ["Welcome to Eliom's world.";
                 "Hello! This is the second message.";
                 "I am the third message of the forum."]

let display_message_list () =
  match !table with
  | [] -> p [em [pcdata "No message"]]
  | m::l ->
      ul
        (li [pcdata m])
        (List.map (fun m -> li [pcdata m]) l)

let display_message n =
  try
    let m = List.nth !table n in
    p [pcdata m]
  with 
  | Failure _
  | Invalid_argument _ -> p [em [pcdata "no such message"]]

let register_message msg = table := !table@[msg]

