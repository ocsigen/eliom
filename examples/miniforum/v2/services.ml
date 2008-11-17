open Eliom_services
open Eliom_parameters


let mainpage = new_service ~path:[] ~get_params:unit ()
let msgpage = new_service  ~path:[] ~get_params:(int "n") ()
let addmsgpage = 
  new_post_service ~fallback:mainpage ~post_params:(string "msg") ()
let newmsgpage = new_service ~path:["newmessage"] ~get_params:unit ()

let connect_action = 
  new_post_coservice' ~name:"connect" ~post_params:(string "login") ()
let disconnect_action = 
  new_post_coservice' ~name:"disconnect" ~post_params:unit ()

