(* At most two volatile data sessions without a group in the site. *)

open Eliom
module P = Parameter

let text, get, register = Services.(text, get, register)

let () =
  State.set_default_max_volatile_data_sessions_without_group
    ~override_configfile:true 2

(* The limit of the parent of the session of the request, which has no
   group *)
let () =
  register
    (get ["max_in_parent"] P.(int "n"))
    (fun n () ->
       State.set_max_volatile_data_states_in_parent
         ~scope:(Common.default_session_scope :> Common.user_scope)
         n;
       text "set")

(* The limit of the sessions without a group of the site *)
let () =
  register
    (get ["max_without_group"] P.(int "n"))
    (fun n () ->
       match
         State.set_default_max_volatile_data_sessions_without_group
           ~override_configfile:true n
       with
       | () -> text "set"
       | exception Invalid_argument _ -> text "invalid")

let () = Services.start ()
