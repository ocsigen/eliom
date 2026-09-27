(* The limit of session groups of the site, set with a session group scope.
   It applies to the whole server: each run tests one kind of state, and no
   session is opened without a group. *)

open Eliom
module P = Parameter
module V = Reference.Volatile

let text, get, register = Services.(text, get, register)
let session_scope = Common.default_session_scope
let group_scope = (Common.default_group_scope :> Common.user_scope)

(* Services: the browser joins a group of services, then gets a coservice of
   its session, and sets the limit of groups *)

let fallback = get ["fallback"] P.unit
let () = register fallback (fun () () -> text "fallback")

let () =
  register
    (get ["service"; "join"] P.(string "name"))
    (fun name () ->
       State.set_service_session_group ~scope:session_scope name;
       let service =
         Service.create_attached_get ~fallback ~get_params:P.unit ()
       in
       Registration.String.register ~scope:session_scope ~service (fun () () ->
         text "coservice");
       text (Eliom_uri.make_string_uri ~absolute_path:true ~service ()))

let () =
  register
    (get ["service"; "max_groups"] P.(int "n"))
    (fun n () ->
       State.set_max_service_states_for_group_or_subnet ~scope:group_scope n;
       text "set")

(* Data: the browser joins a group of data, then sets a session reference,
   and sets the limit of groups *)

let () =
  register
    (get ["data"; "join"] P.(string "name" ** string "v"))
    (fun (name, v) () ->
       State.set_volatile_data_session_group ~scope:session_scope name;
       V.set Services.session_value v;
       text "joined")

let () =
  register
    (get ["data"; "max_groups"] P.(int "n"))
    (fun n () ->
       State.set_max_volatile_data_states_for_group_or_subnet ~scope:group_scope
         n;
       text "set")

let () = Services.start ()
