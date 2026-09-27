(* Global and default timeouts, scope hierarchies and session groups. The
   timeouts set by the services apply to the whole site. *)

open Eliom
open Lwt.Syntax
module P = Parameter
module V = Reference.Volatile

let text, get, register = Services.(text, get, register)
let session_scope = Common.default_session_scope
let cookie_scope = (session_scope :> Common.cookie_scope)

let () =
  register
    (get ["global_timeout"] P.(opt (float "t")))
    (fun t () ->
       State.set_global_volatile_data_state_timeout ~cookie_scope
         ~override_configfile:true t;
       text "set")

let () =
  register
    (get ["default_timeout"] P.(opt (float "t")))
    (fun t () ->
       State.set_default_global_volatile_data_state_timeout
         ~cookie_level:`Session ~override_configfile:true t;
       text "set")

let () =
  register
    (get ["timeout"] P.(opt (float "t")))
    (fun t () ->
       State.set_volatile_data_state_timeout ~cookie_scope t;
       text "set")

(* A session reference of another scope hierarchy *)

let other_scope = `Session (Common.create_scope_hierarchy "other")
let other_value = V.eref ~scope:other_scope ""

let () =
  register
    (get ["other"; "set"] P.(string "v"))
    (fun v () -> V.set other_value v; text "set")

let () =
  register (get ["other"; "get"] P.unit) (fun () () -> text (V.get other_value))

let () =
  register
    (get ["discard"] P.(string "scope"))
    (fun scope () ->
       let* () =
         match scope with
         | "session" -> State.discard ~scope:session_scope ()
         | "other" -> State.discard ~scope:other_scope ()
         | s -> failwith ("unknown scope " ^ s)
       in
       text "discarded")

let () =
  register (get ["discard_all_scopes"] P.unit) (fun () () ->
    let* () = State.discard_all_scopes () in
    text "discarded")

(* Session groups *)

let group_value = V.eref ~scope:Common.default_group_scope ""

let () =
  register
    (get ["group"; "join"] P.(string "name"))
    (fun name () ->
       State.set_volatile_data_session_group ~scope:session_scope name;
       text "joined")

let () =
  register
    (get ["group"; "leave"] P.unit)
    (fun () () ->
       State.unset_volatile_data_session_group ~scope:session_scope ();
       text "left")

let () =
  register
    (get ["group"; "name"] P.unit)
    (fun () () ->
       text
         (Option.value ~default:"none"
            (State.get_volatile_data_session_group ~scope:session_scope ())))

let () =
  register
    (get ["group"; "set"] P.(string "v"))
    (fun v () -> V.set group_value v; text "set")

let () =
  register (get ["group"; "get"] P.unit) (fun () () -> text (V.get group_value))

let () =
  register (get ["groups"] P.unit) (fun () () ->
    text
      (String.concat ","
         (List.sort compare (State.Ext.get_session_group_list ()))))

let () = Services.start ()
