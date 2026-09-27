(* Global and default timeouts, scope hierarchies and session groups. The
   timeouts set by the services apply to the whole site. *)

open Eliom
open Lwt.Syntax
module P = Parameter
module V = Reference.Volatile

let text, get, register = Services.(text, get, register)
let session_scope = Common.default_session_scope
let cookie_scope = (session_scope :> Common.cookie_scope)

(* Global and default timeouts of the states of a kind (data, service or
   persistent), [None] without [t] *)

let () =
  register
    (get ["global_timeout"]
       P.(string "kind" ** opt (float "t") ** bool "recompute"))
    (fun (kind, (t, recompute_expdates)) () ->
       let set =
         match kind with
         | "data" -> State.set_global_volatile_data_state_timeout
         | "service" -> State.set_global_service_state_timeout
         | "persistent" -> State.set_global_persistent_data_state_timeout
         | k -> failwith ("unknown kind " ^ k)
       in
       set ~cookie_scope ~recompute_expdates ~override_configfile:true t;
       text "set")

let () =
  register
    (get ["default_timeout"] P.(string "kind" ** opt (float "t")))
    (fun (kind, t) () ->
       let set =
         match kind with
         | "data" -> State.set_default_global_volatile_data_state_timeout
         | "service" -> State.set_default_global_service_state_timeout
         | "persistent" ->
             State.set_default_global_persistent_data_state_timeout
         | k -> failwith ("unknown kind " ^ k)
       in
       set ~cookie_level:`Session ~override_configfile:true t;
       text "set")

(* A persistent session reference *)

let persistent_value =
  Reference.eref ~scope:session_scope
    ~persistent:("test_site_wide_value", [%json: string])
    ""

let () =
  register
    (get ["persistent"; "set"] P.(string "v"))
    (fun v () ->
       let* () = Reference.set persistent_value v in
       text "set")

let () =
  register
    (get ["persistent"; "get"] P.unit)
    (fun () () ->
       let* v = Reference.get persistent_value in
       text v)

(* A coservice of the session, whose URL is the answer of /coservice *)

let fallback = get ["fallback"] P.unit
let () = register fallback (fun () () -> text "fallback")

let () =
  register (get ["coservice"] P.unit) (fun () () ->
    let service = Service.create_attached_get ~fallback ~get_params:P.unit () in
    Registration.String.register ~scope:session_scope ~service (fun () () ->
      text "coservice");
    text (Eliom_uri.make_string_uri ~absolute_path:true ~service ()))

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

let other_persistent_value =
  Reference.eref ~scope:other_scope
    ~persistent:("test_site_wide_other_value", [%json: string])
    ""

let () =
  register
    (get ["other"; "persistent"; "set"] P.(string "v"))
    (fun v () ->
       let* () = Reference.set other_persistent_value v in
       text "set")

let () =
  register
    (get ["other"; "persistent"; "get"] P.unit)
    (fun () () ->
       let* v = Reference.get other_persistent_value in
       text v)

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
