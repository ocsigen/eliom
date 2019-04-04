open Lwt

(* We use a hashtable associating resourceid to a weak set of
   (userid option, notif_ev) corresponding to each tab that want to
   get updates of this box.
   We keep a strong reference on these data in process state.
*)

module type S = sig
  type identity
  type key
  type server_notif
  type client_notif
  val init : unit -> unit Lwt.t
  val deinit : unit -> unit
  val listen : key -> unit
  val unlisten : key -> unit
  module Ext : sig
    val unlisten :
      ?sitedata:Eliom_common.sitedata ->
      ([< `Client_process ], [< `Data ]) Eliom_state.Ext.state
      -> key -> unit
  end
  val notify : ?notfor:[`Me | `Id of identity] -> key -> server_notif -> unit
  val client_ev : unit -> (key * client_notif) Eliom_react.Down.t
  val clean : unit -> unit
end

module type ARG = sig
  type identity
  type key
  type server_notif
  type client_notif
  val prepare : identity -> server_notif -> client_notif option Lwt.t
  val equal_key                  : key -> key -> bool
  val equal_identity             : identity -> identity -> bool
  val get_identity               : unit -> identity Lwt.t
  val max_resource               : int
  val max_identity_per_resource  : int
end

module Make (A : ARG) : S
  with type identity = A.identity
   and type key = A.key
   and type server_notif = A.server_notif
   and type client_notif = A.client_notif
= struct

  type key = A.key
  type identity = A.identity
  type server_notif = A.server_notif
  type client_notif = A.client_notif

  type notification_data = A.key * A.client_notif

  type notification_react =
    notification_data Eliom_react.Down.t
    * (?step: React.step -> notification_data -> unit)

  module Notif_hashtbl = Hashtbl.Make(struct
    type t    = A.key
    let equal = A.equal_key
    let hash  = Hashtbl.hash
  end)

  module Weak_tbl = Weak.Make (struct
    type t = (A.identity * notification_react) option
    let equal a b = match a, b with
      | None, None ->
        true
      | Some (a, b), Some (c, d) ->
        A.equal_identity a c && b == d
      | _ -> false
    let hash = Hashtbl.hash
  end)

  module I = struct

    let tbl = Notif_hashtbl.create A.max_resource

    let remove_if_empty wt key =
      if Weak_tbl.count wt = 0
      then Notif_hashtbl.remove tbl key

    let remove v key =
      try
        let wt = Notif_hashtbl.find tbl key in
        Weak_tbl.remove wt v;
        remove_if_empty wt key
      with Not_found -> ()

    let add v key =
      let wt =
        try
          Notif_hashtbl.find tbl key
        with Not_found ->
          let wt = Weak_tbl.create A.max_identity_per_resource in
          Notif_hashtbl.add tbl key wt;
          wt
      in
      if not (Weak_tbl.mem wt v)
      then Weak_tbl.add wt v

    let iter =
      let iter (f : Weak_tbl.data -> unit Lwt.t) wt : unit =
        Weak_tbl.iter
          (fun data -> Lwt.async (fun () -> f data))
          wt
      in
      fun f key ->
        try
          let wt = Notif_hashtbl.find tbl key in
          let g data = match data with
            | None ->
              Weak_tbl.remove wt data;
              remove_if_empty wt key;
              Lwt.return_unit
            | Some v ->
              f v
          in
          iter g wt;
        with Not_found -> ()

  end

  let identity_r
    : (A.identity * notification_react) option Eliom_reference.Volatile.eref =
    Eliom_reference.Volatile.eref
      ~scope:Eliom_common.default_process_scope
      None

  (* notif_e consists in a server side react event,
     its client side counterpart,
     and the server side function to trigger it. *)
  let notif_e : notification_react Eliom_reference.Volatile.eref =
    Eliom_reference.Volatile.eref_from_fun
      ~scope:Eliom_common.default_process_scope
      (fun () ->
         let e, send_e = React.E.create () in
         let client_ev = Eliom_react.Down.of_react
             (*VVV If we add throttling, some events may be lost
               even if buffer size is not 1 :O *)
             ~size: 100 (*VVV ? *)
             ~scope:Eliom_common.default_process_scope
             e
         in
         (client_ev, send_e))

  let set_identity identity =
    (* For each tab connected to the app,
       we keep a pointer to (identity, notif_ev) option in process state,
       because the table resourceid -> (identity, notif_ev) option
       is weak.
    *)
    let notif_e = Eliom_reference.Volatile.get notif_e in
    Eliom_reference.Volatile.set identity_r (Some (identity, notif_e))

  let set_current_identity () =
    A.get_identity () >>= fun identity ->
    set_identity identity;
    Lwt.return_unit

  let init : unit -> unit Lwt.t = fun () ->
    set_current_identity ()

  let deinit () = Eliom_reference.Volatile.set identity_r None

  let listen (key : A.key) =
    let identity = Eliom_reference.Volatile.get identity_r in
    I.add identity key

  let unlisten (id : A.key) =
    let identity = Eliom_reference.Volatile.get identity_r in
    I.remove identity id

  module Ext = struct
    let unlisten ?sitedata:_ state (key : A.key) =
      let uc = Eliom_reference.Volatile.Ext.get state identity_r in
      I.remove uc key
  end

  let notify ?notfor key content =
    let f = fun (identity, ((_, send_e) as notif)) ->
      let blocked = match notfor with
        | Some `Me ->
            (*TODO: fails outside of a request*)
            let notif_e = Eliom_reference.Volatile.get notif_e in
            notif == notif_e
        | Some (`Id id) -> identity = id
        | None -> false
      in
      if blocked
      then Lwt.return_unit
      else
        A.prepare identity content >>= fun content -> match content with
        | Some content -> send_e (key, content); Lwt.return_unit
        | None -> Lwt.return_unit
    in
    (* on all tabs listening on this resource *)
    I.iter f key

  let client_ev () =
    let (ev, _) = Eliom_reference.Volatile.get notif_e in
    ev

  let clean () =
    let f key weak_tbl =
      if Weak_tbl.count weak_tbl = 0
      then Notif_hashtbl.remove I.tbl key
    in
    Notif_hashtbl.iter f I.tbl

end

module type ARG_SIMPLE = sig
  type identity
  type key
  type notification
  val get_identity               : unit -> identity Lwt.t
end

module Make_Simple(A : ARG_SIMPLE) = Make
  (struct
    type identity      = A.identity
    type key           = A.key
    type server_notif  = A.notification
    type client_notif  = A.notification
    let prepare _ n    = Lwt.return_some n
    let equal_key      = (=)
    let equal_identity = (=)
    let get_identity   = A.get_identity
    let max_resource   = 1000
    let max_identity_per_resource = 10
  end)
