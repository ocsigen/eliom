open Lwt

module type S = sig
  type identity
  type key
  type notification
  val equal_key                  : key -> key -> bool
  val equal_identity             : identity -> identity -> bool
  val get_identity               : unit -> identity Lwt.t
  val max_resource               : int
  val max_identity_per_resource  : int
end

module Make (A : S) = struct

  type notification_data = A.key * A.notification

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
      
    let lock = Lwt_mutex.create ()

    let async_locked f = Lwt.async (fun () ->
      Lwt_mutex.lock lock >>= fun () ->
      f ();
      Lwt.return (Lwt_mutex.unlock lock)
    )

    let remove_if_empty wt key = async_locked (fun () ->
      if Weak_tbl.count wt = 0
      then Notif_hashtbl.remove tbl key
    )

    let remove v key = async_locked (fun () ->
      let () =
	try
	  let wt = Notif_hashtbl.find tbl key in
          Weak_tbl.remove wt v;
	  remove_if_empty wt key
	with Not_found -> ()
      in
      Lwt.return ()
    )

    let add v key = async_locked (fun () ->
      let wt =
	try
	  Notif_hashtbl.find tbl key
        with Not_found ->
	  let wt = Weak_tbl.create A.max_identity_per_resource in
          Notif_hashtbl.add tbl key wt;
          wt
      in
      if not (Weak_tbl.mem wt v)
      then Weak_tbl.add wt v;
      Lwt.return ()
    )

    let iter =
      let iter (f : Weak_tbl.data -> unit Lwt.t) wt : unit =
	Weak_tbl.iter
	  (fun data -> Lwt.async (fun () -> f data))
	  wt
      in
      fun f key -> async_locked (fun () ->
	let () =
	  try
	    let wt = Notif_hashtbl.find tbl key in
	    let g data = match data with
              | None ->
		Weak_tbl.remove wt data;
		remove_if_empty wt key;
		Lwt.return ()
              | Some v ->
		f v;
		Lwt.return ()
	    in
            iter g wt;
	  with Not_found -> ()
	in
	Lwt.return ()
      )
  end

  let identity_r : (A.identity * notification_react) option Eliom_reference.eref =
    Eliom_reference.eref
      ~scope:Eliom_common.default_process_scope
      None

  (* notif_e consists in a server side react event,
     its client side counterpart,
     and the server side function to trigger it. *)
  let notif_e : notification_react option Eliom_reference.eref =
    Eliom_reference.eref
      ~scope:Eliom_common.default_process_scope
      None

  let of_option = function
    | Some x -> x
    | None -> assert false

  let set_identity identity =
    (* For each tab connected to the app,
       we keep a pointer to (identity, notif_ev) option in process state,
       because the table resourceid -> (identity, notif_ev) option
       is weak.
    *)
    Eliom_reference.get notif_e >>= fun notif_o ->
    Eliom_reference.set identity_r (Some (identity, of_option notif_o))

  let set_notif_e () =
    let notif =
      let e, send_e = React.E.create () in
      let client_ev = Eliom_react.Down.of_react
      (*VVV If we add throttling, some events may be lost
            even if buffer size is not 1 :O *)
	~size: 100 (*VVV ? *)
	~scope:Eliom_common.default_process_scope
	e
      in
      (client_ev, send_e)
    in
    Eliom_reference.set notif_e (Some notif)

  let set_current_identity () =
    A.get_identity () >>= fun identity ->
    set_identity identity

  let listen (key : A.key) = Lwt.async (fun () ->
    set_notif_e () >>= fun () ->
    set_current_identity () >>= fun () ->
    Eliom_reference.get identity_r >>= fun identity ->
    I.add identity key;
    Lwt.return ()
  )

  let unlisten (id : A.key) = Lwt.async (fun () ->
    Eliom_reference.get identity_r >>= fun identity ->
    I.remove identity id;
    Lwt.return ()
  )

  let notify ?(notforme = false) key content_gen =
    let f = fun (identity, ((_, send_e) as notif)) ->
      Eliom_reference.get notif_e >>= fun notif_o ->
      if notforme && notif == (of_option notif_o) then
	Lwt.return ()
      else
        content_gen identity >>= fun content -> match content with
        | Some content -> send_e (key, content); Lwt.return ()
        | None -> Lwt.return ()
    in
    (* on all tabs registered on this data *)
    I.iter f key

  let client_ev () =
    Eliom_reference.get notif_e >>= fun notif_o ->
    Lwt.return (of_option notif_o) >>= fun (ev, _) ->
    Lwt.return ev

  let clean () =
    let f key weak_tbl = I.async_locked (fun () -> 
      if Weak_tbl.count weak_tbl = 0
      then Notif_hashtbl.remove I.tbl key
    ) in
    Lwt.return @@ Notif_hashtbl.iter f I.tbl

end
