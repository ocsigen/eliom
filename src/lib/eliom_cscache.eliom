open%server Eio.Std

(* Copyright Vincent Balat *)

[%%shared.start]

type ('a, 'b) t =
  (unit -> ('a, 'b Eio.Promise.or_exn) Hashtbl.t) Eliom_shared.Value.t

let%client create_ () =
  let c = Hashtbl.create 100 in
  fun () -> c

let%server create_ () =
  let c =
    Eliom_reference.Volatile.eref_from_fun ~scope:Eliom_common.request_scope
      (fun () -> Hashtbl.create 10)
  in
  fun () -> Eliom_reference.Volatile.get c

let%server create () =
  Eliom_shared.Value.create (create_ ()) [%client.unsafe create_ ()]

let do_cache_raw cache id data =
  let c = Eliom_shared.Value.local cache () in
  Hashtbl.replace c id data;
  (* Do not cache exceptions *)
  Eliom_lib.fork (fun () ->
    try ignore (Eio.Promise.await_exn data)
    with e -> Hashtbl.remove c id; raise e)

let do_cache cache id data =
  do_cache_raw cache id (Eio.Promise.create_resolved (Ok data))

let%server do_cache cache id v =
  do_cache cache id v;
  ignore [%client.unsafe (do_cache ~%cache ~%id ~%v : unit)]

let%server find cache get_data id =
  try Promise.await_exn (Hashtbl.find ((Eliom_shared.Value.local cache) ()) id)
  with Not_found ->
    let p =
      Eliom_lib.fork_promise (fun () ->
        let v = get_data id in
        ignore [%client.unsafe (do_cache ~%cache ~%id ~%v : unit)];
        v)
    in
    (* On server side, we put immediately in table the thread that is fetching
       the data. in order to avoid fetching it several times. *)
    do_cache_raw cache id p; Promise.await_exn p

let%client load cache get_data id =
  let p = Eliom_lib.fork_promise (fun () -> get_data id) in
  (* On client side, we put immediately in table the thread that is
     fetching the data.  Thus, [get_data_from_cache] returns
     immediately (in order to display a spinner). *)
  do_cache_raw cache id p; Eio.Promise.await_exn p

let%client find cache get_data id =
  match Hashtbl.find ((Eliom_shared.Value.local cache) ()) id with
  | p -> Eio.Promise.await_exn p
  | exception Not_found -> load cache get_data id

exception Not_ready

let local_find cache id =
  Eio.Promise.await_exn (Hashtbl.find ((Eliom_shared.Value.local cache) ()) id)

let find_if_ready cache id =
  let p = Hashtbl.find ((Eliom_shared.Value.local cache) ()) id in
  match Eio.Promise.peek p with
  | Some (Ok v) -> v
  | Some (Error e) -> raise e
  | _ -> raise Not_ready
