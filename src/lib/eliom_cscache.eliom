(* Copyright Vincent Balat *)

[%%shared.start]

type ('a, 'b) t =
  (unit -> ('a, 'b Lwt.t) Hashtbl.t) Eliom_shared.Value.t

let%client create_ () = let c = Hashtbl.create 100 in fun () -> c

let%server create_ () =
  let c = Eliom_reference.Volatile.eref_from_fun
      ~scope:Eliom_common.request_scope
      (fun () -> Hashtbl.create 10)
  in
  fun () -> Eliom_reference.Volatile.get c

let%server create () =
  Eliom_shared.Value.create (create_ ())  [%client  create_ () ]

let do_cache_raw cache id data =
  let c = Eliom_shared.Value.local cache () in
  Hashtbl.replace c id data;
  (* Do not cache exceptions *)
  ignore (Lwt.catch (fun _ -> data) (fun e -> Hashtbl.remove c id; Lwt.fail e))

let do_cache cache id data = do_cache_raw cache id (Lwt.return data)

let%server do_cache cache id v =
  do_cache cache id v;
  ignore [%client ( do_cache ~%cache ~%id ~%v : unit)]

let%server find cache get_data id =
  try Hashtbl.find ((Eliom_shared.Value.local cache) ()) id
  with Not_found ->
    let th =
      let%lwt v = get_data id in
      ignore [%client ( do_cache ~%cache ~%id ~%v : unit)];
      Lwt.return v
    in
    (* On server side, we put immediately in table the thread that is
       fetching the data.  in order to avoid fetching it several
       times. *)
    do_cache_raw cache id th;
    th

let%server cache_list cache get_data ids =
  let main_thread, wakeup = Lwt.task () in
  let tbl = Hashtbl.create 10 in
  let thread id =
    let%lwt () = main_thread in
    Lwt.return @@ Hashtbl.find tbl id
  in
  let enqueue id = if Hashtbl.mem (Eliom_shared.Value.local cache ()) id
    then false
    else let () = do_cache_raw cache id (thread id) in true
  in
  try%lwt
    let not_cached = List.filter enqueue ids in
    let%lwt data = get_data not_cached in
    List.iter (fun (id,v) -> Hashtbl.add tbl id v) data;
    ignore [%client (List.iter (fun (id,v) -> do_cache ~%cache id v) ~%data : unit)];
    Lwt.return @@ Lwt.wakeup wakeup ()
  with e -> Lwt.wakeup_exn wakeup e; Lwt.fail e

let%client load cache get_data id =
  let th = get_data id in
  (* On client side, we put immediately in table the thread that is
     fetching the data.  Thus, [get_data_from_cache] returns
     immediately (in order to display a spinner). *)
  do_cache_raw cache id th;
  th

let%client find cache get_data id =
  try Hashtbl.find ((Eliom_shared.Value.local cache) ()) id
  with Not_found -> load cache get_data id

let%server find_list cache get_data ids =
  let%lwt () = cache_list cache get_data ids in
  Lwt_list.map_s (find cache (fun _ -> raise Not_found)) ids

exception Not_ready

let local_find cache id =
  Hashtbl.find ((Eliom_shared.Value.local cache) ()) id

let find_if_ready cache id =
  let v = local_find cache id in
  match Lwt.state v with
  | Lwt.Return v -> v
  | _ -> raise Not_ready
