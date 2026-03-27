open Lwt.Syntax

(* Copyright Vincent Balat *)

[%%shared.start]

type ('a, 'b) t = (unit -> ('a, 'b Lwt.t) Hashtbl.t) Eliom_shared.Value.t

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
  ignore (Lwt.catch (fun _ -> data) (fun e -> Hashtbl.remove c id; Lwt.fail e))

let do_cache cache id data = do_cache_raw cache id (Lwt.return data)

let%server do_cache cache id v =
  do_cache cache id v;
  ignore [%client.unsafe (do_cache ~%cache ~%id ~%v : unit)]

let%server find cache get_data id =
  try Hashtbl.find ((Eliom_shared.Value.local cache) ()) id
  with Not_found ->
    let th =
      let* v = get_data id in
      ignore [%client.unsafe (do_cache ~%cache ~%id ~%v : unit)];
      Lwt.return v
    in
    (* On server side, we put immediately in table the thread that is
       fetching the data.  in order to avoid fetching it several
       times. *)
    do_cache_raw cache id th; th

let%client load cache get_data id =
  let th = get_data id in
  (* On client side, we put immediately in table the thread that is
     fetching the data.  Thus, [get_data_from_cache] returns
     immediately (in order to display a spinner). *)
  do_cache_raw cache id th; th

let%client find cache get_data id =
  try Hashtbl.find ((Eliom_shared.Value.local cache) ()) id
  with Not_found -> load cache get_data id

exception Not_ready

let local_find cache id = Hashtbl.find ((Eliom_shared.Value.local cache) ()) id

let find_if_ready cache id =
  let v = local_find cache id in
  match Lwt.state v with Lwt.Return v -> v | _ -> raise Not_ready
