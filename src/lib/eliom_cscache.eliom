(* Copyright Vincent Balat *)

[%%shared.start]

open Eliom_lib
open Eliom_content.Html
open Eliom_content.Html.F

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

let partition : ('c -> [`Left of 'a | `Right of 'b]) -> 'c list -> 'a list * 'b list
  = fun p lst ->
      let rec loop left right = function
        | [] -> left, right
        | h :: t -> match p h with
            | `Left l -> loop (l::left) right t
            | `Right r -> loop left (r::right) t
      in
      loop [] [] lst

module L = struct
  let find cache get_data ids =
    let lookup id =
      try `Left (id, Hashtbl.find (Eliom_shared.Value.local cache ()) id)
      with Not_found -> `Right id
    in
    let cached, not_cached = partition lookup ids in
    let tbl = Hashtbl.create 10 in
    let main_thread =
      let%lwt () = cached |> Lwt_list.iter_s @@ fun (id, v) -> (*iter_p?*)
        let%lwt v = v in Lwt.return @@ Hashtbl.add tbl id v in
      let%lwt vs = get_data not_cached in
      List.iter (fun (id,v) -> Hashtbl.add tbl id v) vs;
      ignore [%client (List.iter (fun (id,v) -> do_cache ~%cache id v) ~%vs : unit)];
      Lwt.return tbl
    in
    let thread id =
      let%lwt tbl = main_thread in
      Lwt.return @@ Hashtbl.find tbl id
    in
    List.iter (fun id -> do_cache_raw cache id @@ thread id) ids;
    Lwt.return @@ List.map (Hashtbl.find tbl) ids
end

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

exception Not_ready

let local_find cache id =
  Hashtbl.find ((Eliom_shared.Value.local cache) ()) id

let find_if_ready cache id =
  let v = local_find cache id in
  match Lwt.state v with
  | Lwt.Return v -> v
  | _ -> raise Not_ready
