(* Copyright Vincent Balat *)

{shared{
open Eliom_lib
open Eliom_content.Html5
open Eliom_content.Html5.F
}}

{shared{
  type ('a, 'b) t = (unit -> ('a, 'b Lwt.t) Hashtbl.t) shared_value
}}

{client{
  let create_ () = let c = Hashtbl.create 100 in fun () -> c
}}

{server{
  let create_ () =
    let c = Eliom_reference.Volatile.eref_from_fun
        ~scope:Eliom_common.request_scope
        (fun () -> Hashtbl.create 10)
    in
    fun () -> Eliom_reference.Volatile.get c

let create () =
  Eliom_lib.create_shared_value (create_ ()) {{ create_ () }}
}}

{shared{
  let do_cache_raw cache id data =
    Hashtbl.remove ((Eliom_shared.Value.local cache) ()) id;
    Hashtbl.add ((Eliom_shared.Value.local cache) ()) id data

  let do_cache cache id data = do_cache_raw cache id (Lwt.return data)
}}

{server{

  let do_cache cache id v =
    do_cache cache id v;
    ignore {unit{ do_cache %cache %id %v }}

}}

{server{

  let find cache get_data id =
    try Hashtbl.find ((Eliom_shared.Value.local cache) ()) id
    with Not_found ->
      let th =
        lwt v = get_data id in
        ignore {unit{ do_cache %cache %id %v }};
        Lwt.return v
      in
      (* On server side,
         we put immediately in table the thread that is fetching the data.
         in order to avoid fetching it several times. *)
      do_cache_raw cache id th;
      th
}}

{client{

  let load cache get_data id =
    let th = get_data id in
    (* On client side,
       we put immediately in table the thread that is fetching the data.
       Thus, [get_data_from_cache] returns immediately
       (in order to display a spinner). *)
    do_cache_raw cache id th;
    th

let find cache get_data id =
    try Hashtbl.find ((Eliom_shared.Value.local cache) ()) id
    with Not_found -> load cache get_data id

}}

{shared{
  exception Not_ready

  let local_find cache id =
    Hashtbl.find ((Eliom_shared.Value.local cache) ()) id

  let find_if_ready cache id =
    let v = local_find cache id in
    match Lwt.state v with
    | Lwt.Return v -> v
    | _ -> raise Not_ready

}}
