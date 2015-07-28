(* Copyright Vincent Balat *)
{shared{
open Eliom_lib
open Eliom_content.Html5
open Eliom_content.Html5.F
open Eliom_csreact
}}

{client{
  module MsgCache = Hashtbl.Make (struct
      type t = int
      let equal = (=)
      let hash = Hashtbl.hash
    end)
  let msgcache = MsgCache.create 100
  let msg_ids_cache = ref None
  let init_msgs_ids_cache s = msg_ids_cache := Some s
  let get_msg_ids () =
    match !msg_ids_cache with
    | None -> failwith "client cache not initialized"
    | Some (s, _) -> Lwt.return s
  let cache_msg i s = MsgCache.add msgcache i s
  let add_msg_id i =
    match !msg_ids_cache with
    | None -> failwith "client cache not initialized"
    | Some (_, h) -> ReactiveData.RList.cons i h
}}
{server{
  let get_msg_ids () =
    lwt () = Lwt_unix.sleep 1. in
    let v = [1;2;3;4] in
    let s = SharedReactiveData.RList.make v in
    let _ = {unit{ init_msgs_ids_cache %s }} in
    Lwt.return (fst s)
  let get_msg i =
    lwt () = Lwt_unix.sleep 1. in
    Lwt.return (string_of_int i)
  let get_msg_and_cache i =
    lwt v = get_msg i in
    (* Warning: if you want to make possible to generate html from server
       side even if the data is already present on client side, you must give
       the ~default parameter to SharedReact.S.create and
       SharedReactiveData.RList.make above. *)
    let signal = fst (SharedReact.S.create v) in
    let _ = {unit{ cache_msg %i %signal }} in
    Lwt.return signal
  let get_msg_rpc = server_function Json.t<int> get_msg
}}
{client{
  let get_msg_and_cache i =
    try Lwt.return (MsgCache.find msgcache i)
    with Not_found ->
      lwt msg = %get_msg_rpc i in
      let (s, set) = React.S.create msg in
      cache_msg i s;
      Lwt.return s
}}
