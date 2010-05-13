open Js
open JSOO

(*
let register_closure : int -> (unit -> unit) -> unit =
  match js_external "caml_register_closure" 2 with
    | None -> failwith "unbound external"
    | Some f -> f

let get_closure_arg : unit -> Obj.t =
  match js_external "caml_get_closure_arg" 1 with
    | None -> failwith "unbound external"
    | Some f -> f
*)

external register_closure
  : int -> (unit -> unit) -> unit
  = "caml_register_closure"

external get_closure_arg
  : unit -> 'a
  = "caml_get_closure_arg"

let register_closure id f =
  register_closure id (fun () ->
                         try
                           ignore (f (Obj.obj (get_closure_arg ()))) ;
                           Thread.exit ()
                         with _ ->
                           Thread.exit ())

let nodes : (int, Js.Node.t) Hashtbl.t = Hashtbl.create 200

let set_node_id node id =
  Hashtbl.replace nodes id node

let retrieve_node id =
  Hashtbl.find nodes id

type ref_tree = Ref_tree of int option * (int * ref_tree) list

(* Relinking DOM nodes *)
let _ =
  let rec recons root = fun (Ref_tree (id, subs)) ->
    begin match id with
      | Some id ->
	  set_node_id root id
      | None ->
	  ()
    end ;
    let children = Node.children root in
      List.iter
	(fun (n, sub) ->
	   recons (List.nth children n) sub
	)
	subs
  in recons
       (JSOO.eval "document.body")
       (Obj.obj (eval "eliom_id_tree" >>> as_block) : ref_tree)


(* == Global application data *)
let global_appl_data_table : ((float * int), unit) Hashtbl.t = Hashtbl.create 50

(* Loading global Eliom application data *)
let _ =
  let create_global_data_table ((reqnum, size), l) =
    List.fold_left
      (fun b v -> 
         let n = b-1 in
         Hashtbl.replace global_appl_data_table (reqnum, n) v;
         n
      )
      size
      l
  in
  create_global_data_table
    (Obj.obj (eval "eliom_global_data" >>> as_block) : 
       (float * int) * (unit list))

let unwrap (key : 'a Eliom_client_types.data_key) : 'a = 
  Obj.magic (Hashtbl.find global_appl_data_table 
               (Eliom_client_types.of_data_key_ key))


let unwrap_sp = unwrap
