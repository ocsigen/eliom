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
