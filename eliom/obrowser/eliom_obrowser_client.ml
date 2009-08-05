open Js

let register_closure : int -> (unit -> unit) -> unit =
  match js_external "caml_register_closure" 2 with
    | None -> failwith "unbound external"
    | Some f -> f

let get_closure_arg : unit -> Obj.t =
  match js_external "caml_get_closure_arg" 1 with
    | None -> failwith "unbound external"
    | Some f -> f

(* WAS:
external register_closure
  : int -> (unit -> unit) -> unit
  = "caml_register_closure"

external get_closure_arg
  : unit -> 'a
  = "caml_get_closure_arg"
*)

let register_closure id f =
  register_closure id (fun () ->
                         try
                           ignore (f (Obj.obj (get_closure_arg ()))) ;
                           Thread.exit ()
                         with _ ->
                           Thread.exit ())

