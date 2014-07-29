(* Copyright Vincent Balat *)
{shared{
open Eliom_lib
open Eliom_content.Html5
open Eliom_content.Html5.F
open Ereact
}}

{client{
 (* une référence pour simuler le cache côté client *)
 let cache = ref None
let save (a : (string Ereact.React.S.t * (string -> unit)))   = cache := Some a

let _ = Js.Unsafe.global##updateme <- (fun s -> match !cache with
    | None -> print_endline "no_cache"
    | Some (_,u) -> u (Js.to_string s))
}}

{shared{

 let box s =
   let (signal : string Ereact.React.S.t), (set_signal : (string -> unit) shared_value) = Ereact.React.S.create (s : string) in
   (* c'est le React d'Eliom_csreact *)
   let pdom = p [R.pcdata signal] in
   ignore {unit{ save (%signal, %set_signal)}};
   pdom

}}

let mainservice = Eliom_testsuite_base.My_appl.register_service
 ~path:["csreact1"]
 ~get_params:Eliom_parameter.unit
 (fun () () ->
    Lwt.return (
   html
     (head (title (pcdata "plouf")) [])
     (body [box "toto"])
    )
 )
