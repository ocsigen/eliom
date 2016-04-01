(* Copyright Vincent Balat *)
{shared{
open Eliom_lib
open Eliom_content.Html5
open Eliom_content.Html5.F
}}


{shared{
  let display_msg0 = function | "" -> p [em [pcdata "empty message"]]
                              | msg -> p [pcdata msg]
}}
{shared{
  let display_msg id =
    lwt msg_signal = Eliom_testsuite7_db.get_msg_and_cache id in
    Lwt.return
      (R.node (Eliom_shared.React.S.map
                 {shared#{ display_msg0 }}
                 msg_signal))
 }}

{shared{

  let display_msg_list () =
    lwt msg_ids_signal = Eliom_testsuite7_db.get_msg_ids () in
    lwt content =
      Eliom_shared.ReactiveData.RList.Lwt.map_p
        {shared#{ display_msg }}
        msg_ids_signal
    in
    Lwt.return (R.div content)

}}
let mainservice = Eliom_testsuite_base.My_appl.register_service
 ~path:["csreact2"]
 ~get_params:Eliom_parameter.unit
 (fun () () ->
    lwt c = display_msg_list () in
    Lwt.return (
      html
        (head (title (pcdata "plouf")) [])
        (body [c])
    )
 )


{client{
    (* fake messages received on client side without their content: *)
    let _ =
      let rec f i =
        lwt () = Lwt_js.sleep (float_of_int (Random.int 4 + 2)) in
        print_endline ("You have a new message: "^(string_of_int i));
        Eliom_testsuite7_db.add_msg_id i;
        f (i+1)
    in f 5

let test, testh = ReactiveData.RList.create
    ([pcdata "a"; pcdata "b"; ] : Html5_types.div_content_fun elt list)
 }}
{shared{
  let test0 _ = Lwt.return (D.div [D.h1 [pcdata "ze"]])
}}
{shared{
  let testfun () =
    lwt content =
      Eliom_shared.ReactiveData.RList.Lwt.map_p
        {shared#{ test0 }}
        (fst (Eliom_shared.ReactiveData.RList.create [1; 2]))
    in
    Lwt.return (D.div [p [pcdata "les boîtes :"]; R.div content])

}}
{client{
  let testc () =
    lwt content =
      Eliom_shared.ReactiveData.RList.Lwt.map_p
        test0
        (fst (Eliom_shared.ReactiveData.RList.create [1; 2; 3]))
    in
    Lwt.return (D.div [p [pcdata "les boîtes :"]; R.div content])

}}
{client{
let _ = Lwt.async (fun () ->
    lwt () = Lwt_js.sleep 1. in
    Dom.appendChild
      (Dom_html.document##body)
      (To_dom.of_element (R.div test));
    let s = fst (React.S.create (pcdata "single thon **")) in
    Dom.appendChild
      (Dom_html.document##body)
      (To_dom.of_element
         (R.div (ReactiveData.RList.singleton_s s)));
    lwt t = testfun () in
    Dom.appendChild
      (Dom_html.document##body)
      (To_dom.of_element t);
    Lwt.return ())

    let _ =
      let rec f i =
        lwt () = Lwt_js.sleep (float_of_int (Random.int 4 + 2)) in
        ReactiveData.RList.cons (pcdata (string_of_int i)) testh;
        f (i+1)
    in f 0


}}
