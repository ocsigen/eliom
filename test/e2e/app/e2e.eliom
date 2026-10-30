(* The application of the browser tests *)

[%%shared.start]

open Eliom.Content.Html.F

module App = Eliom.Registration.App (struct
    let application_name = "e2e"
    let global_data_path = None
  end)

[%%server.start]

let main =
  Eliom.Service.create ~path:(Eliom.Service.Path [])
    ~meth:(Eliom.Service.Get Eliom.Parameter.unit)
    ()

let greeting = "sent by the server"

[%%client.start]

(* An injection, read when the module is loaded *)
let greeting = ~%greeting

let fill id text =
  Js_of_ocaml.(
    Js.Opt.iter
      (Dom_html.document##getElementById (Js.string id))
      (fun e -> e##.textContent := Js.some (Js.string text)))

let () = Eliom.Client.onload (fun () -> fill "injection" greeting)

[%%server.start]

(* The page: a client value runs at load, and a button changes a D node *)
let page () =
  let counter = Eliom.Content.Html.D.span [txt "0"] in
  let increment =
    button
      ~a:
        [ a_id "increment"
        ; a_onclick
            [%client
              let n = ref 0 in
              fun _ ->
                incr n;
                Eliom.Content.Html.Manip.replaceChildren ~%counter
                  [txt (string_of_int !n)]] ]
      [txt "+"]
  in
  let _ : unit Eliom.Client_value.t =
    [%client Eliom.Client.onload (fun () -> fill "onload" "ran at load")]
  in
  Lwt.return
    (html
       (head (title (txt "e2e")) [])
       (body
          [ p ~a:[a_id "injection"] []
          ; p ~a:[a_id "onload"] []
          ; p [span ~a:[a_id "counter"] [counter]; increment] ]))

let () = App.register ~service:main (fun () () -> page ())
