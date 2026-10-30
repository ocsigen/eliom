(* The application of the browser tests *)

[%%shared.start]

open Eliom.Content.Html.F

module App = Eliom.Registration.App (struct
    let application_name = "e2e"
    let global_data_path = None
  end)

[%%server.start]

let get path =
  Eliom.Service.create ~path:(Eliom.Service.Path path)
    ~meth:(Eliom.Service.Get Eliom.Parameter.unit)
    ()

let main = get []
let two = get ["two"]
let server_page = get ["server"]
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

(* Tells the tests that the client program has handled the loaded page *)
let () =
  Eliom.Client.onload (fun () ->
    Js_of_ocaml.Js.Unsafe.set Js_of_ocaml.Js.Unsafe.global "e2eReady"
      Js_of_ocaml.Js._true)

[%%server.start]

let links () =
  p
    [ a ~a:[a_id "to-main"] ~service:main [txt "main"] ()
    ; a ~a:[a_id "to-two"] ~service:two [txt "two"] ()
    ; a ~a:[a_id "to-server"] ~service:server_page [txt "server"] () ]

let page name content =
  Lwt.return
    (html
       (head (title (txt name)) [])
       (body (h1 ~a:[a_id "title"] [txt name] :: links () :: content)))

(* The main page: a client value runs at load, and a button changes a D
   node *)
let main_page () =
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
  page "main"
    [ p ~a:[a_id "injection"] []
    ; p ~a:[a_id "onload"] []
    ; p [span ~a:[a_id "counter"] [counter]; increment] ]

let () = App.register ~service:main (fun () () -> main_page ())

(* A second page of the application, with its own code run at load *)
let () =
  App.register ~service:two (fun () () ->
    let _ : unit Eliom.Client_value.t =
      [%client Eliom.Client.onload (fun () -> fill "two-onload" "ran at load")]
    in
    page "two" [p ~a:[a_id "two-onload"] []])

(* A page outside the application *)
let () =
  Eliom.Registration.Html.register ~service:server_page (fun () () ->
    page "server" [])

(* Forms, submitted through XHR or natively (~xhr:false). Each one is sent
   to an echo page that shows what the server received. *)

module P = Eliom.Parameter
module Form = Eliom.Content.Html.F.Form

let fields = P.(string "t" ** set string "b" ** set string "n")
let image_fields = P.(string "t" ** opt (coordinates "img"))

let echo_get =
  Eliom.Service.create ~path:(Eliom.Service.Path ["echo"])
    ~meth:(Eliom.Service.Get fields) ()

let echo_image_get =
  Eliom.Service.create ~path:(Eliom.Service.Path ["echo_image"])
    ~meth:(Eliom.Service.Get image_fields) ()

let echo_post =
  Eliom.Service.create ~path:(Eliom.Service.Path ["echo_post"])
    ~meth:(Eliom.Service.Post (P.unit, fields))
    ()

let echo_image_post =
  Eliom.Service.create ~path:(Eliom.Service.Path ["echo_image_post"])
    ~meth:(Eliom.Service.Post (P.unit, image_fields))
    ()

let echo received = page "echo" [pre ~a:[a_id "received"] [txt received]]

let show (t, (buttons, names)) =
  Printf.sprintf "t=%s b=[%s] n=[%s]" t
    (String.concat "," buttons)
    (String.concat "," names)

let show_image (t, img) =
  Printf.sprintf "t=%s img=%s" t
    (match img with
    | None -> "none"
    | Some {P.abscissa; ordinate} -> Printf.sprintf "%d,%d" abscissa ordinate)

let () =
  App.register ~service:echo_get (fun fields () -> echo (show fields));
  App.register ~service:echo_image_get (fun fields () ->
    echo (show_image fields));
  App.register ~service:echo_post (fun () fields -> echo (show fields));
  App.register ~service:echo_image_post (fun () fields ->
    echo (show_image fields))

(* A text field, a decoy button, the submit buttons A, B, C (an input) and D
   (whose label is a span), a named input of type button, and a button that
   sends the form to /echo with GET *)
let main_fields (t, (b, n)) =
  [ Form.input ~a:[a_id "t"] ~input_type:`Text ~name:t ~value:"hello" Form.string
  ; Form.button_no_value ~a:[a_id "decoy"] ~button_type:`Button [txt "decoy"]
  ; Form.button
      ~a:[a_id "a"]
      ~button_type:`Submit ~name:b ~value:"a" Form.string
      [txt "A"]
  ; Form.button
      ~a:[a_id "b"]
      ~button_type:`Submit ~name:b ~value:"b" Form.string
      [txt "B"]
  ; Form.input ~a:[a_id "c"] ~input_type:`Submit ~name:b ~value:"c" Form.string
  ; Form.button
      ~a:[a_id "d"]
      ~button_type:`Submit ~name:b ~value:"d" Form.string
      [span ~a:[a_id "d-label"] [txt "D"]]
  ; Form.input ~a:[a_id "n"] ~input_type:`Button ~name:n ~value:"x" Form.string
  ; Form.button
      ~a:
        [ a_id "elsewhere"
        ; a_formaction (Eliom.Content.Xml.uri_of_string "/echo")
        ; a_formmethod `Get ]
      ~button_type:`Submit ~name:b ~value:"e" Form.string
      [txt "elsewhere"] ]

(* A 1x1 image, shown on 20x20 pixels *)
let pixel =
  Eliom.Content.Xml.uri_of_string
    "data:image/gif;base64,R0lGODlhAQABAIAAAAAAAP///yH5BAEAAAAALAAAAAABAAEAAAICRAEAOw=="

let image_form_fields (t, img) =
  [ Form.input
      ~a:[a_id "it"]
      ~input_type:`Text ~name:t ~value:"image" Form.string
  ; Form.image_input
      ~a:[a_id "img"; a_style "width: 20px; height: 20px"]
      ~name:img ~src:pixel () ]

let forms_page ~xhr meth =
  match meth with
  | `Get ->
      page "forms"
        [ Form.get_form ~xhr ~a:[a_id "main-form"] ~service:echo_get main_fields
        ; Form.get_form ~xhr
            ~a:[a_id "image-form"]
            ~service:echo_image_get image_form_fields ]
  | `Post ->
      page "forms"
        [ Form.post_form ~xhr
            ~a:[a_id "main-form"]
            ~service:echo_post main_fields ()
        ; Form.post_form ~xhr
            ~a:[a_id "image-form"]
            ~service:echo_image_post image_form_fields () ]

let () =
  List.iter
    (fun (path, xhr, meth) ->
       App.register ~service:(get path) (fun () () -> forms_page ~xhr meth))
    [ ["forms"; "get"], true, `Get
    ; ["forms"; "post"], true, `Post
    ; ["native"; "get"], false, `Get
    ; ["native"; "post"], false, `Post ]
