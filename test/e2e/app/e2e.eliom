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

(* Tells the tests that the client program has handled the loaded page, and
   which one it is: js_of_ocaml or wasm_of_ocaml *)
let () =
  Eliom.Client.onload (fun () ->
    let backend =
      match Sys.backend_type with
      | Other name -> name
      | Native -> "native"
      | Bytecode -> "bytecode"
    in
    Js_of_ocaml.Js.Unsafe.set Js_of_ocaml.Js.Unsafe.global "e2eBackend"
      (Js_of_ocaml.Js.string backend);
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

(* The target of a button with formaction, distinct from the action of the
   forms *)
let elsewhere =
  Eliom.Service.create ~path:(Eliom.Service.Path ["elsewhere"])
    ~meth:(Eliom.Service.Get fields) ()

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
    echo (show_image fields));
  App.register ~service:elsewhere (fun fields () -> echo (show fields))

(* A text field, a named button that does not submit (decoy), the submit
   buttons A, B, C (an input) and D (whose label is a span), a named input of
   type button, and a button that sends the form to /elsewhere with GET *)
let main_fields (t, (b, n)) =
  [ Form.input ~a:[a_id "t"] ~input_type:`Text ~name:t ~value:"hello" Form.string
  ; Form.button
      ~a:[a_id "decoy"]
      ~button_type:`Button ~name:b ~value:"decoy" Form.string
      [txt "decoy"]
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
        ; a_formaction (Eliom.Content.Xml.uri_of_string "/elsewhere")
        ; a_formmethod `Get ]
      ~button_type:`Submit ~name:b ~value:"e" Form.string
      [txt "elsewhere"] ]

(* A 1x1 image, shown at 20x20 pixels *)
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

(* Client and server: server functions, a bus, React events and signals of the
   server, and state of the client process (tab) and of the session *)

let double =
  Eliom.Client.server_function [%json: int] (fun n -> Lwt.return (2 * n))

let fail =
  Eliom.Client.server_function [%json: unit] (fun () : unit Lwt.t ->
    failwith "on purpose")

(* A server function that returns a client value *)
let client_value =
  Eliom.Client.server_function [%json: unit] (fun () ->
    Lwt.return [%client ("made by a client value" : string)])

let bus = Eliom.Bus.create [%json: string]
let counter, set_counter = React.S.create 0
let counter_down = Eliom.Eliom_react.S.Down.of_react counter

let increment =
  Eliom.Client.server_function [%json: unit] (fun () ->
    set_counter (React.S.value counter + 1);
    Lwt.return_unit)

let up = Eliom.Eliom_react.Up.create (Eliom.Parameter.ocaml "v" [%json: string])
let last_up = ref ""

let () =
  Lwt_react.E.keep
    (React.E.map (fun v -> last_up := v) (Eliom.Eliom_react.Up.to_react up))

let tab_count =
  Eliom.Reference.Volatile.eref ~scope:Eliom.Common.default_process_scope 0

let session_count =
  Eliom.Reference.Volatile.eref ~scope:Eliom.Common.default_session_scope 0

let count =
  Eliom.Client.server_function [%json: unit] (fun () ->
    let incr r = Eliom.Reference.Volatile.(set r (get r + 1)) in
    incr tab_count;
    incr session_count;
    Lwt.return
      ( Eliom.Reference.Volatile.get tab_count
      , Eliom.Reference.Volatile.get session_count ))

(* An action of the server, called by the client *)
let server_action =
  Eliom.Service.create ~path:Eliom.Service.No_path
    ~meth:(Eliom.Service.Post (Eliom.Parameter.unit, Eliom.Parameter.unit))
    ()

let () =
  Eliom.Registration.Action.register ~options:`NoReload ~service:server_action
    (fun () () -> Lwt.return_unit)

let client_server = get ["client-server"]

let () =
  App.register ~service:client_server (fun () () ->
    let _ : unit Eliom.Client_value.t =
      [%client
        Eliom.Client.onload (fun () ->
          Lwt.async (fun () ->
            Lwt_stream.iter (fill "bus") (Eliom.Bus.stream ~%bus)))]
    in
    let action id label (f : (unit -> string Lwt.t) Eliom.Client_value.t) =
      button
        ~a:
          [ a_id id
          ; a_onclick
              [%client
                fun _ ->
                  Lwt.async (fun () ->
                    let open Lwt.Syntax in
                    let* text =
                      Lwt.catch ~%f (function
                        | Eliom.Client_value.Exception_on_server _ ->
                            Lwt.return "exception on the server"
                        | e -> Lwt.return (Printexc.to_string e))
                    in
                    fill (~%id ^ "-result") text;
                    Lwt.return_unit)] ]
        [txt label]
    in
    let result id = span ~a:[a_id (id ^ "-result")] [] in
    let actions =
      [ "double", [%client fun () -> Lwt.map string_of_int (~%double 21)]
      ; ( "fail"
        , [%client fun () -> Lwt.map (fun () -> "no exception") (~%fail ())] )
      ; "client-value", [%client fun () -> ~%client_value ()]
      ; ( "increment"
        , [%client fun () -> Lwt.map (fun () -> "sent") (~%increment ())] )
      ; "up", [%client fun () -> Lwt.map (fun () -> "sent") (~%up "sent up")]
      ; ( "bus-write"
        , [%client
            fun () ->
              Lwt.map (fun () -> "sent") (Eliom.Bus.write ~%bus "on the bus")] )
      ; ( "action"
        , [%client
            fun () ->
              Lwt.map
                (fun _ -> "done")
                (Eliom.Client.call_service ~service:~%server_action () ())] )
      ; ( "count"
        , [%client
            fun () ->
              Lwt.map
                (fun (tab, session) ->
                   Printf.sprintf "tab %d, session %d" tab session)
                (~%count ())] ) ]
    in
    page "client-server"
      (p ~a:[a_id "last-up"] [txt !last_up]
      :: p
           [ txt "counter: "
           ; span
               ~a:[a_id "counter-down"]
               [ Eliom.Content.Html.C.node
                   [%client
                     Eliom.Content.Html.R.txt
                       (React.S.map string_of_int ~%counter_down)] ] ]
      :: p [txt "bus: "; span ~a:[a_id "bus"] []]
      :: List.map
           (fun (id, f) -> p [action id id f; txt " "; result id])
           actions))

(* Raw HTML (Unsafe.data) gives the same nodes whether the server or the
   client builds the page *)

[%%shared.start]

(* Raw HTML in a paragraph, with several nodes, as a D node, in a table, in
   SVG, in a script, where it stays text, and with a script, which runs *)
let raw_html () =
  [ p [Unsafe.data "<b>bold</b>"]
  ; p [Unsafe.data "a <i>b</i> c"]
  ; p [Eliom.Content.Html.D.Unsafe.data "<b>d</b>"]
  ; tablex [tbody [Unsafe.data "<tr><td>cell</td></tr>"]]
  ; svg [Eliom.Content.Svg.F.Unsafe.data "<circle r=\"1\"></circle>"]
  ; script (Unsafe.data "/* <b>not an element</b> */")
  ; p
      [ Unsafe.data
          "<script>window.e2eRawScripts = (window.e2eRawScripts || 0) + 1</script>"
      ] ]

[%%server.start]

let () =
  App.register ~service:(get ["raw-html"]) (fun () () ->
    let open Eliom.Content.Html in
    let client_raw = D.div ~a:[a_id "client-raw"] []
    and client_top = D.p ~a:[a_id "client-top"] []
    and client_rows = D.tbody []
    and client_injected = D.div ~a:[a_id "client-injected"] []
    and client_d = D.p ~a:[a_id "client-d"] []
    and client_react = D.p ~a:[a_id "client-react"] [] in
    (* A node of the server, rebuilt by the client *)
    let injected = p [Unsafe.data "<b>injected</b>"] in
    let build =
      button
        ~a:
          [ a_id "build"
          ; a_onclick
              [%client
                fun _ ->
                  let open Eliom.Content.Html in
                  Manip.appendChildren ~%client_raw (raw_html ());
                  (* Raw HTML without a parent element *)
                  Manip.appendChild ~%client_top
                    (F.Unsafe.data "<b>top</b> <i>two</i>");
                  Manip.appendChild ~%client_rows
                    (F.Unsafe.data "<tr><td>row</td></tr>");
                  Manip.appendChild ~%client_injected ~%injected;
                  (* A D node of raw HTML is the element it gives, which Manip
                     removes *)
                  let d = D.Unsafe.data "<b>removed</b>" in
                  Manip.appendChild ~%client_d d;
                  Manip.removeSelf d;
                  (* Raw HTML as a reactive node *)
                  let s, set = React.S.create "<b>0</b>" in
                  Manip.appendChild ~%client_react
                    (R.node (React.S.map F.Unsafe.data s));
                  set "<b>1</b>"] ]
        [txt "build"]
    in
    page "raw-html"
      [ div ~a:[a_id "server-raw"] (raw_html ())
      ; build
      ; client_raw
      ; client_top
      ; tablex ~a:[a_id "client-table"] [client_rows]
      ; client_injected
      ; client_d
      ; client_react ])
