open%shared Eliom_content.Html
open%client Js_of_ocaml
open%client Js_of_ocaml_lwt

let%shared random_color () =
  (* Generate a random well saturated color. *)
  let c = [|Random.int 256; 0x80; 0xFF|] in
  Array.shuffle ~rand:Random.int c;
  Printf.sprintf "rgb(%d, %d, %d)" c.(0) c.(1) c.(2)

let%shared colored_element contents =
  let span = D.(span ~a:[a_style ("color: " ^ random_color ())] contents) in
  ignore
    [%client
      (let span = To_dom.of_span ~%span in
       Lwt.async (fun () ->
         Lwt_js_events.clicks span (fun _ _ ->
           span##.style##.color := Js.string (random_color ());
           Lwt.return_unit))
       : unit)];
  span

let%shared main_page () =
  F.(div [h1 [txt "Welcome to "; colored_element [txt "Eliom!"]]])
