open Eliom_content

let with_js_file path =
  let uri =
    Html5.F.make_uri
      (Eliom_service.static_dir () )
      path
  in
  let script =
    Html5.F.js_script ~uri ()
  in
  ignore
    Dom_html.document##head##appendChild
      (Html5.To_dom.of_node script)

let with_css_file path =
  let uri =
    Html5.F.make_uri
      (Eliom_service.static_dir () )
      path
  in
  let link =
    Html5.F.css_link ~uri ()
  in
  ignore
    Dom_html.document##head##appendChild
      (Html5.To_dom.of_node link)
