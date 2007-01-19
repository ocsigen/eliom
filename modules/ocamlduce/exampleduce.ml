open XHTML.M
open Ocsigen
open Ocsigenduce.Xhtml
open Lwt

let plop = 
  register_new_service 
    ~url:[""]
    ~get_params:unit
    (fun sp () () -> 
      return
        {{ <html>
             [<head> [<title> ""]
              <body> [<h1> "This page has been typed by OcamlDuce"]] }})


