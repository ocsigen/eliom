(*zap*
   This is the Eliom documentation.
   You can find a more readable version of comments on http://www.ocsigen.org
*zap*)
(*wiki*
%<div class='leftcol'|%<leftcoldoc version="dev">%>%
      %<div class="colprincipale"|
        ==4. 
        
        ===@@id="p4basics"@@
        
        %<div class="onecol"|
          
*wiki*)
open XHTML.M
open Eliom_parameters
open Eliom_predefmod.Xhtml
open Eliom_services

let obrowser =
  Eliom_predefmod.Xhtml.register_new_service
    ~path:["obrowser"]
    ~get_params:unit
    (fun sp () () ->
      Lwt.return
        (html
           (head
              (title (pcdata "Eliom + O'Browser"))
              [
                js_script 
                  ~uri:(make_uri ~service:(static_dir sp) ~sp ["vm.js"]) ();
                js_script 
                  ~uri:(make_uri ~service:(static_dir sp) ~sp ["eliom_obrowser.js"]) ();
                script ~contenttype:"text/javascript"
                  (cdata_script
      "window.onload = function () {
        main_vm = exec_caml (\"tutoeliom_ocsigen2_client.uue\") ;
      }")])
           (body [h1 ~a:[a_onclick 
                           ((fun.client (() : unit) -> Js.alert "clicked!") ())]
                    [pcdata "I am a clickable title"]])))
(*wiki*
        %> <<|onecol>>
      %> <<|colprincipale>>
*wiki*)
