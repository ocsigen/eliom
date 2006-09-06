open XHTML.M
open Ocsigen
open Ocsigen.Xhtml
open Lwt

let looong = 
  register_new_service 
    ~url:["looong"]
    ~get_params:unit
    (fun sp () () -> 
      Lwt_unix.sleep 10.0 >>= (fun _ ->
	failwith "lkljk";
	return
        (html
	  (head (title (pcdata "")) [])
	  (body [h1 [pcdata "Ok now, you can read the page."]]))))



(************************)

let no_post_param_service =
  register_new_service
   ~url:["post"]
     ~get_params:unit
      (fun _ () () -> return
        (html         
          (head (title (pcdata "")) [])
          (body [h1 [pcdata     
	    "Version of the page without POST parameters"]])))
    
let my_service_with_post_params = register_new_post_service
   ~fallback:no_post_param_service
   ~post_params:(string "name" ** string "file")
	 (fun _ () (name,file) ->
	 let to_display = 
	 let full = ("/tmp/"^file) in
	(* if (Unix.stat full).Unix.st_size > 1024 
	 then begin Sys.remove full; "File too large" end
	 else begin *)
	 let fd_in = open_in ("/tmp/"^file) in
	 try
	 let line = input_line fd_in in close_in fd_in; line (*end*)
	 with End_of_file -> "vide"
	 in
	 return
	    (html
		(head (title (pcdata name)) [])
		(body [h1 [pcdata to_display]])))


let form2 = register_new_service ["envoi"] unit
  (fun sp () () ->
    let f =
     (post_form ~a:[(XHTML.M.a_enctype "multipart/form-data")] my_service_with_post_params sp
     (*post_form my_service_with_post_params sp        *)
	(fun (chaine,file) ->
          [p [pcdata "Write a string: ";
              string_input chaine;
	      br ();
	      file_input file]]) ()) in  return
         (html
           (head (title (pcdata "form")) [])
           (body [f])))
