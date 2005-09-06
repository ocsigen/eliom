(* Copyright Vincent Balat et Denis Berthod 2005 *)
(* Do not redistribute *)

open Lwt
open Messages
open Omlet
open Http_frame
open Http_com
open Sender_helpers

module Content = 
  struct
    type t = string
    let content_of_string c = c
    let string_of_content s = s
  end

module Xhtml_content =
  struct
    type t = Xhtmlpp.xhtml
    let string_of_content c = Xhtmlpp.xh_print c
    (*il n'y a pas encore de parser pour ce type*)
    let content_of_string s =assert false
  end

module Http_frame = FHttp_frame (Content)


module Http_receiver = FHttp_receiver (Content)


let listening_port = int_of_string Sys.argv.(1)

(*let _ = Unix.set_nonblock Unix.stdin
let _ = Unix.set_nonblock Unix.stdout
let _ = Unix.set_nonblock Unix.stderr*)

let new_socket () = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0
let local_addr num = Unix.ADDR_INET (Unix.inet_addr_any, num)


exception Omlet_Malformed_Url

(* *)
let get_frame_infos http_frame =
(*  let remove_slash = function
      ""::l -> l
    | l -> l
  in *)
  try 
    let url = Http_header.get_url http_frame.Http_frame.header in
      warning url;
    let url2 = 
      Neturl.parse_url 
	~base_syntax:(Hashtbl.find Neturl.common_url_syntax "http")
	url
    in
    let params_string = try
      Neturl.url_query ~encoded:true url2
    with Not_found -> ""
    in
    let get_params = Netencoding.Url.dest_url_encoded_parameters params_string 
    in
    let post_params =
      match http_frame.Http_frame.content with
	  None -> []
	| Some s -> Netencoding.Url.dest_url_encoded_parameters s
    in
    let internal_state,post_params2 = 
      try (Some (int_of_string (List.assoc state_param_name post_params)),
	   List.remove_assoc state_param_name post_params)
      with Not_found -> (None, post_params)
    in
    let internal_state2,get_params2 = 
      try 
	match internal_state with
	    None ->
	      (Some (int_of_string (List.assoc state_param_name get_params)),
	       List.remove_assoc state_param_name get_params)
	  | _ -> (internal_state, get_params)
      with Not_found -> (internal_state, get_params)
    in
    let useragent = (Http_header.get_headers_value
		       http_frame.Http_frame.header "user-agent")
    in
      ((*remove_slash*) (Neturl.url_path url2), (* the url path *)
       internal_state2,
       get_params2,
       post_params2,
       useragent)
  with _ -> raise Omlet_Malformed_Url

    
(* C'est de la bidouille. Cette fonction est à refaire proprement
   (pour entêtes http)
*)
(*
let really_write code keep_alive ?cookie out_ch buffer pos len =
  let rec really_write_aux out_ch buffer pos len =
    Lwt_unix.write out_ch buffer pos len >>=
      (
	fun len' ->
          if len = len'
          then return ()
          else really_write_aux out_ch buffer (pos+len') (len-len')
      )
  in 
  let header =
bip 700;
("HTTP/1.1 "^code^"
Date: Tue, 31 May 2006 16:34:59 GMT
Server: ploplop (Unix)  (Gentoo/Linux) omlet
Last-Modified: Wed, 20 Oct 1900 12:51:24 GMT
Accept-Ranges: bytes
Cache-Control: no-cache
"^(match cookie with None -> "" | Some c -> ("Set-Cookie: session="^c^"\n"))
^"Content-Length: "^(string_of_int len)^"
Connection: "^(if keep_alive then "Keep-Alive" else "close")^"
Content-Type: text/html\n\n") in
print_endline "J'envoie :";
print_endline header;
    Lwt_unix.write out_ch header 0 (String.length header) >>= (fun _ ->
print_endline buffer;
 really_write_aux out_ch buffer pos len)
*)

let service http_frame in_ch sockaddr xhtml_sender file_sender () =
  try 
    let cookie = 
      try 
	Some
	  (List.assoc "session"
	     (Netencoding.Url.dest_url_encoded_parameters 
		(Http_header.get_headers_value 
		   http_frame.Http_frame.header "cookie")))
	  (* en fait dest_url_encoded_parameters n'est pas exactement
	     ce que je veux si les params sont séparés par des ; au lieu
	     de &... à revoir !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
      with _ -> None
    in
    let cookie2,page = 
      get_page (get_frame_infos http_frame) sockaddr cookie in
    let keep_alive = false 
      (* Je préfère pour l'instant ne jamais faire de keep-alive pour
	 éviter d'avoir un nombre de threads qui croit sans arrêt *) in
    (*let page_string = (Xhtmlpp.xh_print page) in
      really_write "200 OK" 
	keep_alive 
	?cookie:(if cookie2 <> cookie then cookie2 else None)
	in_ch page_string 0 
	(String.length page_string) >>= (fun _ ->
					   return keep_alive)*)
    (*debug*)
    print_endline "avant send_page";
    send_page ~keep_alive:keep_alive 
    ?cookie:(if cookie2 <> cookie then cookie2 else None)
    page xhtml_sender >>=
      (fun _ ->
        (*debug*)
        print_endline "après send page";
        return keep_alive)
  with Not_found -> 
   (*really_write "404 Not Found" false in_ch "error 404 \n" 0 11 *)
   send_error ~error_num:404 xhtml_sender
   (*send_file ~code:404 "../pages/error.html" file_sender*)
   (*send_file ~code:200 "../pages/Volta.GIF" file_sender*)
   >>= (fun _ ->
     return true (* idem *))
    | Omlet_Malformed_Url ->
    (*really_write "404 Not Found ??" false in_ch "error ??? (Malformed URL) \n"
    * 0 11 *)
    (*send_error ~error_num:400 xhtml_sender*)
    send_file ~code:200 "../pages/Volta.GIF" file_sender
    >>= (fun _ ->
    return true (* idem *))
                                              



(** Thread waiting for events on a the listening port *)
let listen () =

  let listen_connexion receiver in_ch sockaddr xhtml_sender file_sender=

    let rec listen_connexion_aux () =
      let analyse_http () = 
        Http_receiver.get_http_frame receiver () >>=(fun
          http_frame ->
             catch (service http_frame in_ch sockaddr xhtml_sender file_sender)
            (fun ex ->
              match ex with
              | _ -> fail ex
            )
            >>= (fun keep_alive -> 
              if keep_alive then
		(warning "---------------->KEEP ALIVE!<-------------------";
                listen_connexion_aux ())
                (* Pour laisser la connexion ouverte, je relance *)
              else (warning "---------------->CLOSE!<--------------";return ())
            )
        ) in
      catch analyse_http 
      (function
        |Com_buffer.End_of_file -> return ()
        |Http_error.Http_exception (_,_) as http_ex->
            (*let mes = Http_error.string_of_http_exception http_ex in
            really_write "404 Plop" (* à revoir ! *) 
	      false in_ch mes 0 
	      (String.length mes);*)
            send_error ~http_exception:http_ex xhtml_sender;
            return ()
        |ex -> fail ex
      )

    in listen_connexion_aux ()

    in 
    let wait_connexion socket =
      let rec wait_connexion_rec () =
        Lwt_unix.accept socket >>= (fun (inputchan, sockaddr) ->
	warning "NEW CONNECTION";
        let xhtml_sender =
          create_xhtml_sender ~server_name:"ploplop (Unix) (gentoo/Linux) omlet"
        inputchan 
        in
        let file_sender =
          create_file_sender ~server_name:"ploplop (Unix) (gentoo/Linux) omlet"
        inputchan
        in
	listen_connexion 
	  (Http_receiver.create inputchan) inputchan sockaddr xhtml_sender
        file_sender;
          wait_connexion_rec ()) (* je relance une autre attente *)
      in wait_connexion_rec ()
    
    in
    ((* Initialize the listening address *)
    new_socket () >>= (fun listening_socket ->
      Unix.setsockopt listening_socket Unix.SO_REUSEADDR true;
      Unix.bind listening_socket (local_addr listening_port);
      Unix.listen listening_socket 1;
      wait_connexion  listening_socket
    ))



let _ = 
  Lwt_unix.run (
    (* Initialisations *)
    (* On charge les modules *)
    (try
       Dynlink.init();
(*       Dynlink.loadfile "/opt/godi/lib/ocaml/pkg-lib/pcre/pcre.cma";
       Dynlink.loadfile "/opt/godi/lib/ocaml/site-lib/postgres/postgres.cma";
       Dynlink.loadfile "/opt/godi/lib/ocaml/std-lib/dbi/dbi.cma";
       Dynlink.loadfile "/opt/godi/lib/ocaml/std-lib/dbi/dbi_postgres.cmo";*)
       Dynlink.loadfile "../lib/db_create.cmo";
       Dynlink.loadfile "../lib/persistant.cmo";
       Dynlink.loadfile "../lib/cache.cmo";
       Dynlink.loadfile "../lib/krokodata.cmo";
       Dynlink.loadfile "../lib/krokobj.cmo";
       load_aaaaa_module ~dir:[""] ~cmo:"../lib/moduleexample.cmo";
       load_aaaaa_module ~dir:["kiko"] ~cmo:"../lib/krokoxample.cmo";
     with Aaaaa_error_while_loading m -> (warning ("Error while loading "^m)));
    listen ()
  )



